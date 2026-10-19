# Saved data are measured, never reconstructed, by the classification module.
appusage_category_usage_table <- function(x) {
  if (is.data.frame(x)) return(x)
  daily <- x$daily
  if (!is.data.frame(daily)) return(data.frame())
  if (!"daily_source" %in% names(daily)) return(daily)
  typ <- unique(stats::na.omit(as.character(daily$source_export_type)))
  if (!length(typ)) typ <- unique(stats::na.omit(as.character(x$episode$source_export_type)))
  source <- if (length(typ) == 1L) switch(typ,
    line = "line_episodes", meta = "meta_episodes", day = "day_export", app = "app_export") else NULL
  if (is.null(source)) return(data.frame())
  table <- daily[daily$daily_source %in% source, , drop = FALSE]
  if (!nrow(table) && identical(source, "meta_episodes")) {
    expected <- attr(x, "daily_aggregation_expected", exact = TRUE)$meta_episodes$rows
    if (is.data.frame(expected)) table <- expected
  }
  table
}

appusage_category_inventory <- function(x, source_id) {
  frames <- if (is.data.frame(x)) list(x) else x[intersect(c("event", "episode", "daily"), names(x))]
  rows <- lapply(frames, function(f) {
    if (!is.data.frame(f) || !nrow(f) || !all(c("package_name", "app_name") %in% names(f))) return(NULL)
    total <- standardize_package_name(f$package_name) %in% "ALL"
    if ("is_all_apps" %in% names(f)) total <- total | f$is_all_apps %in% TRUE
    keep <- !total
    data.frame(package_name = as.character(f$package_name[keep]), app_name = as.character(f$app_name[keep]),
      stringsAsFactors = FALSE)
  })
  rows <- Filter(Negate(is.null), rows)
  out <- if (length(rows)) unique(do.call(rbind, rows)) else data.frame(package_name = character(), app_name = character())
  out$app_key <- appusage_category_key(out$package_name, out$app_name)
  out$source_id <- rep(source_id, nrow(out))
  rownames(out) <- NULL
  out
}

appusage_category_measure_source <- function(x, source_id) {
  table <- appusage_category_usage_table(x)
  empty <- data.frame(source_id = character(), app_key = character(), duration_ms = double(), share = double())
  reason <- "no_saved_canonical_table"
  if (nrow(table) && all(c("duration_ms", "activity_type", "package_name", "app_name") %in% names(table))) {
    total <- standardize_package_name(table$package_name) %in% "ALL"
    if ("is_all_apps" %in% names(table)) total <- total | table$is_all_apps %in% TRUE
    f <- table[table$activity_type %in% "foreground" & !total, , drop = FALSE]
    bad <- !is.finite(f$duration_ms) | f$duration_ms < 0
    if ("date" %in% names(f)) bad <- bad | is.na(f$date)
    if (!nrow(f)) reason <- "no_foreground_records" else if (any(bad)) reason <- "invalid_foreground_metric" else {
      keys <- appusage_category_key(f$package_name, f$app_name)
      denominator <- sum(f$duration_ms)
      if (denominator > 0) {
        valid <- !is.na(keys)
        sums <- if (any(valid)) rowsum(f$duration_ms[valid], keys[valid], reorder = FALSE) else
          matrix(numeric(), ncol = 1L, dimnames = list(character(), NULL))
        empty <- data.frame(source_id = rep(source_id, nrow(sums)), app_key = rownames(sums) %||% character(),
          duration_ms = as.numeric(sums[, 1]), share = as.numeric(sums[, 1]) / denominator,
          stringsAsFactors = FALSE)
        reason <- if (any(!valid & f$duration_ms > 0)) "missing_foreground_identity" else "ok"
      } else reason <- "zero_foreground_total"
    }
  }
  typ <- if (is.data.frame(table) && "source_export_type" %in% names(table))
    unique(stats::na.omit(as.character(table$source_export_type))) else character()
  if (!length(typ) && is.list(x) && !is.data.frame(x))
    typ <- unique(stats::na.omit(as.character(x$daily$source_export_type)))
  list(metrics = empty, status = data.frame(source_id = source_id, metric_status = reason,
    export_type = if (length(typ) == 1L) typ else NA_character_,
    measurement_scope = if (identical(typ, "app")) "exported_app_scope" else "recorded_source_foreground",
    stringsAsFactors = FALSE))
}

appusage_category_manifest <- function(manifest) {
  required <- c("source_id", "person_id", "observation_id", "selected_primary")
  if (!is.data.frame(manifest) || !all(required %in% names(manifest)))
    cli::cli_abort("Category profiling requires source_id, person_id, observation_id and selected_primary.")
  manifest <- as.data.frame(manifest, stringsAsFactors = FALSE)
  for (nm in required[1:3]) {
    if (!is.character(manifest[[nm]]) || (nm == "source_id" &&
        (anyNA(manifest[[nm]]) || any(!nzchar(manifest[[nm]])))))
      cli::cli_abort("Category source_id must be explicit; person/observation identities must be character values.")
  }
  if (!is.logical(manifest$selected_primary) || anyNA(manifest$selected_primary))
    cli::cli_abort("Category selected_primary must be logical without missing values.")
  # Legitimate cross-bank copies have the same observation/source/person.
  key <- paste(manifest$observation_id, manifest$source_id, sep = "\r")
  groups <- split(manifest$person_id, key)
  if (any(vapply(groups, function(v) length(unique(v)) != 1L, logical(1))))
    cli::cli_abort("One observation/source cannot refer to different people.")
  roles <- split(manifest$selected_primary, key)
  if (any(vapply(roles, function(v) length(unique(v)) != 1L, logical(1))))
    cli::cli_abort("Cross-bank copies must agree on the selected primary role.")
  manifest <- manifest[!duplicated(key), , drop = FALSE]
    primary <- manifest[manifest$selected_primary, , drop = FALSE]
  known_observation <- !is.na(primary$observation_id) & nzchar(primary$observation_id)
  if (anyDuplicated(primary$observation_id[known_observation])) cli::cli_abort("Each observation must have one primary source.")
  rownames(manifest) <- NULL
  manifest
}

#' Profile unknown applications using explicit person and primary-source identities
#'
#' Only saved canonical foreground measurements are used. Repeated observations
#' use maximum within-person primary-file share; prevalence requires positive
#' foreground time. Missing measurements are never imputed to zero.
#' @param manifest Data frame with source_id, person_id, observation_id,
#'   selected_primary and rda_path (unless data or precomputed tables are supplied).
#'   Missing person/observation identities remain pending and are excluded from
#'   population denominators; source_id must always be explicit.
#' @param dictionary NULL uses the bundled dictionary.
#' @param policy Policy from [appusage_category_policy()].
#' @param data Optional named list of saved data objects keyed by source_id.
#' @param inventory,metrics,source_status Optional source tables for checkpointed
#'   extraction. Supply all three together; they must cover the manifest sources.
#' @return An `appusage_category_profile` with apps, review, person_app, source_status
#'   and an immutable classification contract.
#' @examples
#' dictionary <- data.frame(App_UUID = "org.example.known", App_Name = "Known",
#'   Level_1_Category = "Utilities and Tools", Level_2_Category = "System Utilities")
#' sources <- data.frame(source_id = "s1", person_id = "p1", observation_id = "o1",
#'   selected_primary = TRUE)
#' daily <- data.frame(date = as.Date("2024-01-01"),
#'   package_name = c("org.example.known", "org.example.new"),
#'   app_name = c("Known", "New"), activity_type = "foreground",
#'   duration_ms = c(90000, 10000))
#' profile <- profile_uncoded_apps(sources, dictionary, data = list(s1 = daily))
#' profile$apps[, c("app_key", "disposition")]
#' @export
profile_uncoded_apps <- function(manifest, dictionary = NULL,
                                policy = appusage_category_policy(), data = NULL,
                                inventory = NULL, metrics = NULL, source_status = NULL) {
  manifest <- appusage_category_manifest(manifest)
  dictionary <- as_app_category_dictionary(dictionary)
  policy <- appusage_validate_category_policy(policy)
  supplied <- c(!is.null(inventory), !is.null(metrics), !is.null(source_status))
  if (any(supplied) && !all(supplied)) cli::cli_abort("Supply inventory, metrics and source_status together.")
  if (!all(supplied)) {
    sources <- manifest[!duplicated(manifest$source_id), , drop = FALSE]
    inv <- met <- states <- vector("list", nrow(sources))
    for (i in seq_len(nrow(sources))) {
      id <- sources$source_id[[i]]
      x <- if (!is.null(data)) data[[id]] else {
        if (!"rda_path" %in% names(sources)) cli::cli_abort("Profiling needs rda_path or supplied data.")
        load_appusage_data_object(sources$rda_path[[i]])
      }
      if (is.null(x)) cli::cli_abort("Missing saved data for source {id}.")
      inv[[i]] <- appusage_category_inventory(x, id)
      one <- appusage_category_measure_source(x, id)
      met[[i]] <- one$metrics
      states[[i]] <- one$status
      if (is.null(data) && "rda_path" %in% names(sources)) {
        signature <- appusage_artifact_signature(sources$rda_path[[i]])
        states[[i]]$rda_md5 <- signature$md5
        states[[i]]$rda_size <- signature$size
      }
    }
    inventory <- do.call(rbind, inv); metrics <- do.call(rbind, met); source_status <- do.call(rbind, states)
  }
  if (!all(c("source_id", "app_key", "package_name", "app_name") %in% names(inventory)) ||
      !all(c("source_id", "app_key", "duration_ms", "share") %in% names(metrics)) ||
      !all(c("source_id", "metric_status") %in% names(source_status)))
    cli::cli_abort("Precomputed category source tables have invalid columns.")
  ids <- unique(manifest$source_id)
  if (anyDuplicated(source_status$source_id) || !setequal(ids, source_status$source_id) ||
      any(!inventory$source_id %in% ids) || any(!metrics$source_id %in% ids))
    cli::cli_abort("Profile source tables must exactly cover manifest sources.")
  if (any(!is.finite(metrics$duration_ms) | metrics$duration_ms < 0 | !is.finite(metrics$share) |
      metrics$share < 0 | metrics$share > 1)) cli::cli_abort("Invalid precomputed category metrics.")
  if (anyDuplicated(paste(metrics$source_id, metrics$app_key, sep = "\r")))
    cli::cli_abort("Each source/app measurement must occur once.")
  annotations <- appusage_resolve_category_keys(inventory$package_name, inventory$app_name, dictionary)
  inventory$match_status <- annotations$app_category_match_status
  inventory$disposition <- annotations$app_category_disposition
  primary <- manifest[manifest$selected_primary, , drop = FALSE]
  primary$metric_status <- source_status$metric_status[match(primary$source_id, source_status$source_id)]
  primary$identity_complete <- !is.na(primary$person_id) & nzchar(trimws(primary$person_id)) &
    !is.na(primary$observation_id) & nzchar(trimws(primary$observation_id))
  good <- primary[primary$metric_status == "ok" & primary$identity_complete, , drop = FALSE]
  people <- unique(good$person_id)
  m <- merge(metrics, good[c("source_id", "person_id", "observation_id")], by = "source_id", sort = FALSE)
  if (nrow(m)) {
    person_app <- stats::aggregate(m[c("share", "duration_ms")],
      m[c("person_id", "app_key")], function(v) max(v))
  } else person_app <- data.frame(person_id = character(), app_key = character(), share = double(), duration_ms = double())
  m <- m[m$duration_ms > 0, , drop = FALSE]
  # Duration is reported at observation level; do not sum maximum shares as durations.
  groups <- split(seq_len(nrow(inventory)), inventory$app_key)
  person_groups <- split(seq_len(nrow(person_app)), person_app$app_key)
  metric_groups <- split(seq_len(nrow(m)), m$app_key)
  primary_groups <- split(seq_len(nrow(primary)), primary$source_id)
  complete_measurements <- merge(metrics, good["source_id"], by = "source_id", sort = FALSE)
  measured_groups <- split(complete_measurements$source_id, complete_measurements$app_key)
  profile_rows <- lapply(names(groups), function(key) {
    rows <- inventory[groups[[key]], , drop = FALSE]
    pa <- person_app[person_groups[[key]] %||% integer(), , drop = FALSE]
    app_primary <- primary[unlist(primary_groups[unique(rows$source_id)], use.names = FALSE), , drop = FALSE]
    measured_sources <- unique(measured_groups[[key]])
    unmeasured <- !nrow(app_primary) || any(!app_primary$identity_complete) || any(app_primary$metric_status != "ok") ||
      any(!app_primary$source_id %in% measured_sources)
    positive_people <- sum(pa$duration_ms > 0)
    prevalence <- if (length(people)) positive_people / length(people) else NA_real_
    max_share <- if (nrow(pa)) max(pa$share) else NA_real_
    conflict <- any(rows$match_status == "conflict")
    unknown <- any(rows$match_status == "unmatched")
    high <- (!is.na(max_share) && max_share >= policy$share_threshold) ||
      (!is.na(prevalence) && prevalence >= policy$prevalence_threshold)
    disposition <- if (conflict) "needs_review_conflict" else if (!unknown) "dictionary_matched" else
      if (high) "needs_review" else if (unmeasured || !length(people)) "insufficient_metrics" else
        if (policy$auto_other) "auto_other" else "needs_review"
    apk <- if (startsWith(key, "pkg:")) substring(key, 5) else NA_character_
    candidate <- if (!is.na(apk)) dictionary[dictionary$.app_uuid_key %in% apk, , drop = FALSE] else dictionary[0, ]
    if (!nrow(candidate)) {
      candidate <- dictionary[dictionary$.app_name_repaired_key %in% category_name_key(rows$app_name), , drop = FALSE]
      if (!nrow(candidate)) candidate <- dictionary[dictionary$.app_name_key %in% category_name_key(rows$app_name), , drop = FALSE]
    }
    pairs <- unique(candidate[c("Level_1_Category", "Level_2_Category")])
    data.frame(app_key = key, package_name = paste(unique(stats::na.omit(rows$package_name)), collapse = " | "),
      app_names = paste(unique(stats::na.omit(rows$app_name)), collapse = " | "),
      n_people_positive = positive_people, n_people_denominator = length(people), prevalence = prevalence,
      max_person_share = max_share, primary_foreground_ms = sum(m$duration_ms[metric_groups[[key]] %||% integer()]),
      has_unmeasured_primary = unmeasured, disposition = disposition,
      n_primary_observations_missing_identity = sum(!app_primary$identity_complete),
      contains_app_scope_exports = any(source_status$export_type[source_status$source_id %in% rows$source_id] %in% "app"),
      dictionary_candidates = paste(paste(pairs$Level_1_Category, pairs$Level_2_Category, sep = " / "), collapse = " | "),
      proposed_l1 = NA_character_, proposed_l2 = NA_character_,
      approved_l1 = NA_character_, approved_l2 = NA_character_, approval_status = "pending",
      review_reason = if (conflict) "conflicting_dictionary_candidates" else if (high)
        paste(c(if (!is.na(max_share) && max_share >= policy$share_threshold) "share_threshold",
          if (!is.na(prevalence) && prevalence >= policy$prevalence_threshold) "prevalence_threshold"), collapse = "|") else
          if (any(!app_primary$identity_complete)) "insufficient_statistical_identity" else
            if (unmeasured) "insufficient_or_alternative_only_metrics" else "below_both_thresholds",
      stringsAsFactors = FALSE)
  })
  apps <- if (length(profile_rows)) do.call(rbind, profile_rows) else data.frame(app_key = character(), disposition = character())
  contract <- list(version = "category-profile-1", dictionary = attr(dictionary, "dictionary_content_hash"),
    policy = appusage_object_fingerprint(unclass(policy)),
    corpus = appusage_category_fingerprint(manifest), sources = appusage_category_fingerprint(source_status),
    measurements = appusage_category_fingerprint(list(
      source_status = source_status, metrics = metrics, inventory = inventory)),
    decisions = appusage_category_fingerprint(apps))
  structure(list(apps = apps, review = apps[apps$disposition %in% c("needs_review", "needs_review_conflict", "insufficient_metrics"), , drop = FALSE],
    manifest = manifest, source_ids = ids,
    person_app = person_app, source_status = source_status, contract = contract,
    fingerprint = appusage_object_fingerprint(contract)), class = c("appusage_category_profile", "list"))
}

appusage_validate_category_profile <- function(profile, dictionary, policy, verify = TRUE) {
  if (!inherits(profile, "appusage_category_profile") ||
      !identical(profile$contract$dictionary, attr(dictionary, "dictionary_content_hash")) ||
      !identical(profile$contract$policy, appusage_object_fingerprint(unclass(policy))) ||
      (verify && (!identical(profile$contract$decisions, appusage_category_fingerprint(profile$apps)) ||
        !identical(profile$contract$sources, appusage_category_fingerprint(profile$source_status)) ||
        !identical(profile$contract$corpus, appusage_category_fingerprint(profile$manifest)) ||
        !identical(profile$source_ids, unique(profile$manifest$source_id)) ||
        !identical(profile$fingerprint, appusage_object_fingerprint(profile$contract)))))
    cli::cli_abort("Category profile does not match the dictionary and priority policy.")
  invisible(profile)
}

appusage_category_fingerprint <- function(x) {
  file <- tempfile("category-contract-")
  on.exit(unlink(file), add = TRUE)
  saveRDS(x, file, compress = FALSE, version = 2)
  unname(as.character(tools::md5sum(file)))
}
