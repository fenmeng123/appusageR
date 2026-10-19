#' Read a manual app category dictionary
#'
#' Reads and standardizes the user's manually coded app category dictionary.
#' The dictionary must contain `App_UUID`, `App_Name`, `Level_1_Category`, and
#' `Level_2_Category`. `App_UUID` is matched against APP Usage `package_name`.
#'
#' @param path Path to an Excel dictionary file; NULL uses the bundled dictionary.
#' @param sheet Sheet passed to [readxl::read_excel()].
#' @param ... Additional arguments passed to [readxl::read_excel()].
#'
#' @return A tibble with dictionary columns and matching diagnostics as
#'   attributes.
#' @export
read_app_category_dictionary <- function(path = NULL, sheet = 1, ...) {
  if (is.null(path)) return(standardize_category_dict(appusage_default_dictionary(),
    dictionary_source = "bundled:app_category_dictionary", dictionary_sheet = "Sheet1"))
  if (!requireNamespace("readxl", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg readxl} is required to read app category dictionaries."
    ))
  }
  if (length(path) != 1 || is.na(path) || !file.exists(path)) {
    cli::cli_abort("Dictionary file does not exist: {.path {path}}")
  }
  raw <- readxl::read_excel(path, sheet = sheet, ...)
  standardize_category_dict(
    raw,
    dictionary_source = normalizePath(path, winslash = "/", mustWork = FALSE),
    dictionary_sheet = as.character(sheet)
  )
}

#' Add manual app categories to APP Usage data
#'
#' Adds `Level_1_Category` and `Level_2_Category` using a manually coded
#' dictionary. Matching first uses `package_name` against dictionary `App_UUID`,
#' then falls back to unambiguous exact app-name matches against
#' `App_Name_Repaired` and `App_Name`.
#'
#' @param data A data frame or second-level APP Usage `data` list.
#' @param dictionary NULL uses the bundled dictionary; otherwise a dictionary data frame, a standardized dictionary returned
#'   by `read_app_category_dictionary()`, or a dictionary file path.
#' @param overwrite Whether to overwrite existing non-missing category values.
#' @param policy Priority policy from [appusage_category_policy()].
#' @param profile Frozen result from [profile_uncoded_apps()], or NULL for dictionary-only matching.
#'
#' @return `data` with category columns added. Matching summaries are attached
#'   as the `app_category_summary` attribute.
#' @export
add_app_categories <- function(data, dictionary = NULL, overwrite = TRUE,
                               policy = appusage_category_policy(), profile = NULL) {
  policy <- appusage_validate_category_policy(policy)
  dictionary <- as_app_category_dictionary(dictionary)
  if (!is.null(profile)) appusage_validate_category_profile(profile, dictionary, policy)
  appusage_add_categories_prepared(data, dictionary, overwrite, policy, profile)
}

appusage_add_categories_prepared <- function(data, dictionary, overwrite, policy, profile) {

  if (is.data.frame(data)) {
    return(add_app_categories_frame(data, dictionary, overwrite = overwrite, policy = policy, profile = profile))
  }
  if (!is.list(data)) {
    cli::cli_abort(c(
      "`data` must be a data frame or a second-level APP Usage list."
    ))
  }

  out <- data
  summaries <- list()
  for (nm in names(out)) {
    has_app_id <- any(c("package_name", "app_name") %in% names(out[[nm]]))
    if (is.data.frame(out[[nm]]) && has_app_id) {
      out[[nm]] <- add_app_categories_frame(
        out[[nm]],
        dictionary,
        overwrite = overwrite, policy = policy, profile = profile
      )
      summary <- attr(out[[nm]], "app_category_summary", exact = TRUE)
      summary$grain <- nm
      summaries[[length(summaries) + 1L]] <- summary
    }
  }
  combined <- combine_category_summaries(summaries)
  keys <- lapply(out[intersect(c("event", "episode", "daily"), names(out))], function(frame) {
    if (!is.data.frame(frame) || !all(c("package_name", "app_name", "app_category_match_status") %in% names(frame))) return(NULL)
    key <- appusage_category_key(frame$package_name, frame$app_name)
    key[frame$app_category_disposition == "not_app"] <- NA_character_
    data.frame(key = key, matched = frame$app_category_match_status == "matched")
  })
  keys <- Filter(Negate(is.null), keys)
  if (length(keys)) {
    identities <- do.call(rbind, keys)
    combined$n_category_grain_app_appearances <- combined$n_category_unique_apps
    combined$n_category_unique_apps <- length(unique(stats::na.omit(identities$key)))
    combined$n_category_matched_apps <- length(unique(stats::na.omit(identities$key[identities$matched])))
    combined$n_category_unmatched_apps <- combined$n_category_unique_apps - combined$n_category_matched_apps
    combined$category_match_rate <- safe_rate(combined$n_category_matched_apps, combined$n_category_unique_apps)
  }
  attr(out, "app_category_summary") <- combined
  out
}

#' Batch add manual app categories to second-level caches
#'
#' Reads each `proc-2` RDA in a project, adds manual app categories to every
#' second-level table with app identifiers, and writes the updated `data` object
#' back to the same `proc-2` RDA file.
#'
#' @param project_dir Project folder produced by [read_appusage_batch()] and
#'   [write_second_level_batch()].
#' @param dictionary NULL uses the bundled dictionary; otherwise a data frame or file
#'   path readable by `read_app_category_dictionary()`.
#' @param overwrite Whether to overwrite existing non-missing category values.
#' @param progress Whether to print simple progress messages.
#' @param policy,profile Priority policy and optional frozen profile.
#' @param resume Reuse intact matching category contracts.
#' @param parallel,workers File-worker execution controls.
#'
#' @return Invisibly returns a tibble summary of category matching.
#' @export
write_app_categories_batch <- function(project_dir, dictionary = NULL,
                                       overwrite = TRUE, progress = TRUE,
                                       policy = appusage_category_policy(), profile = NULL,
                                       resume = TRUE, parallel = FALSE, workers = 8L) {
  if (length(project_dir) != 1L || is.na(project_dir) || !dir.exists(project_dir))
    cli::cli_abort("project_dir must be an existing project directory.")
  directory <- file.path(project_dir, "proclevel-2")
  if (!dir.exists(directory)) cli::cli_abort("Project is missing proclevel-2.")
  paths <- sort(list.files(directory, pattern = "_proc-2[.]rda$", full.names = TRUE))
  manifest <- data.frame(rda_path = paths, stringsAsFactors = FALSE)
  if (!is.null(profile)) manifest$source_id <- profile$manifest$source_id[match(
    normalized_summary_path(paths), normalized_summary_path(profile$manifest$rda_path))]
  result <- update_app_categories_cache(manifest, dictionary, policy, profile,
    overwrite = overwrite, resume = resume, parallel = parallel, workers = workers, progress = progress)
  if (any(result$action == "run")) update_category_qc_summary(project_dir)
  invisible(tibble::as_tibble(result))
}

standardize_category_dict <- function(dictionary,
                                      dictionary_source = NA_character_,
                                      dictionary_sheet = NA_character_) {
  provenance <- attr(dictionary, "dictionary_provenance", exact = TRUE)
  dictionary <- tibble::as_tibble(dictionary)
  required <- c("App_UUID", "App_Name", "Level_1_Category", "Level_2_Category")
  missing <- setdiff(required, names(dictionary))
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Dictionary is missing required column(s): ",
      appusage_text_paste(missing, collapse = ", ")
    ))
  }
  if (!"App_Name_Repaired" %in% names(dictionary)) {
    dictionary$App_Name_Repaired <- NA_character_
  }
  dictionary <- dictionary[, c(
    "App_Name", "App_Name_Repaired", "App_UUID",
    "Level_1_Category", "Level_2_Category"
  )]
  dictionary$App_Name <- category_trim(dictionary$App_Name)
  dictionary$App_Name_Repaired <- category_trim(dictionary$App_Name_Repaired)
  dictionary$App_UUID <- category_trim(dictionary$App_UUID)
  dictionary$Level_1_Category <- category_trim(dictionary$Level_1_Category)
  dictionary$Level_2_Category <- category_trim(dictionary$Level_2_Category)
  dictionary$.app_uuid_key <- standardize_package_name(dictionary$App_UUID)
  dictionary$.app_name_key <- category_name_key(dictionary$App_Name)
  dictionary$.app_name_repaired_key <- category_name_key(
    dictionary$App_Name_Repaired
  )

  if (anyNA(dictionary$Level_1_Category) || anyNA(dictionary$Level_2_Category))
    cli::cli_abort("Dictionary labels must be complete; unknown apps are not dictionary rows.")
  parents <- split(dictionary$Level_1_Category, dictionary$Level_2_Category)
  if (any(vapply(parents, function(x) length(unique(x)) > 1L, logical(1))))
    cli::cli_abort("Dictionary Level 2 categories must have a single Level 1 parent.")
  lookups <- build_category_lookups(dictionary)
  diagnostics <- dictionary_diagnostics(dictionary, lookups)
  attr(dictionary, "dictionary_provenance") <- provenance
  attr(dictionary, "dictionary_content_hash") <- appusage_object_fingerprint(as.data.frame(dictionary))
  conflict_rows <- dictionary$.app_uuid_key %in% lookups$app_uuid$conflicts$key
  attr(dictionary, "app_category_candidates") <- split(dictionary[conflict_rows, ],
    dictionary$.app_uuid_key[conflict_rows])
  attr(dictionary, "dictionary_source") <- dictionary_source
  attr(dictionary, "dictionary_sheet") <- dictionary_sheet
  attr(dictionary, "app_category_lookups") <- lookups
  attr(dictionary, "dictionary_index_hash") <- appusage_object_fingerprint(list(lookups = lookups,
    candidates = attr(dictionary, "app_category_candidates", exact = TRUE)))
  attr(dictionary, "app_category_diagnostics") <- diagnostics
  class(dictionary) <- unique(c(
    "appusage_category_dictionary",
    class(dictionary)
  ))
  dictionary
}

as_app_category_dictionary <- function(dictionary) {
  if (is.null(dictionary)) dictionary <- appusage_default_dictionary()
  if (inherits(dictionary, "appusage_category_dictionary")) {
    if (is.null(attr(dictionary, "dictionary_index_hash", exact = TRUE)))
      return(standardize_category_dict(dictionary,
        attr(dictionary, "dictionary_source", exact = TRUE), attr(dictionary, "dictionary_sheet", exact = TRUE)))
    if (!identical(attr(dictionary, "dictionary_content_hash", exact = TRUE),
        appusage_object_fingerprint(as.data.frame(dictionary))) ||
        !identical(attr(dictionary, "dictionary_index_hash", exact = TRUE),
          appusage_object_fingerprint(list(lookups = attr(dictionary, "app_category_lookups", exact = TRUE),
            candidates = attr(dictionary, "app_category_candidates", exact = TRUE)))))
      cli::cli_abort("Standardized dictionary was modified; recompile an explicit dictionary data frame.")
    return(dictionary)
  }
  if (is.character(dictionary) && length(dictionary) == 1) {
    return(read_app_category_dictionary(dictionary))
  }
  if (is.data.frame(dictionary)) {
    return(standardize_category_dict(dictionary))
  }
  cli::cli_abort("`dictionary` must be a file path or data frame.")
}

.appusage_category_index_cache <- new.env(parent = emptyenv())

build_category_lookups <- function(dictionary) {
  key <- appusage_category_fingerprint(lapply(dictionary[c(".app_uuid_key", ".app_name_key",
    ".app_name_repaired_key", "Level_1_Category", "Level_2_Category")], unname))
  if (exists(key, envir = .appusage_category_index_cache, inherits = FALSE))
    return(get(key, envir = .appusage_category_index_cache, inherits = FALSE))
  out <- list(
    app_uuid = build_category_lookup(dictionary, ".app_uuid_key"),
    app_name_repaired = build_category_lookup(
      dictionary,
      ".app_name_repaired_key"
    ),
    app_name = build_category_lookup(dictionary, ".app_name_key")
  )
  if (length(ls(.appusage_category_index_cache, all.names = TRUE)) >= 8L)
    rm(list = ls(.appusage_category_index_cache, all.names = TRUE)[[1L]], envir = .appusage_category_index_cache)
  assign(key, out, envir = .appusage_category_index_cache)
  out
}

build_category_lookup <- function(dictionary, key_col) {
  keep <- !is.na(dictionary[[key_col]]) & !is.na(dictionary$Level_1_Category) &
    !is.na(dictionary$Level_2_Category)
  working <- unique(as.data.frame(dictionary[keep,
    c(key_col, "Level_1_Category", "Level_2_Category"), drop = FALSE]))
  names(working)[[1]] <- "key"
  if (!nrow(working)) return(list(matches = empty_category_lookup(), conflicts = empty_category_conflicts()))
  counts <- table(working$key)
  multi <- names(counts)[counts > 1L]
  matches <- working[!working$key %in% multi, , drop = FALSE]
  rownames(matches) <- NULL
  conflicts <- lapply(multi, function(key) {
    values <- working[working$key == key, , drop = FALSE]
    data.frame(key = key, n_category_pairs = nrow(values),
      categories = paste(paste(values$Level_1_Category, values$Level_2_Category, sep = " / "), collapse = " | "),
      stringsAsFactors = FALSE)
  })
  list(matches = matches, conflicts = bind_data_frames(conflicts, empty_category_conflicts()))
}

add_app_categories_frame <- function(data, dictionary, overwrite,
                                     policy = appusage_category_policy(), profile = NULL) {
  out <- data
  n <- nrow(out)
  package <- if ("package_name" %in% names(out)) out$package_name else rep(NA_character_, n)
  name <- if ("app_name" %in% names(out)) out$app_name else rep(NA_character_, n)
  total <- if ("is_all_apps" %in% names(out)) out$is_all_apps else NULL
  annotations <- appusage_resolve_category_keys(package, name, dictionary, total)
  if (!is.null(profile)) {
    appusage_validate_category_profile(profile, dictionary, policy, verify = FALSE)
    idx <- match(appusage_category_key(package, name), profile$apps$app_key)
    eligible <- annotations$app_category_match_status == "unmatched" & !is.na(idx) &
      annotations$app_category_disposition != "missing_identity" &
      profile$apps$disposition[idx] != "dictionary_matched"
    annotations$app_category_disposition[eligible] <- profile$apps$disposition[idx[eligible]]
    other <- eligible & annotations$app_category_disposition == "auto_other" & isTRUE(policy$auto_other)
    annotations$Level_1_Category[other] <- policy$other_l1
    annotations$Level_2_Category[other] <- policy$other_l2
    annotations$app_category_match_method[other] <- "priority_threshold"
    annotations$app_category_match_key[other] <- appusage_category_key(package, name)[other]
  }
  if (!overwrite && all(c("Level_1_Category", "Level_2_Category") %in% names(out))) {
    preserved <- !is.na(category_trim(out$Level_1_Category)) & !is.na(category_trim(out$Level_2_Category)) &
      annotations$app_category_disposition != "not_app"
    annotations$Level_1_Category[preserved] <- as.character(out$Level_1_Category[preserved])
    annotations$Level_2_Category[preserved] <- as.character(out$Level_2_Category[preserved])
    annotations$app_category_match_status[preserved] <- "preexisting"
    annotations$app_category_disposition[preserved] <- "preexisting"
  }
  for (nm in names(annotations)) out[[nm]] <- annotations[[nm]]
  attr(out, "app_category_summary") <- summarize_category_frame(out,
    standardize_package_name(package), category_name_key(name))
  out
}

apply_category_match <- function(data, row_key, lookup, method, eligible) {
  key <- row_key
  key[!eligible] <- NA_character_
  conflict <- !is.na(key) & key %in% lookup$conflicts$key
  data$app_category_match_status[conflict] <- "conflict"
  data$app_category_match_method[conflict] <- appusage_text_paste0(method, "_conflict")
  data$app_category_match_key[conflict] <- key[conflict]

  idx <- match(key, lookup$matches$key)
  matched <- !is.na(idx) & !conflict
  data$Level_1_Category[matched] <-
    lookup$matches$Level_1_Category[idx[matched]]
  data$Level_2_Category[matched] <-
    lookup$matches$Level_2_Category[idx[matched]]
  data$app_category_match_status[matched] <- "matched"
  data$app_category_match_method[matched] <- method
  data$app_category_match_key[matched] <- key[matched]
  data
}

summarize_category_frame <- function(data, package_key, app_name_key) {
  app_key <- ifelse(
    !is.na(package_key),
    appusage_text_paste0("pkg:", package_key),
    appusage_text_paste0("name:", app_name_key)
  )
  app_key[is.na(package_key) & is.na(app_name_key)] <- NA_character_
  app_key[data$app_category_disposition == "not_app"] <- NA_character_
  matched <- data$app_category_match_status == "matched"
  conflict <- data$app_category_match_status == "conflict"
  unique_apps <- unique(stats::na.omit(app_key))
  matched_apps <- unique(stats::na.omit(app_key[matched]))
  method_counts <- table(factor(
    data$app_category_match_method[matched],
    levels = c("app_uuid", "app_name_repaired", "app_name", "app_uuid_name", "app_uuid_name_repaired")
  ))
  list(
    n_category_auto_other_rows = sum(data$app_category_disposition == "auto_other"),
    n_category_pending_rows = sum(data$app_category_disposition %in%
      c("needs_profile", "needs_review", "needs_review_conflict", "insufficient_metrics", "missing_identity")),
    n_category_not_app_rows = sum(data$app_category_disposition == "not_app"),
    n_category_name_disagreement_rows = sum(data$app_category_name_disagreement),
    n_category_rows = nrow(data),
    n_category_matched_rows = sum(matched, na.rm = TRUE),
    n_category_unmatched_rows = sum(
      data$app_category_match_status == "unmatched",
      na.rm = TRUE
    ),
    n_category_conflict_rows = sum(conflict, na.rm = TRUE),
    category_row_match_rate = safe_rate(sum(matched, na.rm = TRUE), nrow(data)),
    n_category_unique_apps = length(unique_apps),
    n_category_matched_apps = length(matched_apps),
    n_category_unmatched_apps = max(
      length(unique_apps) - length(matched_apps),
      0L
    ),
    category_match_rate = safe_rate(length(matched_apps), length(unique_apps)),
    n_category_app_uuid_rows = sum(method_counts[c("app_uuid", "app_uuid_name", "app_uuid_name_repaired")]),
    n_category_app_name_repaired_rows =
      unname(method_counts[["app_name_repaired"]]),
    n_category_app_name_rows = unname(method_counts[["app_name"]])
  )
}

combine_category_summaries <- function(summaries) {
  if (length(summaries) == 0) {
    return(empty_category_summary())
  }
  rows <- lapply(summaries, function(x) {
    x$grain <- x$grain %||% NA_character_
    as.data.frame(x, stringsAsFactors = FALSE)
  })
  frame <- tibble::as_tibble(do.call(rbind, rows))
  method_cols <- c(
    "n_category_app_uuid_rows",
    "n_category_app_name_repaired_rows",
    "n_category_app_name_rows"
  )
  list(
    n_category_auto_other_rows = sum(frame$n_category_auto_other_rows, na.rm = TRUE),
    n_category_pending_rows = sum(frame$n_category_pending_rows, na.rm = TRUE),
    n_category_not_app_rows = sum(frame$n_category_not_app_rows, na.rm = TRUE),
    n_category_name_disagreement_rows = sum(frame$n_category_name_disagreement_rows, na.rm = TRUE),
    by_grain = frame,
    n_category_rows = sum(frame$n_category_rows, na.rm = TRUE),
    n_category_matched_rows = sum(frame$n_category_matched_rows, na.rm = TRUE),
    n_category_unmatched_rows = sum(
      frame$n_category_unmatched_rows,
      na.rm = TRUE
    ),
    n_category_conflict_rows = sum(
      frame$n_category_conflict_rows,
      na.rm = TRUE
    ),
    category_row_match_rate = safe_rate(
      sum(frame$n_category_matched_rows, na.rm = TRUE),
      sum(frame$n_category_rows, na.rm = TRUE)
    ),
    n_category_unique_apps = sum(frame$n_category_unique_apps, na.rm = TRUE),
    n_category_matched_apps = sum(frame$n_category_matched_apps, na.rm = TRUE),
    n_category_unmatched_apps = sum(
      frame$n_category_unmatched_apps,
      na.rm = TRUE
    ),
    category_match_rate = safe_rate(
      sum(frame$n_category_matched_apps, na.rm = TRUE),
      sum(frame$n_category_unique_apps, na.rm = TRUE)
    ),
    n_category_app_uuid_rows = sum(frame[[method_cols[[1]]]], na.rm = TRUE),
    n_category_app_name_repaired_rows = sum(
      frame[[method_cols[[2]]]],
      na.rm = TRUE
    ),
    n_category_app_name_rows = sum(frame[[method_cols[[3]]]], na.rm = TRUE)
  )
}

write_app_categories_one <- function(second_level_rda, project_dir,
                                     dictionary, overwrite) {
  started_at <- Sys.time()
  entities <- parse_appusage_filename(second_level_rda)
  result <- tryCatch(
    {
      data <- load_appusage_data_object(second_level_rda)
      data <- add_app_categories(data, dictionary, overwrite = overwrite)
      summary <- attr(data, "app_category_summary", exact = TRUE)
      write_app_category_second_level_pair(
        data = data,
        project_dir,
        second_level_rda,
        summary,
        dictionary,
        overwrite = overwrite
      )
      list(status = "success", error = NULL, summary = summary)
    },
    error = function(e) {
      list(status = "error", error = e, summary = empty_category_summary())
    }
  )
  finished_at <- Sys.time()
  category_summary_row(
    entities = entities,
    second_level_rda = second_level_rda,
    status = result$status,
    summary = result$summary,
    error = result$error,
    started_at = started_at,
    finished_at = finished_at
  )
}

write_app_category_second_level_pair <- function(data, project_dir,
                                                 second_level_rda, summary,
                                                 dictionary, overwrite = TRUE,
                                                 policy = appusage_category_policy(), profile = NULL,
                                                 expected = NULL, metadata = NULL,
                                                 metadata_file = NULL) {
  metadata_file <- metadata_file %||% sub("[.]rda$", ".json", second_level_rda)
  if (!file.exists(metadata_file)) {
    metadata_file <- create_missing_second_level_metadata(second_level_rda, project_dir)
  }
  if (is.null(metadata)) metadata <- appusage_read_json(metadata_file, simplifyVector = FALSE)
  metadata <- update_category_metadata(
    metadata, summary, dictionary, second_level_rda, metadata_file
  )
  appusage_category_recover_pair(second_level_rda, metadata_file)
  transaction <- appusage_category_transaction_paths(
    second_level_rda, metadata_file
  )
  on.exit(
    appusage_cleanup_paths(c(transaction$temp_rda, transaction$temp_json)),
    add = TRUE
  )
  appusage_save_second_level_data(data, transaction$temp_rda)
  metadata$module_state$artifact <- appusage_artifact_signature(transaction$temp_rda)
  metadata$module_state$category <- expected %||% appusage_category_contract(dictionary, overwrite, policy, profile)
  appusage_validate_nonempty_file(
    transaction$temp_rda, "Category-enriched second-level RDA temporary artifact"
  )
  jsonlite::write_json(appusage_category_json_ready(metadata), transaction$temp_json, auto_unbox = TRUE,
    pretty = TRUE, na = "null", null = "null", digits = NA)
  appusage_validate_second_level_success_metadata(
    transaction$temp_json, second_level_rda, metadata_file
  )
  saveRDS(list(rda = second_level_rda, json = metadata_file,
    old_artifact = appusage_artifact_signature(second_level_rda),
    old_json_md5 = unname(appusage_file_md5(metadata_file)),
    new_artifact = metadata$module_state$artifact, new_contract = metadata$module_state$category),
    transaction$journal)
  appusage_publish_second_level_pair(
    transaction = transaction,
    output_file = second_level_rda,
    metadata_file = metadata_file
  )
  unlink(transaction$journal)
  invisible(metadata_file)
}

update_matching_second_level_metadata <- function(project_dir, second_level_rda,
                                                 summary, dictionary) {
  metadata_file <- second_level_metadata_path(second_level_rda)
  if (!file.exists(metadata_file)) {
    metadata_file <- create_missing_second_level_metadata(second_level_rda, project_dir)
  }
  metadata <- appusage_read_json(metadata_file, simplifyVector = TRUE)
  metadata <- update_category_metadata(
    metadata,
    summary,
    dictionary,
    second_level_rda
  )
  appusage_atomic_write_metadata_json(metadata, metadata_file)
  invisible(metadata_file)
}

update_category_metadata <- function(metadata, summary, dictionary,
                                     second_level_rda,
                                     metadata_file = sub("[.]rda$", ".json", second_level_rda)) {
  if (is.null(metadata$processing) || !is.list(metadata$processing)) {
    metadata$processing <- list()
  }
  if (is.null(metadata$counts) || !is.list(metadata$counts)) {
    metadata$counts <- list()
  }
  if (is.null(metadata$outputs) || !is.list(metadata$outputs)) {
    metadata$outputs <- list()
  }
  diagnostics <- attr(dictionary, "app_category_diagnostics", exact = TRUE)
  metadata$updated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  metadata$processing$app_category_status <- "success"
  metadata$outputs$second_level_rda <- normalizePath(
    second_level_rda,
    winslash = "/",
    mustWork = FALSE
  )
  metadata$outputs$metadata_json <- normalizePath(
    metadata_file,
    winslash = "/",
    mustWork = FALSE
  )
  if (!is.null(metadata$outputs$second_level_metadata_json))
    metadata$outputs$second_level_metadata_json <- metadata$outputs$metadata_json
  metadata$counts$n_category_matched_apps <- summary$n_category_matched_apps
  metadata$counts$category_match_rate <- summary$category_match_rate
  metadata$counts$n_category_auto_other_rows <- summary$n_category_auto_other_rows
  metadata$counts$n_category_pending_rows <- summary$n_category_pending_rows
  metadata$category_dictionary <- c(
    dictionary_metadata(dictionary),
    list(
      n_category_auto_other_rows = summary$n_category_auto_other_rows,
      n_category_pending_rows = summary$n_category_pending_rows,
      n_category_not_app_rows = summary$n_category_not_app_rows,
      n_category_name_disagreement_rows = summary$n_category_name_disagreement_rows,
      n_category_rows = summary$n_category_rows,
      n_category_matched_rows = summary$n_category_matched_rows,
      n_category_unmatched_rows = summary$n_category_unmatched_rows,
      n_category_conflict_rows = summary$n_category_conflict_rows,
      category_row_match_rate = summary$category_row_match_rate,
      n_category_unique_apps = summary$n_category_unique_apps,
      n_category_matched_apps = summary$n_category_matched_apps,
      n_category_unmatched_apps = summary$n_category_unmatched_apps,
      category_match_rate = summary$category_match_rate,
      n_category_app_uuid_rows = summary$n_category_app_uuid_rows,
      n_category_app_name_repaired_rows =
        summary$n_category_app_name_repaired_rows,
      n_category_app_name_rows = summary$n_category_app_name_rows,
      dictionary_n_app_uuid_conflicts = diagnostics$n_app_uuid_conflicts,
      dictionary_n_app_name_repaired_conflicts =
        diagnostics$n_app_name_repaired_conflicts,
      dictionary_n_app_name_conflicts = diagnostics$n_app_name_conflicts
    )
  )
  metadata
}

update_category_qc_summary <- function(project_dir) {
  proclevel_2 <- file.path(project_dir, "proclevel-2")
  if (!dir.exists(proclevel_2)) {
    return(invisible(NULL))
  }
  metadata_files <- sort(list.files(
    proclevel_2,
    pattern = "_proc-2[.]json$",
    full.names = TRUE
  ))
  if (length(metadata_files) == 0) {
    return(invisible(NULL))
  }
  summary <- appusage_project_summary(project_dir,
    fresh = build_qc_summary_from_metadata(metadata_files), write = FALSE)
  summary_file <- file.path(
    project_dir,
    "analytic_summary_table_proclevel-2.csv"
  )
  utils::write.csv(summary, summary_file, row.names = FALSE, na = "")
  write_dataset_description_json(
    project_info_from_root(project_dir),
    summary = summary,
    proclevel = 2,
    summary_file = summary_file,
    status = "success"
  )
  invisible(summary)
}

category_summary_row <- function(entities, second_level_rda, status, summary,
                                 error, started_at, finished_at) {
  data.frame(
    participant_id = entities$sub %||% NA_character_,
    detected_type = entities$type %||% NA_character_,
    status = status,
    second_level_rda = normalizePath(
      second_level_rda,
      winslash = "/",
      mustWork = FALSE
    ),
    n_category_rows = summary$n_category_rows,
    n_category_matched_rows = summary$n_category_matched_rows,
    n_category_unmatched_rows = summary$n_category_unmatched_rows,
    n_category_conflict_rows = summary$n_category_conflict_rows,
    category_row_match_rate = summary$category_row_match_rate,
    n_category_unique_apps = summary$n_category_unique_apps,
    n_category_matched_apps = summary$n_category_matched_apps,
    n_category_unmatched_apps = summary$n_category_unmatched_apps,
    category_match_rate = summary$category_match_rate,
    n_category_app_uuid_rows = summary$n_category_app_uuid_rows,
    n_category_app_name_repaired_rows =
      summary$n_category_app_name_repaired_rows,
    n_category_app_name_rows = summary$n_category_app_name_rows,
    error_message = if (is.null(error)) {
      NA_character_
    } else {
      conditionMessage(error)
    },
    started_at = format(started_at, "%Y-%m-%d %H:%M:%OS3 %z"),
    finished_at = format(finished_at, "%Y-%m-%d %H:%M:%OS3 %z"),
    elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
    stringsAsFactors = FALSE
  )
}

dictionary_metadata <- function(dictionary) {
  diagnostics <- attr(dictionary, "app_category_diagnostics", exact = TRUE)
  list(
    classifier_version = as.character(utils::packageVersion("appusageR")),
    dictionary_provenance = attr(dictionary, "dictionary_provenance", exact = TRUE),
    dictionary_version = attr(dictionary, "dictionary_provenance", exact = TRUE)$dictionary_version %||% NA_character_,
    dictionary_content_hash = attr(dictionary, "dictionary_content_hash", exact = TRUE),
    dictionary_source =
      attr(dictionary, "dictionary_source", exact = TRUE) %||% NA_character_,
    dictionary_sheet =
      attr(dictionary, "dictionary_sheet", exact = TRUE) %||% NA_character_,
    dictionary_n_rows = diagnostics$n_rows,
    dictionary_n_unique_app_uuid = diagnostics$n_unique_app_uuid,
    dictionary_n_unique_app_name = diagnostics$n_unique_app_name,
    dictionary_n_unique_app_name_repaired =
      diagnostics$n_unique_app_name_repaired
  )
}

dictionary_diagnostics <- function(dictionary, lookups = build_category_lookups(dictionary)) {
  list(
    n_rows = nrow(dictionary),
    n_unique_app_uuid = length(
      unique(stats::na.omit(dictionary$.app_uuid_key))
    ),
    n_unique_app_name = length(
      unique(stats::na.omit(dictionary$.app_name_key))
    ),
    n_unique_app_name_repaired = length(
      unique(stats::na.omit(dictionary$.app_name_repaired_key))
    ),
    n_app_uuid_conflicts = nrow(lookups$app_uuid$conflicts),
    n_app_name_repaired_conflicts = nrow(lookups$app_name_repaired$conflicts),
    n_app_name_conflicts = nrow(lookups$app_name$conflicts)
  )
}

empty_category_summary <- function() {
  list(n_category_auto_other_rows = NA_integer_, n_category_pending_rows = NA_integer_,
    n_category_not_app_rows = NA_integer_, n_category_name_disagreement_rows = NA_integer_,
    n_category_rows = NA_integer_,
    n_category_matched_rows = NA_integer_,
    n_category_unmatched_rows = NA_integer_,
    n_category_conflict_rows = NA_integer_,
    category_row_match_rate = NA_real_,
    n_category_unique_apps = NA_integer_,
    n_category_matched_apps = NA_integer_,
    n_category_unmatched_apps = NA_integer_,
    category_match_rate = NA_real_,
    n_category_app_uuid_rows = NA_integer_,
    n_category_app_name_repaired_rows = NA_integer_,
    n_category_app_name_rows = NA_integer_
  )
}

empty_category_lookup <- function() {
  data.frame(
    key = character(),
    Level_1_Category = character(),
    Level_2_Category = character(),
    stringsAsFactors = FALSE
  )
}

empty_category_conflicts <- function() {
  data.frame(
    key = character(),
    n_category_pairs = integer(),
    categories = character(),
    stringsAsFactors = FALSE
  )
}

bind_data_frames <- function(rows, empty) {
  if (length(rows) == 0) {
    return(empty)
  }
  do.call(rbind, rows)
}

category_trim <- function(x) {
  x <- stringi::stri_trim_both(as.character(x))
  x[x %in% c("", "NA", "NULL", "null", "NaN")] <- NA_character_
  x
}

category_name_key <- function(x) {
  category_trim(x)
}

safe_rate <- function(numerator, denominator) {
  if (is.na(denominator) || denominator <= 0) {
    return(NA_real_)
  }
  numerator / denominator
}
