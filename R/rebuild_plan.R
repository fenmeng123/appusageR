#' Preview targeted APP Usage cache rebuild actions
#'
#' Builds a metadata-only, one-row-per-source rebuild plan from an existing
#' project output. It reads manifests, CSV summaries and JSON metadata, never
#' loads RDA payloads, and never launches preprocessing. Selected second-level
#' rows can subsequently be passed to [rerun_second_level_project_subset()] by
#' `source_record_key`.
#'
#' @param project_dir Existing appusageR project output directory.
#' @param current_provenance Optional current implementation provenance. When
#'   omitted, it is computed from the loaded package with `tz`.
#' @param tz Effective timezone for provenance comparison. Defaults to
#'   `"Asia/Shanghai"`.
#' @param write_plan Whether to explicitly write the preview CSV. Defaults to
#'   `FALSE`; planning itself is non-mutating.
#' @param plan_file Optional preview CSV path. Defaults to
#'   `rebuild_plan_preview.csv` under `project_dir` when writing is requested.
#' @param failure_audit Optional package-failure audit data frame or CSV path.
#'   Audit evidence is filtered to the current `project_id` and matched by
#'   source path and/or summary index. It is never loaded by default.
#'
#' @return A tibble with one row per manifest source, deterministic action and
#'   reason fields, cache-pair state and provenance comparisons.
#' @export
plan_appusage_project_rebuild <- function(
    project_dir, current_provenance = NULL,
    tz = appusage_default_timezone(), write_plan = FALSE,
    plan_file = NULL, failure_audit = NULL) {
  if (length(project_dir) != 1L || is.na(project_dir) || !dir.exists(project_dir)) {
    cli::cli_abort("`project_dir` must be an existing appusageR project output directory.")
  }
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  current <- appusage_resolve_run_provenance(current_provenance, tz = tz)
  project_id <- appusage_plan_project_id(project_dir)
  audit <- appusage_plan_read_failure_audit(failure_audit, project_id)
  manifest <- appusage_plan_read_csv(file.path(
    project_dir, "diagnostics", "project_manifest.csv"
  ))
  first <- appusage_plan_read_csv(file.path(
    project_dir, "analytic_summary_table_proclevel-1.csv"
  ))
  second <- appusage_plan_read_csv(file.path(
    project_dir, "analytic_summary_table_proclevel-2.csv"
  ))
  if (nrow(manifest) > 0L && "is_txt" %in% names(manifest)) {
    manifest <- manifest[manifest$is_txt %in% TRUE, , drop = FALSE]
  }
  base <- if (nrow(manifest) > 0L) manifest else first
  if (nrow(base) == 0L) {
    cli::cli_abort("No project manifest or first-level summary rows were found.")
  }
  if (!"index" %in% names(base)) base$index <- seq_len(nrow(base))
  first_index <- appusage_plan_match_rows(base, first)
  source_keys <- appusage_plan_column_from_match(
    base, first, first_index, "source_record_key", NA_character_
  )
  source_fingerprints <- appusage_plan_column_from_match(
    base, first, first_index, "source_fingerprint", NA_character_
  )
  detected_types <- appusage_plan_column_from_match(
    base, first, first_index, "detected_type", NA_character_
  )
  source_files <- appusage_plan_column_from_match(
    base, first, first_index, "source_file", NA_character_
  )
  proc1_data_files <- appusage_plan_column_from_match(
    base, first, first_index, "data_file", NA_character_
  )
  participant_ids <- appusage_plan_column_from_match(
    base, first, first_index, "participant_id", NA_character_
  )
  native_export_created_at <- appusage_plan_column_from_match(
    base, first, first_index, "native_export_created_at", NA_character_
  )
  wenjuanxing_sequence_id <- appusage_plan_column_from_match(
    base, first, first_index, "wenjuanxing_sequence_id", NA_character_
  )
  audit_first_index <- first_index
  audit_source_files <- rep(NA_character_, nrow(base))
  audit_summary_indices <- rep(NA_integer_, nrow(base))
  audit_valid <- !is.na(audit_first_index) & audit_first_index >= 1L &
    audit_first_index <= nrow(first)
  if ("source_file" %in% names(first)) {
    audit_source_files[audit_valid] <- as.character(
      first$source_file[audit_first_index[audit_valid]]
    )
  }
  if ("index" %in% names(first)) {
    audit_summary_indices[audit_valid] <- suppressWarnings(as.integer(
      first$index[audit_first_index[audit_valid]]
    ))
  } else {
    audit_summary_indices[audit_valid] <- audit_first_index[audit_valid]
  }
  manifest_source_path <- appusage_plan_normalize_path(
    appusage_plan_vector(base, "source_file")
  )
  resolved_first_source_path <- appusage_plan_normalize_path(audit_source_files)
  first_source_path_disagreement <- !is.na(manifest_source_path) &
    !is.na(resolved_first_source_path) &
    manifest_source_path != resolved_first_source_path
  source_order <- data.frame(
    source_record_key = source_keys,
    source_fingerprint = source_fingerprints,
    source_file = source_files,
    audit_source_file = audit_source_files,
    audit_summary_index = audit_summary_indices,
    proc1_data_file = proc1_data_files,
    participant_id = participant_ids,
    wenjuanxing_sequence_id = wenjuanxing_sequence_id,
    detected_type = detected_types,
    native_export_created_at = native_export_created_at,
    index = base$index,
    stringsAsFactors = FALSE
  )
  bridge <- appusage_plan_cross_schema_mapping(source_order, second)
  second_index <- bridge$first_summary_index_by_source
  proc2_identity_count <- bridge$summary_count_by_source
  proc2_missing_row <- proc2_identity_count == 0L
  proc2_duplicate_identity <- proc2_identity_count > 1L
  proc2_extra_row_count <- sum(is.na(bridge$source_index_by_summary))
  audit_match <- appusage_plan_match_failure_audit(source_order, audit)
  valid_key <- !is.na(source_keys) & nzchar(source_keys)
  duplicate_key <- valid_key & (
    duplicated(source_keys) | duplicated(source_keys, fromLast = TRUE)
  )
  output_dir <- file.path(project_dir, "proclevel-2")

  rows <- lapply(seq_len(nrow(base)), function(i) {
    fi <- first_index[[i]]
    si <- second_index[[i]]
    first_row <- appusage_plan_row(first, fi)
    second_row <- appusage_plan_row(second, si)
    pair <- appusage_plan_pair_state(first, fi, output_dir)
    if (isTRUE(duplicate_key[[i]])) {
      pair$pair_state <- "source_key_collision"
      pair$status <- "collision"
      pair$reason <- "duplicate_source_record_key"
    }
    first_metadata <- appusage_plan_metadata(first_row, "metadata_file")
    second_metadata <- pair$metadata %||%
      appusage_plan_metadata(second_row, "second_level_metadata_file")
    first_provenance <- appusage_metadata_provenance(first_metadata)
    second_provenance <- appusage_metadata_provenance(second_metadata)
    audit_row <- if (length(audit_match$indices[[i]])) {
      audit[audit_match$indices[[i]], , drop = FALSE]
    } else {
      tibble::tibble()
    }
    failure_evidence <- appusage_plan_failure_evidence(
      first_row = first_row,
      first_metadata = first_metadata,
      audit_row = audit_row,
      audit_match_status = audit_match$status[[i]],
      audit_ambiguity = audit_match$ambiguity[[i]]
    )
    decision <- appusage_plan_rebuild_decision(
      first_row, second_row, pair,
      first_provenance, second_provenance, current,
      cardinality = list(
        missing = proc2_missing_row[[i]],
        duplicate = proc2_duplicate_identity[[i]],
        identity_count = proc2_identity_count[[i]],
        extra_count = proc2_extra_row_count
      ),
      failure_evidence = failure_evidence
    )
    cardinality_reasons <- c(
      if (proc2_missing_row[[i]]) "proc2_summary_missing_source" else character(),
      if (proc2_duplicate_identity[[i]])
        "proc2_summary_duplicate_source_identity" else character(),
      if (proc2_extra_row_count > 0L) "proc2_summary_extra_rows" else character()
    )
    data.frame(
      manifest_index = as.integer(base$index[[i]]),
      source_record_key = source_keys[[i]],
      source_fingerprint = source_fingerprints[[i]],
      resolved_first_summary_index = first_index[[i]],
      first_summary_source_path_disagreement = first_source_path_disagreement[[i]],
      participant_id = appusage_plan_first_value(
        base[i, , drop = FALSE], first_row, "participant_id"
      ),
      detected_type = detected_types[[i]],
      first_level_status = appusage_plan_value(first_row, "status"),
      second_level_status = appusage_plan_value(
        second_row, "second_level_status",
        appusage_plan_value(second_row, "status")
      ),
      current_pair_state = pair$pair_state %||% "not_applicable",
      current_pair_reason = pair$reason %||% NA_character_,
      parser_provenance_status = appusage_compare_provenance(
        first_provenance, current, "parser_implementation_fingerprint"
      ),
      second_level_provenance_status = appusage_compare_provenance(
        second_provenance, current, "second_level_implementation_fingerprint"
      ),
      source_qc_provenance_status = appusage_compare_provenance(
        second_provenance, current, "source_qc_config_fingerprint"
      ),
      timezone_provenance_status = appusage_compare_provenance(
        second_provenance, current, "effective_timezone"
      ),
      output_schema_provenance_status = appusage_compare_provenance(
        second_provenance, current, "output_schema_version"
      ),
      proc2_match_rule = bridge$match_rule_by_source[[i]],
      proc2_match_ambiguity = bridge$ambiguity_by_source[[i]],
      proc2_identity_count = proc2_identity_count[[i]],
      proc2_missing_row = proc2_missing_row[[i]],
      proc2_duplicate_identity = proc2_duplicate_identity[[i]],
      proc2_extra_row_count = proc2_extra_row_count,
      proc2_cardinality_status = if (
        proc2_missing_row[[i]] || proc2_duplicate_identity[[i]] ||
          proc2_extra_row_count > 0L
      ) "invalid" else "valid",
      proc2_cardinality_reason_codes = if (length(cardinality_reasons))
        paste(cardinality_reasons, collapse = ";") else "current",
      failure_attribution_source = failure_evidence$attribution_source,
      failure_attribution = failure_evidence$attribution,
      failure_family_evidence = failure_evidence$family,
      failure_audit_pattern = failure_evidence$audit_pattern,
      failure_audit_recommended_action = failure_evidence$recommended_action,
      failure_audit_row_id = failure_evidence$audit_row_id,
      failure_audit_match_count = audit_match$matched_count[[i]],
      failure_audit_row_ids = audit_match$row_ids[[i]],
      failure_audit_match_status = failure_evidence$audit_match_status,
      failure_audit_ambiguity = failure_evidence$audit_ambiguity,
      requested_action = decision$requested_action,
      requested_actions = decision$requested_actions,
      resume_stage = decision$resume_stage,
      reason_codes = decision$reason_codes,
      action_eligible = decision$action_eligible,
      matching_refresh_required = decision$matching_refresh_required,
      execution_helper = decision$execution_helper,
      stringsAsFactors = FALSE
    )
  })
  plan <- tibble::as_tibble(do.call(rbind, rows))
  attr(plan, "project_dir") <- project_dir
  attr(plan, "current_provenance") <- current
  if (isTRUE(write_plan)) {
    plan_file <- plan_file %||% file.path(project_dir, "rebuild_plan_preview.csv")
    appusage_atomic_write_plan_csv(plan, plan_file)
    attr(plan, "plan_file") <- normalizePath(plan_file, winslash = "/", mustWork = FALSE)
  }
  plan
}

appusage_plan_read_csv <- function(path) {
  if (!file.exists(path)) return(tibble::tibble())
  tibble::as_tibble(utils::read.csv(
    path, stringsAsFactors = FALSE, check.names = FALSE
  ))
}

appusage_plan_project_id <- function(project_dir) {
  parsed <- tryCatch(
    appusage_parse_project_folder(basename(project_dir)),
    error = function(e) list(project_id = NA_character_)
  )
  value <- as.character(parsed$project_id %||% NA_character_)
  if (is_present_string(value)) return(value)
  match <- regexec("ProjectID-([^_]+)", basename(project_dir), perl = TRUE)
  groups <- regmatches(basename(project_dir), match)[[1L]]
  if (length(groups) >= 2L) groups[[2L]] else NA_character_
}

appusage_plan_read_failure_audit <- function(failure_audit, project_id) {
  if (is.null(failure_audit)) return(tibble::tibble())
  audit <- if (is.data.frame(failure_audit)) {
    tibble::as_tibble(failure_audit)
  } else if (length(failure_audit) == 1L && is_present_string(failure_audit) &&
    file.exists(failure_audit)) {
    tibble::as_tibble(utils::read.csv(
      failure_audit, stringsAsFactors = FALSE, check.names = FALSE,
      colClasses = "character"
    ))
  } else {
    cli::cli_abort("`failure_audit` must be NULL, a data frame, or an existing CSV path.")
  }
  required <- c(
    "project_id", "summary_index", "source_file", "attribution", "pattern",
    "recommended_action"
  )
  missing <- setdiff(required, names(audit))
  if (length(missing)) {
    cli::cli_abort(paste0(
      "`failure_audit` is missing required column(s): ",
      paste(missing, collapse = ", ")
    ))
  }
  if (!is_present_string(project_id)) {
    cli::cli_abort("The current project ID could not be resolved for audit filtering.")
  }
  audit$.audit_row_id <- seq_len(nrow(audit))
  audit$project_id <- as.character(audit$project_id)
  audit <- audit[!is.na(audit$project_id) & audit$project_id == project_id, , drop = FALSE]
  tibble::as_tibble(audit)
}

appusage_plan_vector <- function(x, candidates, default = NA_character_) {
  if (!is.data.frame(x) || nrow(x) == 0L) return(character())
  out <- rep(default, nrow(x))
  for (candidate in candidates) {
    if (!candidate %in% names(x)) next
    value <- as.character(x[[candidate]])
    use <- (is.na(out) | !nzchar(out)) & !is.na(value) & nzchar(value)
    out[use] <- value[use]
  }
  out
}

appusage_plan_normalize_path <- function(x) {
  x <- as.character(x)
  out <- rep(NA_character_, length(x))
  valid <- !is.na(x) & nzchar(trimws(x))
  out[valid] <- tolower(normalizePath(
    trimws(x[valid]), winslash = "/", mustWork = FALSE
  ))
  out
}

appusage_plan_participant_type_identity <- function(x) {
  participant <- appusage_plan_vector(x, c("participant_id", "sub"))
  type <- tolower(appusage_plan_vector(
    x, c("detected_type", "native_export_type", "filename_export_type", "type")
  ))
  valid <- !is.na(participant) & nzchar(participant) & !is.na(type) & nzchar(type)
  out <- rep(NA_character_, nrow(x))
  out[valid] <- paste(participant[valid], type[valid], sep = "|")
  out
}

appusage_plan_participant_type_timestamp_identity <- function(x) {
  base <- appusage_plan_participant_type_identity(x)
  timestamp <- appusage_plan_vector(
    x, c("native_export_created_at", "native_export_timestamp")
  )
  valid <- !is.na(base) & !is.na(timestamp) & nzchar(timestamp)
  out <- rep(NA_character_, nrow(x))
  out[valid] <- paste(base[valid], timestamp[valid], sep = "|")
  out
}

appusage_plan_cross_schema_mapping <- function(source_order, second) {
  n_source <- nrow(source_order)
  n_second <- nrow(second)
  source_index <- rep(NA_integer_, n_second)
  rule <- rep(NA_character_, n_second)
  ambiguity <- rep(NA_character_, n_second)
  assign_candidates <- function(left, right, label, require_right_unique = FALSE) {
    left <- as.character(left)
    right <- as.character(right)
    left_valid <- !is.na(left) & nzchar(left)
    right_valid <- !is.na(right) & nzchar(right)
    left_count <- table(left[left_valid])
    right_count <- table(right[right_valid])
    for (j in which(is.na(source_index) & right_valid)) {
      value <- right[[j]]
      candidate <- which(left_valid & left == value)
      if (length(candidate) == 1L &&
        (!require_right_unique || identical(unname(right_count[[value]]), 1L))) {
        source_index[[j]] <<- candidate
        rule[[j]] <<- label
      } else if (length(candidate) > 1L ||
        (require_right_unique && !is.null(right_count[[value]]) &&
          right_count[[value]] > 1L)) {
        ambiguity[[j]] <<- paste0("ambiguous_", label)
      }
    }
  }
  if (n_second > 0L) {
    assign_candidates(
      appusage_plan_vector(source_order, "source_record_key"),
      appusage_plan_vector(second, "source_record_key"), "source_record_key"
    )
    assign_candidates(
      appusage_plan_vector(source_order, "source_fingerprint"),
      appusage_plan_vector(second, "source_fingerprint"), "source_fingerprint"
    )
    assign_candidates(
      appusage_plan_normalize_path(appusage_plan_vector(source_order, "source_file")),
      appusage_plan_normalize_path(appusage_plan_vector(second, "source_file")),
      "source_file"
    )
    assign_candidates(
      appusage_plan_normalize_path(appusage_plan_vector(source_order, "proc1_data_file")),
      appusage_plan_normalize_path(appusage_plan_vector(
        second, c("first_level_rda", "first_level_data_file")
      )),
      "proc1_rda_bridge"
    )
    assign_candidates(
      appusage_plan_vector(source_order, "wenjuanxing_sequence_id"),
      appusage_plan_vector(second, "wenjuanxing_sequence_id"),
      "wenjuanxing_sequence_id", require_right_unique = TRUE
    )
    assign_candidates(
      appusage_plan_participant_type_timestamp_identity(source_order),
      appusage_plan_participant_type_timestamp_identity(second),
      "participant_type_native_timestamp", require_right_unique = TRUE
    )
    assign_candidates(
      appusage_plan_participant_type_identity(source_order),
      appusage_plan_participant_type_identity(second),
      "participant_type_unique", require_right_unique = TRUE
    )
    source_group <- appusage_plan_participant_type_identity(source_order)
    second_group <- appusage_plan_participant_type_identity(second)
    pending <- which(is.na(source_index) & !is.na(second_group))
    unused <- setdiff(seq_len(n_source), unique(stats::na.omit(source_index)))
    common_groups <- intersect(
      unique(source_group[unused]), unique(second_group[pending])
    )
    common_groups <- common_groups[!is.na(common_groups)]
    for (group in common_groups) {
      source_members <- unused[source_group[unused] == group]
      second_members <- pending[second_group[pending] == group]
      if (length(source_members) > 0L &&
        length(source_members) == length(second_members)) {
        source_index[second_members] <- source_members
        rule[second_members] <- "participant_type_group_order"
        unused <- setdiff(unused, source_members)
        pending <- setdiff(pending, second_members)
      }
    }
    assign_candidates(
      appusage_plan_vector(source_order, "index"),
      appusage_plan_vector(second, c("index", "summary_index")),
      "legacy_index_unique", require_right_unique = TRUE
    )
  }
  count <- tabulate(source_index[!is.na(source_index)], nbins = n_source)
  first_index <- vapply(seq_len(n_source), function(i) {
    hit <- which(source_index == i)
    if (length(hit)) hit[[1L]] else NA_integer_
  }, integer(1))
  rule_by_source <- vapply(seq_len(n_source), function(i) {
    values <- unique(stats::na.omit(rule[source_index == i]))
    if (length(values)) paste(values, collapse = ";") else NA_character_
  }, character(1))
  ambiguity_by_source <- vapply(seq_len(n_source), function(i) {
    values <- unique(stats::na.omit(ambiguity[source_index == i]))
    if (length(values)) paste(values, collapse = ";") else NA_character_
  }, character(1))
  list(
    source_index_by_summary = source_index,
    match_rule_by_summary = rule,
    ambiguity_by_summary = ambiguity,
    first_summary_index_by_source = first_index,
    summary_count_by_source = count,
    match_rule_by_source = rule_by_source,
    ambiguity_by_source = ambiguity_by_source
  )
}

appusage_plan_match_failure_audit <- function(source_order, audit) {
  n <- nrow(source_order)
  out <- list(
    index = rep(NA_integer_, n), status = rep("audit_not_supplied", n),
    ambiguity = rep(NA_character_, n), row_id = rep(NA_integer_, n),
    indices = rep(list(integer()), n), matched_count = integer(n),
    row_ids = rep(NA_character_, n)
  )
  if (!is.data.frame(audit) || nrow(audit) == 0L) return(out)
  out$status[] <- "audit_unmatched"
  source_path <- appusage_plan_normalize_path(appusage_plan_vector(
    source_order, c("audit_source_file", "source_file")
  ))
  audit_path <- appusage_plan_normalize_path(audit$source_file)
  audit_has_path_evidence <- any(!is.na(audit_path))
  audit_index <- suppressWarnings(as.integer(as.character(audit$summary_index)))
  source_index <- suppressWarnings(as.integer(appusage_plan_vector(
    source_order, c("audit_summary_index", "index")
  )))
  used <- rep(FALSE, nrow(audit))
  for (i in seq_len(n)) {
    path_hit <- if (!is.na(source_path[[i]])) which(audit_path == source_path[[i]]) else integer()
    index_hit <- if (!is.na(source_index[[i]])) which(audit_index == source_index[[i]]) else integer()
    source_has_path <- !is.na(source_path[[i]])
    if (source_has_path && audit_has_path_evidence) {
      candidates <- path_hit
      if (length(candidates) > 1L && length(index_hit)) {
        narrowed <- intersect(candidates, index_hit)
        if (length(narrowed)) candidates <- narrowed
      }
      if (length(candidates) == 0L) {
        out$status[[i]] <- "audit_path_unmatched"
        if (length(index_hit)) {
          out$ambiguity[[i]] <- "index_fallback_blocked_by_path_evidence"
        }
        next
      }
    } else {
      candidates <- index_hit
    }
    available <- candidates[!used[candidates]]
    if (length(available) == 1L && length(candidates) == 1L) {
      selected <- available[[1L]]
      out$index[[i]] <- selected
      out$indices[[i]] <- selected
      out$status[[i]] <- if (source_has_path && audit_has_path_evidence) {
        "matched_source_file"
      } else {
        "matched_summary_index"
      }
      out$row_id[[i]] <- suppressWarnings(as.integer(audit$.audit_row_id[[selected]]))
      used[[selected]] <- TRUE
      selected_audit_index <- audit_index[[selected]]
      if (!is.na(source_index[[i]]) && !is.na(selected_audit_index) &&
        source_index[[i]] != selected_audit_index) {
        out$ambiguity[[i]] <- "source_path_authoritative_index_disagrees"
      }
    } else if (length(candidates) > 1L) {
      out$status[[i]] <- "audit_ambiguous"
      out$ambiguity[[i]] <- paste0("candidate_rows:", paste(candidates, collapse = ","))
    } else if (length(candidates) == 1L && used[candidates[[1L]]]) {
      out$status[[i]] <- "audit_row_reuse_blocked"
      out$ambiguity[[i]] <- paste0("audit_row_already_assigned:", candidates[[1L]])
    }
  }
  unused_audit <- which(!used & !is.na(audit_path))
  for (j in unused_audit) {
    source_candidates <- which(!is.na(source_path) & source_path == audit_path[[j]])
    if (length(source_candidates) > 1L && !is.na(audit_index[[j]])) {
      indexed <- source_candidates[
        !is.na(source_index[source_candidates]) &
          source_index[source_candidates] == audit_index[[j]]
      ]
      if (length(indexed) == 1L) source_candidates <- indexed
    }
    if (length(source_candidates) == 1L) {
      i <- source_candidates[[1L]]
      out$indices[[i]] <- c(out$indices[[i]], j)
      used[[j]] <- TRUE
      out$status[[i]] <- "matched_source_file_multiple_audit_rows"
      extra_note <- "multiple_audit_rows_for_resolved_source"
      out$ambiguity[[i]] <- if (is_present_string(out$ambiguity[[i]])) {
        paste(out$ambiguity[[i]], extra_note, sep = ";")
      } else {
        extra_note
      }
    }
  }
  out$indices <- lapply(out$indices, sort)
  out$matched_count <- vapply(out$indices, length, integer(1))
  out$row_ids <- vapply(out$indices, function(indices) {
    if (!length(indices)) return(NA_character_)
    paste(audit$.audit_row_id[indices], collapse = ";")
  }, character(1))
  out
}

appusage_plan_metadata_error <- function(metadata) {
  candidates <- list(
    class = list(
      c("errors", "class"), c("error", "class"), c("processing", "error_class"),
      c("processing", "first_level_error_class"), c("error_class")
    ),
    message = list(
      c("errors", "message"), c("error", "message"),
      c("processing", "error_message"), c("processing", "first_level_error_message"),
      c("error_message")
    )
  )
  find_value <- function(paths) {
    for (path in paths) {
      value <- appusage_nested_value(metadata, path)
      if (is_present_string(value)) return(as.character(value))
    }
    NA_character_
  }
  list(class = find_value(candidates$class), message = find_value(candidates$message))
}

appusage_plan_failure_evidence <- function(first_row, first_metadata,
                                           audit_row = tibble::tibble(),
                                           audit_match_status = "audit_not_supplied",
                                           audit_ambiguity = NA_character_) {
  summary_family <- tolower(as.character(appusage_plan_value(
    first_row, "failure_family", "parser_or_unknown"
  )))
  summary_attribution <- tolower(as.character(appusage_plan_value(
    first_row, "failure_attribution", NA_character_
  )))
  audit_matched <- is.data.frame(audit_row) && nrow(audit_row) >= 1L
  if (audit_matched) {
    attribution_values <- unique(stats::na.omit(as.character(audit_row$attribution)))
    pattern_values <- unique(stats::na.omit(as.character(audit_row$pattern)))
    action_values <- unique(stats::na.omit(as.character(audit_row$recommended_action)))
    conflicting <- length(attribution_values) != 1L || length(pattern_values) != 1L
    if (conflicting) {
      audit_ambiguity <- paste(
        stats::na.omit(c(audit_ambiguity, "conflicting_audit_evidence")),
        collapse = ";"
      )
    }
    return(list(
      family = if (length(pattern_values)) tolower(pattern_values[[1L]]) else NA_character_,
      attribution = if (length(attribution_values))
        tolower(attribution_values[[1L]]) else NA_character_,
      attribution_source = if (conflicting) "failure_audit_ambiguous" else "failure_audit",
      audit_pattern = if (length(pattern_values))
        paste(pattern_values, collapse = ";") else NA_character_,
      recommended_action = if (length(action_values))
        paste(action_values, collapse = ";") else NA_character_,
      audit_row_id = paste(audit_row$.audit_row_id, collapse = ";"),
      audit_match_status = audit_match_status,
      audit_ambiguity = audit_ambiguity
    ))
  }
  metadata_error <- appusage_plan_metadata_error(first_metadata)
  classified <- appusage_classify_failure_family(
    error_class = metadata_error$class,
    error_message = metadata_error$message,
    status = tolower(as.character(appusage_plan_value(first_row, "status", "error")))
  )
  cache_exists <- grepl("appusage_cache_exists|cache already exists", paste(
    metadata_error$class, metadata_error$message
  ), ignore.case = TRUE)
  if (isTRUE(cache_exists)) classified <- "cache_exists"
  metadata_informative <- is_present_string(classified) &&
    !classified %in% c("parser_or_unknown", "unknown")
  family <- if (metadata_informative) classified else summary_family
  package_families <- c(
    "memory_allocation", "parser_control_flow", "parser_error", "package_error",
    "runtime_error", "cache_exists"
  )
  attribution <- if (metadata_informative && family %in% package_families) {
    "package_code"
  } else {
    summary_attribution
  }
  list(
    family = family,
    attribution = attribution,
    attribution_source = if (metadata_informative) "first_level_json" else
      if (is_present_string(summary_family)) "first_level_summary" else "none",
    audit_pattern = NA_character_, recommended_action = NA_character_,
    audit_row_id = NA_integer_,
    audit_match_status = audit_match_status, audit_ambiguity = audit_ambiguity
  )
}

appusage_plan_identity <- function(x) {
  if (!is.data.frame(x) || nrow(x) == 0L) return(character())
  key <- if ("source_record_key" %in% names(x)) as.character(x$source_record_key) else
    rep(NA_character_, nrow(x))
  source <- if ("source_file" %in% names(x)) as.character(x$source_file) else
    rep(NA_character_, nrow(x))
  fingerprint <- if ("source_fingerprint" %in% names(x)) as.character(x$source_fingerprint) else
    rep(NA_character_, nrow(x))
  out <- ifelse(!is.na(key) & nzchar(key), paste0("key:", key), NA_character_)
  source_ok <- is.na(out) & !is.na(source) & nzchar(source)
  out[source_ok] <- paste0("source:", tolower(normalizePath(
    source[source_ok], winslash = "/", mustWork = FALSE
  )))
  fp_ok <- is.na(out) & !is.na(fingerprint) & nzchar(fingerprint)
  out[fp_ok] <- paste0("fp:", fingerprint[fp_ok])
  if ("index" %in% names(x)) {
    idx_ok <- is.na(out) & !is.na(x$index)
    out[idx_ok] <- paste0("index:", x$index[idx_ok])
  }
  out
}

appusage_plan_match_rows <- function(base, summary) {
  if (!is.data.frame(summary) || nrow(summary) == 0L) return(rep(NA_integer_, nrow(base)))
  out <- rep(NA_integer_, nrow(base))
  used <- rep(FALSE, nrow(summary))
  base_index <- suppressWarnings(as.integer(appusage_plan_vector(base, "index")))
  summary_index <- suppressWarnings(as.integer(appusage_plan_vector(
    summary, c("index", "summary_index")
  )))
  match_column <- function(column, normalize = FALSE) {
    if (!column %in% names(base) || !column %in% names(summary)) return(invisible(NULL))
    left <- as.character(base[[column]])
    right <- as.character(summary[[column]])
    if (isTRUE(normalize)) {
      left <- appusage_plan_normalize_path(left)
      right <- appusage_plan_normalize_path(right)
    }
    for (i in which(is.na(out) & !is.na(left) & nzchar(left))) {
      candidates <- which(!used & !is.na(right) & right == left[[i]])
      if (length(candidates) > 1L && !is.na(base_index[[i]])) {
        indexed <- candidates[
          !is.na(summary_index[candidates]) &
            summary_index[candidates] == base_index[[i]]
        ]
        if (length(indexed) == 1L) candidates <- indexed
      }
      if (length(candidates) == 1L) {
        out[[i]] <<- candidates[[1L]]
        used[[candidates[[1L]]]] <<- TRUE
      }
    }
    invisible(NULL)
  }
  match_column("source_record_key")
  match_column("source_fingerprint")
  match_column("source_file", normalize = TRUE)
  match_column("index")
  out
}

appusage_plan_column_from_match <- function(base, summary, index, column, default) {
  out <- if (column %in% names(base)) base[[column]] else rep(default, nrow(base))
  if (!column %in% names(summary)) return(out)
  matched <- !is.na(index)
  missing <- if (is.character(out)) is.na(out) | !nzchar(out) else is.na(out)
  use <- matched & missing
  out[use] <- summary[[column]][index[use]]
  out
}

appusage_plan_row <- function(x, index) {
  if (!is.data.frame(x) || is.na(index) || index < 1L || index > nrow(x)) {
    return(tibble::tibble())
  }
  x[index, , drop = FALSE]
}

appusage_plan_value <- function(row, column, default = NA_character_) {
  if (!is.data.frame(row) || nrow(row) == 0L || !column %in% names(row)) return(default)
  value <- row[[column]][[1L]]
  if (length(value) == 0L || is.null(value)) default else value
}

appusage_plan_first_value <- function(base_row, summary_row, column) {
  value <- appusage_plan_value(summary_row, column)
  if (is_present_string(value)) return(value)
  if (column %in% names(base_row)) as.character(base_row[[column]][[1L]]) else NA_character_
}

appusage_plan_metadata <- function(row, column) {
  path <- appusage_plan_value(row, column)
  if (!is_present_string(path) || !file.exists(path)) return(list())
  tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) list())
}

appusage_plan_pair_state <- function(first, index, output_dir) {
  if (!is.data.frame(first) || is.na(index) ||
    !identical(tolower(as.character(first$status[[index]])), "success")) {
    return(list(status = "not_applicable", pair_state = "not_applicable",
      reason = "upstream_first_level_not_success", metadata = list()
    ))
  }
  rda <- appusage_plan_value(first[index, , drop = FALSE], "data_file")
  if (!is_present_string(rda)) {
    return(list(status = "incomplete", pair_state = "missing_first_level_rda",
      reason = "missing_first_level_rda", metadata = list()
    ))
  }
  second_level_existing_cache_status(
    first_level_rda = rda, output_dir = output_dir,
    batch_summary = first, index = index
  )
}

appusage_plan_truth <- function(row, column) {
  isTRUE(as.logical(appusage_plan_value(row, column, FALSE)))
}

appusage_plan_number <- function(row, column, default = NA_real_) {
  suppressWarnings(as.numeric(appusage_plan_value(row, column, default)))
}

appusage_plan_rebuild_decision <- function(first_row, second_row, pair,
                                            first_provenance,
                                            second_provenance,
                                            current_provenance,
                                            cardinality = list(),
                                            failure_evidence = list()) {
  first_status <- tolower(as.character(appusage_plan_value(first_row, "status")))
  second_status <- tolower(as.character(appusage_plan_value(
    second_row, "second_level_status", appusage_plan_value(second_row, "status")
  )))
  type <- tolower(as.character(appusage_plan_value(first_row, "detected_type")))
  family <- tolower(as.character(
    failure_evidence$family %||% appusage_plan_value(first_row, "failure_family")
  ))
  attribution <- tolower(as.character(
    failure_evidence$attribution %||%
      appusage_plan_value(first_row, "failure_attribution")
  ))
  audit_pattern <- tolower(as.character(failure_evidence$audit_pattern %||% NA_character_))
  audit_authoritative <- identical(
    failure_evidence$attribution_source %||% "", "failure_audit"
  )
  package_attribution <- attribution %in% c("package", "package_code")
  audit_retry_patterns <- c(
    "memory_allocation", "memory_pressure_or_na_control_flow_not_retryable",
    "encoding_candidate_not_portable_on_windows"
  )
  audit_header_gap <- audit_authoritative && package_attribution &&
    identical(audit_pattern, "content_detection_or_header_variant_gap")
  audit_collision <- audit_authoritative && package_attribution &&
    identical(audit_pattern, "resume_cache_collision")
  package_retry <- if (audit_authoritative) {
    package_attribution && audit_pattern %in% audit_retry_patterns
  } else {
    identical(first_status, "error") && (
      package_attribution || family %in% c(
        "memory_allocation", "parser_control_flow", "parser_error",
        "package_error", "runtime_error", "cache_exists"
      )
    )
  }
  collision <- identical(pair$pair_state, "source_key_collision") || audit_collision
  parser_fp <- appusage_compare_provenance(
    first_provenance, current_provenance, "parser_implementation_fingerprint"
  )
  mixed_stale <- appusage_plan_truth(first_row, "mixed_content") &&
    parser_fp %in% c("stale", "missing")
  structural_critical <- appusage_plan_truth(
    first_row, "structural_quality_critical"
  )
  structural <- mixed_stale || structural_critical || audit_header_gap
  daily_mismatch <- any(c(
    appusage_plan_number(second_row, "daily_self_check_numeric_mismatch", 0),
    appusage_plan_number(second_row, "daily_self_check_duplicate_keys", 0),
    abs(appusage_plan_number(second_row, "daily_self_check_conservation_diff_ms", 0))
  ) > 0, na.rm = TRUE) ||
    appusage_plan_value(second_row, "daily_self_check_status") %in% c("error", "failed")
  second_fp <- appusage_compare_provenance(
    second_provenance, current_provenance, "second_level_implementation_fingerprint"
  )
  timezone_status <- appusage_compare_provenance(
    second_provenance, current_provenance, "effective_timezone"
  )
  line_stale <- identical(first_status, "success") && identical(type, "line") &&
    (daily_mismatch || second_fp %in% c("stale", "missing"))
  meta_stale <- identical(first_status, "success") && identical(type, "meta") && (
    second_fp %in% c("stale", "missing") || timezone_status %in% c("stale", "missing")
  )
  summary_missing <- isTRUE(cardinality$missing) || nrow(second_row) == 0L
  summary_duplicate <- isTRUE(cardinality$duplicate)
  summary_extra <- isTRUE(as.integer(cardinality$extra_count %||% 0L) > 0L)
  first_failure_missing_skip <- !identical(first_status, "success") &&
    !identical(second_status, "skipped")
  qc_status <- tolower(as.character(appusage_plan_value(second_row, "qc_status")))
  qc_fp <- appusage_compare_provenance(
    second_provenance, current_provenance, "source_qc_config_fingerprint"
  )
  qc_refresh <- identical(first_status, "success") && identical(pair$status, "complete") &&
    (!identical(qc_status, "success") || qc_fp %in% c("stale", "missing"))
  expected_path <- pair$rda_file %||% NA_character_
  recorded_path <- appusage_plan_value(second_row, "second_level_data_file",
    appusage_plan_value(second_row, "second_level_rda")
  )
  path_changed <- is_present_string(expected_path) && is_present_string(recorded_path) &&
    !identical(
      normalizePath(expected_path, winslash = "/", mustWork = FALSE),
      normalizePath(recorded_path, winslash = "/", mustWork = FALSE)
    )
  preferred_changed <- appusage_plan_truth(second_row, "preferred_source_changed")
  matching_present <- nrow(second_row) > 0L && any(vapply(names(second_row), function(name) {
    grepl("^self_report_", name) && is_present_string(second_row[[name]][[1L]])
  }, logical(1)))
  matching_refresh <- matching_present && (path_changed || preferred_changed)

  reasons <- character()
  actions <- character()
  add <- function(condition, reason, action) {
    if (isTRUE(condition)) {
      reasons <<- c(reasons, reason)
      actions <<- c(actions, action)
    }
  }
  add(package_retry, if (audit_authoritative)
    paste0("failure_audit_", audit_pattern) else
      "package_attributed_first_level_failure", "retry_first_level")
  add(collision, if (audit_collision) "failure_audit_resume_cache_collision" else
    "source_key_cache_collision", "reconcile_cache_identity")
  add(structural, if (audit_header_gap)
    "failure_audit_content_detection_or_header_variant_gap" else if (structural_critical)
      "critical_structural_quality" else "mixed_content_legacy_parser_provenance",
  "rebuild_first_and_second_level")
  add(line_stale, if (daily_mismatch) "line_daily_self_check_stale" else
    "line_second_level_implementation_stale", "rebuild_second_level")
  add(meta_stale, if (timezone_status %in% c("stale", "missing"))
    "meta_timezone_provenance_stale" else "meta_second_level_implementation_stale",
  "rebuild_second_level")
  add(summary_missing && identical(first_status, "success"),
    "proc2_summary_missing_source", "refresh_proc2_summary")
  add(first_failure_missing_skip,
    "first_level_failure_not_represented_as_skip", "refresh_proc2_summary")
  add(summary_duplicate, "proc2_summary_duplicate_source_identity",
    "refresh_proc2_summary")
  add(summary_extra, "proc2_summary_extra_rows", "refresh_proc2_summary")
  add(qc_refresh, if (!identical(qc_status, "success")) "qc_not_success" else
    "source_qc_configuration_stale", "refresh_qc")
  add(matching_refresh, if (path_changed) "matched_cache_path_changed" else
    "preferred_source_row_changed", "refresh_matching")
  action_order <- c(
    "rebuild_first_and_second_level", "retry_first_level",
    "reconcile_cache_identity", "rebuild_second_level",
    "refresh_proc2_summary", "refresh_qc", "refresh_matching"
  )
  actions <- action_order[action_order %in% unique(actions)]
  reasons <- unique(reasons)
  primary <- if (length(actions)) actions[[1L]] else "none"
  resume_stage <- if (any(actions %in% c(
    "rebuild_first_and_second_level", "retry_first_level", "reconcile_cache_identity"
  ))) "first_level" else if ("rebuild_second_level" %in% actions) {
    "second_level"
  } else if ("refresh_proc2_summary" %in% actions) {
    "summary"
  } else if ("refresh_qc" %in% actions) {
    "qc"
  } else if ("refresh_matching" %in% actions) {
    "self_report_matching"
  } else {
    "none"
  }
  helper <- if ("rebuild_second_level" %in% actions) {
    "rerun_second_level_project_subset"
  } else if ("refresh_proc2_summary" %in% actions) {
    "refresh_second_level_summary_from_metadata"
  } else if ("refresh_qc" %in% actions) {
    "write_qc_metadata_batch"
  } else {
    NA_character_
  }
  list(
    requested_action = primary,
    requested_actions = if (length(actions)) paste(actions, collapse = ";") else "none",
    resume_stage = resume_stage,
    reason_codes = if (length(reasons)) paste(reasons, collapse = ";") else "current",
    action_eligible = length(actions) > 0L,
    matching_refresh_required = matching_refresh,
    execution_helper = helper
  )
}

appusage_atomic_write_plan_csv <- function(plan, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  temporary <- tempfile("appusage-rebuild-plan-", tmpdir = dirname(path), fileext = ".csv")
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  utils::write.csv(plan, temporary, row.names = FALSE, na = "")
  check <- utils::read.csv(temporary, stringsAsFactors = FALSE, check.names = FALSE)
  if (nrow(check) != nrow(plan) || !identical(names(check), names(plan))) {
    stop("Rebuild plan preview failed read-back validation.")
  }
  if (file.exists(path)) unlink(path, force = TRUE)
  if (!file.rename(temporary, path)) stop("Could not publish rebuild plan preview.")
  invisible(normalizePath(path, winslash = "/", mustWork = FALSE))
}
