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
#'
#' @return A tibble with one row per manifest source, deterministic action and
#'   reason fields, cache-pair state and provenance comparisons.
#' @export
plan_appusage_project_rebuild <- function(
    project_dir, current_provenance = NULL,
    tz = appusage_default_timezone(), write_plan = FALSE,
    plan_file = NULL) {
  if (length(project_dir) != 1L || is.na(project_dir) || !dir.exists(project_dir)) {
    cli::cli_abort("`project_dir` must be an existing appusageR project output directory.")
  }
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  current <- appusage_resolve_run_provenance(current_provenance, tz = tz)
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
  source_order <- data.frame(
    source_record_key = source_keys,
    source_fingerprint = source_fingerprints,
    detected_type = detected_types,
    index = base$index,
    stringsAsFactors = FALSE
  )
  second_index <- appusage_plan_match_rows(source_order, second)
  manifest_identity <- appusage_plan_identity(source_order)
  second_identity <- appusage_plan_identity(second)
  proc2_identity_count <- vapply(manifest_identity, function(identity) {
    if (is.na(identity) || !nzchar(identity)) return(0L)
    sum(!is.na(second_identity) & second_identity == identity)
  }, integer(1))
  proc2_missing_row <- proc2_identity_count == 0L
  proc2_duplicate_identity <- proc2_identity_count > 1L
  known_manifest_identity <- unique(manifest_identity[
    !is.na(manifest_identity) & nzchar(manifest_identity)
  ])
  proc2_extra_row <- is.na(second_identity) | !nzchar(second_identity) |
    !second_identity %in% known_manifest_identity
  proc2_extra_row_count <- sum(proc2_extra_row)
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
    decision <- appusage_plan_rebuild_decision(
      first_row, second_row, pair,
      first_provenance, second_provenance, current,
      cardinality = list(
        missing = proc2_missing_row[[i]],
        duplicate = proc2_duplicate_identity[[i]],
        identity_count = proc2_identity_count[[i]],
        extra_count = proc2_extra_row_count
      )
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
  match_column <- function(column, normalize = FALSE) {
    if (!column %in% names(base) || !column %in% names(summary)) return(invisible(NULL))
    left <- as.character(base[[column]])
    right <- as.character(summary[[column]])
    if (isTRUE(normalize)) {
      left <- tolower(normalizePath(left, winslash = "/", mustWork = FALSE))
      right <- tolower(normalizePath(right, winslash = "/", mustWork = FALSE))
    }
    candidate <- match(left, right)
    use <- is.na(out) & !is.na(left) & nzchar(left) & !is.na(candidate)
    out[use] <<- candidate[use]
    invisible(NULL)
  }
  match_column("source_record_key")
  match_column("source_file", normalize = TRUE)
  match_column("source_fingerprint")
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
                                            cardinality = list()) {
  first_status <- tolower(as.character(appusage_plan_value(first_row, "status")))
  second_status <- tolower(as.character(appusage_plan_value(
    second_row, "second_level_status", appusage_plan_value(second_row, "status")
  )))
  type <- tolower(as.character(appusage_plan_value(first_row, "detected_type")))
  family <- tolower(as.character(appusage_plan_value(first_row, "failure_family")))
  attribution <- tolower(as.character(appusage_plan_value(first_row, "failure_attribution")))
  package_retry <- identical(first_status, "error") && (
    identical(attribution, "package") || family %in% c(
      "memory_allocation", "parser_control_flow", "parser_error",
      "package_error", "runtime_error"
    )
  )
  collision <- identical(pair$pair_state, "source_key_collision")
  parser_fp <- appusage_compare_provenance(
    first_provenance, current_provenance, "parser_implementation_fingerprint"
  )
  mixed_stale <- appusage_plan_truth(first_row, "mixed_content") &&
    parser_fp %in% c("stale", "missing")
  structural_critical <- appusage_plan_truth(
    first_row, "structural_quality_critical"
  )
  structural <- mixed_stale || structural_critical
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
  add(package_retry, "package_attributed_first_level_failure", "retry_first_level")
  add(collision, "source_key_cache_collision", "reconcile_cache_identity")
  add(structural, if (structural_critical)
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
