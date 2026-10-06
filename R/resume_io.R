# Call-local reuse is keyed by actual bytes, never by a path or modification
# time. Rehash on every access so writes and changes between stages are visible.
# Only validation fields and one-row projections survive a JSON read; raw text,
# QC details and scientific payloads are released immediately.
appusage_read_validation_json <- function(path) {
  context <- getOption("appusageR.runtime_context")
  if (is.null(context)) return(appusage_read_json(path, simplifyVector = TRUE))
  key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  signature <- appusage_artifact_signature(key)
  cached <- context$validation_json[[key]]
  if (!is.null(signature) && identical(signature, cached$signature)) {
    appusage_count("validation_json_reuse")
    return(cached$value)
  }
  metadata <- appusage_read_json(key, simplifyVector = TRUE)
  if (!identical(signature, appusage_artifact_signature(key))) {
    cli::cli_abort("Metadata changed during content verification: {.path {key}}")
  }
  value <- appusage_compact_validation_metadata(metadata, key)
  context$validation_json[[key]] <- list(signature = signature, value = value)
  value
}

appusage_compact_validation_metadata <- function(metadata, path) {
  keep <- c("participant_id", "participant_id_source", "identity", "processing",
    "outputs", "module_state", "source_record_key", "source_fingerprint",
    "implementation_provenance", "provenance")
  value <- metadata[intersect(keep, names(metadata))]
  value$source <- metadata$source[intersect(c("source_fingerprint", "source_cache_key"),
    names(metadata$source))]
  value$export <- metadata$export[intersect(c("detected_type", "timezone"), names(metadata$export))]
  check <- metadata$daily_aggregation_self_check
  value$daily_aggregation_self_check <- check[intersect(c("status", "n_missing_or_unmatched_source_keys",
    "n_missing_daily_duration", "n_nonmissing_numeric_mismatch", "n_episode_count_mismatch",
    "n_duplicate_daily_keys", "order_violation", "duration_conservation_difference_ms"), names(check))]
  source <- metadata$source_anomaly_qc %||% metadata$anomaly_qc$source_anomaly_qc
  # Source-QC projection reads scalar counters from these fixed sections.
  value$source_anomaly_qc <- source[intersect(c("status", "rule_version", "severity"), names(source))]
  for (section in c("line_foreground_overlap", "line_timestamp_date", "meta_cumulative_summary",
      "meta_reconstruction", "eligibility")) {
    fields <- source[[section]]
    if (is.list(fields)) value$source_anomaly_qc[[section]] <-
      lapply(fields[!vapply(fields, is.list, logical(1))], function(x) utils::head(x, 1L))
  }
  attr(value, "appusage_summary_row") <- qc_summary_row_from_loaded_metadata(metadata, path)
  class(value) <- c("appusage_validation_metadata", "list")
  value
}

appusage_runtime_invalidate <- function(paths) {
  context <- getOption("appusageR.runtime_context")
  if (is.null(context)) return(invisible(NULL))
  for (path in paths) {
    key <- normalizePath(path, winslash = "/", mustWork = FALSE)
    for (kind in c("validation_json", "summary_csv")) {
      if (exists(key, envir = context[[kind]], inherits = FALSE))
        rm(list = key, envir = context[[kind]])
    }
  }
  invisible(NULL)
}

# Extend the existing project projection control with a compact decoding index.
# This is not a scientific cache boundary: every use still checks actual owner
# bytes. A missing, damaged or incompatible index falls back to decoding JSON/CSV.
appusage_resume_index_digest <- function(payload) {
  path <- tempfile("appusage-validation-index-", fileext = ".bin")
  on.exit(unlink(path), add = TRUE)
  writeBin(serialize(payload, NULL, version = 3L), path)
  unname(tools::md5sum(path)[[1L]])
}

appusage_resume_index_implementation <- function() {
  appusage_runtime_implementation("validation_index", c("appusage_compact_validation_metadata",
    "qc_summary_row_from_loaded_metadata", "appusage_read_summary_csv"))
}

appusage_resume_index_load <- function(project_dir, context = getOption("appusageR.runtime_context")) {
  if (is.null(context)) return(invisible(NULL))
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  if (project_dir %in% context$index_projects) return(invisible(NULL))
  context$index_projects <- c(context$index_projects, project_dir)
  index <- appusage_projection_state(project_dir)$resume_index
  if (is.null(index) || !identical(index$implementation, appusage_resume_index_implementation()) ||
      !identical(index$digest, appusage_resume_index_digest(index$payload))) return(invisible(NULL))
  allowed <- function(paths, json = FALSE) {
    if (json) dirname(paths) %in% file.path(project_dir, c("proclevel-1", "proclevel-2")) &
      stringi::stri_detect_regex(paths, "_proc-[12][.]json$") else
      paths %in% file.path(project_dir, c("analytic_summary_table_proclevel-1.csv",
        "analytic_summary_table_proclevel-2.csv"))
  }
  for (kind in c("validation_json", "summary_csv")) {
    records <- index$payload[[kind]]
    if (!is.list(records) || is.null(names(records))) next
    records <- records[allowed(names(records), kind == "validation_json")]
    for (key in names(records)) context[[kind]][[key]] <- records[[key]]
  }
  appusage_count("validation_index_load")
  invisible(NULL)
}

appusage_resume_index_save <- function(project_dir, context = getOption("appusageR.runtime_context")) {
  if (is.null(context)) return(invisible(NULL))
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  records <- as.list(context$validation_json)
  keys <- names(records) %||% character()
  records <- records[dirname(keys) %in% file.path(project_dir, c("proclevel-1", "proclevel-2")) &
    stringi::stri_detect_regex(keys, "_proc-[12][.]json$") & !vapply(records, is.null, logical(1))]
  summaries <- as.list(context$summary_csv)
  summaries <- summaries[names(summaries) %in% file.path(project_dir,
    c("analytic_summary_table_proclevel-1.csv", "analytic_summary_table_proclevel-2.csv"))]
  payload <- list(validation_json = records, summary_csv = summaries)
  appusage_save_projection_state(project_dir, "resume_index", list(
    implementation = appusage_resume_index_implementation(), payload = payload,
    digest = appusage_resume_index_digest(payload)))
  invisible(NULL)
}

appusage_reuse_second_batch <- function(first, output_dir, arguments) {
  project <- infer_project_root_from_summary(first)
  if (!is_present_string(project)) return(NULL)
  options <- appusage_second_effective_options(arguments)
  requested <- appusage_research_contract(options, arguments$provenance)
  actions <- rep("blocked", nrow(first))
  for (i in which(first$status %in% "success")) {
    cache <- second_level_existing_cache_status(first$data_file[[i]], output_dir, first, i)
    cache <- appusage_second_cache_for_options(cache, options, arguments$provenance, requested = requested)
    if (!identical(cache$status, "complete")) return(NULL)
    # A label-only change still needs the existing synchronization path.
    if (!appusage_contract_equal(cache$metadata$module_state$research_data, requested)) return(NULL)
    actions[[i]] <- "reuse"
  }
  if (!appusage_projection_valid(appusage_projection_state(project)$summary, project)) return(NULL)
  old <- appusage_read_summary_csv(file.path(project, "analytic_summary_table_proclevel-2.csv"))
  at <- match(first_level_summary_key(first), second_level_summary_key(old))
  if (anyNA(at) || anyDuplicated(at)) return(NULL)
  result <- old[at, , drop = FALSE]
  attr(result, "execution_actions") <- actions
  attr(result, "stage_reused") <- TRUE
  result
}

appusage_reuse_workbook_read <- function(project, file, sheet, n_max, guess_max, col_types) {
  saved <- file.path(project, "workflow_configuration.rds")
  if (!file.exists(saved) || !is_present_string(file) || !file.exists(file)) return(NULL)
  previous <- tryCatch(readRDS(saved), error = function(e) NULL)
  if (is.null(previous)) return(NULL)
  requested <- list(self_report_sheet = sheet, self_report_n_max = n_max,
    self_report_guess_max = if (is.null(guess_max)) NA_real_ else guess_max,
    self_report_col_types = col_types %||% character())
  if (!appusage_contract_equal(previous[names(requested)], requested)) return(NULL)
  matching <- appusage_projection_state(project)$matching
  if (!appusage_contract_equal(matching$request$input,
      list(artifact = appusage_artifact_signature(file))) ||
      !appusage_artifact_valid(file.path(project, "self_report_link_result.rds"), matching$owner)) return(NULL)
  # Keep original R types (including typed NA fields). Round-tripping the
  # diagnostic through JSON would turn those NA values into NULL.
  read <- matching$workbook_read
  if (is.null(read)) return(NULL)
  path <- read$diagnostics_file
  diagnostics <- read$diagnostics
  if (is.null(diagnostics) || !identical(diagnostics$read_status, "success") ||
      !identical(normalized_summary_path(diagnostics$source_workbook), normalized_summary_path(file)) ||
      !identical(as.numeric(diagnostics$n_rows), as.numeric(previous$self_report_read$n_rows)) ||
      !identical(as.numeric(diagnostics$warning_count), 0)) return(NULL)
  list(data = NULL, diagnostics = diagnostics, diagnostics_file = path)
}
