#' Match a questionnaire against resolved APP Usage source references
#'
#' This in-memory module preserves unmatched questionnaire rows. The manifest
#' must explicitly identify eligible second-level paths; it does not open them.
#' @param self_report A questionnaire data frame.
#' @param manifest Source manifest with `source_file`, optional filename
#'   metadata, and `second_level_rda` for available research data.
#' @param project_dir Root used to make relative matched paths.
#' @param sequence_col,upload_col,submit_time_col Questionnaire column names.
#' @param export_type_priority Existing export-type preference for duplicates.
#' @return An `appusage_link_result` containing questionnaire rows, source
#'   matches, and diagnostics.
#' @export
match_appusage_self_report <- function(self_report, manifest, project_dir,
                                        sequence_col = "\u5e8f\u53f7", upload_col,
                                        submit_time_col = NULL,
                                        export_type_priority = c("line", "meta", "day", "app")) {
  if (!is.data.frame(self_report)) cli::cli_abort("`self_report` must be a data frame.")
  manifest <- appusage_matching_manifest(manifest)
  manifest$second_level_rda_relative <- vapply(manifest$second_level_rda,
    function(path) if (is_present_string(path)) appusage_relative_path(path, project_dir) else NA_character_,
    character(1))
  result <- appusage_match_self_report_table(self_report, manifest, project_dir,
    first = NULL, second = NULL, sequence_col = sequence_col, upload_col = upload_col,
    submit_time_col = submit_time_col, export_type_priority = export_type_priority,
    resolved = TRUE)
  class(result) <- c("appusage_link_result", "list")
  result
}

appusage_matching_manifest <- function(manifest) {
  manifest <- appusage_workflow_manifest(manifest)
  filename <- lapply(manifest$source_file, parse_wenjuanxing_upload_filename)
  defaults <- list(source_basename = basename(manifest$source_file),
    is_txt = appusage_text_grepl("[.]txt$", manifest$source_file, ignore.case = TRUE),
    second_level_rda = rep(NA_character_, nrow(manifest)))
  mapping <- c(wenjuanxing_sequence_id = "wenjuanxing_sequence_id",
    filename_export_type = "native_export_type_from_filename",
    native_export_created_at = "native_export_created_at",
    native_export_file_name = "native_export_file_name", uploaded_file_name = "uploaded_file_name")
  for (name in names(mapping)) defaults[[name]] <- vapply(filename,
    function(z) as.character(z[[mapping[[name]]]] %||% NA_character_), character(1))
  for (name in names(defaults)) if (!name %in% names(manifest)) manifest[[name]] <- defaults[[name]]
  manifest
}

appusage_run_matching_stage <- function(project_dir, config, first, second,
                                         adapter = NULL, publish = TRUE) {
  settings <- config$matching
  adapter <- adapter %||% appusage_matching_adapter(project_dir, settings)
  requested <- appusage_matching_request(project_dir, config, first, second, adapter = adapter)
  prior_state <- appusage_projection_state(project_dir)$matching
  owner <- file.path(project_dir, "self_report_link_result.rds")
  if (config$execution$resume && !config$execution$overwrite &&
      appusage_matching_request_valid(prior_state, requested, project_dir, "content")) {
    prior <- readRDS(owner)
    if (!is.null(adapter$workbook_read) &&
        isTRUE(adapter$workbook_read$diagnostics$warning_count == 0)) {
      prior_state$workbook_read <- adapter$workbook_read
      appusage_save_projection_state(project_dir, "matching", prior_state)
    }
    attr(prior, "stage_reused") <- TRUE
    if (publish) appusage_publish_project(project_dir, config, matching = prior, adapter = adapter)
    return(prior)
  }
  self_report <- settings$self_report %||% settings$self_report_file
  if (is.null(self_report)) cli::cli_abort("Matching requires a questionnaire table or file.")
  workbook_read <- NULL
  self_report <- if (!is.null(adapter$data)) adapter$data else if (!is.null(adapter)) {
    workbook_read <- appusage_read_self_report_workbook(self_report, sheet = adapter$self_report_sheet,
      n_max = adapter$self_report_n_max,
      guess_max = if (is.na(adapter$self_report_guess_max)) NULL else adapter$self_report_guess_max,
      col_types = if (length(adapter$self_report_col_types)) adapter$self_report_col_types else NULL,
      diagnostics_dir = file.path(project_dir, "diagnostics"))
    workbook_read$data
  } else appusage_read_self_report_rows(self_report)
  manifest <- adapter$manifest %||% appusage_matching_manifest(data.frame(source_file = first$source_file))
  missing_sources <- first$source_file[!normalized_summary_path(first$source_file) %in%
    normalized_summary_path(manifest$source_file)]
  if (length(missing_sources)) manifest <- bind_appusage_summary_rows(manifest,
    appusage_matching_manifest(data.frame(source_file = missing_sources)))
  manifest <- appusage_manifest_with_proc2_paths(manifest, first, second, project_dir)
  contract <- list(configuration = settings[setdiff(names(settings), c("self_report", "self_report_file"))],
    questionnaire = appusage_object_fingerprint(self_report),
    references = appusage_object_fingerprint(manifest),
    implementation = appusage_matching_implementation())
  result <- appusage_match_self_report_table(self_report, manifest, project_dir,
    first = first, second = second, sequence_col = settings$sequence_col,
    upload_col = settings$upload_col, submit_time_col = settings$submit_time_col,
    export_type_priority = settings$export_type_priority, resolved = TRUE,
    project_id = adapter$project_id %||% NA_character_,
    project_name = adapter$project_name %||% NA_character_)
  class(result) <- c("appusage_link_result", "list")
  result$contract <- contract
  appusage_save_link_result(project_dir, result)
  appusage_write_match_metadata(project_dir, result$diagnostics)
  appusage_save_projection_state(project_dir, "matching", list(request = requested,
    input_stat = if (is.character(settings$self_report_file)) appusage_source_stat(settings$self_report_file) else NULL,
    owner = appusage_artifact_signature(owner),
    workbook_read = {
      read <- if (!is.null(workbook_read)) workbook_read[c("diagnostics", "diagnostics_file")] else
        adapter$workbook_read
      if (isTRUE(read$diagnostics$warning_count == 0)) read else NULL
    }))
  if (publish) appusage_publish_project(project_dir, config, matching = result, adapter = adapter)
  attr(result, "stage_reused") <- FALSE
  if (!is.null(workbook_read)) {
    workbook_read$data <- NULL
    attr(result, "self_report_read") <- workbook_read
  }
  result
}
