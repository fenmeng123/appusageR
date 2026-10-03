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

appusage_run_matching_stage <- function(project_dir, config, first, second) {
  settings <- config$matching
  self_report <- settings$self_report %||% settings$self_report_file
  if (is.null(self_report)) cli::cli_abort("Matching requires a questionnaire table or file.")
  self_report <- appusage_read_self_report_rows(self_report)
  manifest <- appusage_matching_manifest(data.frame(source_file = first$source_file))
  manifest <- appusage_manifest_with_proc2_paths(manifest, first, second, project_dir)
  contract <- list(configuration = settings[setdiff(names(settings), c("self_report", "self_report_file"))],
    questionnaire = appusage_object_fingerprint(self_report),
    references = appusage_object_fingerprint(manifest),
    implementation = appusage_function_fingerprint(c("appusage_match_self_report_table",
      "extract_wenjuanxing_upload_filenames", "appusage_match_one_self_report_row",
      "appusage_resolve_duplicate_manifest_candidates", "appusage_build_manifest_match_index")))
  owner <- file.path(project_dir, "self_report_link_result.rds")
  if (file.exists(owner) && config$execution$resume && !config$execution$overwrite) {
    prior <- tryCatch(readRDS(owner), error = function(e) NULL)
    if (appusage_contract_equal(prior$contract, contract)) return(prior)
  }
  result <- match_appusage_self_report(self_report, manifest, project_dir,
    settings$sequence_col, settings$upload_col, settings$submit_time_col,
    settings$export_type_priority)
  result$contract <- contract
  appusage_save_link_result(project_dir, result)
  appusage_write_match_metadata(project_dir, result$diagnostics)
  appusage_project_summary(project_dir)
  result
}
