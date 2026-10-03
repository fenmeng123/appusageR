# One source-level projection owns the project CSV. Module writers provide only
# their current rows; existing annotations and upstream failures remain visible.
appusage_project_summary <- function(project_dir, fresh = NULL, write = TRUE) {
  path <- file.path(project_dir, "analytic_summary_table_proclevel-2.csv")
  old <- appusage_read_summary_csv(path)
  first <- appusage_read_summary_csv(file.path(project_dir,
    "analytic_summary_table_proclevel-1.csv"))
  if (is.null(fresh)) {
    metadata <- sort(list.files(file.path(project_dir, "proclevel-2"),
      pattern = "_proc-2[.]json$", full.names = TRUE))
    fresh <- build_qc_summary_from_metadata(metadata)
  }
  out <- if (nrow(first)) {
    reconcile_second_level_summary_cardinality(
      merge_subset_second_level_summary(old, fresh, first), tibble::tibble(), first)
  } else {
    restore_previous_matching_fields(fresh, old)
  }
  owner <- file.path(project_dir, "self_report_link_result.rds")
  if (file.exists(owner)) {
    links <- readRDS(owner)
    out <- appusage_apply_match_projection(out, links$file_matches)
  }
  if (isTRUE(write)) utils::write.csv(out, path, row.names = FALSE, na = "")
  tibble::as_tibble(out)
}

appusage_apply_match_projection <- function(summary, file_matches) {
  if (is.null(file_matches) || !nrow(file_matches) || !nrow(summary)) return(summary)
  matched <- match(normalized_summary_path(appusage_summary_proc2_paths(summary)),
    normalized_summary_path(file_matches$second_level_rda))
  # A missing path is never an identity. Resolve old records only with unique
  # source/identity keys, retaining ambiguous cases for diagnostics.
  paths <- normalized_summary_path(appusage_summary_proc2_paths(summary))
  matched[is.na(paths) | paths == ""] <- NA_integer_
  missing <- is.na(matched)
  if (any(missing)) {
    summary_key <- appusage_identity_summary_key(summary)
    file_key <- appusage_text_paste(file_matches$wenjuanxing_sequence_id,
      file_matches$filename_export_type, sep = "\r")
    ambiguous <- duplicated(file_key) | duplicated(file_key, fromLast = TRUE)
    file_key[ambiguous] <- NA_character_
    matched[missing] <- match(summary_key[missing], file_key)
  }
  summary$self_report_match_status <- NA_character_
  summary$self_report_sequence_id <- NA_integer_
  has_match <- !is.na(matched)
  summary$self_report_match_status[has_match] <-
    file_matches$self_report_match_status[matched[has_match]]
  summary$self_report_sequence_id[has_match] <-
    file_matches$self_report_sequence_id[matched[has_match]]
  summary
}

appusage_save_link_result <- function(project_dir, result) {
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(project_dir, "self_report_link_result.rds")
  # Matched questionnaire rows are retained even when no source exists.
  saveRDS(result, path)
  invisible(path)
}

appusage_publish_first_summary <- function(project_dir, summary) {
  path <- file.path(project_dir, "analytic_summary_table_proclevel-1.csv")
  old <- appusage_read_summary_csv(path)
  if (nrow(old)) {
    keep <- !normalized_summary_path(old$source_file) %in% normalized_summary_path(summary$source_file)
    all <- bind_appusage_summary_rows(summary, old[keep, , drop = FALSE])
  } else all <- summary
  all$index <- seq_len(nrow(all))
  utils::write.csv(all, path, row.names = FALSE, na = "")
  invisible(path)
}
