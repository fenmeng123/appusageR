# One source-level projection owns the project CSV. Module writers provide only
# their current rows; existing annotations and upstream failures remain visible.
appusage_project_summary <- function(project_dir, fresh = NULL, write = TRUE) {
  appusage_count("summary_projection", 0)
  path <- file.path(project_dir, "analytic_summary_table_proclevel-2.csv")
  old <- appusage_read_summary_csv(path)
  if (isTRUE(attr(fresh, "stage_reused")) && nrow(old) &&
      appusage_projection_valid(appusage_projection_state(project_dir)$summary, project_dir)) {
    attr(old, "stage_reused") <- TRUE
    return(old)
  }
  first <- appusage_read_summary_csv(file.path(project_dir,
    "analytic_summary_table_proclevel-1.csv"))
  if (is.null(fresh)) {
    metadata <- sort(list.files(file.path(project_dir, "proclevel-2"),
      pattern = "_proc-2[.]json$", full.names = TRUE))
    fresh <- build_qc_summary_from_metadata(metadata)
    # Failed proc-1 sources have no proc-2 JSON. Reconstruct their standard
    # blocked rows through the batch service instead of a generic fallback.
    failed <- which(!first[["status"]] %in% "success")
    if (length(failed)) {
      indexed_first <- first
      if (is.null(indexed_first[["index"]])) indexed_first$index <- seq_len(nrow(first))
      skipped <- data.frame(index = indexed_first$index[failed], status = "skipped",
        skip_reason = "upstream_first_level_error", stringsAsFactors = FALSE)
      fresh <- bind_appusage_summary_rows(fresh,
        second_level_skipped_summary_rows(skipped, indexed_first))
    }
  }
  out <- if (nrow(first)) {
    reconcile_second_level_summary_cardinality(
      merge_subset_second_level_summary(old, fresh, first), tibble::tibble(), first)
  } else {
    restore_previous_matching_fields(fresh, old)
  }
  projection <- appusage_projection_state(project_dir)$summary
  restore_annotations <- !nrow(old) && length(projection$columns) &&
    appusage_contract_equal(projection$dependencies, appusage_projection_dependencies(project_dir))
  restore_layout <- restore_annotations && identical(projection$implementation,
    appusage_projection_implementation())
  if (restore_annotations && !is.null(projection$annotations$source_record_key)) {
    at <- match(out$source_record_key, projection$annotations$source_record_key)
    for (name in setdiff(names(projection$annotations), "source_record_key"))
      out[[name]] <- projection$annotations[[name]][at]
  }
  owner <- file.path(project_dir, "self_report_link_result.rds")
  if (file.exists(owner)) {
    links <- readRDS(owner)
    out <- appusage_apply_match_projection(out, links$file_matches)
  }
  if (restore_layout) {
    for (name in setdiff(projection$columns, names(out))) out[[name]] <- NA
    out <- out[c(projection$columns, setdiff(names(out), projection$columns))]
  }
  if (isTRUE(write)) appusage_write_csv_if_changed(out, path)
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
  # The WJX projection has always placed diagnostic references after matching
  # fields. Preserve that layout when matching is first added by this service.
  diagnostics <- intersect(c("diagnostic_report", "diagnostic_json"), names(summary))
  summary[c(setdiff(names(summary), diagnostics), diagnostics)]
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
  appusage_write_csv_if_changed(all, path)
  invisible(path)
}
