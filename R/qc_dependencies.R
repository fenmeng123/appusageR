# Duration thresholds also contribute labels/counts inside research tables.
# Recompute those owned fields from proc-1, keeping all numerical values and
# category annotations fixed. A mismatch fails before publishing a mixed pair.
appusage_sync_qc_labels <- function(metadata, metadata_file, max_episode_ms,
                                    max_daily_app_ms) {
  recorded <- metadata$module_state$research_data$configuration %||%
    metadata$second_level$parameters
  if (is.null(recorded)) return(metadata)
  if (identical(as.numeric(recorded$max_episode_ms), as.numeric(max_episode_ms)) &&
      identical(as.numeric(recorded$max_daily_app_ms), as.numeric(max_daily_app_ms))) return(metadata)
  path <- infer_second_level_rda_path(metadata, metadata_file, dirname(metadata_file))
  first_path <- metadata$outputs$first_level_rda
  if (!is_present_string(first_path) || !file.exists(first_path)) {
    cli::cli_abort("Changing duration thresholds requires the first-level RDA to synchronize research anomaly labels.")
  }
  options <- recorded[intersect(names(recorded), names(formals(make_second_level_appusage)))]
  options$tz <- options$tz %||% recorded$timezone %||% metadata$processing$effective_timezone %||% "Asia/Shanghai"
  options$max_episode_ms <- max_episode_ms
  options$max_daily_app_ms <- max_daily_app_ms
  fresh <- do.call(make_second_level_appusage,
    c(list(data = load_appusage_data_object(first_path)), options))
  data <- load_appusage_data_object(path)
  for (grain in c("event", "episode", "daily")) {
    owned <- names(fresh[[grain]])[appusage_text_grepl("^anomaly_|^n_anomalies$", names(fresh[[grain]]))]
    identity <- setdiff(names(fresh[[grain]]), owned)
    same <- all(vapply(identity, function(name) identical(data[[grain]][[name]], fresh[[grain]][[name]]), logical(1)))
    if (!same || nrow(data[[grain]]) != nrow(fresh[[grain]])) {
      cli::cli_abort("Research values changed while refreshing QC labels; rebuild research_data explicitly.")
    }
    data[[grain]][owned] <- fresh[[grain]][owned]
  }
  metadata$second_level$parameters$max_episode_ms <- max_episode_ms
  metadata$second_level$parameters$max_daily_app_ms <- max_daily_app_ms
  metadata$module_state$research_data <- appusage_research_contract(
    appusage_second_effective_options(options))
  transaction <- appusage_second_level_transaction_paths(path, metadata_file)
  on.exit(appusage_cleanup_paths(c(transaction$temp_rda, transaction$temp_json)), add = TRUE)
  appusage_save_second_level_data(data, transaction$temp_rda)
  metadata$module_state$artifact <- appusage_artifact_signature(transaction$temp_rda)
  metadata$module_state$qc <- NULL
  metadata$processing$qc_status <- "not_run"
  write_metadata_json(metadata, transaction$temp_json)
  appusage_validate_second_level_success_metadata(transaction$temp_json, path, metadata_file)
  appusage_publish_second_level_pair(transaction, path, metadata_file)
  metadata
}
