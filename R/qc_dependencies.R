# Duration thresholds also contribute labels/counts inside research tables.
# Recompute those owned fields from proc-1, keeping all numerical values and
# category annotations fixed. A mismatch fails before publishing a mixed pair.
appusage_sync_qc_labels <- function(metadata, metadata_file, max_episode_ms,
                                    max_daily_app_ms, provenance = NULL) {
  recorded <- metadata$module_state$research_data$configuration %||%
    metadata$second_level$parameters
  if (is.null(recorded)) return(metadata)
  if (identical(as.numeric(recorded$max_episode_ms), as.numeric(max_episode_ms)) &&
      identical(as.numeric(recorded$max_daily_app_ms), as.numeric(max_daily_app_ms))) return(metadata)
  if (inherits(metadata, "appusage_validation_metadata"))
    metadata <- appusage_read_json(metadata_file, simplifyVector = TRUE)
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
    # Merged meta episodes also copy reconstruction warnings into parse_warning.
    # Only the existing duration-derived token may change there; original parse
    # diagnostics remain protected before this mixed-ownership field is copied.
    if (!identical(appusage_duration_parse_warnings(data[[grain]]$parse_warning),
                   appusage_duration_parse_warnings(fresh[[grain]]$parse_warning))) {
      cli::cli_abort("Parse diagnostics changed while refreshing QC labels; rebuild research_data explicitly.")
    }
    owned <- appusage_duration_label_fields(names(fresh[[grain]]))
    identity <- setdiff(names(fresh[[grain]]), owned)
    same <- all(vapply(identity, function(name) identical(data[[grain]][[name]], fresh[[grain]][[name]]), logical(1)))
    if (!same || nrow(data[[grain]]) != nrow(fresh[[grain]])) {
      cli::cli_abort("Research values changed while refreshing QC labels; rebuild research_data explicitly.")
    }
    data[[grain]][owned] <- fresh[[grain]][owned]
  }
  # This diagnostic is stored both on the episode table and its containing
  # object. Only its threshold-derived warning count belongs to this refresh.
  refresh_diagnostics <- function(existing, expected) {
    if (is.null(expected)) return(existing)
    stable <- setdiff(names(expected), "n_reconstruction_warnings")
    if (!identical(existing[stable], expected[stable])) {
      cli::cli_abort("Reconstruction diagnostics changed while refreshing QC labels; rebuild research_data explicitly.")
    }
    existing$n_reconstruction_warnings <- expected$n_reconstruction_warnings
    existing
  }
  for (grain in c("episode", "object")) {
    current <- if (grain == "object") data else data[[grain]]
    expected <- if (grain == "object") fresh else fresh[[grain]]
    attr(current, "meta_reconstruction_diagnostics") <- refresh_diagnostics(
      attr(current, "meta_reconstruction_diagnostics", exact = TRUE),
      attr(expected, "meta_reconstruction_diagnostics", exact = TRUE))
    if (grain == "object") data <- current else data[[grain]] <- current
  }
  if (!is.null(metadata$meta_reconstruction$n_reconstruction_warnings)) {
    metadata$meta_reconstruction$n_reconstruction_warnings <-
      attr(data, "meta_reconstruction_diagnostics", exact = TRUE)$n_reconstruction_warnings %||% 0L
  }
  if (!is.null(metadata$meta_daily_comparison$n_reconstruction_warnings)) {
    metadata$meta_daily_comparison$n_reconstruction_warnings <-
      summarize_meta_daily_comparison(data,
        selected_source = recorded$meta_daily_source %||% "summary",
        reconstruction_used = isTRUE(recorded$reconstruct_meta))$n_reconstruction_warnings
  }
  metadata$second_level$parameters$max_episode_ms <- max_episode_ms
  metadata$second_level$parameters$max_daily_app_ms <- max_daily_app_ms
  metadata$module_state$research_data <- appusage_research_contract(
    appusage_second_effective_options(options), provenance)
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

appusage_duration_label_fields <- function(columns) {
  # The existing meta algorithm also derives reconstruction-warning labels,
  # warning counts and summary eligibility from duration thresholds. They are
  # annotation-owned fields; durations, endpoints, pair identities and other
  # numerical research values still undergo the exact equality check above.
  unique(c(columns[appusage_text_grepl("^anomaly_|^n_anomalies$", columns)],
    intersect(columns, c("parse_warning", "reconstruction_warning", "reconstruction_warning_count",
      "summary_duration_over_24h", "analysis_eligible_daily", "analysis_ineligibility_reason"))))
}

appusage_duration_parse_warnings <- function(x) {
  if (is.null(x)) return(NULL)
  pieces <- stringi::stri_split_fixed(x, "; ", omit_empty = FALSE)
  vapply(pieces, function(values)
    compact_character_values(values[is.na(values) | values != "overlong_episode"]), character(1))
}
