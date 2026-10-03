#' Build a validated APP Usage workflow configuration
#'
#' Scientific settings are separate from execution controls. Omitted fields
#' retain package defaults; unknown fields are rejected. Configurations contain
#' no run state and can be saved with [saveRDS()].
#' @param parse,time,reconstruction,daily,qc,category,matching,execution Named
#'   lists overriding settings in the respective module. See the workflow
#'   vignette for the supported fields.
#' @return An `appusage_config` named list.
#' @export
appusage_config <- function(parse = list(), time = list(), reconstruction = list(),
                            daily = list(), qc = list(), category = list(),
                            matching = list(), execution = list()) {
  defaults <- list(
    parse = list(type = "auto", input = "file", encoding = "auto", parser_strict = TRUE),
    time = list(tz = "Asia/Shanghai"),
    reconstruction = list(include_collection_app = TRUE, reconstruct_meta = TRUE,
      meta_pairing = "package", meta_start_event_types = 1,
      meta_end_event_types = c(2, 23), merge_meta_episodes = TRUE,
      meta_episode_merge_gap_ms = 30000),
    daily = list(meta_daily_source = "summary"),
    qc = list(enabled = TRUE, require_all_weekdays = TRUE, min_nonempty_days = 7,
      use_all_apps_row = FALSE, drop_likely_total_all_rows = TRUE,
      all_row_tolerance = 0.10, max_episode_ms = 86400000,
      max_daily_app_ms = 86400000, max_daily_total_ms = 86400000,
      max_export_lookback_days = 31, meta_diff_abs_ms = 60000, meta_diff_ratio = 0.20),
    category = list(enabled = FALSE, dictionary = NULL, overwrite = TRUE),
    matching = list(enabled = FALSE, self_report = NULL, self_report_file = NULL,
      sequence_col = "\u5e8f\u53f7", upload_col = NULL, submit_time_col = NULL,
      export_type_priority = c("line", "meta", "day", "app")),
    execution = list(parallel = FALSE, workers = 1L, strict = FALSE, progress = TRUE,
      resume = TRUE, overwrite = FALSE, checkpoint_every = 100L,
      source_verification = "content")
  )
  supplied <- list(parse = parse, time = time, reconstruction = reconstruction,
    daily = daily, qc = qc, category = category, matching = matching, execution = execution)
  out <- lapply(names(defaults), function(section) {
    appusage_config_merge(defaults[[section]], supplied[[section]], section)
  })
  names(out) <- names(defaults)
  out$time$tz <- appusage_resolve_timezone(out$time$tz)
  if (!is.character(out$parse$encoding) || length(out$parse$encoding) != 1L ||
      is.na(out$parse$encoding) || !appusage_text_nzchar(out$parse$encoding)) {
    cli::cli_abort("`parse$encoding` must be one nonempty character value.")
  }
  out$parse$type <- match.arg(out$parse$type, c("auto", "line", "meta", "day", "app"))
  out$parse$input <- match.arg(out$parse$input, c("file", "text", "lines"))
  out$reconstruction$meta_pairing <- match.arg(out$reconstruction$meta_pairing,
    c("package", "package_class"))
  out$daily$meta_daily_source <- match.arg(out$daily$meta_daily_source,
    c("summary", "episodes", "both"))
  out$execution$source_verification <- match.arg(out$execution$source_verification,
    c("content", "metadata", "cache_only"))
  for (section in names(defaults)) {
    for (name in names(defaults[[section]])) {
      template <- defaults[[section]][[name]]
      value <- out[[section]][[name]]
      if (is.logical(template) && (length(value) != 1L || !is.logical(value) || is.na(value))) {
        cli::cli_abort("`{section}${name}` must be TRUE or FALSE.")
      }
      if (is.numeric(template) && (!is.numeric(value) || !length(value) ||
          anyNA(value) || any(!is.finite(value)) || any(value < 0))) {
        cli::cli_abort("`{section}${name}` must contain finite non-negative numbers.")
      }
      if (is.numeric(template) && length(template) == 1L &&
          !name %in% c("meta_start_event_types", "meta_end_event_types") && length(value) != 1L) {
        cli::cli_abort("`{section}${name}` must be a scalar.")
      }
    }
  }
  if (out$execution$workers < 1 || out$execution$workers %% 1 != 0 ||
      out$execution$checkpoint_every < 1 || out$execution$checkpoint_every %% 1 != 0) {
    cli::cli_abort("Workers and checkpoint interval must be positive integers.")
  }
  if (!out$reconstruction$reconstruct_meta && out$daily$meta_daily_source != "summary") {
    cli::cli_abort("Episode-derived meta daily data require meta reconstruction.")
  }
  if (out$category$enabled && is.null(out$category$dictionary)) {
    cli::cli_abort("Enabled category enrichment requires a dictionary.")
  }
  if (out$matching$enabled && is.null(out$matching$upload_col)) {
    cli::cli_abort("Enabled matching requires an upload column.")
  }
  priority <- out$matching$export_type_priority
  if (!is.character(priority) || !length(priority) || anyNA(priority) ||
      anyDuplicated(priority) || any(!priority %in% c("line", "meta", "day", "app"))) {
    cli::cli_abort("Matching export-type priority must contain unique supported export types.")
  }
  structure(out, class = c("appusage_config", "list"))
}

appusage_config_merge <- function(defaults, values, section) {
  if (!is.list(values) || (length(values) && (is.null(names(values)) ||
      any(names(values) == "") || anyDuplicated(names(values))))) {
    cli::cli_abort("`{section}` must be a uniquely named list.")
  }
  unknown <- setdiff(names(values), names(defaults))
  if (length(unknown)) cli::cli_abort("Unknown {section} setting(s): {paste(unknown, collapse = ', ')}")
  defaults[names(values)] <- values
  defaults
}

appusage_validate_config <- function(config) {
  if (is.null(config)) return(appusage_config())
  if (!is.list(config)) cli::cli_abort("`config` must be an appusage configuration.")
  if (length(config) && (is.null(names(config)) || any(names(config) == "") || anyDuplicated(names(config)))) {
    cli::cli_abort("Configuration sections must be uniquely named.")
  }
  unknown <- setdiff(names(config), names(formals(appusage_config)))
  if (length(unknown)) cli::cli_abort("Unknown configuration section(s): {paste(unknown, collapse = ', ')}")
  do.call(appusage_config, unclass(config))
}

appusage_config_second_options <- function(config) {
  qc <- config$qc[setdiff(names(config$qc), "enabled")]
  c(config$reconstruction, config$daily, config$time,
    list(inline_qc = config$qc$enabled), qc)
}

appusage_second_effective_options <- function(args = list(), tz = NULL, run_qc = NULL) {
  defaults <- formals(write_second_level_appusage)
  excluded <- c("first_level_rda", "output_dir", "overwrite", "provenance")
  defaults <- lapply(defaults[setdiff(names(defaults), excluded)], eval,
    envir = environment(write_second_level_appusage))
  defaults$meta_pairing <- defaults$meta_pairing[[1]]
  defaults$meta_daily_source <- defaults$meta_daily_source[[1]]
  values <- args[setdiff(names(args), "provenance")]
  out <- appusage_config_merge(defaults, values, "second_level")
  out$tz <- appusage_resolve_timezone(tz %||% out$tz)
  out$meta_pairing <- match.arg(out$meta_pairing, c("package", "package_class"))
  out$meta_daily_source <- match.arg(out$meta_daily_source, c("summary", "episodes", "both"))
  if (!is.null(run_qc)) out$inline_qc <- isTRUE(run_qc)
  out
}

appusage_parse_contract <- function(type, input, encoding, tz, parser_strict = TRUE,
                                    provenance = NULL) {
  list(configuration = list(type = type, input = input, encoding = encoding,
      tz = tz, parser_strict = parser_strict),
    implementation = provenance$parser_implementation_fingerprint %||%
      appusage_parser_implementation_fingerprint())
}

appusage_research_contract <- function(options, provenance = NULL) {
  fields <- setdiff(names(formals(make_second_level_appusage)), c("data", "export_type"))
  list(configuration = options[fields],
    implementation = provenance$research_implementation_fingerprint %||%
      appusage_research_implementation_fingerprint())
}

appusage_qc_contract <- function(options, provenance = NULL) {
  fields <- c("tz", "include_collection_app", setdiff(names(appusage_config()$qc), "enabled"))
  list(configuration = options[fields],
    implementation = provenance$qc_implementation_fingerprint %||%
      appusage_qc_implementation_fingerprint())
}

appusage_research_implementation_fingerprint <- function() {
  appusage_function_fingerprint(c(appusage_text_implementation_functions(),
    "make_second_level_appusage", "standardize_appusage", "build_appusage_daily",
    "appusage_frame_timezone", "second_level_events", "second_level_episodes",
    "second_level_daily", "second_level_meta_summary", "conform_second_episode",
    "coerce_episode_column", "conform_second_daily", "daily_from_episodes",
    "reconstruct_meta_episodes", "appusage_meta_pair_indices", "appusage_meta_rows_from_indices",
    "merge_contiguous_meta_episodes", "clip_overlapping_meta_timeline",
    "aggregate_meta_episodes_daily", "compare_meta_daily_sources",
    "add_duration_anomalies", "appusage_interval_segments", "appusage_daily_segment_index",
    "appusage_interval_calendar", "appusage_midnight_ms", "appusage_order_daily",
    "appusage_validate_second_level_daily", "ms_to_datetime_validated",
    "appusage_date_from_datetime_validated"))
}

appusage_qc_implementation_fingerprint <- function() {
  appusage_function_fingerprint(c(appusage_text_implementation_functions(),
    "qc_appusage_day", "qc_appusage_anomalies", "run_qc_for_second_level_data",
    "appusage_source_anomaly_qc", "appusage_qc_context", "appusage_qc_col",
    "appusage_qc_date", "appusage_qc_episode_view", "appusage_qc_valid_intervals",
    "appusage_check_episode_anomalies", "appusage_check_daily_anomalies",
    "appusage_check_export_span_anomalies", "appusage_line_overlap_qc",
    "appusage_line_timestamp_qc", "appusage_meta_reconstruction_qc",
    "appusage_source_qc_config", "appusage_source_qc_interval_segments"))
}

appusage_contract_equal <- function(recorded, requested) {
  normalize <- function(x) {
    if (is.list(x)) return(lapply(x, normalize))
    if (is.numeric(x)) return(as.numeric(x))
    x
  }
  !is.null(recorded) && identical(appusage_object_fingerprint(normalize(recorded)),
    appusage_object_fingerprint(normalize(requested)))
}
