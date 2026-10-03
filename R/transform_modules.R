#' Standardize faithful APP Usage tables without reconstructing events
#'
#' This computational module retains native grains and meta inputs separately.
#' It does not read/write files, reconstruct episodes, filter apps or perform QC.
#' @param data Faithful parser output or a proc-1 data list.
#' @param export_type Optional explicit export type.
#' @param tz Effective IANA timezone.
#' @param max_episode_ms,max_daily_app_ms Existing row anomaly thresholds.
#' @return An `appusage_standardized` list suitable for [build_appusage_daily()].
#' @export
standardize_appusage <- function(data, export_type = NULL, tz = "Asia/Shanghai",
                                 max_episode_ms = 86400000,
                                 max_daily_app_ms = 86400000) {
  tz <- appusage_resolve_timezone(tz)
  first <- normalize_first_level_appusage(data, export_type)
  event <- if (!is.null(first$meta_events)) second_level_events(first$meta_events, tz) else
    empty_second_event_tibble()
  episode <- if (!is.null(first$line)) second_level_episodes(first$line, max_episode_ms, tz) else
    empty_second_episode_tibble()
  daily <- empty_second_daily_tibble()
  if (!is.null(first$day)) daily <- second_level_daily(first$day, max_daily_app_ms)
  if (!is.null(first$app)) daily <- second_level_daily(first$app, max_daily_app_ms)
  summary <- if (!is.null(first$meta_summary)) {
    second_level_meta_summary(first$meta_summary, max_daily_app_ms, tz)
  } else empty_second_daily_tibble()
  structure(list(event = appusage_frame_timezone(event, tz),
    episode = appusage_frame_timezone(episode, tz), native_daily = daily,
    meta_summary_daily = summary, meta_events = first$meta_events,
    capabilities = list(line = !is.null(first$line),
      meta = !is.null(first$meta_summary) || !is.null(first$meta_events),
      native_daily = !is.null(first$day) || !is.null(first$app)),
    effective_timezone = tz), class = c("appusage_standardized", "list"))
}

#' Build daily APP Usage data from standardized tables and episodes
#'
#' Computes the daily module independently of parsing, storage and QC. Meta
#' summary and episode sources remain distinct; `both` never adds them together.
#' @param data An object returned by [standardize_appusage()].
#' @param meta_episodes Optional explicitly reconstructed meta episodes. NULL
#'   means reconstruction was disabled, rather than an empty successful result.
#' @param meta_daily_source One of `summary`, `episodes` or `both`.
#' @param max_daily_app_ms Existing daily row anomaly threshold.
#' @param tz Effective timezone; defaults to the standardized input's timezone.
#' @return An `appusage_daily_result` list with `daily`, component tables and
#'   explicit aggregation diagnostics. No files are written.
#' @export
build_appusage_daily <- function(data, meta_episodes = NULL,
                                 meta_daily_source = c("summary", "episodes", "both"),
                                 max_daily_app_ms = 86400000,
                                 tz = data$effective_timezone) {
  if (!inherits(data, "appusage_standardized")) {
    cli::cli_abort("`data` must come from standardize_appusage().")
  }
  tz <- appusage_resolve_timezone(tz)
  meta_daily_source <- match.arg(meta_daily_source)
  if (data$capabilities$meta && is.null(meta_episodes) && meta_daily_source != "summary") {
    cli::cli_abort("Episode-derived daily data require explicit meta episodes.")
  }
  daily <- data$native_daily
  line_segmentation <- line_expected <- meta_expected <- NULL
  if (isTRUE(data$capabilities$line)) {
    daily <- daily_from_episodes(data$episode, max_daily_app_ms, tz)
    line_segmentation <- attr(daily, "line_interval_segmentation_diagnostics", exact = TRUE)
    line_expected <- attr(daily, "daily_aggregation_expected", exact = TRUE)
  }
  summary <- data$meta_summary_daily
  episode_daily <- empty_second_daily_tibble()
  if (!is.null(meta_episodes)) {
    episode_daily <- aggregate_meta_episodes_daily(meta_episodes, summary,
      max_daily_app_ms = max_daily_app_ms, tz = tz)
    meta_expected <- attr(episode_daily, "daily_aggregation_expected", exact = TRUE)
  }
  if (isTRUE(data$capabilities$meta)) {
    summary <- compare_meta_daily_sources(summary, episode_daily)
    episode_daily <- compare_meta_daily_sources(episode_daily, summary)
    daily <- switch(meta_daily_source, summary = summary, episodes = episode_daily,
      both = conform_second_daily(rbind(summary, episode_daily)))
  }
  structure(list(daily = daily, meta_summary_daily = summary,
    meta_episode_daily = episode_daily, line_segmentation = line_segmentation,
    line_expected = line_expected, meta_expected = meta_expected,
    effective_timezone = tz), class = c("appusage_daily_result", "list"))
}

appusage_frame_timezone <- function(data, tz) {
  for (name in names(data)) {
    if (inherits(data[[name]], "POSIXct") && !identical(attr(data[[name]], "tzone"), tz)) {
      attr(data[[name]], "tzone") <- tz
    }
  }
  data
}

#' Evaluate complete APP Usage quality in memory
#'
#' Runs the same anomaly, source and daily coverage checks as inline/file QC.
#' Unlike the legacy coverage-only [run_qc_appusage()] in-memory mode, this
#' function returns the complete result used in proc-2 metadata.
#' @param data A research data list with event, episode and daily tables.
#' @param metadata Optional source/processing metadata used by source QC.
#' @param participant_id Source owner used for coverage diagnostics.
#' @param config QC settings, or a complete [appusage_config()] object.
#' @param include_collection_app Whether coverage includes the collection app.
#' @return A complete QC result list, including counts, anomalies and eligibility.
#' @export
assess_appusage_qc <- function(data, metadata = NULL, participant_id = "record-000001",
                               config = list(), include_collection_app = NULL) {
  if (inherits(data, "appusage_second_level")) data <- data$data
  qc <- if (inherits(config, "appusage_config")) config$qc else appusage_config(qc = config)$qc
  include_collection_app <- include_collection_app %||%
    if (inherits(config, "appusage_config")) config$reconstruction$include_collection_app else TRUE
  args <- qc[setdiff(names(qc), "enabled")]
  do.call(run_qc_for_second_level_data, c(list(data = data,
    second_level_rda = NA_character_, participant_id = participant_id,
    metadata = metadata, include_collection_app = include_collection_app), args))
}
