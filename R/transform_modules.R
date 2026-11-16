#' Standardize faithful APP Usage tables without reconstructing events
#'
#' This computational module retains native grains and meta inputs separately.
#' It does not read/write files, reconstruct episodes, filter apps or perform QC.
#' @param data Faithful parser output or a proc-1 data list.
#' @param export_type Optional explicit export type.
#' @param tz Effective IANA timezone.
#' @param max_episode_ms,max_daily_app_ms Existing row anomaly thresholds.
#' @return An `appusage_standardized` list suitable for [build_appusage_daily()].
#' @details
#' The return separates `event`, native `episode`, `native_daily` and
#' `meta_summary_daily`, with capability flags. Use [build_appusage_daily()]
#' for daily aggregation or [run_second_level_appusage()] for the composed
#' second-level result.
#' @seealso [appusage_demo], [appusage_synthetic_project]
#' @examples
#' data(appusage_demo)
#' folder <- system.file("extdata", "synth", package = "appusageR", mustWork = TRUE)
#' path <- file.path(folder, appusage_demo$sources$source_relpath[1L])
#' first <- run_first_level_appusage(path, strict = TRUE)
#' native <- standardize_appusage(first$data)
#' native$capabilities
#' stopifnot(nrow(native$episode) == 14L, nrow(native$event) == 0L)
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
#' @param meta_daily_source One of `summary`, `episodes` or `both` for meta
#'   data. Omit for mixed line/meta input to use native line daily totals.
#' @param max_daily_app_ms Existing daily row anomaly threshold.
#' @param tz Effective timezone; defaults to the standardized input's timezone.
#' @return An `appusage_daily_result` list with `daily`, component tables and
#'   explicit aggregation diagnostics. No files are written.
#' @details
#' Use the selected `daily` table for analysis. Component tables and
#' aggregation diagnostics explain its origin; summing the selected table
#' and its component tables would count the same use more than once.
#' @seealso [appusage_demo], [appusage_synthetic_project]
#' @examples
#' data(appusage_demo)
#' folder <- system.file("extdata", "synth", package = "appusageR", mustWork = TRUE)
#' path <- file.path(folder, appusage_demo$sources$source_relpath[1L])
#' first <- run_first_level_appusage(path, strict = TRUE)
#' native <- standardize_appusage(first$data)
#' daily <- build_appusage_daily(native)
#' stopifnot(sum(daily$daily$duration_ms) == 12600000)
#' head(daily$daily[, c("date", "app_name", "duration_ms")])
#' @export
build_appusage_daily <- function(data, meta_episodes = NULL,
                                 meta_daily_source = c("summary", "episodes", "both"),
                                 max_daily_app_ms = 86400000,
                                 tz = data$effective_timezone) {
  appusage_count("daily_module", 0)
  explicit_daily <- !missing(meta_daily_source)
  if (!inherits(data, "appusage_standardized")) {
    cli::cli_abort("`data` must come from standardize_appusage().")
  }
  tz <- appusage_resolve_timezone(tz)
  meta_daily_source <- match.arg(meta_daily_source)
  if (data$capabilities$line && data$capabilities$meta &&
      (!is.null(meta_episodes) || explicit_daily)) {
    cli::cli_abort("Mixed line/meta sources use line daily totals. Parse the meta component separately for meta daily totals.",
      class = "appusage_mixed_argument_conflict")
  }
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
  if (isTRUE(data$capabilities$meta) && !data$capabilities$line) {
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
#' @details
#' Returns execution `qc_status`, row `counts`, `anomaly_qc` diagnostics
#' and `qc` with coverage and `analysis_eligible_*` flags. Pass retained source
#' metadata separately: a wrapper supplies its data, but does not automatically
#' supply its metadata argument. QC evaluates and reports; it does not delete
#' input rows or write files. Missing source metadata limits source-context checks.
#' @seealso [appusage_demo], [appusage_synthetic_project]
#' @examples
#' data(appusage_demo)
#' record <- appusage_demo$records$mixed_line_meta
#' qc <- assess_appusage_qc(record, metadata = record$metadata,
#'                          participant_id = "105")
#' qc$qc[c("pass_qc", "analysis_eligible_event", "analysis_eligible_daily")]
#' stopifnot(qc$qc_status == "success", qc$qc$pass_qc)
#' short <- appusage_demo$records$recorded_blank_absent_days
#' short_qc <- assess_appusage_qc(short, metadata = short$metadata,
#'                                participant_id = "106")
#' stopifnot(!short_qc$qc$pass_qc)
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
