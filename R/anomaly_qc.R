#' Summarize anomaly and reasonableness checks for appusage data
#'
#' `qc_appusage_anomalies()` counts implausible records in second-level appusage
#' data without filtering or modifying the data. The checks are intended for
#' metadata and summary reporting.
#'
#' @param data A second-level appusage list with `event`, `episode`, and/or
#'   `daily` data frames, or a single data frame.
#' @param metadata Optional metadata list used for export-date-span checks.
#' @param max_episode_ms Maximum plausible episode duration in milliseconds.
#' @param max_daily_app_ms Maximum plausible app-level daily duration in
#'   milliseconds.
#' @param max_daily_total_ms Maximum plausible summed daily duration in
#'   milliseconds within one source file.
#' @param max_export_lookback_days Maximum expected lookback in days from native
#'   export timestamp to observed record dates.
#' @param meta_diff_abs_ms Absolute meta summary-vs-episode duration difference
#'   threshold in milliseconds.
#' @param meta_diff_ratio Relative meta summary-vs-episode duration difference
#'   threshold.
#'
#' @param source_qc_config Optional named list overriding the coherent 0.3.4-F
#'   source-anomaly QC thresholds. This changes flags and eligibility only;
#'   source rows are never removed or rewritten.
#' @return A compact list of anomaly metrics suitable for JSON metadata.
#' @export
qc_appusage_anomalies <- function(data, metadata = NULL,
                                  max_episode_ms = 24 * 60 * 60 * 1000,
                                  max_daily_app_ms = 24 * 60 * 60 * 1000,
                                  max_daily_total_ms = 24 * 60 * 60 * 1000,
                                  max_export_lookback_days = 31,
                                  meta_diff_abs_ms = 60 * 1000,
                                  meta_diff_ratio = 0.20,
                                  source_qc_config = NULL) {
  source_qc_config <- appusage_source_qc_config(source_qc_config)
  thresholds <- appusage_anomaly_thresholds(
    max_episode_ms = max_episode_ms,
    max_daily_app_ms = max_daily_app_ms,
    max_daily_total_ms = max_daily_total_ms,
    max_export_lookback_days = max_export_lookback_days,
    meta_diff_abs_ms = meta_diff_abs_ms,
    meta_diff_ratio = meta_diff_ratio,
    source_qc_config = source_qc_config
  )

  grains <- appusage_normalize_anomaly_input(data)
  metrics <- appusage_empty_anomaly_qc(
    status = "success",
    thresholds = thresholds
  )

  metrics <- appusage_check_episode_anomalies(
    metrics,
    grains$episode,
    max_episode_ms = max_episode_ms
  )
  metrics <- appusage_check_event_anomalies(metrics, grains$event)
  metrics <- appusage_check_daily_anomalies(
    metrics,
    grains$daily,
    max_daily_app_ms = max_daily_app_ms,
    max_daily_total_ms = max_daily_total_ms,
    meta_diff_abs_ms = meta_diff_abs_ms,
    meta_diff_ratio = meta_diff_ratio
  )
  metrics <- appusage_check_export_span_anomalies(
    metrics,
    grains,
    metadata,
    max_export_lookback_days = max_export_lookback_days
  )
  metrics$source_anomaly_qc <- appusage_source_anomaly_qc(
    grains,
    config = source_qc_config,
    metadata = metadata
  )
  metrics <- appusage_add_checks(
    metrics,
    appusage_source_qc_anomaly_checks(metrics$source_anomaly_qc)
  )

  metrics$n_anomalies_total <- sum(unlist(metrics$n_anomalies_by_type),
    na.rm = TRUE
  )
  metrics$n_critical_anomalies <- metrics$n_anomalies_by_severity$critical
  metrics$n_warning_anomalies <- metrics$n_anomalies_by_severity$warning
  metrics$has_critical_anomaly <- metrics$n_critical_anomalies > 0
  metrics$has_warning_anomaly <- metrics$n_warning_anomalies > 0
  metrics$n_episode_anomalies <- metrics$n_anomalies_by_grain$episode
  metrics$n_event_anomalies <- metrics$n_anomalies_by_grain$event
  metrics$n_daily_anomalies <- metrics$n_anomalies_by_grain$daily
  metrics$n_export_span_anomalies <- metrics$n_anomalies_by_grain$export_span
  metrics
}

appusage_anomaly_thresholds <- function(max_episode_ms,
                                        max_daily_app_ms,
                                        max_daily_total_ms,
                                        max_export_lookback_days,
                                        meta_diff_abs_ms,
                                        meta_diff_ratio,
                                        source_qc_config = NULL) {
  list(
    max_episode_ms = max_episode_ms,
    max_daily_app_ms = max_daily_app_ms,
    max_daily_total_ms = max_daily_total_ms,
    max_export_lookback_days = max_export_lookback_days,
    meta_diff_abs_ms = meta_diff_abs_ms,
    meta_diff_ratio = meta_diff_ratio,
    source_qc = appusage_source_qc_config(source_qc_config)
  )
}

appusage_empty_anomaly_qc <- function(status = "not_run",
                                      thresholds = NULL,
                                      error_message = NA_character_) {
  if (is.null(thresholds)) {
    thresholds <- appusage_anomaly_thresholds(
      max_episode_ms = 24 * 60 * 60 * 1000,
      max_daily_app_ms = 24 * 60 * 60 * 1000,
      max_daily_total_ms = 24 * 60 * 60 * 1000,
      max_export_lookback_days = 31,
      meta_diff_abs_ms = 60 * 1000,
      meta_diff_ratio = 0.20,
      source_qc_config = NULL
    )
  }

  list(
    status = status,
    rule_version = "0.2.8",
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    thresholds = thresholds,
    n_anomalies_total = 0L,
    n_critical_anomalies = 0L,
    n_warning_anomalies = 0L,
    n_anomalies_by_grain = list(
      event = 0L,
      episode = 0L,
      daily = 0L,
      export_span = 0L
    ),
    n_anomalies_by_type = list(),
    n_anomalies_by_severity = list(
      critical = 0L,
      warning = 0L
    ),
    has_critical_anomaly = FALSE,
    has_warning_anomaly = FALSE,
    n_episode_anomalies = 0L,
    n_event_anomalies = 0L,
    n_daily_anomalies = 0L,
    n_export_span_anomalies = 0L,
    n_meta_duration_disagreements = 0L,
    max_abs_meta_duration_diff_ms = NA_real_,
    max_daily_total_ms_observed = NA_real_,
    max_observed_export_lookback_days = NA_real_,
    error_message = error_message,
    source_anomaly_qc = list(
      status = "not_run",
      rule_version = "0.3.4-F"
    )
  )
}

appusage_normalize_anomaly_input <- function(data) {
  empty <- data.frame()
  out <- list(event = empty, episode = empty, daily = empty)

  if (is.data.frame(data)) {
    out[[appusage_infer_anomaly_grain(data)]] <- data
    return(out)
  }

  if (!is.list(data)) {
    return(out)
  }

  for (grain in names(out)) {
    if (is.data.frame(data[[grain]])) {
      out[[grain]] <- data[[grain]]
    }
  }
  out
}

appusage_infer_anomaly_grain <- function(x) {
  names_x <- names(x)
  if (any(c("event_ts_ms", "event_type", "event_type_label") %in% names_x)) {
    return("event")
  }
  if (any(c("start_ts_ms", "end_ts_ms", "reconstruction_status") %in% names_x)) {
    return("episode")
  }
  "daily"
}

appusage_add_anomaly <- function(metrics, grain, type, severity, count) {
  count <- as.integer(count)
  if (is.na(count) || count <= 0L) {
    return(metrics)
  }

  metrics$n_anomalies_by_grain[[grain]] <-
    metrics$n_anomalies_by_grain[[grain]] + count
  metrics$n_anomalies_by_type[[type]] <-
    appusage_null_int(metrics$n_anomalies_by_type[[type]]) + count
  metrics$n_anomalies_by_severity[[severity]] <-
    metrics$n_anomalies_by_severity[[severity]] + count
  metrics
}

appusage_null_int <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return(0L)
  }
  as.integer(x)
}

appusage_has_col <- function(x, col) {
  is.data.frame(x) && col %in% names(x)
}

appusage_n <- function(x) {
  if (is.data.frame(x)) {
    nrow(x)
  } else {
    0L
  }
}

appusage_num_col <- function(x, col) {
  if (!appusage_has_col(x, col)) {
    return(rep(NA_real_, appusage_n(x)))
  }
  suppressWarnings(as.numeric(x[[col]]))
}

appusage_chr_col <- function(x, col) {
  if (!appusage_has_col(x, col)) {
    return(rep(NA_character_, appusage_n(x)))
  }
  as.character(x[[col]])
}

appusage_lgl_col <- function(x, col) {
  if (!appusage_has_col(x, col)) {
    return(rep(FALSE, appusage_n(x)))
  }
  out <- x[[col]]
  if (is.logical(out)) {
    return(out %in% TRUE)
  }
  as.character(out) %in% c("TRUE", "true", "1", "yes")
}

appusage_missing_any_time <- function(x, cols) {
  n <- appusage_n(x)
  found <- cols[cols %in% names(x)]
  if (length(found) == 0L) {
    return(rep(TRUE, n))
  }

  missing <- rep(TRUE, n)
  for (col in found) {
    value <- x[[col]]
    if (is.numeric(value)) {
      value_missing <- is.na(value)
    } else {
      value_chr <- as.character(value)
      value_missing <- is.na(value_chr) | !nzchar(trimws(value_chr))
    }
    missing <- missing & value_missing
  }
  missing
}

appusage_count_true <- function(x) {
  sum(x %in% TRUE, na.rm = TRUE)
}

appusage_check_episode_anomalies <- function(metrics, episode,
                                             max_episode_ms) {
  if (!is.data.frame(episode) || nrow(episode) == 0L) {
    return(metrics)
  }

  duration_ms <- appusage_num_col(episode, "duration_ms")
  missing_start <- appusage_missing_any_time(
    episode,
    c("start_ts_ms", "start_datetime", "start_time")
  )
  missing_end <- appusage_missing_any_time(
    episode,
    c("end_ts_ms", "end_datetime", "end_time")
  )
  missing_duration <- is.na(duration_ms)
  negative_duration <- !is.na(duration_ms) & duration_ms < 0
  zero_duration <- !is.na(duration_ms) & duration_ms == 0
  overlong_duration <- !is.na(duration_ms) & duration_ms > max_episode_ms
  cross_date <- appusage_lgl_col(episode, "anomaly_cross_date")
  if (!any(cross_date) && appusage_has_col(episode, "start_ts_ms") &&
    appusage_has_col(episode, "end_ts_ms")) {
    start_date <- appusage_date_from_ms(appusage_num_col(episode, "start_ts_ms"))
    end_date <- appusage_date_from_ms(appusage_num_col(episode, "end_ts_ms"))
    cross_date <- !is.na(start_date) & !is.na(end_date) & start_date != end_date
  }
  device_boundary <- appusage_lgl_col(episode, "device_boundary_involved")
  unmatched_start <- appusage_lgl_col(episode, "unmatched_start")
  unmatched_end <- appusage_lgl_col(episode, "unmatched_end")
  reconstruction_status <- appusage_chr_col(episode, "reconstruction_status")
  invalid_pair <- reconstruction_status %in% "invalid_pair" |
    appusage_lgl_col(episode, "invalid_pair")
  warning_text <- appusage_chr_col(episode, "reconstruction_warning")
  reconstruction_warning <- !is.na(warning_text) & nzchar(trimws(warning_text))

  checks <- list(
    episode_missing_start_timestamp = list(
      grain = "episode", severity = "critical", count = appusage_count_true(missing_start)
    ),
    episode_missing_end_timestamp = list(
      grain = "episode", severity = "critical", count = appusage_count_true(missing_end)
    ),
    episode_missing_duration = list(
      grain = "episode", severity = "critical", count = appusage_count_true(missing_duration)
    ),
    episode_negative_duration = list(
      grain = "episode", severity = "critical", count = appusage_count_true(negative_duration)
    ),
    episode_zero_duration = list(
      grain = "episode", severity = "warning", count = appusage_count_true(zero_duration)
    ),
    episode_overlong_duration = list(
      grain = "episode", severity = "warning", count = appusage_count_true(overlong_duration)
    ),
    episode_cross_date = list(
      grain = "episode", severity = "warning", count = appusage_count_true(cross_date)
    ),
    episode_device_boundary = list(
      grain = "episode", severity = "warning", count = appusage_count_true(device_boundary)
    ),
    episode_unmatched_start = list(
      grain = "episode", severity = "warning", count = appusage_count_true(unmatched_start)
    ),
    episode_unmatched_end = list(
      grain = "episode", severity = "warning", count = appusage_count_true(unmatched_end)
    ),
    episode_invalid_pair = list(
      grain = "episode", severity = "warning", count = appusage_count_true(invalid_pair)
    ),
    episode_reconstruction_warning = list(
      grain = "episode", severity = "warning", count = appusage_count_true(reconstruction_warning)
    )
  )

  appusage_add_checks(metrics, checks)
}

appusage_check_event_anomalies <- function(metrics, event) {
  if (!is.data.frame(event) || nrow(event) == 0L) {
    return(metrics)
  }

  missing_timestamp <- appusage_missing_any_time(
    event,
    c("event_ts_ms", "event_datetime", "event_time")
  )
  missing_type <- appusage_missing_any_time(
    event,
    c("event_type", "event_type_label")
  )
  event_label <- appusage_chr_col(event, "event_type_label")
  unknown_type <- !is.na(event_label) & grepl("^EVENT_TYPE_[0-9]+$", event_label)
  non_monotonic <- appusage_non_monotonic_events(event)

  checks <- list(
    event_missing_timestamp = list(
      grain = "event", severity = "critical", count = appusage_count_true(missing_timestamp)
    ),
    event_missing_type = list(
      grain = "event", severity = "critical", count = appusage_count_true(missing_type)
    ),
    event_unknown_type = list(
      grain = "event", severity = "warning", count = appusage_count_true(unknown_type)
    ),
    event_non_monotonic_timestamp = list(
      grain = "event", severity = "warning", count = appusage_count_true(non_monotonic)
    )
  )

  appusage_add_checks(metrics, checks)
}

appusage_non_monotonic_events <- function(event) {
  n <- appusage_n(event)
  if (!appusage_has_col(event, "event_ts_ms")) {
    return(rep(FALSE, n))
  }
  ts <- appusage_num_col(event, "event_ts_ms")
  package <- if (appusage_has_col(event, "package_name")) {
    appusage_chr_col(event, "package_name")
  } else {
    rep("__all__", n)
  }
  out <- rep(FALSE, n)
  for (key in unique(package)) {
    idx <- which(package %in% key)
    if (length(idx) < 2L) {
      next
    }
    out[idx[-1L]] <- diff(ts[idx]) < 0
  }
  out
}

appusage_check_daily_anomalies <- function(metrics, daily,
                                           max_daily_app_ms,
                                           max_daily_total_ms,
                                           meta_diff_abs_ms,
                                           meta_diff_ratio) {
  if (!is.data.frame(daily) || nrow(daily) == 0L) {
    return(metrics)
  }

  duration_ms <- appusage_num_col(daily, "duration_ms")
  missing_duration <- is.na(duration_ms)
  negative_duration <- !is.na(duration_ms) & duration_ms < 0
  zero_duration <- !is.na(duration_ms) & duration_ms == 0
  overlong_app <- !is.na(duration_ms) & duration_ms > max_daily_app_ms
  total_by_date <- appusage_daily_total_by_date(
    daily,
    duration_ms,
    max_daily_total_ms
  )
  impossible_total <- total_by_date$row_flag

  duration_diff <- appusage_num_col(daily, "duration_diff_ms")
  duration_diff_pct <- abs(appusage_num_col(daily, "duration_diff_pct"))
  abs_diff <- abs(duration_diff)
  agreement <- appusage_chr_col(daily, "duration_agreement_status")
  meta_disagreement <- agreement %in% "matched_with_difference" &
    (
      (!is.na(abs_diff) & abs_diff > meta_diff_abs_ms) |
        (!is.na(duration_diff_pct) & duration_diff_pct > meta_diff_ratio)
    )
  summary_only <- agreement %in% "summary_only"
  episode_only <- agreement %in% "episode_only"

  if (any(!is.na(abs_diff))) {
    metrics$max_abs_meta_duration_diff_ms <- max(abs_diff, na.rm = TRUE)
  }
  metrics$max_daily_total_ms_observed <- total_by_date$max_total
  metrics$n_meta_duration_disagreements <- appusage_count_true(meta_disagreement)

  checks <- list(
    daily_missing_duration = list(
      grain = "daily", severity = "critical", count = appusage_count_true(missing_duration)
    ),
    daily_negative_duration = list(
      grain = "daily", severity = "critical", count = appusage_count_true(negative_duration)
    ),
    daily_zero_duration = list(
      grain = "daily", severity = "warning", count = appusage_count_true(zero_duration)
    ),
    daily_overlong_app_duration = list(
      grain = "daily", severity = "warning", count = appusage_count_true(overlong_app)
    ),
    daily_impossible_total_duration = list(
      grain = "daily", severity = "critical", count = appusage_count_true(impossible_total)
    ),
    daily_meta_duration_disagreement = list(
      grain = "daily", severity = "warning", count = appusage_count_true(meta_disagreement)
    ),
    daily_meta_summary_only = list(
      grain = "daily", severity = "warning", count = appusage_count_true(summary_only)
    ),
    daily_meta_episode_only = list(
      grain = "daily", severity = "warning", count = appusage_count_true(episode_only)
    )
  )

  appusage_add_checks(metrics, checks)
}

appusage_daily_total_by_date <- function(daily, duration_ms,
                                         max_daily_total_ms) {
  dates <- appusage_date_col(daily, "date")
  if (all(is.na(dates))) {
    dates <- appusage_date_col(daily, "table_date")
  }

  row_flag <- rep(FALSE, appusage_n(daily))
  if (all(is.na(dates))) {
    return(list(row_flag = row_flag, max_total = NA_real_))
  }

  valid <- !is.na(dates) & !is.na(duration_ms) & duration_ms >= 0
  if (!any(valid)) {
    return(list(row_flag = row_flag, max_total = NA_real_))
  }

  totals <- stats::aggregate(
    duration_ms[valid],
    by = list(date = dates[valid]),
    FUN = sum
  )
  names(totals) <- c("date", "total")
  max_total <- max(totals$total, na.rm = TRUE)
  over_dates <- totals$date[totals$total > max_daily_total_ms]
  row_flag <- !is.na(dates) & dates %in% over_dates
  list(row_flag = row_flag, max_total = max_total)
}

appusage_check_export_span_anomalies <- function(metrics, grains, metadata,
                                                 max_export_lookback_days) {
  export_date <- appusage_metadata_export_date(metadata)
  observed_dates <- appusage_observed_dates(grains)
  if (is.na(export_date) || length(observed_dates) == 0L) {
    return(metrics)
  }

  observed_dates <- unique(observed_dates[!is.na(observed_dates)])
  if (length(observed_dates) == 0L) {
    return(metrics)
  }

  lookback_days <- as.numeric(export_date - observed_dates)
  metrics$max_observed_export_lookback_days <- max(lookback_days, na.rm = TRUE)
  after_export <- observed_dates > export_date
  older_than_expected <- lookback_days > max_export_lookback_days
  wide_span <- as.numeric(max(observed_dates) - min(observed_dates)) >
    max_export_lookback_days

  checks <- list(
    export_record_after_export_date = list(
      grain = "export_span", severity = "critical", count = appusage_count_true(after_export)
    ),
    export_record_older_than_lookback = list(
      grain = "export_span", severity = "warning", count = appusage_count_true(older_than_expected)
    ),
    export_observed_span_too_wide = list(
      grain = "export_span", severity = "warning", count = as.integer(wide_span)
    )
  )

  appusage_add_checks(metrics, checks)
}

appusage_add_checks <- function(metrics, checks) {
  for (type in names(checks)) {
    check <- checks[[type]]
    metrics <- appusage_add_anomaly(
      metrics,
      grain = check$grain,
      type = type,
      severity = check$severity,
      count = check$count
    )
  }
  metrics
}

appusage_date_from_ms <- function(x, tz = "Asia/Shanghai") {
  appusage_date_from_datetime(ms = x, tz = tz)
}

appusage_date_col <- function(x, col, tz = "Asia/Shanghai") {
  if (!appusage_has_col(x, col)) {
    return(rep(as.Date(NA), appusage_n(x)))
  }
  appusage_as_date(x[[col]], tz = tz)
}

appusage_as_date <- function(x, tz = "Asia/Shanghai") {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(appusage_date_from_datetime(x, tz = tz))
  }
  if (is.numeric(x)) {
    return(appusage_date_from_ms(x, tz = tz))
  }
  x_chr <- as.character(x)
  suppressWarnings(as.Date(substr(x_chr, 1L, 10L)))
}

appusage_observed_dates <- function(grains) {
  dates <- as.Date(character())

  daily <- grains$daily
  if (is.data.frame(daily) && nrow(daily) > 0L) {
    dates <- c(dates, appusage_date_col(daily, "date"))
    dates <- c(dates, appusage_date_col(daily, "table_date"))
  }

  episode <- grains$episode
  if (is.data.frame(episode) && nrow(episode) > 0L) {
    dates <- c(dates, appusage_date_col(episode, "date"))
    dates <- c(dates, appusage_date_col(episode, "start_date"))
    if (appusage_has_col(episode, "start_ts_ms")) {
      dates <- c(dates, appusage_date_from_ms(appusage_num_col(episode, "start_ts_ms")))
    }
    if (appusage_has_col(episode, "end_ts_ms")) {
      dates <- c(dates, appusage_date_from_ms(appusage_num_col(episode, "end_ts_ms")))
    }
  }

  event <- grains$event
  if (is.data.frame(event) && nrow(event) > 0L) {
    dates <- c(dates, appusage_date_col(event, "date"))
    if (appusage_has_col(event, "event_ts_ms")) {
      dates <- c(dates, appusage_date_from_ms(appusage_num_col(event, "event_ts_ms")))
    }
  }

  dates[!is.na(dates)]
}

appusage_metadata_export_date <- function(metadata) {
  if (!is.list(metadata)) {
    return(as.Date(NA))
  }

  candidates <- list(
    metadata$export$native_export_created_at,
    metadata$source$native_export_created_at,
    metadata$source$filename$native_export_created_at,
    metadata$filename$native_export_created_at,
    metadata$native_export_created_at
  )
  for (candidate in candidates) {
    if (!is.null(candidate) && length(candidate) > 0L && !is.na(candidate[[1L]])) {
      parsed <- appusage_parse_export_date(
        candidate[[1L]],
        tz = metadata$export$timezone %||% metadata$processing$effective_timezone %||%
          appusage_default_timezone()
      )
      if (!is.na(parsed)) {
        return(parsed)
      }
    }
  }
  as.Date(NA)
}

appusage_parse_export_date <- function(x, tz = "Asia/Shanghai") {
  if (inherits(x, "Date")) {
    return(x)
  }
  if (inherits(x, "POSIXt")) {
    return(appusage_date_from_datetime(x, tz = tz))
  }
  if (is.numeric(x)) {
    return(appusage_date_from_ms(x, tz = tz))
  }
  x_chr <- as.character(x)
  if (!nzchar(trimws(x_chr))) {
    return(as.Date(NA))
  }
  suppressWarnings(as.Date(substr(x_chr, 1L, 10L)))
}
