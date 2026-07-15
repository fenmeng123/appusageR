appusage_default_timezone <- function() {
  "Asia/Shanghai"
}

appusage_resolve_timezone <- function(tz = NULL) {
  if (is.null(tz) || length(tz) == 0L || is.na(tz[[1L]]) ||
      !nzchar(trimws(as.character(tz[[1L]])))) {
    return(appusage_default_timezone())
  }
  tz <- as.character(tz[[1L]])
  if (!tz %in% OlsonNames()) {
    cli::cli_abort("`tz` must be a valid IANA time zone name; received {.val {tz}}.")
  }
  tz
}

appusage_date_from_datetime <- function(x = NULL, ms = NULL,
                                        tz = appusage_default_timezone()) {
  tz <- appusage_resolve_timezone(tz)
  if (!is.null(ms)) x <- ms_to_datetime(ms, tz = tz)
  if (is.null(x)) return(as.Date(character()))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) {
    return(as.Date(format(x, "%Y-%m-%d", tz = tz), format = "%Y-%m-%d"))
  }
  if (is.numeric(x)) return(appusage_date_from_datetime(ms = x, tz = tz))
  parsed_date <- safe_as_date(x)
  datetime_like <- is.na(parsed_date) & !is.na(x)
  if (any(datetime_like)) {
    parsed_datetime <- safe_as_datetime(x[datetime_like], tz = tz)
    parsed_date[datetime_like] <- appusage_date_from_datetime(parsed_datetime, tz = tz)
  }
  parsed_date
}

appusage_interval_segments <- function(data, tz = appusage_default_timezone()) {
  tz <- appusage_resolve_timezone(tz)
  data <- tibble::as_tibble(data)
  if (nrow(data) == 0L) {
    data$.source_row_id <- integer()
    data$.segment_index <- integer()
    data$.segment_count <- integer()
    data$.interval_status <- character()
    attr(data, "interval_segmentation_diagnostics") <-
      appusage_interval_segmentation_diagnostics(data, data, tz)
    return(data)
  }
  n <- nrow(data)
  start_ms <- suppressWarnings(as.numeric(data$start_ts_ms))
  end_ms <- suppressWarnings(as.numeric(data$end_ts_ms))
  duration_ms <- suppressWarnings(as.numeric(data$duration_ms))
  fallback_date <- if ("date" %in% names(data)) {
    appusage_date_from_datetime(data$date, tz = tz)
  } else {
    rep(as.Date(NA), n)
  }
  canonical_date <- appusage_date_from_datetime(ms = start_ms, tz = tz)
  use_fallback <- is.na(canonical_date)
  canonical_date[use_fallback] <- fallback_date[use_fallback]

  status <- rep("valid", n)
  missing_interval <- is.na(start_ms) | is.na(end_ms) | is.na(duration_ms)
  negative_interval <- !missing_interval & (end_ms < start_ms | duration_ms < 0)
  zero_interval <- !missing_interval & !negative_interval &
    (end_ms == start_ms | duration_ms == 0)
  status[missing_interval] <- "missing_interval"
  status[negative_interval] <- "invalid_negative_interval"
  status[zero_interval] <- "zero_interval"
  valid <- status == "valid"

  start_date <- rep(as.Date(NA), n)
  end_date <- rep(as.Date(NA), n)
  start_date[valid] <- appusage_date_from_datetime(ms = start_ms[valid], tz = tz)
  end_date[valid] <- appusage_date_from_datetime(ms = end_ms[valid] - 0.001, tz = tz)
  segment_count <- rep(1L, n)
  segment_count[valid] <- as.integer(end_date[valid] - start_date[valid]) + 1L

  source_row_id <- rep.int(seq_len(n), segment_count)
  segment_index <- sequence(segment_count)
  expanded_valid <- valid[source_row_id]
  expanded_date <- canonical_date[source_row_id]
  expanded_date[expanded_valid] <- start_date[source_row_id[expanded_valid]] +
    segment_index[expanded_valid] - 1L

  out <- data[source_row_id, , drop = FALSE]
  out$date <- expanded_date
  out$.source_row_id <- source_row_id
  out$.segment_index <- as.integer(segment_index)
  out$.segment_count <- segment_count[source_row_id]
  out$.interval_status <- status[source_row_id]

  valid_position <- which(expanded_valid)
  if (length(valid_position) > 0L) {
    source_index <- source_row_id[valid_position]
    source_segment_index <- segment_index[valid_position]
    source_segment_count <- segment_count[source_index]
    segment_date <- expanded_date[valid_position]
    midnight_ms <- as.numeric(as.POSIXct(
      paste(segment_date, "00:00:00"),
      format = "%Y-%m-%d %H:%M:%S", tz = tz
    )) * 1000
    next_midnight_ms <- as.numeric(as.POSIXct(
      paste(segment_date + 1L, "00:00:00"),
      format = "%Y-%m-%d %H:%M:%S", tz = tz
    )) * 1000
    segment_start_ms <- ifelse(
      source_segment_index == 1L,
      start_ms[source_index],
      midnight_ms
    )
    segment_end_ms <- ifelse(
      source_segment_index == source_segment_count,
      end_ms[source_index],
      next_midnight_ms
    )
    wall_ms <- segment_end_ms - segment_start_ms
    allocated <- duration_ms[source_index] * wall_ms /
      (end_ms[source_index] - start_ms[source_index])

    single <- source_segment_count == 1L
    allocated[single] <- duration_ms[source_index[single]]
    nonlast <- !single & source_segment_index < source_segment_count
    last <- !single & source_segment_index == source_segment_count
    if (any(last)) {
      nonlast_sum <- rowsum(
        allocated[nonlast],
        source_index[nonlast],
        reorder = FALSE
      )
      sum_by_source <- numeric(n)
      sum_by_source[as.integer(rownames(nonlast_sum))] <- nonlast_sum[, 1L]
      allocated[last] <- duration_ms[source_index[last]] -
        sum_by_source[source_index[last]]
    }
    out$duration_ms[valid_position] <- allocated
    if ("duration_min" %in% names(out)) {
      out$duration_min[valid_position] <- allocated / 60000
    }
  }
  out <- tibble::as_tibble(out)
  attr(out, "interval_segmentation_diagnostics") <-
    appusage_interval_segmentation_diagnostics(data, out, tz)
  out
}

appusage_interval_segmentation_diagnostics <- function(source, segments, tz) {
  source_duration <- suppressWarnings(as.numeric(source$duration_ms))
  valid_source <- !is.na(source$start_ts_ms) & !is.na(source$end_ts_ms) &
    !is.na(source_duration) & source$end_ts_ms >= source$start_ts_ms &
    source_duration >= 0
  segment_duration <- suppressWarnings(as.numeric(segments$duration_ms))
  valid_segment <- segments$.interval_status %in% c("valid", "zero_interval") &
    !is.na(segment_duration) & segment_duration >= 0
  segment_counts <- if (nrow(segments) > 0L) {
    tapply(segments$.segment_count, segments$.source_row_id, max, na.rm = TRUE)
  } else numeric()
  input_total <- sum(source_duration[valid_source], na.rm = TRUE)
  output_total <- sum(segment_duration[valid_segment], na.rm = TRUE)
  list(
    effective_timezone = tz,
    n_source_intervals = nrow(source),
    n_daily_segments = sum(valid_segment),
    n_cross_midnight_intervals = sum(segment_counts > 1L, na.rm = TRUE),
    n_zero_intervals = sum(segments$.interval_status == "zero_interval", na.rm = TRUE),
    n_missing_intervals = sum(segments$.interval_status == "missing_interval", na.rm = TRUE),
    n_negative_or_invalid_intervals = sum(segments$.interval_status == "invalid_negative_interval", na.rm = TRUE),
    source_duration_ms = input_total,
    segmented_duration_ms = output_total,
    duration_conservation_diff_ms = output_total - input_total
  )
}
