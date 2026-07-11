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
  rows <- vector("list", nrow(data))
  for (i in seq_len(nrow(data))) {
    source <- data[i, , drop = FALSE]
    start_ms <- suppressWarnings(as.numeric(source$start_ts_ms[[1L]]))
    end_ms <- suppressWarnings(as.numeric(source$end_ts_ms[[1L]]))
    duration_ms <- suppressWarnings(as.numeric(source$duration_ms[[1L]]))
    fallback_date <- if ("date" %in% names(source)) source$date[[1L]] else as.Date(NA)
    canonical_date <- appusage_date_from_datetime(ms = start_ms, tz = tz)
    if (length(canonical_date) == 0L || is.na(canonical_date)) {
      canonical_date <- appusage_date_from_datetime(fallback_date, tz = tz)
    }
    status <- if (is.na(start_ms) || is.na(end_ms) || is.na(duration_ms)) {
      "missing_interval"
    } else if (end_ms < start_ms || duration_ms < 0) {
      "invalid_negative_interval"
    } else if (end_ms == start_ms || duration_ms == 0) {
      "zero_interval"
    } else {
      "valid"
    }
    if (!identical(status, "valid")) {
      source$date <- canonical_date
      source$.source_row_id <- i
      source$.segment_index <- 1L
      source$.segment_count <- 1L
      source$.interval_status <- status
      rows[[i]] <- source
      next
    }
    start_date <- appusage_date_from_datetime(ms = start_ms, tz = tz)
    end_date <- appusage_date_from_datetime(ms = end_ms - 0.001, tz = tz)
    day_sequence <- seq(start_date, end_date, by = "day")
    if (length(day_sequence) <= 1L) {
      source$date <- start_date
      source$.source_row_id <- i
      source$.segment_index <- 1L
      source$.segment_count <- 1L
      source$.interval_status <- "valid"
      rows[[i]] <- source
      next
    }
    midnight_dates <- day_sequence[-1L]
    boundaries <- as.numeric(as.POSIXct(
      paste(midnight_dates, "00:00:00"),
      format = "%Y-%m-%d %H:%M:%S", tz = tz
    )) * 1000
    endpoints <- c(start_ms, boundaries[boundaries > start_ms & boundaries < end_ms], end_ms)
    wall_ms <- diff(endpoints)
    allocated <- duration_ms * wall_ms / sum(wall_ms)
    if (length(allocated) > 1L) {
      allocated[[length(allocated)]] <- duration_ms - sum(allocated[-length(allocated)])
    }
    segments <- source[rep(1L, length(allocated)), , drop = FALSE]
    segments$date <- appusage_date_from_datetime(ms = endpoints[-length(endpoints)], tz = tz)
    segments$duration_ms <- allocated
    if ("duration_min" %in% names(segments)) segments$duration_min <- allocated / 60000
    segments$.source_row_id <- i
    segments$.segment_index <- seq_along(allocated)
    segments$.segment_count <- length(allocated)
    segments$.interval_status <- "valid"
    rows[[i]] <- segments
  }
  out <- tibble::as_tibble(do.call(rbind, rows))
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
