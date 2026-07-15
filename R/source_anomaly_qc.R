appusage_source_qc_config <- function(config = NULL,
                                      tz = appusage_default_timezone()) {
  defaults <- list(
    rule_version = "0.3.4-F",
    effective_timezone = appusage_resolve_timezone(tz),
    line_overlap_warning_ratio = 0.01,
    line_overlap_critical_ratio = 0.10,
    line_duplicate_critical_ratio = 0.01,
    max_line_daily_foreground_ms = 24 * 60 * 60 * 1000,
    max_line_interval_ms = 24 * 60 * 60 * 1000,
    max_meta_summary_duration_ms = 24 * 60 * 60 * 1000,
    meta_diff_abs_ms = 60 * 1000,
    meta_diff_ratio = 0.20
  )
  if (is.null(config)) return(defaults)
  if (!is.list(config)) {
    cli::cli_abort("`source_qc_config` must be NULL or a named list.")
  }
  unknown <- setdiff(names(config), names(defaults))
  if (length(unknown) > 0L) {
    cli::cli_abort("Unknown source QC configuration field(s): {paste(unknown, collapse = ', ')}")
  }
  out <- utils::modifyList(defaults, config)
  out$effective_timezone <- appusage_resolve_timezone(out$effective_timezone)
  numeric_names <- setdiff(names(defaults), c("rule_version", "effective_timezone"))
  invalid <- vapply(out[numeric_names], function(x) {
    length(x) != 1L || is.na(x) || !is.numeric(x) || !is.finite(x) || x < 0
  }, logical(1))
  if (any(invalid)) {
    cli::cli_abort("Source QC numeric thresholds must be finite non-negative scalars.")
  }
  if (out$line_overlap_warning_ratio > out$line_overlap_critical_ratio) {
    cli::cli_abort("Line overlap warning ratio cannot exceed the critical ratio.")
  }
  out
}

appusage_source_anomaly_qc <- function(data, config = NULL, metadata = NULL) {
  grains <- appusage_normalize_anomaly_input(data)
  config <- appusage_source_qc_config(config)
  line_overlap <- appusage_line_overlap_qc(grains$episode, config)
  line_timestamp <- appusage_line_timestamp_qc(grains$episode, config)
  meta_summary <- appusage_meta_summary_qc(grains$daily, config)
  meta_reconstruction <- appusage_meta_reconstruction_qc(
    grains$episode,
    grains$daily,
    config
  )

  episode_reasons <- c(
    if (isTRUE(line_overlap$critical)) "line_foreground_overlap_critical",
    if (isTRUE(line_timestamp$critical)) "line_interval_critical",
    if (isTRUE(meta_reconstruction$critical)) "meta_reconstruction_global_overlap"
  )
  daily_reasons <- c(
    episode_reasons,
    if (isTRUE(meta_summary$critical)) "meta_cumulative_summary_critical"
  )
  severity <- if (length(c(episode_reasons, daily_reasons)) > 0L) {
    "critical"
  } else if (any(c(
    line_overlap$warning,
    line_timestamp$warning,
    meta_summary$warning,
    meta_reconstruction$warning
  ))) {
    "warning"
  } else if (any(c(
    line_overlap$diagnostic,
    line_timestamp$diagnostic,
    meta_summary$diagnostic,
    meta_reconstruction$diagnostic
  ))) {
    "diagnostic"
  } else {
    "none"
  }

  list(
    status = "success",
    rule_version = config$rule_version,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    config = config,
    severity = severity,
    line_foreground_overlap = line_overlap,
    line_timestamp_date = line_timestamp,
    meta_cumulative_summary = meta_summary,
    meta_reconstruction = meta_reconstruction,
    eligibility = list(
      episode_ineligible = length(episode_reasons) > 0L,
      daily_ineligible = length(daily_reasons) > 0L,
      episode_reasons = compact_character_values(episode_reasons),
      daily_reasons = compact_character_values(daily_reasons)
    )
  )
}

appusage_line_episode_rows <- function(episode) {
  if (!is.data.frame(episode) || nrow(episode) == 0L) return(episode[0, , drop = FALSE])
  export_type <- appusage_chr_col(episode, "source_export_type")
  episode_source <- appusage_chr_col(episode, "episode_source")
  episode[export_type %in% "line" | episode_source %in% "line", , drop = FALSE]
}

appusage_line_overlap_qc <- function(episode, config) {
  empty <- list(
    status = "not_applicable", n_foreground_intervals = 0L,
    n_background_intervals = 0L, n_collection_intervals = 0L,
    n_invalid_intervals = 0L, overlap_count = 0L, overlap_ms = 0,
    cross_package_overlap_count = 0L, contained_interval_count = 0L,
    exact_duplicate_count = 0L, exact_duplicate_ratio = 0,
    overlap_ratio = 0, max_concurrent_intervals = 0L,
    max_daily_foreground_ms = NA_real_, n_daily_foreground_over_24h = 0L,
    daily = list(), diagnostic = FALSE, warning = FALSE, critical = FALSE
  )
  line <- appusage_line_episode_rows(episode)
  if (!is.data.frame(line) || nrow(line) == 0L) return(empty)

  start <- appusage_num_col(line, "start_ts_ms")
  end <- appusage_num_col(line, "end_ts_ms")
  duration <- appusage_num_col(line, "duration_ms")
  valid <- !is.na(start) & !is.na(end) & !is.na(duration) &
    end >= start & duration >= 0
  activity <- appusage_chr_col(line, "activity_type")
  activity[is.na(activity) | activity == ""] <- "foreground"
  collection <- appusage_lgl_col(line, "is_collection_app")
  foreground <- valid & activity == "foreground" & !is.na(collection) & !collection
  background <- valid & activity != "foreground" & !is.na(collection) & !collection

  empty$status <- "success"
  empty$n_foreground_intervals <- sum(foreground)
  empty$n_background_intervals <- sum(background)
  empty$n_collection_intervals <- sum(valid & collection)
  empty$n_invalid_intervals <- sum(!valid)
  if (!any(foreground, na.rm = TRUE)) return(empty)

  fg <- line[foreground, , drop = FALSE]
  duplicate_key <- paste(
    appusage_num_col(fg, "start_ts_ms"), appusage_num_col(fg, "end_ts_ms"),
    appusage_chr_col(fg, "package_name"), appusage_chr_col(fg, "app_name"),
    appusage_chr_col(fg, "activity_type"), sep = "\r"
  )
  empty$exact_duplicate_count <- sum(duplicated(duplicate_key))
  empty$exact_duplicate_ratio <- empty$exact_duplicate_count / nrow(fg)

  segments <- appusage_source_qc_interval_segments(fg, config$effective_timezone)
  day_groups <- split(seq_len(nrow(segments)), as.character(segments$date))
  daily <- lapply(day_groups, function(idx) {
    appusage_interval_overlap_day(segments[idx, , drop = FALSE], config)
  })
  empty$daily <- unname(daily)
  empty$overlap_count <- sum(vapply(daily, `[[`, integer(1), "overlap_count"))
  empty$overlap_ms <- sum(vapply(daily, `[[`, numeric(1), "overlap_ms"))
  empty$cross_package_overlap_count <- sum(vapply(
    daily, `[[`, integer(1), "cross_package_overlap_count"
  ))
  empty$contained_interval_count <- sum(vapply(
    daily, `[[`, integer(1), "contained_interval_count"
  ))
  empty$max_concurrent_intervals <- max(vapply(
    daily, `[[`, integer(1), "max_concurrent_intervals"
  ), 0L)
  totals <- vapply(daily, `[[`, numeric(1), "foreground_duration_ms")
  empty$max_daily_foreground_ms <- if (length(totals)) max(totals) else NA_real_
  empty$n_daily_foreground_over_24h <- sum(
    totals > config$max_line_daily_foreground_ms
  )
  total_duration <- sum(totals)
  empty$overlap_ratio <- if (total_duration > 0) empty$overlap_ms / total_duration else 0
  empty$diagnostic <- empty$overlap_count > 0L
  empty$warning <- empty$overlap_ratio > config$line_overlap_warning_ratio
  empty$critical <- empty$overlap_ratio > config$line_overlap_critical_ratio ||
    empty$exact_duplicate_ratio > config$line_duplicate_critical_ratio ||
    empty$n_daily_foreground_over_24h > 0L
  empty
}

appusage_source_qc_interval_segments <- function(x, tz) {
  rows <- vector("list", nrow(x))
  for (i in seq_len(nrow(x))) {
    start <- as.numeric(x$start_ts_ms[[i]])
    end <- as.numeric(x$end_ts_ms[[i]])
    duration <- as.numeric(x$duration_ms[[i]])
    start_date <- appusage_date_from_datetime(ms = start, tz = tz)
    end_date <- appusage_date_from_datetime(ms = end - 0.001, tz = tz)
    dates <- seq(start_date, end_date, by = "day")
    boundaries <- if (length(dates) > 1L) {
      as.numeric(as.POSIXct(
        paste(dates[-1L], "00:00:00"),
        format = "%Y-%m-%d %H:%M:%S", tz = tz
      )) * 1000
    } else numeric()
    endpoints <- c(start, boundaries[boundaries > start & boundaries < end], end)
    wall <- diff(endpoints)
    allocated <- if (sum(wall) > 0) duration * wall / sum(wall) else duration
    if (length(allocated) > 1L) {
      allocated[[length(allocated)]] <- duration - sum(allocated[-length(allocated)])
    }
    rows[[i]] <- data.frame(
      source_row = i,
      date = appusage_date_from_datetime(ms = endpoints[-length(endpoints)], tz = tz),
      start_ts_ms = endpoints[-length(endpoints)],
      end_ts_ms = endpoints[-1L],
      duration_ms = allocated,
      package_name = appusage_chr_col(x[i, , drop = FALSE], "package_name"),
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

appusage_interval_overlap_day <- function(x, config) {
  ord <- order(x$start_ts_ms, -x$end_ts_ms, x$source_row)
  x <- x[ord, , drop = FALSE]
  start <- as.numeric(x$start_ts_ms)
  end <- as.numeric(x$end_ts_ms)
  package <- as.character(x$package_name)
  overlap_count <- appusage_overlap_pair_count(start, end)
  package_present <- !is.na(package)
  cross_package <- 0
  if (any(package_present)) {
    nonmissing_overlap <- appusage_overlap_pair_count(
      start[package_present],
      end[package_present]
    )
    package_groups <- split(
      which(package_present),
      package[package_present],
      drop = TRUE
    )
    same_package_overlap <- sum(vapply(package_groups, function(idx) {
      appusage_overlap_pair_count(start[idx], end[idx])
    }, numeric(1)))
    cross_package <- nonmissing_overlap - same_package_overlap
  }
  prior_max_end <- c(-Inf, cummax(utils::head(end, -1L)))
  contained <- sum(prior_max_end > start & prior_max_end >= end)

  times <- sort(unique(c(x$start_ts_ms, x$end_ts_ms)))
  overlap_ms <- 0
  max_concurrent <- 0L
  if (length(times) > 1L) {
    start_count <- tabulate(match(x$start_ts_ms, times), nbins = length(times))
    end_count <- tabulate(match(x$end_ts_ms, times), nbins = length(times))
    delta <- start_count - end_count
    concurrent <- cumsum(delta)
    max_concurrent <- max(concurrent)
    overlap_ms <- sum(diff(times) * pmax(concurrent[-length(concurrent)] - 1L, 0L))
  }
  list(
    date = as.character(x$date[[1L]]),
    overlap_count = as.integer(overlap_count),
    overlap_ms = as.numeric(overlap_ms),
    cross_package_overlap_count = as.integer(cross_package),
    contained_interval_count = as.integer(contained),
    max_concurrent_intervals = as.integer(max_concurrent),
    foreground_duration_ms = sum(x$duration_ms),
    foreground_over_24h = sum(x$duration_ms) > config$max_line_daily_foreground_ms
  )
}

appusage_overlap_pair_count <- function(start, end) {
  if (length(start) == 0L) return(0)
  start <- as.numeric(start)
  end <- as.numeric(end)
  positive <- end > start
  count <- 0
  if (any(positive)) {
    positive_start <- start[positive]
    positive_end <- end[positive]
    ord <- order(positive_start, -positive_end, seq_along(positive_start))
    positive_start <- positive_start[ord]
    positive_end <- positive_end[ord]
    ended <- findInterval(positive_start, sort(positive_end))
    count <- count + sum(seq_along(positive_start) - 1L - ended)
  }
  zero <- end == start
  if (any(zero) && any(positive)) {
    point <- start[zero]
    active_at_point <- findInterval(point, sort(start[positive])) -
      findInterval(point, sort(end[positive]))
    count <- count + sum(active_at_point)
  }
  as.numeric(count)
}

appusage_line_timestamp_qc <- function(episode, config) {
  out <- list(
    status = "not_applicable", n_missing_start = 0L, n_missing_end = 0L,
    n_missing_duration = 0L, n_negative_interval = 0L, n_zero_interval = 0L,
    n_over_24h_interval = 0L, n_cross_midnight_valid = 0L,
    n_source_date_mismatch_raw = 0L, n_malformed_source_date_mismatch = 0L,
    diagnostic = FALSE, warning = FALSE, critical = FALSE
  )
  line <- appusage_line_episode_rows(episode)
  if (!is.data.frame(line) || nrow(line) == 0L) return(out)
  out$status <- "success"
  start <- appusage_num_col(line, "start_ts_ms")
  end <- appusage_num_col(line, "end_ts_ms")
  duration <- appusage_num_col(line, "duration_ms")
  start_date <- appusage_date_from_datetime(ms = start, tz = config$effective_timezone)
  end_date <- appusage_date_from_datetime(ms = end, tz = config$effective_timezone)
  valid <- !is.na(start) & !is.na(end) & !is.na(duration) & end >= start & duration >= 0
  cross <- valid & start_date != end_date
  source_date <- appusage_date_col(line, "source_table_date")
  raw_mismatch <- appusage_lgl_col(line, "source_date_timestamp_date_mismatch")
  acceptable_cross <- cross & !is.na(source_date) &
    (source_date == start_date | source_date == end_date)
  malformed <- raw_mismatch & !acceptable_cross
  out$n_missing_start <- sum(is.na(start))
  out$n_missing_end <- sum(is.na(end))
  out$n_missing_duration <- sum(is.na(duration))
  out$n_negative_interval <- sum(
    (!is.na(start) & !is.na(end) & end < start) | (!is.na(duration) & duration < 0)
  )
  out$n_zero_interval <- sum(valid & (end == start | duration == 0))
  out$n_over_24h_interval <- sum(valid & duration > config$max_line_interval_ms)
  out$n_cross_midnight_valid <- sum(cross)
  out$n_source_date_mismatch_raw <- sum(raw_mismatch)
  out$n_malformed_source_date_mismatch <- sum(malformed)
  out$diagnostic <- out$n_cross_midnight_valid > 0L || out$n_zero_interval > 0L
  out$warning <- out$n_malformed_source_date_mismatch > 0L || out$n_zero_interval > 0L
  out$critical <- sum(c(
    out$n_missing_start, out$n_missing_end, out$n_missing_duration,
    out$n_negative_interval, out$n_over_24h_interval
  )) > 0L
  out
}

appusage_meta_summary_qc <- function(daily, config) {
  out <- list(
    status = "not_applicable", n_rows = 0L, n_interval_crosses_date = 0L,
    n_duration_over_24h = 0L, n_repeated_cumulative_rows = 0L,
    n_episode_daily_available = 0L, n_duration_matched_exact = 0L,
    n_duration_matched_different = 0L, n_duration_not_compared = 0L,
    n_summary_only_keys = 0L, n_episode_only_keys = 0L,
    max_abs_duration_diff_ms = NA_real_, max_abs_duration_diff_ratio = NA_real_,
    diagnostic = FALSE, warning = FALSE, critical = FALSE
  )
  if (!is.data.frame(daily) || nrow(daily) == 0L) return(out)
  source <- appusage_chr_col(daily, "daily_source")
  summary_rows <- !is.na(source) & source == "meta_summary"
  episode_rows <- !is.na(source) & source == "meta_episodes"
  if (!any(summary_rows, na.rm = TRUE) && !any(episode_rows, na.rm = TRUE)) return(out)
  out$status <- "success"
  out$n_rows <- sum(summary_rows, na.rm = TRUE)
  crosses <- appusage_lgl_col(daily, "summary_interval_crosses_date")
  over <- appusage_lgl_col(daily, "summary_duration_over_24h")
  repeated <- appusage_lgl_col(daily, "summary_repeated_cumulative")
  available <- appusage_lgl_col(daily, "episode_daily_available")
  agreement <- appusage_chr_col(daily, "duration_agreement_status")
  diff <- abs(appusage_num_col(daily, "duration_diff_ms"))
  ratio <- abs(appusage_num_col(daily, "duration_diff_pct"))
  out$n_interval_crosses_date <- sum(summary_rows & crosses, na.rm = TRUE)
  out$n_duration_over_24h <- sum(summary_rows & over, na.rm = TRUE)
  out$n_repeated_cumulative_rows <- sum(summary_rows & repeated, na.rm = TRUE)
  out$n_episode_daily_available <- sum(summary_rows & available, na.rm = TRUE)
  out$n_duration_matched_exact <- sum(
    summary_rows & !is.na(agreement) & agreement == "matched_exact", na.rm = TRUE
  )
  out$n_duration_matched_different <- sum(
    summary_rows & !is.na(agreement) & agreement == "matched_with_difference", na.rm = TRUE
  )
  out$n_duration_not_compared <- sum(
    summary_rows & !is.na(agreement) & agreement == "not_compared", na.rm = TRUE
  )
  out$n_summary_only_keys <- sum(
    summary_rows & !is.na(agreement) & agreement == "summary_only", na.rm = TRUE
  )
  out$n_episode_only_keys <- sum(
    episode_rows & !is.na(agreement) & agreement == "episode_only", na.rm = TRUE
  )
  if (any(!is.na(diff))) out$max_abs_duration_diff_ms <- max(diff, na.rm = TRUE)
  if (any(!is.na(ratio))) out$max_abs_duration_diff_ratio <- max(ratio, na.rm = TRUE)
  disagreement <- summary_rows & !is.na(agreement) & agreement == "matched_with_difference" &
    ((!is.na(diff) & diff > config$meta_diff_abs_ms) |
      (!is.na(ratio) & ratio > config$meta_diff_ratio))
  out$diagnostic <- out$n_rows > 0L
  out$warning <- out$n_interval_crosses_date > 0L || any(disagreement, na.rm = TRUE) ||
    out$n_summary_only_keys > 0L || out$n_episode_only_keys > 0L
  out$critical <- out$n_duration_over_24h > 0L || out$n_repeated_cumulative_rows > 0L
  out
}

appusage_meta_reconstruction_qc <- function(episode, daily, config) {
  out <- list(
    status = "not_applicable", n_zero_duration = 0L,
    n_timeline_clipped_to_nonpositive = 0L, n_duration_inferred = 0L,
    n_merged_episode_rows = 0L, total_merged_gap_ms = 0,
    n_source_date_mismatch = 0L, n_summary_only_keys = 0L,
    n_episode_only_keys = 0L, eligible_global_overlap_count = 0L,
    eligible_global_overlap_ms = 0, diagnostic = FALSE, warning = FALSE,
    critical = FALSE
  )
  if (!is.data.frame(episode)) return(out)
  source <- appusage_chr_col(episode, "episode_source")
  meta <- episode[!is.na(source) & source == "meta_events", , drop = FALSE]
  if (nrow(meta) == 0L) return(out)
  out$status <- "success"
  duration <- appusage_num_col(meta, "duration_ms")
  warning <- appusage_chr_col(meta, "reconstruction_warning")
  out$n_zero_duration <- sum(!is.na(duration) & duration == 0)
  out$n_timeline_clipped_to_nonpositive <- sum(grepl(
    "timeline_clipped_to_nonpositive", warning, fixed = TRUE
  ), na.rm = TRUE)
  out$n_duration_inferred <- sum(grepl("duration_inferred", warning, fixed = TRUE), na.rm = TRUE)
  merged_gap <- appusage_num_col(meta, "merged_gap_ms")
  source_count <- appusage_num_col(meta, "source_episode_count")
  out$n_merged_episode_rows <- sum((!is.na(source_count) & source_count > 1) |
    (!is.na(merged_gap) & merged_gap > 0))
  out$total_merged_gap_ms <- sum(merged_gap[!is.na(merged_gap) & merged_gap > 0])
  out$n_source_date_mismatch <- sum(appusage_lgl_col(
    meta, "source_date_timestamp_date_mismatch"
  ))
  agreement <- appusage_chr_col(daily, "duration_agreement_status")
  out$n_summary_only_keys <- sum(agreement == "summary_only", na.rm = TRUE)
  out$n_episode_only_keys <- sum(agreement == "episode_only", na.rm = TRUE)
  reconstruction_status <- appusage_chr_col(meta, "reconstruction_status")
  complete <- !is.na(reconstruction_status) & reconstruction_status == "complete" &
    !is.na(duration) & duration > 0 &
    !is.na(appusage_num_col(meta, "start_ts_ms")) &
    !is.na(appusage_num_col(meta, "end_ts_ms"))
  if (any(complete, na.rm = TRUE)) {
    eligible <- meta[complete, , drop = FALSE]
    segments <- appusage_source_qc_interval_segments(eligible, config$effective_timezone)
    days <- split(seq_len(nrow(segments)), as.character(segments$date))
    checks <- lapply(days, function(idx) {
      appusage_interval_overlap_day(segments[idx, , drop = FALSE], config)
    })
    out$eligible_global_overlap_count <- sum(vapply(checks, `[[`, integer(1), "overlap_count"))
    out$eligible_global_overlap_ms <- sum(vapply(checks, `[[`, numeric(1), "overlap_ms"))
  }
  out$diagnostic <- any(unlist(out[c(
    "n_zero_duration", "n_timeline_clipped_to_nonpositive", "n_duration_inferred",
    "n_merged_episode_rows", "n_source_date_mismatch", "n_summary_only_keys",
    "n_episode_only_keys", "eligible_global_overlap_count"
  )]) > 0)
  out$warning <- out$diagnostic
  out$critical <- out$eligible_global_overlap_count > 0L
  out
}

appusage_source_qc_anomaly_checks <- function(source_qc) {
  line_overlap <- source_qc$line_foreground_overlap
  line_time <- source_qc$line_timestamp_date
  meta_summary <- source_qc$meta_cumulative_summary
  meta_reconstruction <- source_qc$meta_reconstruction
  checks <- list()
  if (isTRUE(line_overlap$warning) || isTRUE(line_overlap$critical)) {
    checks$line_foreground_overlap <- list(
      grain = "episode",
      severity = if (isTRUE(line_overlap$critical)) "critical" else "warning",
      count = as.integer(line_overlap$overlap_count)
    )
  }
  if (line_overlap$exact_duplicate_count > 0L) {
    checks$line_exact_duplicate <- list(
      grain = "episode",
      severity = if (line_overlap$exact_duplicate_ratio >
        source_qc$config$line_duplicate_critical_ratio) "critical" else "warning",
      count = as.integer(line_overlap$exact_duplicate_count)
    )
  }
  checks$line_daily_foreground_over_24h <- list(
    grain = "daily", severity = "critical",
    count = as.integer(line_overlap$n_daily_foreground_over_24h)
  )
  checks$line_malformed_source_date_mismatch <- list(
    grain = "episode", severity = "warning",
    count = as.integer(line_time$n_malformed_source_date_mismatch)
  )
  checks$meta_summary_duration_over_24h <- list(
    grain = "daily", severity = "critical",
    count = as.integer(meta_summary$n_duration_over_24h)
  )
  checks$meta_summary_repeated_cumulative <- list(
    grain = "daily", severity = "critical",
    count = as.integer(meta_summary$n_repeated_cumulative_rows)
  )
  checks$meta_eligible_global_overlap <- list(
    grain = "episode", severity = "critical",
    count = as.integer(meta_reconstruction$eligible_global_overlap_count)
  )
  appusage_drop_zero_checks(checks)
}

appusage_drop_zero_checks <- function(checks) {
  checks[vapply(checks, function(x) !is.na(x$count) && x$count > 0L, logical(1))]
}

appusage_meta_summary_qc_fields <- function(x, max_duration_ms,
                                             tz = appusage_default_timezone()) {
  tz <- appusage_resolve_timezone(tz)
  start <- appusage_num_col(x, "start_ts_ms")
  end <- appusage_num_col(x, "end_ts_ms")
  if (all(is.na(start)) && "start_datetime" %in% names(x)) {
    start <- as.numeric(as.POSIXct(x$start_datetime, tz = tz)) * 1000
  }
  if (all(is.na(end)) && "end_datetime" %in% names(x)) {
    end <- as.numeric(as.POSIXct(x$end_datetime, tz = tz)) * 1000
  }
  span <- end - start
  start_date <- appusage_date_from_datetime(ms = start, tz = tz)
  end_date <- appusage_date_from_datetime(ms = end, tz = tz)
  duration <- appusage_num_col(x, "total_duration_ms")
  date <- appusage_date_col(x, "table_date")
  package <- appusage_chr_col(x, "package_name")
  app <- appusage_chr_col(x, "app_name")
  repeated_key <- paste(package, app, duration, sep = "\r")
  repeated <- rep(FALSE, nrow(x))
  valid <- !is.na(duration) & !is.na(date)
  if (any(valid)) {
    date_counts <- tapply(as.character(date[valid]), repeated_key[valid], function(z) {
      length(unique(z))
    })
    repeated[valid] <- date_counts[repeated_key[valid]] > 1L
  }
  over <- !is.na(duration) & duration > max_duration_ms
  eligible <- !(over | repeated)
  reason <- rep(NA_character_, nrow(x))
  reason[over] <- append_warning(reason[over], "meta_summary_duration_over_24h")
  reason[repeated] <- append_warning(reason[repeated], "meta_summary_repeated_cumulative")
  list(
    summary_interval_start_ts_ms = start,
    summary_interval_end_ts_ms = end,
    summary_interval_span_ms = span,
    summary_interval_crosses_date = !is.na(start_date) & !is.na(end_date) & start_date != end_date,
    summary_duration_over_24h = over,
    summary_repeated_cumulative = repeated,
    episode_daily_available = rep(FALSE, nrow(x)),
    analysis_eligible_daily = eligible,
    analysis_ineligibility_reason = reason
  )
}

appusage_source_qc_summary_from_metadata <- function(metadata) {
  source <- metadata$source_anomaly_qc %||% metadata$anomaly_qc$source_anomaly_qc
  get <- function(path, default) {
    value <- source
    for (name in path) {
      if (is.null(value) || !is.list(value) || is.null(value[[name]])) return(default)
      value <- value[[name]]
    }
    if (length(value) == 0L || all(is.na(value))) return(default)
    value[[1L]]
  }
  list(
    source_qc_status = get(c("status"), NA_character_),
    source_qc_rule_version = get(c("rule_version"), NA_character_),
    source_qc_severity = get(c("severity"), NA_character_),
    line_overlap_count = as.integer(get(c("line_foreground_overlap", "overlap_count"), NA_integer_)),
    line_overlap_ms = as.numeric(get(c("line_foreground_overlap", "overlap_ms"), NA_real_)),
    line_cross_package_overlap_count = as.integer(get(c("line_foreground_overlap", "cross_package_overlap_count"), NA_integer_)),
    line_contained_interval_count = as.integer(get(c("line_foreground_overlap", "contained_interval_count"), NA_integer_)),
    line_exact_duplicate_count = as.integer(get(c("line_foreground_overlap", "exact_duplicate_count"), NA_integer_)),
    line_exact_duplicate_ratio = as.numeric(get(c("line_foreground_overlap", "exact_duplicate_ratio"), NA_real_)),
    line_overlap_ratio = as.numeric(get(c("line_foreground_overlap", "overlap_ratio"), NA_real_)),
    line_max_concurrent_intervals = as.integer(get(c("line_foreground_overlap", "max_concurrent_intervals"), NA_integer_)),
    line_max_daily_foreground_ms = as.numeric(get(c("line_foreground_overlap", "max_daily_foreground_ms"), NA_real_)),
    line_daily_foreground_over_24h = as.integer(get(c("line_foreground_overlap", "n_daily_foreground_over_24h"), NA_integer_)),
    line_cross_midnight_valid = as.integer(get(c("line_timestamp_date", "n_cross_midnight_valid"), NA_integer_)),
    line_malformed_source_date_mismatch = as.integer(get(c("line_timestamp_date", "n_malformed_source_date_mismatch"), NA_integer_)),
    meta_summary_duration_over_24h = as.integer(get(c("meta_cumulative_summary", "n_duration_over_24h"), NA_integer_)),
    meta_summary_repeated_cumulative = as.integer(get(c("meta_cumulative_summary", "n_repeated_cumulative_rows"), NA_integer_)),
    meta_summary_only_keys = as.integer(get(c("meta_cumulative_summary", "n_summary_only_keys"), NA_integer_)),
    meta_episode_only_keys = as.integer(get(c("meta_cumulative_summary", "n_episode_only_keys"), NA_integer_)),
    meta_zero_duration_episodes = as.integer(get(c("meta_reconstruction", "n_zero_duration"), NA_integer_)),
    meta_clipped_to_nonpositive_episodes = as.integer(get(c("meta_reconstruction", "n_timeline_clipped_to_nonpositive"), NA_integer_)),
    meta_duration_inferred_episodes = as.integer(get(c("meta_reconstruction", "n_duration_inferred"), NA_integer_)),
    meta_merged_episode_rows = as.integer(get(c("meta_reconstruction", "n_merged_episode_rows"), NA_integer_)),
    meta_total_merged_gap_ms = as.numeric(get(c("meta_reconstruction", "total_merged_gap_ms"), NA_real_)),
    meta_eligible_global_overlap_count = as.integer(get(c("meta_reconstruction", "eligible_global_overlap_count"), NA_integer_)),
    source_qc_episode_ineligible = as.logical(get(c("eligibility", "episode_ineligible"), NA)),
    source_qc_daily_ineligible = as.logical(get(c("eligibility", "daily_ineligible"), NA)),
    source_qc_episode_reasons = get(c("eligibility", "episode_reasons"), NA_character_),
    source_qc_daily_reasons = get(c("eligibility", "daily_reasons"), NA_character_)
  )
}

appusage_source_qc_summary_values <- function(metadata_file = NA_character_) {
  if (!is_present_string(metadata_file) || !file.exists(metadata_file)) {
    return(appusage_source_qc_summary_from_metadata(list()))
  }
  metadata <- tryCatch(
    jsonlite::read_json(metadata_file, simplifyVector = TRUE),
    error = function(e) list()
  )
  appusage_source_qc_summary_from_metadata(metadata)
}
