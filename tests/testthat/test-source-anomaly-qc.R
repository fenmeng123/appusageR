source_qc_episode <- function(start, end, package, app = package,
                              activity = "foreground", collection = FALSE,
                              source = "line", date = as.Date("2024-01-01"),
                              source_date = date, duration = end - start,
                              status = NA_character_, warning = NA_character_,
                              source_count = 1L, merged_gap = 0) {
  tibble::tibble(
    date = date,
    source_table_date = source_date,
    source_date_timestamp_date_mismatch = source_date != date,
    app_name = app,
    activity_type = activity,
    package_name = package,
    start_ts_ms = start,
    end_ts_ms = end,
    duration_ms = duration,
    source_episode_count = source_count,
    merged_gap_ms = merged_gap,
    source_export_type = source,
    episode_source = ifelse(source == "meta", "meta_events", "line"),
    reconstruction_status = status,
    reconstruction_warning = warning,
    is_collection_app = collection
  )
}

legacy_interval_overlap_day <- function(x, config) {
  ord <- order(x$start_ts_ms, -x$end_ts_ms, x$source_row)
  x <- x[ord, , drop = FALSE]
  overlap_count <- 0L
  cross_package <- 0L
  contained <- 0L
  for (i in seq_len(nrow(x))) {
    if (i == 1L) next
    prior <- seq_len(i - 1L)
    active <- prior[x$end_ts_ms[prior] > x$start_ts_ms[[i]]]
    overlap_count <- overlap_count + length(active)
    if (length(active)) {
      cross_package <- cross_package + sum(
        x$package_name[active] != x$package_name[[i]], na.rm = TRUE
      )
      contained <- contained + as.integer(any(
        x$end_ts_ms[active] >= x$end_ts_ms[[i]]
      ))
    }
  }
  times <- sort(unique(c(x$start_ts_ms, x$end_ts_ms)))
  overlap_ms <- 0
  max_concurrent <- 0L
  if (length(times) > 1L) {
    delta <- vapply(times, function(value) {
      sum(x$start_ts_ms == value) - sum(x$end_ts_ms == value)
    }, integer(1))
    concurrent <- cumsum(delta)
    max_concurrent <- max(concurrent)
    overlap_ms <- sum(
      diff(times) * pmax(concurrent[-length(concurrent)] - 1L, 0L)
    )
  }
  list(
    date = as.character(x$date[[1L]]),
    overlap_count = as.integer(overlap_count),
    overlap_ms = as.numeric(overlap_ms),
    cross_package_overlap_count = as.integer(cross_package),
    contained_interval_count = as.integer(contained),
    max_concurrent_intervals = as.integer(max_concurrent),
    foreground_duration_ms = sum(x$duration_ms),
    foreground_over_24h = sum(x$duration_ms) >
      config$max_line_daily_foreground_ms
  )
}

test_that("line source QC reports overlap families without removing rows", {
  base <- as.numeric(as.POSIXct("2024-01-01 12:00:00", tz = "Asia/Shanghai")) * 1000
  episode <- dplyr::bind_rows(
    source_qc_episode(base, base + 10000, "pkg.a"),
    source_qc_episode(base, base + 10000, "pkg.a"),
    source_qc_episode(base + 1000, base + 9000, "pkg.b"),
    source_qc_episode(base + 2000, base + 3000, "pkg.c"),
    source_qc_episode(base + 100, base + 8000, "pkg.bg", activity = "background"),
    source_qc_episode(base + 100, base + 8000, "com.w.appusage", collection = TRUE)
  )
  data <- list(event = data.frame(), episode = episode, daily = data.frame())
  qc <- appusageR:::appusage_source_anomaly_qc(data)

  expect_equal(nrow(data$episode), 6L)
  expect_equal(qc$line_foreground_overlap$n_foreground_intervals, 4L)
  expect_equal(qc$line_foreground_overlap$n_background_intervals, 1L)
  expect_equal(qc$line_foreground_overlap$n_collection_intervals, 1L)
  expect_gt(qc$line_foreground_overlap$cross_package_overlap_count, 0L)
  expect_gt(qc$line_foreground_overlap$contained_interval_count, 0L)
  expect_equal(qc$line_foreground_overlap$exact_duplicate_count, 1L)
  expect_equal(qc$line_foreground_overlap$max_concurrent_intervals, 4L)
  expect_true(qc$line_foreground_overlap$critical)
  expect_true(qc$eligibility$episode_ineligible)
})

test_that("line overlap thresholds distinguish diagnostic warning and critical", {
  base <- as.numeric(as.POSIXct("2024-01-01 12:00:00", tz = "Asia/Shanghai")) * 1000
  make_qc <- function(overlap) {
    episode <- dplyr::bind_rows(
      source_qc_episode(base, base + 100000, "pkg.a"),
      source_qc_episode(base + 100000 - overlap, base + 200000 - overlap, "pkg.b")
    )
    appusageR:::appusage_source_anomaly_qc(list(episode = episode))$line_foreground_overlap
  }
  diagnostic <- make_qc(1000)
  warning <- make_qc(5000)
  critical <- make_qc(30000)
  expect_true(diagnostic$diagnostic)
  expect_false(diagnostic$warning)
  expect_true(warning$warning)
  expect_false(warning$critical)
  expect_true(critical$critical)

  custom <- appusageR:::appusage_source_anomaly_qc(
    list(episode = dplyr::bind_rows(
      source_qc_episode(base, base + 100000, "pkg.a"),
      source_qc_episode(base + 99000, base + 199000, "pkg.b")
    )),
    config = list(line_overlap_warning_ratio = 0.001)
  )
  expect_true(custom$line_foreground_overlap$warning)
})

test_that("sweep-line overlap metrics match the legacy pairwise definition", {
  set.seed(3401)
  n <- 250L
  start <- sample(0:100, n, replace = TRUE) * 1000
  duration <- sample(c(0, 1000, 2000, 5000, 20000), n, replace = TRUE)
  intervals <- data.frame(
    source_row = seq_len(n),
    date = as.Date("2024-01-01"),
    start_ts_ms = start,
    end_ts_ms = start + duration,
    duration_ms = duration,
    package_name = sample(c("pkg.a", "pkg.b", "pkg.c", NA_character_),
      n, replace = TRUE
    ),
    stringsAsFactors = FALSE
  )
  config <- appusageR:::appusage_source_qc_config()

  expected <- legacy_interval_overlap_day(intervals, config)
  observed <- appusageR:::appusage_interval_overlap_day(intervals, config)

  expect_equal(observed, expected)
})

test_that("sweep-line overlap QC handles dense intervals without quadratic scans", {
  n <- 10000L
  intervals <- data.frame(
    source_row = seq_len(n),
    date = as.Date("2024-01-01"),
    start_ts_ms = seq_len(n) - 1,
    end_ts_ms = seq_len(n) + 1,
    duration_ms = 2,
    package_name = rep(c("pkg.a", "pkg.b"), length.out = n),
    stringsAsFactors = FALSE
  )
  observed <- appusageR:::appusage_interval_overlap_day(
    intervals,
    appusageR:::appusage_source_qc_config()
  )

  expect_equal(observed$overlap_count, n - 1L)
  expect_equal(observed$cross_package_overlap_count, n - 1L)
  expect_equal(observed$contained_interval_count, 0L)
  expect_equal(observed$max_concurrent_intervals, 2L)
  expect_equal(observed$overlap_ms, n - 1L)
})

test_that("line timestamp QC distinguishes valid midnight crossing from malformed dates", {
  start <- as.numeric(as.POSIXct("2024-01-01 23:59:00", tz = "Asia/Shanghai")) * 1000
  episode <- dplyr::bind_rows(
    source_qc_episode(
      start, start + 120000, "pkg.cross",
      date = as.Date("2024-01-01"), source_date = as.Date("2024-01-02")
    ),
    source_qc_episode(
      start, start + 1000, "pkg.bad",
      date = as.Date("2024-01-01"), source_date = as.Date("2023-12-30")
    ),
    source_qc_episode(NA_real_, NA_real_, "pkg.missing", duration = NA_real_),
    source_qc_episode(start + 1000, start, "pkg.negative", duration = -1000),
    source_qc_episode(start, start, "pkg.zero", duration = 0),
    source_qc_episode(start, start + 25 * 3600000, "pkg.long")
  )
  qc <- appusageR:::appusage_source_anomaly_qc(list(episode = episode))$line_timestamp_date
  expect_equal(qc$n_cross_midnight_valid, 2L)
  expect_equal(qc$n_malformed_source_date_mismatch, 1L)
  expect_equal(qc$n_missing_duration, 1L)
  expect_gt(qc$n_negative_interval, 0L)
  expect_gt(qc$n_zero_interval, 0L)
  expect_gt(qc$n_over_24h_interval, 0L)
  expect_true(qc$critical)
})

test_that("meta summary QC preserves cumulative rows and comparison evidence", {
  summary <- tibble::tibble(
    table_date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    app_name = c("Video", "Video", "Other"),
    package_name = c("pkg.video", "pkg.video", "pkg.other"),
    start_ts_ms = c(0, 86400000, 2 * 86400000),
    end_ts_ms = c(2 * 86400000, 2 * 86400000, 3 * 86400000),
    total_duration_ms = c(25 * 3600000, 25 * 3600000, 1000),
    parse_warning = NA_character_
  )
  daily <- appusageR:::second_level_meta_summary(summary, 24 * 3600000)
  daily$duration_agreement_status <- c("matched_with_difference", "summary_only", "matched_exact")
  daily$duration_diff_ms <- c(120000, NA, 0)
  daily$duration_diff_pct <- c(0.25, NA, 0)
  daily$episode_daily_available <- c(TRUE, FALSE, TRUE)
  qc <- appusageR:::appusage_source_anomaly_qc(list(daily = daily))

  expect_equal(nrow(daily), 3L)
  expect_true(all(daily$summary_duration_over_24h[1:2]))
  expect_true(all(daily$summary_repeated_cumulative[1:2]))
  expect_true(all(!daily$analysis_eligible_daily[1:2]))
  expect_gt(qc$meta_cumulative_summary$n_interval_crosses_date, 0L)
  expect_equal(qc$meta_cumulative_summary$n_duration_over_24h, 2L)
  expect_equal(qc$meta_cumulative_summary$n_repeated_cumulative_rows, 2L)
  expect_true(qc$eligibility$daily_ineligible)
})

test_that("meta reconstruction QC exposes retained reconstruction diagnostics", {
  base <- as.numeric(as.POSIXct("2024-01-01 12:00:00", tz = "Asia/Shanghai")) * 1000
  episode <- dplyr::bind_rows(
    source_qc_episode(base, base + 1000, "pkg.a", source = "meta", status = "complete"),
    source_qc_episode(
      base + 1000, base + 1000, "pkg.b", source = "meta",
      status = "invalid_pair", warning = "timeline_clipped_to_nonpositive", duration = 0
    ),
    source_qc_episode(
      base + 2000, base + 3000, "pkg.c", source = "meta",
      status = "complete", warning = "duration_inferred_from_event_duration",
      source_count = 2L, merged_gap = 50
    )
  )
  daily <- tibble::tibble(duration_agreement_status = c("summary_only", "episode_only"))
  qc <- appusageR:::appusage_source_anomaly_qc(list(episode = episode, daily = daily))
  meta <- qc$meta_reconstruction
  expect_equal(meta$n_zero_duration, 1L)
  expect_equal(meta$n_timeline_clipped_to_nonpositive, 1L)
  expect_equal(meta$n_duration_inferred, 1L)
  expect_equal(meta$n_merged_episode_rows, 1L)
  expect_equal(meta$total_merged_gap_ms, 50)
  expect_equal(meta$n_summary_only_keys, 1L)
  expect_equal(meta$n_episode_only_keys, 1L)
  expect_equal(meta$eligible_global_overlap_count, 0L)
})

test_that("source critical anomalies tighten eligibility without changing routine pass", {
  base <- as.numeric(as.POSIXct("2024-01-01 12:00:00", tz = "Asia/Shanghai")) * 1000
  episode <- dplyr::bind_rows(
    source_qc_episode(base, base + 10000, "pkg.a"),
    source_qc_episode(base, base + 10000, "pkg.b")
  )
  daily <- tibble::tibble(
    participant_id = "p1",
    date = as.Date("2024-01-01") + 0:6,
    package_name = "pkg.a",
    app_name = "A",
    duration_ms = 1000,
    is_all_apps = FALSE,
    is_collection_app = FALSE
  )
  result <- appusageR:::run_qc_for_second_level_data(
    data = list(event = data.frame(), episode = episode, daily = daily),
    second_level_rda = tempfile(fileext = ".rda"), participant_id = "p1",
    require_all_weekdays = TRUE, min_nonempty_days = 7,
    use_all_apps_row = FALSE, include_collection_app = TRUE,
    drop_likely_total_all_rows = TRUE, all_row_tolerance = 0.10
  )
  expect_true(result$qc$pass_qc)
  expect_false(result$qc$analysis_eligible_episode)
  expect_false(result$qc$analysis_eligible_daily)
  expect_equal(result$counts$n_episode_rows, 2L)
  expect_match(result$qc$analysis_ineligible_daily_reasons, "line_foreground_overlap")
})

test_that("source QC fields persist into metadata summary and checkpoint rows", {
  source <- appusageR:::appusage_source_anomaly_qc(list(
    episode = dplyr::bind_rows(
      source_qc_episode(0, 1000, "pkg.a"),
      source_qc_episode(0, 1000, "pkg.b")
    )
  ))
  metadata <- list(
    participant_id = "p1",
    processing = list(second_level_status = "success", qc_status = "success"),
    anomaly_qc = list(source_anomaly_qc = source),
    source_anomaly_qc = source,
    counts = list(n_event_rows = 0L, n_episode_rows = 2L, n_daily_rows = 0L),
    outputs = list(second_level_rda = "x.rda")
  )
  path <- tempfile(fileext = ".json")
  jsonlite::write_json(metadata, path, auto_unbox = TRUE, pretty = TRUE, na = "null")
  summary <- appusageR:::qc_summary_row_from_loaded_metadata(metadata, path)
  checkpoint <- appusageR:::appusage_attach_daily_self_check_summary(
    data.frame(status = "success"), path
  )
  expect_equal(summary$source_qc_rule_version, "0.3.4-F")
  expect_gt(summary$line_overlap_count, 0L)
  expect_equal(checkpoint$source_qc_rule_version, "0.3.4-F")
  expect_true(checkpoint$source_qc_episode_ineligible)
})
