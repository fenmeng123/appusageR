test_that("qc_appusage_anomalies counts episode and daily reasonableness issues", {
  base <- as.POSIXct("2024-01-02 08:00:00", tz = "Asia/Shanghai")
  episode <- tibble::tibble(
    start_ts_ms = c(
      as.numeric(base) * 1000,
      as.numeric(base + 60) * 1000,
      NA_real_,
      as.numeric(base + 120) * 1000,
      as.numeric(as.POSIXct("2024-01-02 23:59:00", tz = "Asia/Shanghai")) * 1000
    ),
    end_ts_ms = c(
      as.numeric(base + 60) * 1000,
      as.numeric(base + 120) * 1000,
      as.numeric(base + 180) * 1000,
      NA_real_,
      as.numeric(as.POSIXct("2024-01-03 00:01:00", tz = "Asia/Shanghai")) * 1000
    ),
    duration_ms = c(0, -100, NA, 90000, 120000),
    device_boundary_involved = c(FALSE, FALSE, FALSE, TRUE, FALSE),
    unmatched_start = c(FALSE, FALSE, TRUE, FALSE, FALSE),
    unmatched_end = c(FALSE, TRUE, FALSE, FALSE, FALSE),
    reconstruction_status = c("complete", "invalid_pair", "unmatched_start", "complete", "complete"),
    reconstruction_warning = c(NA_character_, "negative duration", NA_character_, "boundary", NA_character_)
  )
  daily <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-02", "2024-01-03")),
    duration_ms = c(60000, 70000, 0, NA_real_),
    duration_agreement_status = c(
      "matched_with_difference",
      "summary_only",
      "episode_only",
      "not_compared"
    ),
    duration_diff_ms = c(120000, NA_real_, NA_real_, NA_real_),
    duration_diff_pct = c(0.50, NA_real_, NA_real_, NA_real_)
  )

  result <- qc_appusage_anomalies(
    list(episode = episode, daily = daily),
    max_episode_ms = 60000,
    max_daily_app_ms = 65000,
    max_daily_total_ms = 100000,
    meta_diff_abs_ms = 60000,
    meta_diff_ratio = 0.20
  )

  expect_equal(result$status, "success")
  expect_gt(result$n_critical_anomalies, 0)
  expect_gt(result$n_warning_anomalies, 0)
  expect_gt(result$n_episode_anomalies, 0)
  expect_gt(result$n_daily_anomalies, 0)
  expect_equal(result$n_meta_duration_disagreements, 1L)
  expect_equal(result$max_abs_meta_duration_diff_ms, 120000)
  expect_equal(result$max_daily_total_ms_observed, 130000)
  expect_true(result$has_critical_anomaly)
  expect_true(result$has_warning_anomaly)
})

test_that("qc_appusage_anomalies counts event and export-span anomalies", {
  event <- tibble::tibble(
    package_name = c("pkg", "pkg", "pkg2"),
    event_ts_ms = c(1704585600000, 1704585500000, NA_real_),
    event_type = c(1, 999, NA_real_),
    event_type_label = c(
      "ACTIVITY_RESUMED_OR_MOVE_TO_FOREGROUND",
      "EVENT_TYPE_999",
      NA_character_
    )
  )
  daily <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-02-02")),
    duration_ms = c(1000, 1000)
  )
  metadata <- list(
    export = list(native_export_created_at = "2024-01-10T08:00:00+0800")
  )

  result <- qc_appusage_anomalies(
    list(event = event, daily = daily),
    metadata = metadata,
    max_export_lookback_days = 5
  )

  expect_gt(result$n_event_anomalies, 0)
  expect_gt(result$n_export_span_anomalies, 0)
  expect_equal(result$n_anomalies_by_type$event_missing_timestamp, 1L)
  expect_equal(result$n_anomalies_by_type$event_missing_type, 1L)
  expect_equal(result$n_anomalies_by_type$event_unknown_type, 1L)
  expect_equal(result$n_anomalies_by_type$event_non_monotonic_timestamp, 1L)
  expect_equal(result$n_anomalies_by_type$export_record_after_export_date, 1L)
  expect_equal(result$n_anomalies_by_type$export_record_older_than_lookback, 1L)
  expect_true(result$max_observed_export_lookback_days >= 9)
})

test_that("qc_appusage_anomalies handles individual grains", {
  event_only <- qc_appusage_anomalies(tibble::tibble(
    event_ts_ms = NA_real_,
    event_type = 1
  ))
  episode_only <- qc_appusage_anomalies(tibble::tibble(
    start_ts_ms = NA_real_,
    end_ts_ms = NA_real_,
    duration_ms = NA_real_
  ))
  daily_only <- qc_appusage_anomalies(tibble::tibble(
    date = as.Date("2024-01-01"),
    duration_ms = -1
  ))

  expect_gt(event_only$n_event_anomalies, 0)
  expect_gt(episode_only$n_episode_anomalies, 0)
  expect_gt(daily_only$n_daily_anomalies, 0)
})
