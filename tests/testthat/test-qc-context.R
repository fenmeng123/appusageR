test_that("QC shares selected rows, columns and masks without changing their identity", {
  episode <- data.frame(
    source_export_type = c("line", "meta", NA, "line"),
    episode_source = c("line", "meta_events", "line", NA),
    start_ts_ms = c(1000, 2000, NA, 4000),
    end_ts_ms = c(2000, 1000, 3000, 4000),
    duration_ms = c(1000, -1000, NA, 0), row.names = c("a", "b", "c", "d")
  )
  grains <- appusage_normalize_anomaly_input(list(episode = episode))
  context <- appusage_qc_context(grains, appusage_source_qc_config())
  view <- appusage_qc_episode_view(episode, "line", context)
  expect_identical(view$rows, c(1L, 3L, 4L))
  expect_identical(view$data, appusage_line_episode_rows(episode))
  expect_identical(appusage_qc_episode_view(episode, "meta", context)$rows, 2L)
  expect_identical(appusage_qc_episode_view(episode, "line", context), view)
  expect_identical(appusage_qc_col(view$data, "start_ts_ms", "num", context,
    rows = view$rows), appusage_num_col(view$data, "start_ts_ms"))
  expect_identical(appusage_qc_col(episode, "absent", "lgl", context), rep(FALSE, 4))
  expected <- appusage_qc_valid_intervals(episode$start_ts_ms,
    episode$end_ts_ms, episode$duration_ms)
  expect_identical(appusage_qc_valid_intervals(NULL, NULL, NULL, context), expected)
  expect_identical(appusage_qc_valid_intervals(NULL, NULL, NULL, context,
    view$rows), expected[view$rows])
  expect_identical(context$grains, grains)
})

test_that("QC dates are computed once per vector, zone and endpoint convention", {
  episode <- data.frame(start_ts_ms = c(1710046800000, 1730606400000, NA),
    end_ts_ms = c(1710129600000, 1730696400000, NA))
  calls <- 0L
  original <- appusage_date_from_datetime_validated
  local_mocked_bindings(appusage_date_from_datetime_validated = function(...) {
    calls <<- calls + 1L
    original(...)
  })
  context <- appusage_qc_context(appusage_normalize_anomaly_input(list(episode = episode)),
    appusage_source_qc_config())
  a <- appusage_qc_date(episode, "start_ts_ms", context, ms = TRUE)
  expect_equal(calls, 1L)
  expect_identical(appusage_qc_date(episode[2:3, ], "start_ts_ms", context,
    rows = 2:3, ms = TRUE), a[2:3])
  expect_equal(calls, 1L)
  for (zone in c("Asia/Shanghai", "UTC", "America/New_York")) {
    for (offset in c(0, -0.001)) {
      expected <- appusage_date_from_ms(episode$end_ts_ms + offset, zone)
      expect_identical(appusage_qc_date(episode, "end_ts_ms", context,
        tz = zone, ms = TRUE, offset = offset), expected)
    }
  }
  end <- appusage_qc_date(episode, "end_ts_ms", context, tz = "America/New_York", ms = TRUE)
  before <- appusage_qc_date(episode, "end_ts_ms", context, tz = "America/New_York", ms = TRUE, offset = -0.001)
  # Keep the existing millisecond parser's precision behavior; test the cache
  # keys independently rather than imposing a new timestamp-rounding contract.
  expect_length(ls(context$dates), 7L)
  expect_identical(end, appusage_date_from_ms(episode$end_ts_ms, "America/New_York"))
  expect_identical(before, appusage_date_from_ms(episode$end_ts_ms - 0.001, "America/New_York"))
})

test_that("QC cached interval views retain DST splitting and local source indices", {
  for (zone in c("Asia/Shanghai", "UTC", "America/New_York")) {
    start <- as.numeric(as.POSIXct(c("2024-03-09 23:30:00", "2024-11-02 23:30:00",
      "2024-01-01 12:00:00"), tz = zone)) * 1000
    end <- as.numeric(as.POSIXct(c("2024-03-11 00:00:00", "2024-11-04 00:00:00",
      "2024-01-01 12:00:00"), tz = zone)) * 1000
    episode <- data.frame(start_ts_ms = start, end_ts_ms = end,
      duration_ms = c(123456789.123, 98765432.987, 0), package_name = c("a", NA, "b"))
    context <- appusage_qc_context(appusage_normalize_anomaly_input(list(episode = episode)),
      appusage_source_qc_config(list(effective_timezone = zone)))
    for (rows in list(1:3, c(3L, 2L), integer())) {
      x <- episode[rows, , drop = FALSE]
      expect_identical(appusage_source_qc_interval_segments(x, zone, context, rows),
        appusage_source_qc_interval_segments(x, zone))
    }
  }
})

test_that("cached observed dates preserve types, order and duplicates", {
  grains <- appusage_normalize_anomaly_input(list(
    daily = data.frame(date = as.Date(c("2024-01-01", NA))),
    episode = data.frame(start_ts_ms = c(1704038400000, NA),
      end_ts_ms = c(1704124800000, 1704038400000)),
    event = data.frame(event_ts_ms = c(1704038400000, 1704038400000, NA))
  ))
  context <- appusage_qc_context(grains, appusage_source_qc_config())
  expect_identical(appusage_observed_dates(grains, context), appusage_observed_dates(grains))
  expect_identical(appusage_observed_dates(grains, context), appusage_observed_dates(grains, context))
  result <- qc_appusage_anomalies(grains)
  has_environment <- function(x) is.environment(x) ||
    (is.list(x) && any(vapply(x, has_environment, logical(1))))
  expect_false(has_environment(result))
  expect_identical(grains, context$grains)
})
