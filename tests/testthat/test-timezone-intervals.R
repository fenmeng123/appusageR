local_ms <- function(x, tz = "Asia/Shanghai") {
  as.numeric(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = tz)) * 1000
}

synthetic_line_interval <- function(start, end, source_date = as.Date("2024-01-01"),
                                    tz = "Asia/Shanghai") {
  start_ms <- local_ms(start, tz)
  end_ms <- local_ms(end, tz)
  tibble::tibble(
    participant_id = "p1", source_file = "synthetic.txt", export_type = "line",
    date = source_date, source_table_date = source_date,
    source_date_timestamp_date_mismatch = FALSE,
    app_name = "Example", package_name = "com.example.app",
    start_ts_ms = start_ms, end_ts_ms = end_ms,
    start_datetime = ms_to_datetime(start_ms, tz = tz),
    end_datetime = ms_to_datetime(end_ms, tz = tz),
    start_time_text = NA_character_, end_time_text = NA_character_,
    duration_text = NA_character_, duration_ms = end_ms - start_ms,
    duration_min = (end_ms - start_ms) / 60000,
    is_collection_app = FALSE, parse_warning = NA_character_
  )
}

synthetic_meta_interval <- function(start, end, source_date = as.Date("2024-01-01"),
                                    tz = "Asia/Shanghai") {
  timestamps <- c(local_ms(start, tz), local_ms(end, tz))
  tibble::tibble(
    table_date = source_date,
    source_table_date = source_date,
    date = appusage_date_from_datetime(ms = timestamps, tz = tz),
    source_date_timestamp_date_mismatch = FALSE,
    app_name = "Example", package_name = "com.example.app",
    class_name = "Main", event_datetime = ms_to_datetime(timestamps, tz = tz),
    event_ts_ms = timestamps, event_type = c(1, 2),
    event_type_label = label_event_type(c(1, 2)),
    configuration = NA_character_, parse_warning = NA_character_
  )
}

test_that("default timestamp-derived dates are host-TZ independent Shanghai dates", {
  instant_ms <- as.numeric(as.POSIXct("2024-01-01 16:30:00", tz = "UTC")) * 1000
  observed <- lapply(c("UTC", "America/New_York"), function(process_tz) {
    withr::local_envvar(TZ = process_tz)
    appusage_date_from_datetime(ms = instant_ms)
  })

  expect_equal(observed[[1]], as.Date("2024-01-02"))
  expect_identical(observed[[1]], observed[[2]])
  expect_equal(
    appusage_date_from_datetime(ms = instant_ms, tz = "UTC"),
    as.Date("2024-01-01")
  )
})

test_that("early Shanghai hours do not shift to the prior date", {
  hours <- sprintf("2024-01-02 %02d:30:00", 0:7)
  milliseconds <- vapply(hours, local_ms, numeric(1))
  withr::local_envvar(TZ = "UTC")

  expect_equal(
    appusage_date_from_datetime(ms = milliseconds),
    rep(as.Date("2024-01-02"), 8)
  )
})

test_that("line episodes preserve source date and use canonical timestamp date", {
  line <- synthetic_line_interval(
    "2024-01-02 00:30:00", "2024-01-02 00:31:00",
    source_date = as.Date("2024-01-01")
  )
  second <- make_second_level_appusage(list(line = line))

  expect_equal(second$episode$date, as.Date("2024-01-02"))
  expect_equal(second$episode$source_table_date, as.Date("2024-01-01"))
  expect_true(second$episode$source_date_timestamp_date_mismatch)
})

test_that("public second-level timezone override changes canonical date explicitly", {
  start_ms <- as.numeric(as.POSIXct("2024-01-01 16:30:00", tz = "UTC")) * 1000
  line <- synthetic_line_interval(
    "2024-01-02 00:30:00", "2024-01-02 00:31:00",
    source_date = as.Date("2024-01-01")
  )
  line$start_ts_ms <- start_ms
  line$end_ts_ms <- start_ms + 60000
  line$start_datetime <- ms_to_datetime(line$start_ts_ms, tz = "UTC")
  line$end_datetime <- ms_to_datetime(line$end_ts_ms, tz = "UTC")

  default <- make_second_level_appusage(list(line = line))
  utc <- make_second_level_appusage(list(line = line), tz = "UTC")
  expect_equal(default$episode$date, as.Date("2024-01-02"))
  expect_equal(utc$episode$date, as.Date("2024-01-01"))
})

test_that("meta events preserve source date beside canonical timestamp date", {
  events <- synthetic_meta_interval(
    "2024-01-02 00:30:00", "2024-01-02 00:31:00",
    source_date = as.Date("2024-01-01")
  )
  second <- make_second_level_appusage(
    list(meta_events = events), reconstruct_meta = FALSE
  )
  expect_true(all(second$event$date == as.Date("2024-01-02")))
  expect_true(all(second$event$source_table_date == as.Date("2024-01-01")))
  expect_true(all(second$event$source_date_timestamp_date_mismatch))
})

test_that("line cross-midnight daily allocation conserves exact duration", {
  line <- synthetic_line_interval(
    "2024-01-01 23:59:30", "2024-01-02 00:00:30"
  )
  second <- make_second_level_appusage(list(line = line))
  diagnostics <- attr(second, "interval_segmentation_diagnostics")$line

  expect_equal(nrow(second$episode), 1L)
  expect_equal(second$episode$duration_ms, 60000)
  expect_equal(second$daily$date, as.Date(c("2024-01-01", "2024-01-02")))
  expect_equal(second$daily$duration_ms, c(30000, 30000))
  expect_equal(sum(second$daily$duration_ms), second$episode$duration_ms)
  expect_equal(diagnostics$n_cross_midnight_intervals, 1L)
  expect_equal(diagnostics$duration_conservation_diff_ms, 0)
})

test_that("multi-midnight active duration is conserved with merged-gap semantics", {
  episode <- conform_second_episode(tibble::tibble(
    date = as.Date("2024-01-01"), app_name = "Example",
    activity_type = "foreground", package_name = "com.example.app",
    start_ts_ms = local_ms("2024-01-01 23:00:00"),
    end_ts_ms = local_ms("2024-01-03 01:00:00"),
    start_datetime = as.POSIXct("2024-01-01 23:00:00", tz = "Asia/Shanghai"),
    end_datetime = as.POSIXct("2024-01-03 01:00:00", tz = "Asia/Shanghai"),
    duration_ms = 25 * 60 * 60 * 1000,
    duration_min = 25 * 60, source_episode_count = 2L,
    source_duration_ms = 25 * 60 * 60 * 1000,
    merged_gap_ms = 60 * 60 * 1000, source_export_type = "line",
    is_collection_app = FALSE, episode_source = "line", anomaly_any = FALSE
  ))
  daily <- daily_from_episodes(episode, 48 * 60 * 60 * 1000)

  expect_equal(nrow(daily), 3L)
  expect_equal(sum(daily$duration_ms), episode$duration_ms)
  expect_equal(
    episode$end_ts_ms - episode$start_ts_ms,
    episode$duration_ms + episode$merged_gap_ms
  )
})

test_that("meta cross-midnight episode daily allocation conserves duration", {
  first <- list(
    meta_events = synthetic_meta_interval(
      "2024-01-01 23:59:30", "2024-01-02 00:00:30"
    )
  )
  second <- make_second_level_appusage(
    first, reconstruct_meta = TRUE, meta_daily_source = "episodes"
  )
  diagnostics <- attr(second, "interval_segmentation_diagnostics")$meta

  expect_equal(nrow(second$episode), 1L)
  expect_equal(second$daily$date, as.Date(c("2024-01-01", "2024-01-02")))
  expect_equal(second$daily$duration_ms, c(30000, 30000))
  expect_equal(sum(second$daily$duration_ms), second$episode$duration_ms)
  expect_equal(diagnostics$n_cross_midnight_intervals, 1L)
  expect_equal(diagnostics$duration_conservation_diff_ms, 0)
})

test_that("interval segmentation policies diagnose zero missing and negative rows", {
  base <- synthetic_line_interval("2024-01-01 12:00:00", "2024-01-01 12:01:00")
  zero <- base
  zero$end_ts_ms <- zero$start_ts_ms
  zero$end_datetime <- zero$start_datetime
  zero$duration_ms <- 0
  missing <- base
  missing$start_ts_ms <- NA_real_
  missing$start_datetime <- as.POSIXct(NA_real_, origin = "1970-01-01", tz = "Asia/Shanghai")
  missing$duration_ms <- NA_real_
  negative <- base
  negative$end_ts_ms <- negative$start_ts_ms - 1
  negative$duration_ms <- -1
  segments <- appusage_interval_segments(dplyr::bind_rows(zero, missing, negative))
  diagnostics <- attr(segments, "interval_segmentation_diagnostics")

  expect_setequal(
    segments$.interval_status,
    c("zero_interval", "missing_interval", "invalid_negative_interval")
  )
  expect_equal(diagnostics$n_zero_intervals, 1L)
  expect_equal(diagnostics$n_missing_intervals, 1L)
  expect_equal(diagnostics$n_negative_or_invalid_intervals, 1L)
})

test_that("interval segmentation scales while preserving source order and duration", {
  n <- 10000L
  base <- local_ms("2024-01-01 12:00:00")
  template <- synthetic_line_interval(
    "2024-01-01 12:00:00", "2024-01-01 12:00:01"
  )
  dense <- template[rep(1L, n), , drop = FALSE]
  dense$start_ts_ms <- base + seq_len(n) * 2000
  dense$end_ts_ms <- dense$start_ts_ms + 1000
  dense$duration_ms <- 1000
  dense$duration_min <- dense$duration_ms / 60000
  dense$start_datetime <- ms_to_datetime(dense$start_ts_ms)
  dense$end_datetime <- ms_to_datetime(dense$end_ts_ms)

  segments <- appusage_interval_segments(dense)
  diagnostics <- attr(segments, "interval_segmentation_diagnostics")

  expect_equal(nrow(segments), n)
  expect_identical(segments$.source_row_id, seq_len(n))
  expect_true(all(segments$.segment_index == 1L))
  expect_true(all(segments$.segment_count == 1L))
  expect_true(all(segments$.interval_status == "valid"))
  expect_equal(sum(segments$duration_ms), sum(dense$duration_ms))
  expect_equal(diagnostics$n_source_intervals, n)
  expect_equal(diagnostics$n_daily_segments, n)
  expect_equal(diagnostics$duration_conservation_diff_ms, 0)
})

test_that("complete reconstructed meta episodes retain a non-overlapping timeline", {
  events <- dplyr::bind_rows(
    synthetic_meta_interval("2024-01-01 22:00:00", "2024-01-01 22:30:00"),
    transform(
      synthetic_meta_interval("2024-01-01 22:10:00", "2024-01-01 22:20:00"),
      app_name = "Other", package_name = "com.example.other"
    )
  )
  episodes <- reconstruct_meta_episodes(events)
  complete <- episodes[episodes$reconstruction_status == "complete", , drop = FALSE]
  complete <- complete[order(complete$start_ts_ms, complete$end_ts_ms), , drop = FALSE]

  expect_false(any(complete$start_ts_ms[-1L] < complete$end_ts_ms[-nrow(complete)]))
  expect_true(any(grepl("timeline_clipped", complete$reconstruction_warning)))
  expect_true(all(
    complete$end_ts_ms - complete$start_ts_ms == complete$duration_ms
  ))
})

test_that("workflow configuration records the effective timezone", {
  config <- appusage_build_workflow_configuration(
    raw_data_root = NULL, resolved_project_dir = tempdir(),
    resolved_self_report_file = NULL,
    project = list(project_id = "1", project_name = "Study", project_root = tempdir()),
    output_root = tempdir(), sequence_col = "sequence", upload_col = NULL,
    submit_time_col = NULL, max_files = Inf, self_report_n_max = Inf,
    self_report_sheet = 1, self_report_guess_max = NULL,
    self_report_col_types = NULL, self_report_read = list(),
    export_type_priority = c("line", "meta", "day", "app"),
    effective_timezone = "Asia/Shanghai", resume = TRUE, overwrite = FALSE,
    first_level_options = list(tz = "Asia/Shanghai"),
    second_level_options = list(tz = "Asia/Shanghai"),
    qc_options = list(), category_options = list()
  )
  expect_equal(config$effective_timezone, "Asia/Shanghai")
})

test_that("proc-2 metadata records timezone and interval diagnostics", {
  second <- make_second_level_appusage(list(line = synthetic_line_interval(
    "2024-01-01 23:59:30", "2024-01-02 00:00:30"
  )))
  metadata <- build_second_level_success_metadata(
    first_metadata = list(
      export = list(timezone = "Asia/Shanghai"), processing = list(),
      outputs = list(), counts = list()
    ),
    first_level_rda = tempfile(fileext = ".rda"),
    second_level_rda = tempfile(fileext = ".rda"),
    second_level_data = second, include_collection_app = TRUE,
    max_episode_ms = 86400000, max_daily_app_ms = 86400000,
    reconstruct_meta = FALSE, meta_pairing = "package",
    meta_start_event_types = 1, meta_end_event_types = c(2, 23),
    merge_meta_episodes = TRUE, meta_episode_merge_gap_ms = 30000,
    meta_daily_source = "summary", tz = "Asia/Shanghai",
    started_at = Sys.time(), finished_at = Sys.time()
  )
  expect_equal(metadata$processing$effective_timezone, "Asia/Shanghai")
  expect_equal(metadata$second_level$parameters$timezone, "Asia/Shanghai")
  expect_equal(metadata$interval_segmentation$line$n_cross_midnight_intervals, 1L)
  expect_equal(metadata$interval_segmentation$line$duration_conservation_diff_ms, 0)
})
