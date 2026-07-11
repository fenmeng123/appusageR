daily_test_ms <- function(x) {
  as.numeric(as.POSIXct(x, tz = "Asia/Shanghai")) * 1000
}

daily_test_episodes <- function(package_name, app_name, date, duration_ms,
                                activity_type = "foreground") {
  n <- length(package_name)
  start <- daily_test_ms(paste(date, "12:00:00")) + seq_len(n) * 100000
  end <- start + ifelse(is.na(duration_ms), 0, duration_ms)
  conform_second_episode(tibble::tibble(
    date = as.Date(date), app_name = app_name, activity_type = activity_type,
    package_name = package_name, start_ts_ms = start, end_ts_ms = end,
    start_datetime = ms_to_datetime(start), end_datetime = ms_to_datetime(end),
    duration_ms = duration_ms, duration_min = duration_ms / 60000,
    duration_text = NA_character_, source_episode_count = 1L,
    source_duration_ms = duration_ms, merged_gap_ms = 0,
    source_export_type = "line", parse_warning = NA_character_,
    is_collection_app = FALSE, episode_source = "line",
    anomaly_any = is.na(duration_ms) | duration_ms < 0
  ))
}

expect_daily_ordered <- function(x) {
  expect_identical(appusage_daily_order_index(x), seq_len(nrow(x)))
}

test_that("line daily keys remain aligned for non-alphabetic first occurrence", {
  packages <- c(
    "z.pkg", "a.pkg", "m.pkg", "b.pkg", "y.pkg", "c.pkg",
    "x.pkg", "d.pkg", "w.pkg", "e.pkg", "v.pkg", "f.pkg"
  )
  durations <- seq_along(packages) * 111
  episodes <- daily_test_episodes(
    package_name = c(packages, "z.pkg", "a.pkg"),
    app_name = c(paste0("App ", packages), "App z.pkg", "App a.pkg"),
    date = c(rep("2024-01-02", length(packages)), "2024-01-02", "2024-01-03"),
    duration_ms = c(durations, 889, 222)
  )
  daily <- daily_from_episodes(episodes, max_daily_app_ms = 86400000)
  expected <- stats::aggregate(
    episodes$duration_ms,
    list(
      date = episodes$date, package_name = episodes$package_name,
      app_name = episodes$app_name, activity_type = episodes$activity_type
    ),
    sum
  )
  names(expected)[[5L]] <- "duration_ms"
  daily_key <- appusage_daily_key(daily, include_daily_source = FALSE)
  expected_key <- appusage_daily_key(expected, include_daily_source = FALSE)
  aligned <- match(expected_key, daily_key)

  expect_false(anyNA(aligned))
  expect_equal(daily$duration_ms[aligned], expected$duration_ms)
  expect_equal(daily$episode_count[daily$package_name == "z.pkg"], 2L)
  expect_daily_ordered(daily)
})

test_that("duplicate episodes and NA duration groups retain distinct semantics", {
  duplicate <- daily_test_episodes(
    c("dup.pkg", "dup.pkg"), c("Duplicate", "Duplicate"),
    c("2024-01-01", "2024-01-01"), c(1000, 1000)
  )
  invalid <- daily_test_episodes(
    c("na.pkg", "na.pkg"), c("Missing", "Missing"),
    c("2024-01-01", "2024-01-01"), c(NA_real_, -1)
  )
  daily <- daily_from_episodes(
    dplyr::bind_rows(duplicate, invalid), max_daily_app_ms = 86400000
  )
  dup_row <- daily[daily$package_name == "dup.pkg", , drop = FALSE]
  na_row <- daily[daily$package_name == "na.pkg", , drop = FALSE]

  expect_equal(dup_row$duration_ms, 2000)
  expect_equal(dup_row$episode_count, 2L)
  expect_true(is.na(na_row$duration_ms))
  expect_equal(na_row$episode_count, 2L)
  expect_equal(na_row$n_anomalies, 2L)
})

test_that("cross-midnight unique episode counts and duration are conserved", {
  start <- daily_test_ms("2024-01-01 23:59:30")
  episodes <- daily_test_episodes(
    "cross.pkg", "Cross", "2024-01-01", 60000
  )
  episodes$start_ts_ms <- start
  episodes$end_ts_ms <- start + 60000
  episodes$start_datetime <- ms_to_datetime(episodes$start_ts_ms)
  episodes$end_datetime <- ms_to_datetime(episodes$end_ts_ms)
  daily <- daily_from_episodes(episodes, max_daily_app_ms = 86400000)

  expect_equal(daily$date, as.Date(c("2024-01-01", "2024-01-02")))
  expect_equal(daily$duration_ms, c(30000, 30000))
  expect_equal(daily$episode_count, c(1L, 1L))
  expect_equal(sum(daily$duration_ms), episodes$duration_ms)
})

test_that("self-check distinguishes missing duration from numeric mismatch", {
  episodes <- daily_test_episodes(
    "a.pkg", "A", "2024-01-01", 1000
  )
  data <- list(
    event = empty_second_event_tibble(), episode = episodes,
    daily = daily_from_episodes(episodes, 86400000)
  )
  missing <- data
  missing$daily$duration_ms <- NA_real_
  missing_check <- appusage_validate_second_level_daily(missing)
  mismatch <- data
  mismatch$daily$duration_ms <- mismatch$daily$duration_ms + 1
  mismatch_check <- appusage_validate_second_level_daily(mismatch)
  duplicate <- data
  duplicate$daily <- dplyr::bind_rows(duplicate$daily, duplicate$daily)
  duplicate_check <- appusage_validate_second_level_daily(duplicate)

  expect_equal(missing_check$status, "warning")
  expect_equal(missing_check$n_missing_daily_duration, 1L)
  expect_equal(missing_check$n_nonmissing_numeric_mismatch, 0L)
  expect_equal(mismatch_check$status, "error")
  expect_equal(mismatch_check$n_nonmissing_numeric_mismatch, 1L)
  expect_equal(duplicate_check$status, "error")
  expect_equal(duplicate_check$n_duplicate_daily_keys, 1L)
})

test_that("line grouping and ordering are invariant to input permutation", {
  episodes <- daily_test_episodes(
    c("z.pkg", "a.pkg", "z.pkg", "m.pkg"),
    c("Z", "A", "Z", "M"),
    c("2024-01-02", "2024-01-01", "2024-01-02", "2024-01-01"),
    c(1000, 2000, 3000, 4000)
  )
  first <- daily_from_episodes(episodes, 86400000)
  permuted <- daily_from_episodes(episodes[c(4, 2, 1, 3), ], 86400000)

  expect_identical(first, permuted)
})

test_that("every daily source uses the documented deterministic order", {
  day <- tibble::tibble(
    date = as.Date(c("2024-01-02", "2024-01-01")),
    app_name = c("Z", "A"), package_name = c("z.pkg", "a.pkg"),
    duration_ms = c(1, 2), open_count = c(1L, 1L),
    notification_count = c(0L, 0L), split_screen_ms = c(0, 0),
    export_type = "day", parse_warning = NA_character_
  )
  app <- day
  app$export_type <- "app"
  summary <- tibble::tibble(
    table_date = as.Date(c("2024-01-02", "2024-01-01")),
    app_name = c("Z", "A"), package_name = c("z.pkg", "a.pkg"),
    total_duration_ms = c(1000, 2000), parse_warning = NA_character_
  )
  events <- tibble::tibble(
    table_date = as.Date("2024-01-01"), app_name = "M",
    package_name = "m.pkg", class_name = "Main",
    event_datetime = ms_to_datetime(c(
      daily_test_ms("2024-01-01 10:00:00"),
      daily_test_ms("2024-01-01 10:01:00")
    )),
    event_ts_ms = c(
      daily_test_ms("2024-01-01 10:00:00"),
      daily_test_ms("2024-01-01 10:01:00")
    ),
    event_type = c(1, 2), event_type_label = label_event_type(c(1, 2)),
    configuration = NA_character_, parse_warning = NA_character_
  )
  outputs <- list(
    make_second_level_appusage(list(line = daily_test_episodes(
      c("z.pkg", "a.pkg"), c("Z", "A"),
      c("2024-01-02", "2024-01-01"), c(1, 2)
    )))$daily,
    make_second_level_appusage(list(day = day))$daily,
    make_second_level_appusage(list(app = app))$daily,
    make_second_level_appusage(
      list(meta_summary = summary), reconstruct_meta = FALSE
    )$daily,
    make_second_level_appusage(
      list(meta_summary = summary, meta_events = events),
      reconstruct_meta = TRUE, meta_daily_source = "episodes"
    )$daily,
    make_second_level_appusage(
      list(meta_summary = summary, meta_events = events),
      reconstruct_meta = TRUE, meta_daily_source = "both"
    )$daily
  )
  lapply(outputs, expect_daily_ordered)
})

test_that("daily self-check failure cannot replace an existing valid proc-2 pair", {
  root <- file.path(tempdir(), paste0("daily_atomic_", sample.int(1e8, 1)))
  proc1 <- file.path(root, "proclevel-1")
  proc2 <- file.path(root, "proclevel-2")
  dir.create(proc1, recursive = TRUE)
  first_file <- file.path(proc1, "sub-p1_type-line_proc-1.rda")
  data <- list(line = daily_test_episodes(
    "a.pkg", "A", "2024-01-01", 1000
  ))
  save(data, file = first_file)
  second_file <- write_second_level_appusage(first_file, output_dir = proc2)
  metadata_file <- second_level_metadata_path(second_file)
  original_hash <- tools::md5sum(c(second_file, metadata_file))
  original_make <- make_second_level_appusage
  testthat::local_mocked_bindings(
    make_second_level_appusage = function(...) {
      out <- original_make(...)
      out$daily$duration_ms[[1L]] <- out$daily$duration_ms[[1L]] + 1
      out
    },
    .package = "appusageR"
  )

  expect_error(
    write_second_level_appusage(first_file, output_dir = proc2, overwrite = TRUE),
    class = "appusage_daily_aggregation_error"
  )
  expect_equal(tools::md5sum(c(second_file, metadata_file)), original_hash)
  metadata <- jsonlite::read_json(metadata_file, simplifyVector = TRUE)
  expect_equal(metadata$daily_aggregation_self_check$status, "success")
  summary <- build_qc_summary_from_metadata(metadata_file)
  expect_equal(summary$daily_self_check_status, "success")
  expect_equal(summary$daily_self_check_numeric_mismatch, 0L)
})
