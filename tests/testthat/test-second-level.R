test_that("make_second_level_appusage creates episode and daily data from line", {
  line <- parse_line(testthat::test_path("fixtures", "line_sample.txt"))
  second <- make_second_level_appusage(list(line = line))

  expect_named(second, c("event", "episode", "daily"))
  expect_equal(nrow(second$event), 0)
  expect_equal(nrow(second$episode), 1)
  expect_equal(second$episode$duration_ms[[1]], 12556)
  expect_equal(nrow(second$daily), 1)
  expect_equal(second$daily$duration_ms[[1]], 12556)
  expect_equal(second$daily$episode_count[[1]], 1)
  expect_false(second$episode$anomaly_any[[1]])
})

test_that("make_second_level_appusage creates event and daily data from meta", {
  meta <- parse_meta(testthat::test_path("fixtures", "meta_sample.txt"))
  second <- make_second_level_appusage(meta)

  expect_equal(nrow(second$event), 2)
  expect_equal(nrow(second$episode), 1)
  expect_equal(second$episode$duration_ms[[1]], 1800000)
  expect_equal(nrow(second$daily), 1)
  expect_equal(second$daily$duration_ms[[1]], 1800000)
  expect_equal(
    second$event$event_type_label,
    c("ACTIVITY_RESUMED_OR_MOVE_TO_FOREGROUND", "ACTIVITY_PAUSED_OR_MOVE_TO_BACKGROUND")
  )
})

test_that("activity_type marks full-width and half-width media rows", {
  line <- tibble::tibble(
    date = as.Date(rep("2024-01-01", 4)),
    app_name = c(
      paste0("Video", "\uFF08\u6D41\u5A92\u4F53\uFF09"),
      paste0("Audio", "(\u6D41\u5A92\u4F53)"),
      "Plain App",
      NA_character_
    ),
    package_name = c("video.pkg", "audio.pkg", "plain.pkg", "missing.pkg"),
    start_ts_ms = c(1704067200000, 1704067210000, 1704067220000, 1704067230000),
    end_ts_ms = c(1704067201000, 1704067211000, 1704067221000, 1704067231000),
    start_datetime = as.POSIXct(
      c(
        "2024-01-01 00:00:00", "2024-01-01 00:00:10",
        "2024-01-01 00:00:20", "2024-01-01 00:00:30"
      ),
      tz = "Asia/Shanghai"
    ),
    end_datetime = as.POSIXct(
      c(
        "2024-01-01 00:00:01", "2024-01-01 00:00:11",
        "2024-01-01 00:00:21", "2024-01-01 00:00:31"
      ),
      tz = "Asia/Shanghai"
    ),
    duration_text = "1s",
    duration_ms = rep(1000, 4),
    parse_warning = NA_character_
  )

  second <- make_second_level_appusage(list(line = line))

  expect_equal(nrow(second$episode), 4)
  expect_equal(nrow(second$daily), 4)
  expect_equal(
    second$episode$activity_type,
    c("background", "background", "foreground", "foreground")
  )
  daily_activity <- stats::setNames(
    second$daily$activity_type,
    second$daily$package_name
  )
  expect_equal(daily_activity[["video.pkg"]], "background")
  expect_equal(daily_activity[["audio.pkg"]], "background")
  expect_equal(daily_activity[["plain.pkg"]], "foreground")
  expect_equal(daily_activity[["missing.pkg"]], "foreground")
  expect_equal(sum(second$episode$duration_ms), sum(line$duration_ms))
  expect_equal(sum(second$daily$duration_ms), sum(line$duration_ms))
})

test_that("activity_type exists in day and app daily outputs without changing row counts", {
  daily <- tibble::tibble(
    participant_id = "p1",
    source_file = "synthetic.txt",
    export_type = "day",
    date = as.Date(c("2024-01-01", "2024-01-02", "2024-01-03")),
    weekday = weekdays(as.Date(c("2024-01-01", "2024-01-02", "2024-01-03"))),
    app_name = c(
      paste0("Video", "\uFF08\u6D41\u5A92\u4F53\uFF09"),
      paste0("Audio", "(\u6D41\u5A92\u4F53)"),
      "Plain App"
    ),
    package_name = c("video.pkg", "audio.pkg", "plain.pkg"),
    duration_text = "1s",
    duration_ms = c(1000, 2000, 3000),
    duration_min = c(1000, 2000, 3000) / 60000,
    open_count = c(1L, 2L, 3L),
    notification_count = c(0L, 0L, 0L),
    split_screen_ms = c(0, 0, 0),
    is_all_apps = FALSE,
    is_collection_app = FALSE,
    parse_warning = NA_character_
  )
  app <- daily
  app$export_type <- "app"

  day_second <- make_second_level_appusage(list(day = daily))
  app_second <- make_second_level_appusage(list(app = app))

  expect_equal(nrow(day_second$daily), nrow(daily))
  expect_equal(nrow(app_second$daily), nrow(app))
  expect_equal(day_second$daily$activity_type, c("background", "background", "foreground"))
  expect_equal(app_second$daily$activity_type, c("background", "background", "foreground"))
  expect_equal(day_second$daily$duration_ms, daily$duration_ms)
  expect_equal(app_second$daily$duration_ms, app$duration_ms)
})

test_that("activity_type exists in meta event and daily outputs", {
  meta_events <- tibble::tibble(
    table_date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
    app_name = c(
      paste0("Video", "\uFF08\u6D41\u5A92\u4F53\uFF09"),
      paste0("Audio", "(\u6D41\u5A92\u4F53)"),
      "Plain App"
    ),
    package_name = c("video.pkg", "audio.pkg", "plain.pkg"),
    class_name = "MainActivity",
    event_datetime = as.POSIXct(
      c("2024-01-01 00:00:00", "2024-01-01 00:00:10", "2024-01-01 00:00:20"),
      tz = "Asia/Shanghai"
    ),
    event_ts_ms = c(1704067200000, 1704067210000, 1704067220000),
    event_type = c(1, 2, 1),
    event_type_label = c(
      "ACTIVITY_RESUMED_OR_MOVE_TO_FOREGROUND",
      "ACTIVITY_PAUSED_OR_MOVE_TO_BACKGROUND",
      "ACTIVITY_RESUMED_OR_MOVE_TO_FOREGROUND"
    ),
    configuration = NA_character_,
    parse_warning = NA_character_
  )
  meta_summary <- tibble::tibble(
    table_date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01")),
    app_name = meta_events$app_name,
    package_name = meta_events$package_name,
    total_duration_ms = c(1000, 2000, 3000),
    parse_warning = NA_character_
  )

  second <- make_second_level_appusage(list(
    meta_events = meta_events,
    meta_summary = meta_summary
  ))

  expect_equal(nrow(second$event), nrow(meta_events))
  expect_equal(nrow(second$daily), nrow(meta_summary))
  expect_equal(second$event$activity_type, c("background", "background", "foreground"))
  expect_equal(second$daily$activity_type, c("background", "background", "foreground"))
})

test_that("activity_type is present in empty second-level tibbles", {
  empty <- make_second_level_appusage(list(line = tibble::tibble(
    date = as.Date(character()),
    app_name = character(),
    package_name = character(),
    start_ts_ms = numeric(),
    end_ts_ms = numeric(),
    start_datetime = as.POSIXct(character()),
    end_datetime = as.POSIXct(character()),
    duration_text = character(),
    duration_ms = numeric(),
    parse_warning = character()
  )))

  expect_true("activity_type" %in% names(empty$event))
  expect_true("activity_type" %in% names(empty$episode))
  expect_true("activity_type" %in% names(empty$daily))
})

test_that("line day and app daily outputs share one canonical schema", {
  line <- parse_line(testthat::test_path("fixtures", "line_sample.txt"))
  day <- parse_day(testthat::test_path("fixtures", "day_sample.txt"))
  app <- parse_app(testthat::test_path("fixtures", "app_sample.txt"))
  meta <- parse_meta(testthat::test_path("fixtures", "meta_sample.txt"))

  line_second <- make_second_level_appusage(list(line = line))
  day_second <- make_second_level_appusage(list(day = day))
  app_second <- make_second_level_appusage(list(app = app))
  meta_second <- make_second_level_appusage(meta)

  daily_names <- names(line_second$daily)
  expect_identical(names(day_second$daily), daily_names)
  expect_identical(names(app_second$daily), daily_names)
  expect_identical(names(meta_second$daily), daily_names)
  expect_true("n_anomalies" %in% daily_names)
  expect_type(line_second$daily$n_anomalies, "integer")
  expect_type(day_second$daily$n_anomalies, "integer")
  expect_type(app_second$daily$n_anomalies, "integer")

  n_before <- c(
    line = nrow(line_second$daily),
    day = nrow(day_second$daily),
    app = nrow(app_second$daily)
  )
  duration_before <- c(
    line = sum(line_second$daily$duration_ms, na.rm = TRUE),
    day = sum(day_second$daily$duration_ms, na.rm = TRUE),
    app = sum(app_second$daily$duration_ms, na.rm = TRUE)
  )

  combined <- dplyr::bind_rows(
    line = line_second$daily,
    day = day_second$daily,
    app = app_second$daily,
    .id = "source"
  )
  duration_after <- vapply(
    split(combined$duration_ms, combined$source),
    sum,
    numeric(1),
    na.rm = TRUE
  )

  expect_equal(nrow(combined), sum(n_before))
  expect_equal(duration_after[names(duration_before)], duration_before)
})

test_that("make_second_level_appusage preserves day-level numeric fields", {
  day <- parse_day(testthat::test_path("fixtures", "day_sample.txt"))
  second <- make_second_level_appusage(list(day = day))

  expect_equal(nrow(second$event), 0)
  expect_equal(nrow(second$episode), 0)
  expect_equal(nrow(second$daily), nrow(day))
  expect_equal(second$daily$duration_ms, day$duration_ms)
  expect_equal(second$daily$open_count, day$open_count)
  expect_equal(second$daily$notification_count, day$notification_count)
})

test_that("second-level anomaly flags mark implausible durations", {
  line <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-02")),
    app_name = c("A", "B"),
    package_name = c("a.pkg", "b.pkg"),
    start_ts_ms = c(1704067200000, 1704153600000),
    end_ts_ms = c(1704067199000, 1704243600001),
    start_datetime = as.POSIXct(c("2024-01-01 00:00:00", "2024-01-02 00:00:00"), tz = "Asia/Shanghai"),
    end_datetime = as.POSIXct(c("2023-12-31 23:59:59", "2024-01-03 01:00:00"), tz = "Asia/Shanghai"),
    duration_text = c("display", "display"),
    duration_ms = c(-1000, 90000001),
    parse_warning = NA_character_
  )

  second <- make_second_level_appusage(list(line = line))

  expect_true(second$episode$anomaly_negative_duration[[1]])
  expect_true(second$episode$anomaly_extreme_duration[[2]])
  expect_true(second$episode$anomaly_cross_date[[2]])
  expect_true(any(second$daily$anomaly_any))
})

test_that("write_second_level_appusage writes paired proc-2 RDA and JSON", {
  first <- list(line = parse_line(testthat::test_path("fixtures", "line_sample.txt")))
  output_dir <- file.path(tempdir(), paste0("appusage_second_", as.integer(runif(1, 1, 1e8))))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  first_file <- file.path(output_dir, "sub-1001_type-line_proc-1.rda")
  data <- first
  save(data, file = first_file)

  second_file <- write_second_level_appusage(first_file, overwrite = TRUE)
  metadata_file <- sub("[.]rda$", ".json", second_file)
  expect_match(basename(second_file), "^sub-1001_type-line_proc-2[.]rda$")
  expect_true(file.exists(metadata_file))
  loaded <- load(second_file)
  expect_equal(loaded, "data")
  expect_named(data, c("event", "episode", "daily"))
  expect_equal(data$daily$duration_ms[[1]], 12556)

  metadata <- jsonlite::read_json(metadata_file, simplifyVector = TRUE)
  expect_equal(metadata$processing$second_level_status, "success")
  expect_equal(metadata$processing$qc_status, "success")
  expect_equal(metadata$processing$app_category_status, "not_run")
  expect_equal(metadata$second_level$function_name, "make_second_level_appusage")
  expect_equal(metadata$qc$qc_function, "qc_appusage_day_inline")
  expect_true(!is.null(metadata$second_level$profiling$total_elapsed_sec))
  expect_true(!is.null(metadata$second_level$profiling$worker_pid))
  expect_true(file.exists(metadata$outputs$second_level_rda))
  expect_equal(metadata$counts$n_episode_rows, 1)
  expect_equal(metadata$counts$n_daily_rows, 1)
})

test_that("second-level pair publication commits success JSON last", {
  first <- list(line = parse_line(testthat::test_path("fixtures", "line_sample.txt")))
  output_dir <- file.path(
    tempdir(),
    paste0("appusage_second_atomic_success_", sample.int(1e8, 1))
  )
  dir.create(output_dir, recursive = TRUE)
  first_file <- file.path(output_dir, "sub-atomic_type-line_proc-1.rda")
  data <- first
  save(data, file = first_file)
  output_file <- file.path(output_dir, "sub-atomic_type-line_proc-2.rda")
  metadata_file <- file.path(output_dir, "sub-atomic_type-line_proc-2.json")
  promotions <- character()
  promote <- appusage_promote_file
  testthat::local_mocked_bindings(
    appusage_promote_file = function(from, to) {
      promotions <<- c(promotions, normalizePath(to, winslash = "/", mustWork = FALSE))
      promote(from, to)
    },
    .package = "appusageR"
  )

  result <- write_second_level_appusage(first_file, overwrite = TRUE)

  expect_equal(normalizePath(result, winslash = "/"), normalizePath(output_file, winslash = "/"))
  expect_equal(
    tail(promotions, 2L),
    normalizePath(c(output_file, metadata_file), winslash = "/", mustWork = FALSE)
  )
  env <- new.env(parent = emptyenv())
  expect_identical(load(output_file, envir = env), "data")
  expect_named(env$data, c("event", "episode", "daily"))
  metadata <- jsonlite::read_json(metadata_file, simplifyVector = TRUE)
  expect_equal(metadata$processing$second_level_status, "success")
  expect_equal(
    normalizePath(metadata$outputs$second_level_rda, winslash = "/"),
    normalizePath(output_file, winslash = "/")
  )
  expect_length(
    appusage_second_level_owned_artifacts(output_file, metadata_file),
    0L
  )
})

test_that("success metadata requires matching canonical JSON path", {
  first <- list(line = parse_line(testthat::test_path("fixtures", "line_sample.txt")))
  output_dir <- file.path(
    tempdir(),
    paste0("appusage_second_json_path_", sample.int(1e8, 1))
  )
  dir.create(output_dir, recursive = TRUE)
  first_file <- file.path(output_dir, "sub-json-path_type-line_proc-1.rda")
  data <- first
  save(data, file = first_file)
  output_file <- write_second_level_appusage(first_file, overwrite = TRUE)
  metadata_file <- second_level_metadata_path(output_file)
  valid_metadata <- jsonlite::read_json(metadata_file, simplifyVector = FALSE)

  missing_metadata <- valid_metadata
  missing_metadata$outputs$metadata_json <- NULL
  write_metadata_json(missing_metadata, metadata_file)
  expect_error(
    appusage_validate_second_level_success_metadata(
      metadata_file,
      output_file,
      metadata_file
    ),
    "JSON path is missing"
  )
  missing_status <- second_level_existing_cache_status(first_file, output_dir)
  expect_equal(missing_status$status, "incomplete")
  expect_equal(missing_status$reason, "missing_metadata_json_path")

  mismatched_metadata <- valid_metadata
  mismatched_metadata$outputs$metadata_json <- file.path(output_dir, "other.json")
  write_metadata_json(mismatched_metadata, metadata_file)
  expect_error(
    appusage_validate_second_level_success_metadata(
      metadata_file,
      output_file,
      metadata_file
    ),
    "JSON path failed validation"
  )
  mismatch_status <- second_level_existing_cache_status(first_file, output_dir)
  expect_equal(mismatch_status$status, "incomplete")
  expect_equal(mismatch_status$reason, "metadata_json_mismatch")

  write_metadata_json(valid_metadata, metadata_file)
  expect_equal(
    second_level_existing_cache_status(first_file, output_dir)$status,
    "complete"
  )
  testthat::local_mocked_bindings(
    appusage_file_size_bytes = function(path) NA_real_,
    .package = "appusageR"
  )
  expect_false(appusage_valid_second_level_success_pair(metadata_file))
})

test_that("failure before success JSON promotion cannot create a complete pair", {
  first <- list(line = parse_line(testthat::test_path("fixtures", "line_sample.txt")))
  output_dir <- file.path(
    tempdir(),
    paste0("appusage_second_atomic_fail_", sample.int(1e8, 1))
  )
  dir.create(output_dir, recursive = TRUE)
  first_file <- file.path(output_dir, "sub-fail_type-line_proc-1.rda")
  data <- first
  save(data, file = first_file)
  output_file <- file.path(output_dir, "sub-fail_type-line_proc-2.rda")
  metadata_file <- file.path(output_dir, "sub-fail_type-line_proc-2.json")
  promote <- appusage_promote_file
  testthat::local_mocked_bindings(
    appusage_promote_file = function(from, to) {
      if (appusage_normalized_paths_equal(to, metadata_file)) {
        return(FALSE)
      }
      promote(from, to)
    },
    .package = "appusageR"
  )

  expect_error(
    write_second_level_appusage(first_file, overwrite = TRUE),
    "success JSON marker"
  )

  expect_false(file.exists(metadata_file))
  expect_false(identical(
    second_level_existing_cache_status(first_file, output_dir)$status,
    "complete"
  ))
  expect_length(
    appusage_second_level_owned_artifacts(output_file, metadata_file),
    0L
  )
})

test_that("overwrite publication failure restores the prior valid pair", {
  first <- list(line = parse_line(testthat::test_path("fixtures", "line_sample.txt")))
  output_dir <- file.path(
    tempdir(),
    paste0("appusage_second_atomic_rollback_", sample.int(1e8, 1))
  )
  dir.create(output_dir, recursive = TRUE)
  first_file <- file.path(output_dir, "sub-rollback_type-line_proc-1.rda")
  data <- first
  save(data, file = first_file)
  output_file <- write_second_level_appusage(first_file, overwrite = TRUE)
  metadata_file <- second_level_metadata_path(output_file)
  old_hashes <- unname(tools::md5sum(c(output_file, metadata_file)))
  promote <- appusage_promote_file
  failed_once <- FALSE
  testthat::local_mocked_bindings(
    appusage_promote_file = function(from, to) {
      if (!failed_once && appusage_normalized_paths_equal(to, metadata_file)) {
        failed_once <<- TRUE
        return(FALSE)
      }
      promote(from, to)
    },
    .package = "appusageR"
  )

  expect_error(
    write_second_level_appusage(first_file, overwrite = TRUE),
    "success JSON marker"
  )

  expect_equal(unname(tools::md5sum(c(output_file, metadata_file))), old_hashes)
  expect_equal(
    second_level_existing_cache_status(first_file, output_dir)$status,
    "complete"
  )
  expect_length(
    appusage_second_level_owned_artifacts(output_file, metadata_file),
    0L
  )
})

test_that("second-level writes clean only target-owned stale artifacts", {
  first <- list(line = parse_line(testthat::test_path("fixtures", "line_sample.txt")))
  output_dir <- file.path(
    tempdir(),
    paste0("appusage_second_stale_", sample.int(1e8, 1))
  )
  dir.create(output_dir, recursive = TRUE)
  first_file <- file.path(output_dir, "sub-stale_type-line_proc-1.rda")
  data <- first
  save(data, file = first_file)
  output_file <- file.path(output_dir, "sub-stale_type-line_proc-2.rda")
  metadata_file <- file.path(output_dir, "sub-stale_type-line_proc-2.json")
  stale <- c(
    file.path(output_dir, paste0(".", basename(output_file), ".appusage-tmp-old")),
    file.path(output_dir, paste0(".", basename(metadata_file), ".appusage-backup-old"))
  )
  unrelated <- file.path(output_dir, ".unrelated.tmp")
  invisible(lapply(stale, function(path) writeLines("stale", path)))
  writeLines("keep", unrelated)

  write_second_level_appusage(first_file, overwrite = TRUE)

  expect_false(any(file.exists(stale)))
  expect_true(file.exists(unrelated))
  expect_length(
    appusage_second_level_owned_artifacts(output_file, metadata_file),
    0L
  )
})

test_that("second-level error status JSON is atomically published", {
  output_dir <- file.path(
    tempdir(),
    paste0("appusage_second_status_atomic_", sample.int(1e8, 1))
  )
  dir.create(output_dir, recursive = TRUE)
  first_file <- file.path(output_dir, "sub-error_type-line_proc-1.rda")
  data <- list(line = data.frame())
  save(data, file = first_file)
  batch <- data.frame(
    participant_id = "error",
    detected_type = "line",
    data_file = first_file,
    metadata_file = NA_character_,
    stringsAsFactors = FALSE
  )

  metadata_file <- write_second_level_status_metadata(
    batch,
    index = 1L,
    output_dir = output_dir,
    status = "error",
    error = simpleError("synthetic conversion error")
  )

  metadata <- jsonlite::read_json(metadata_file, simplifyVector = TRUE)
  expect_equal(metadata$processing$second_level_status, "error")
  output_file <- sub("[.]json$", ".rda", metadata_file)
  expect_length(
    appusage_second_level_owned_artifacts(output_file, metadata_file),
    0L
  )
})

test_that("line episode-to-daily aggregation preserves grouped totals and diagnostics", {
  line <- tibble::tibble(
    date = as.Date(c("2024-01-01", "2024-01-01", "2024-01-01", "2024-01-02")),
    app_name = c("A", "A", paste0("A", "(\u6D41\u5A92\u4F53)"), "B"),
    package_name = c("a.pkg", "a.pkg", "a.pkg", "b.pkg"),
    start_ts_ms = c(1704067200000, 1704067210000, 1704067220000, 1704153600000),
    end_ts_ms = c(1704067201000, 1704067212000, 1704067223000, 1704153599000),
    start_datetime = as.POSIXct(
      c("2024-01-01 00:00:00", "2024-01-01 00:00:10", "2024-01-01 00:00:20", "2024-01-02 00:00:00"),
      tz = "Asia/Shanghai"
    ),
    end_datetime = as.POSIXct(
      c("2024-01-01 00:00:01", "2024-01-01 00:00:12", "2024-01-01 00:00:23", "2024-01-01 23:59:59"),
      tz = "Asia/Shanghai"
    ),
    duration_text = "display",
    duration_ms = c(1000, 2000, 3000, -1000),
    parse_warning = c(NA_character_, "warn-a", NA_character_, "warn-b")
  )

  second <- make_second_level_appusage(list(line = line))
  daily <- second$daily

  expect_equal(nrow(daily), 3)
  foreground_a <- daily[daily$package_name == "a.pkg" & daily$activity_type == "foreground", ]
  background_a <- daily[daily$package_name == "a.pkg" & daily$activity_type == "background", ]
  bad_b <- daily[daily$package_name == "b.pkg", ]
  expect_equal(foreground_a$duration_ms[[1]], 3000)
  expect_equal(foreground_a$episode_count[[1]], 2L)
  expect_equal(foreground_a$parse_warning[[1]], "warn-a")
  expect_equal(background_a$duration_ms[[1]], 3000)
  expect_equal(background_a$episode_count[[1]], 1L)
  expect_true(bad_b$anomaly_any[[1]])
  expect_equal(bad_b$n_anomalies[[1]], 1L)
  expect_equal(sum(daily$duration_ms, na.rm = TRUE), sum(line$duration_ms[line$duration_ms >= 0], na.rm = TRUE))
})

test_that("line episode-to-daily aggregation handles duplicate warnings and invalid durations", {
  base_ms <- 1704067200000
  duration_ms <- c(1000, 2000, 3000, 4000, NA_real_, -500, 0)
  start_ts_ms <- base_ms + seq_along(duration_ms) * 10000
  end_ts_ms <- start_ts_ms + ifelse(is.na(duration_ms), 0, duration_ms)
  line <- tibble::tibble(
    date = as.Date(c(
      "2024-01-01", "2024-01-01", "2024-01-01",
      "2024-01-01", "2024-01-02", "2024-01-02",
      "2024-01-03"
    )),
    app_name = c("A", "A", "A", "A", "B", "B", "C"),
    package_name = c("a.pkg", "a.pkg", "a.alt", "a.alt", "b.pkg", "b.pkg", "c.pkg"),
    start_ts_ms = start_ts_ms,
    end_ts_ms = end_ts_ms,
    start_datetime = as.POSIXct(start_ts_ms / 1000, origin = "1970-01-01", tz = "Asia/Shanghai"),
    end_datetime = as.POSIXct(end_ts_ms / 1000, origin = "1970-01-01", tz = "Asia/Shanghai"),
    duration_text = "display",
    duration_ms = duration_ms,
    parse_warning = c("dup", "dup", "alpha", "beta", "missing-duration", "negative-duration", NA_character_)
  )

  second <- make_second_level_appusage(list(line = line))
  daily <- second$daily

  expect_equal(nrow(daily), 4)
  a_pkg <- daily[daily$date == as.Date("2024-01-01") & daily$package_name == "a.pkg", ]
  a_alt <- daily[daily$date == as.Date("2024-01-01") & daily$package_name == "a.alt", ]
  b_pkg <- daily[daily$date == as.Date("2024-01-02") & daily$package_name == "b.pkg", ]
  c_pkg <- daily[daily$date == as.Date("2024-01-03") & daily$package_name == "c.pkg", ]

  expect_equal(a_pkg$duration_ms[[1]], 3000)
  expect_equal(a_pkg$episode_count[[1]], 2L)
  expect_equal(a_pkg$parse_warning[[1]], "dup")
  expect_equal(a_alt$duration_ms[[1]], 7000)
  expect_equal(a_alt$parse_warning[[1]], "alpha; beta")
  expect_true(is.na(b_pkg$duration_ms[[1]]))
  expect_equal(b_pkg$episode_count[[1]], 2L)
  expect_equal(b_pkg$n_anomalies[[1]], 2L)
  expect_true(b_pkg$anomaly_any[[1]])
  expect_equal(c_pkg$duration_ms[[1]], 0)
  expect_false(c_pkg$anomaly_any[[1]])
  expect_equal(c_pkg$n_anomalies[[1]], 0L)
  expect_equal(sum(daily$duration_ms, na.rm = TRUE), 10000)
})

test_that("line episode-to-daily aggregation handles empty and many-group inputs", {
  empty <- make_second_level_appusage(list(line = tibble::tibble(
    date = as.Date(character()),
    app_name = character(),
    package_name = character(),
    start_ts_ms = numeric(),
    end_ts_ms = numeric(),
    start_datetime = as.POSIXct(character(), tz = "Asia/Shanghai"),
    end_datetime = as.POSIXct(character(), tz = "Asia/Shanghai"),
    duration_text = character(),
    duration_ms = numeric(),
    parse_warning = character()
  )))
  expect_equal(nrow(empty$daily), 0L)
  expect_identical(names(empty$daily), names(empty_second_daily_tibble()))

  n <- 120L
  dates <- as.Date("2024-01-01") + rep(0:2, length.out = n)
  package_name <- sprintf("pkg.%03d", seq_len(n))
  start_ts_ms <- 1704067200000 + seq_len(n) * 1000
  duration_ms <- rep(c(1000, 2000, 3000), length.out = n)
  line <- tibble::tibble(
    date = dates,
    app_name = paste0("App ", seq_len(n)),
    package_name = package_name,
    start_ts_ms = start_ts_ms,
    end_ts_ms = start_ts_ms + duration_ms,
    start_datetime = as.POSIXct(start_ts_ms / 1000, origin = "1970-01-01", tz = "Asia/Shanghai"),
    end_datetime = as.POSIXct((start_ts_ms + duration_ms) / 1000, origin = "1970-01-01", tz = "Asia/Shanghai"),
    duration_text = "display",
    duration_ms = duration_ms,
    parse_warning = ifelse(seq_len(n) %% 10L == 0L, "periodic-warning", NA_character_)
  )

  second <- make_second_level_appusage(list(line = line))

  expect_equal(nrow(second$daily), n)
  expect_equal(sum(second$daily$duration_ms, na.rm = TRUE), sum(duration_ms))
  expect_equal(sum(!is.na(second$daily$parse_warning)), n %/% 10L)
  expect_true(all(second$daily$daily_source == "line_episodes"))
})

synthetic_meta_events <- function(event_type, offset_ms,
                                  package_name = "com.example.app",
                                  class_name = "MainActivity",
                                  app_name = "Example App") {
  n <- length(event_type)
  base_ms <- 1704067200000
  event_ts_ms <- base_ms + offset_ms
  event_datetime <- as.POSIXct(event_ts_ms / 1000,
    origin = "1970-01-01",
    tz = "Asia/Shanghai"
  )
  tibble::tibble(
    table_date = as.Date(event_datetime),
    app_name = rep(app_name, length.out = n),
    package_name = rep(package_name, length.out = n),
    class_name = rep(class_name, length.out = n),
    event_datetime = event_datetime,
    event_ts_ms = event_ts_ms,
    event_type = event_type,
    event_type_label = label_event_type(event_type),
    configuration = NA_character_,
    parse_warning = NA_character_
  )
}

synthetic_meta_summary <- function(duration_ms = 60000) {
  tibble::tibble(
    table_date = as.Date("2024-01-01"),
    app_name = "Example App",
    package_name = "com.example.app",
    total_duration_ms = duration_ms,
    parse_warning = NA_character_
  )
}

test_that("reconstruct_meta_episodes pairs type 1 to type 2", {
  events <- synthetic_meta_events(c(1, 2), c(0, 5000))

  episodes <- reconstruct_meta_episodes(events)

  expect_equal(nrow(episodes), 1)
  expect_equal(episodes$duration_ms[[1]], 5000)
  expect_equal(episodes$episode_source[[1]], "meta_events")
  expect_equal(episodes$reconstruction_status[[1]], "complete")
  expect_false(episodes$unmatched_start[[1]])
  expect_false(episodes$unmatched_end[[1]])
})

test_that("reconstruct_meta_episodes allows type 23 to close type 1", {
  events <- synthetic_meta_events(c(1, 23), c(0, 7000))

  episodes <- reconstruct_meta_episodes(events)

  expect_equal(nrow(episodes), 1)
  expect_equal(episodes$duration_ms[[1]], 7000)
  expect_equal(episodes$end_event_type[[1]], 23)
  expect_equal(episodes$reconstruction_status[[1]], "complete")
})

test_that("reconstruct_meta_episodes drops unmatched rows into diagnostics", {
  events <- synthetic_meta_events(
    c(1, 2),
    c(0, 5000),
    package_name = c("com.start", "com.end")
  )

  episodes <- reconstruct_meta_episodes(events)
  diagnostics <- attr(episodes, "meta_reconstruction_diagnostics")

  expect_equal(nrow(episodes), 0)
  expect_equal(diagnostics$n_dropped_unmatched_events, 2)
  expect_equal(diagnostics$n_dropped_unmatched_starts, 1)
  expect_equal(diagnostics$n_dropped_unmatched_ends, 1)
  expect_equal(diagnostics$dropped_unmatched_event_proportion, 1)
})

test_that("meta package and package_class pairing behave differently", {
  events <- synthetic_meta_events(
    c(1, 2),
    c(0, 5000),
    class_name = c("ActivityA", "ActivityB")
  )

  by_package <- reconstruct_meta_episodes(events, pairing = "package")
  by_class <- reconstruct_meta_episodes(events, pairing = "package_class")

  expect_equal(nrow(by_package), 1)
  expect_equal(by_package$reconstruction_status[[1]], "complete")
  expect_equal(nrow(by_class), 0)
  expect_equal(
    attr(by_class, "meta_reconstruction_diagnostics")$n_dropped_unmatched_events,
    2
  )
})

test_that("type 23 extends a type 2 close in one continuous package segment", {
  events <- synthetic_meta_events(c(1, 2, 23), c(0, 5000, 7000))

  episodes <- reconstruct_meta_episodes(events)

  expect_equal(nrow(episodes), 1)
  expect_equal(episodes$duration_ms[[1]], 7000)
  expect_equal(episodes$end_event_type[[1]], 23)
})

test_that("adjacent complete meta episodes merge by default using source duration sums", {
  events <- synthetic_meta_events(c(1, 2, 1, 2), c(0, 5000, 6000, 10000))

  episodes <- reconstruct_meta_episodes(events)
  diagnostics <- attr(episodes, "meta_reconstruction_diagnostics")

  expect_equal(nrow(episodes), 1)
  expect_equal(episodes$start_ts_ms[[1]], 1704067200000)
  expect_equal(episodes$end_ts_ms[[1]], 1704067210000)
  expect_equal(episodes$duration_ms[[1]], 9000)
  expect_equal(episodes$source_duration_ms[[1]], 9000)
  expect_equal(episodes$merged_gap_ms[[1]], 1000)
  expect_equal(episodes$source_episode_count[[1]], 2)
  expect_equal(diagnostics$n_premerge_episode_rows, 2)
  expect_equal(diagnostics$n_episode_merge_groups, 1)
  expect_equal(diagnostics$n_episode_merge_edges, 1)
  expect_equal(diagnostics$n_episode_rows_reduced_by_merge, 1)
})

test_that("meta episode merging can be disabled or limited by gap threshold", {
  events <- synthetic_meta_events(c(1, 2, 1, 2), c(0, 5000, 6000, 10000))

  disabled <- reconstruct_meta_episodes(events, merge_contiguous = FALSE)
  tight <- reconstruct_meta_episodes(events, merge_gap_ms = 500)

  expect_equal(nrow(disabled), 2)
  expect_equal(disabled$duration_ms, c(5000, 4000))
  expect_equal(nrow(tight), 2)
  expect_equal(tight$duration_ms, c(5000, 4000))
})

test_that("meta episode merging respects app adjacency and identity", {
  events <- dplyr::bind_rows(
    synthetic_meta_events(c(1, 2), c(0, 5000),
      package_name = "com.example.a",
      app_name = "App A"
    ),
    synthetic_meta_events(c(1, 2), c(5500, 7000),
      package_name = "com.example.b",
      app_name = "App B"
    ),
    synthetic_meta_events(c(1, 2), c(8000, 10000),
      package_name = "com.example.a",
      app_name = "App A"
    )
  )

  episodes <- reconstruct_meta_episodes(events)

  expect_equal(nrow(episodes), 3)
  expect_equal(episodes$app_name, c("App A", "App B", "App A"))
  expect_equal(episodes$duration_ms, c(5000, 1500, 2000))
})

test_that("cross-app meta timeline overlap clips the previous complete episode", {
  events <- synthetic_meta_events(
    c(1, 1, 2, 2),
    c(0, 10000, 20000, 30000),
    package_name = c("com.example.a", "com.example.b", "com.example.b", "com.example.a"),
    app_name = c("App A", "App B", "App B", "App A")
  )

  episodes <- reconstruct_meta_episodes(events)
  diagnostics <- attr(episodes, "meta_reconstruction_diagnostics")
  complete <- episodes[episodes$reconstruction_status == "complete", , drop = FALSE]
  complete <- complete[order(complete$start_ts_ms, complete$end_ts_ms), , drop = FALSE]

  expect_equal(nrow(complete), 2)
  expect_equal(complete$app_name, c("App A", "App B"))
  expect_equal(complete$start_ts_ms[[1]], 1704067200000)
  expect_equal(complete$end_ts_ms[[1]], complete$start_ts_ms[[2]])
  expect_equal(complete$duration_ms[[1]], 10000)
  expect_equal(complete$source_duration_ms[[1]], 30000)
  expect_match(complete$reconstruction_warning[[1]], "timeline_clipped")
  expect_match(complete$anomaly_reason[[1]], "timeline_clipped")
  expect_false(any(complete$start_ts_ms[-1] < complete$end_ts_ms[-nrow(complete)]))
  expect_equal(diagnostics$n_timeline_clipped_episodes, 1L)
  expect_equal(diagnostics$total_timeline_clipped_ms, 20000)
  expect_equal(diagnostics$n_timeline_clipped_to_nonpositive, 0L)
})

test_that("non-overlapping cross-app meta timeline remains unchanged", {
  events <- synthetic_meta_events(
    c(1, 2, 1, 2),
    c(0, 10000, 20000, 30000),
    package_name = c("com.example.a", "com.example.a", "com.example.b", "com.example.b"),
    app_name = c("App A", "App A", "App B", "App B")
  )

  episodes <- reconstruct_meta_episodes(events)
  diagnostics <- attr(episodes, "meta_reconstruction_diagnostics")
  complete <- episodes[episodes$reconstruction_status == "complete", , drop = FALSE]
  complete <- complete[order(complete$start_ts_ms, complete$end_ts_ms), , drop = FALSE]

  expect_equal(nrow(complete), 2)
  expect_equal(complete$duration_ms, c(10000, 10000))
  expect_equal(complete$source_duration_ms, c(10000, 10000))
  warnings <- complete$reconstruction_warning
  warnings[is.na(warnings)] <- ""
  expect_false(any(grepl("timeline_clipped", warnings)))
  expect_false(any(complete$start_ts_ms[-1] < complete$end_ts_ms[-nrow(complete)]))
  expect_equal(diagnostics$n_timeline_clipped_episodes, 0L)
  expect_equal(diagnostics$total_timeline_clipped_ms, 0)
})

test_that("unknown and non-episode meta events are ignored for episode construction", {
  events <- synthetic_meta_events(c(999, 7, 1, 2), c(0, 1000, 2000, 6000))

  episodes <- reconstruct_meta_episodes(events)

  expect_equal(nrow(episodes), 1)
  expect_equal(episodes$start_event_type[[1]], 1)
  expect_equal(episodes$end_event_type[[1]], 2)
  expect_equal(episodes$duration_ms[[1]], 4000)
})

test_that("reconstruct_meta_episodes flags boundary cross-date and overlong diagnostics", {
  events <- synthetic_meta_events(c(1, 26, 2), c(86390000, 86400000, 86500000))

  episodes <- reconstruct_meta_episodes(events, max_episode_ms = 1000)

  expect_equal(nrow(episodes), 1)
  expect_true(episodes$device_boundary_involved[[1]])
  expect_true(episodes$anomaly_cross_date[[1]])
  expect_true(episodes$anomaly_extreme_duration[[1]])
  expect_match(episodes$reconstruction_warning[[1]], "device_boundary_involved")
  expect_match(episodes$reconstruction_warning[[1]], "cross_date")
  expect_match(episodes$reconstruction_warning[[1]], "overlong_episode")
})

test_that("reconstructed meta episodes bind with line-derived episodes", {
  line <- parse_line(testthat::test_path("fixtures", "line_sample.txt"))
  line_second <- make_second_level_appusage(list(line = line))
  meta_episodes <- reconstruct_meta_episodes(synthetic_meta_events(c(1, 2), c(0, 5000)))

  expect_identical(names(meta_episodes), names(line_second$episode))
  combined <- dplyr::bind_rows(line_second$episode, meta_episodes)
  expect_equal(nrow(combined), nrow(line_second$episode) + nrow(meta_episodes))
  expect_equal(sort(unique(combined$episode_source)), c("line", "meta_events"))
})

test_that("make_second_level_appusage reconstructs meta episodes by default and can opt out", {
  first <- list(
    meta_events = synthetic_meta_events(c(1, 2), c(0, 5000)),
    meta_summary = synthetic_meta_summary(duration_ms = 123456)
  )

  default_second <- make_second_level_appusage(first)
  opt_out_second <- make_second_level_appusage(first, reconstruct_meta = FALSE)

  expect_equal(nrow(default_second$episode), 1)
  expect_equal(default_second$episode$duration_ms[[1]], 5000)
  expect_equal(nrow(opt_out_second$episode), 0)
  expect_equal(default_second$daily$duration_ms[[1]], 123456)
  expect_equal(opt_out_second$daily$duration_ms[[1]], 123456)
})

test_that("write_second_level_appusage records meta reconstruction metadata", {
  first <- list(
    meta_events = synthetic_meta_events(c(1, 2), c(0, 5000)),
    meta_summary = synthetic_meta_summary()
  )
  output_dir <- file.path(tempdir(), paste0("appusage_meta_reconstruct_", as.integer(runif(1, 1, 1e8))))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  first_file <- file.path(output_dir, "sub-2002_type-meta_proc-1.rda")
  data <- first
  save(data, file = first_file)

  second_file <- write_second_level_appusage(
    first_file,
    overwrite = TRUE,
    reconstruct_meta = TRUE,
    meta_pairing = "package"
  )
  metadata_file <- sub("[.]rda$", ".json", second_file)
  loaded <- load(second_file)

  expect_equal(loaded, "data")
  expect_equal(nrow(data$episode), 1)
  metadata <- jsonlite::read_json(metadata_file, simplifyVector = TRUE)
  expect_true(metadata$meta_reconstruction$requested)
  expect_equal(metadata$meta_reconstruction$pairing_strategy, "package")
  expect_equal(metadata$meta_reconstruction$n_complete_episodes, 1)
  expect_equal(metadata$meta_reconstruction$n_unmatched_starts, 0)
  expect_equal(metadata$meta_reconstruction$n_unmatched_ends, 0)
})

test_that("meta daily defaults to Table 1 summary even when episodes are reconstructed", {
  first <- list(
    meta_events = synthetic_meta_events(c(1, 2), c(0, 5000)),
    meta_summary = synthetic_meta_summary(duration_ms = 123456)
  )

  second <- make_second_level_appusage(first)

  expect_equal(nrow(second$episode), 1)
  expect_equal(nrow(second$daily), 1)
  expect_equal(second$daily$daily_source[[1]], "meta_summary")
  expect_equal(second$daily$duration_ms[[1]], 123456)
  expect_equal(second$daily$summary_duration_ms[[1]], 123456)
  expect_equal(second$daily$episode_duration_ms[[1]], 5000)
  expect_equal(second$daily$duration_diff_ms[[1]], -118456)
  expect_equal(second$daily$duration_agreement_status[[1]], "matched_with_difference")
})

test_that("episode-derived meta daily requires explicit reconstruction", {
  first <- list(
    meta_events = synthetic_meta_events(c(1, 2), c(0, 5000)),
    meta_summary = synthetic_meta_summary()
  )

  expect_error(
    make_second_level_appusage(first, reconstruct_meta = FALSE, meta_daily_source = "episodes"),
    "requires `reconstruct_meta = TRUE`"
  )
  expect_error(
    make_second_level_appusage(first, reconstruct_meta = FALSE, meta_daily_source = "both"),
    "requires `reconstruct_meta = TRUE`"
  )
})

test_that("complete meta event sequence aggregates to episode-derived daily duration", {
  first <- list(
    meta_events = synthetic_meta_events(c(1, 2), c(0, 10000)),
    meta_summary = synthetic_meta_summary(duration_ms = 10000)
  )

  second <- make_second_level_appusage(
    first,
    reconstruct_meta = TRUE,
    meta_daily_source = "episodes"
  )

  expect_equal(nrow(second$episode), 1)
  expect_equal(nrow(second$daily), 1)
  expect_equal(second$daily$daily_source[[1]], "meta_episodes")
  expect_equal(second$daily$duration_ms[[1]], 10000)
  expect_equal(second$daily$episode_duration_ms[[1]], 10000)
  expect_equal(second$daily$summary_duration_ms[[1]], 10000)
  expect_equal(second$daily$duration_diff_ms[[1]], 0)
  expect_equal(second$daily$duration_agreement_status[[1]], "matched_exact")
  expect_equal(second$daily$complete_episode_count[[1]], 1)
})

test_that("incomplete meta event sequence is flagged in episode-derived daily diagnostics", {
  first <- list(
    meta_events = synthetic_meta_events(1, 0),
    meta_summary = synthetic_meta_summary(duration_ms = 10000)
  )

  second <- make_second_level_appusage(
    first,
    reconstruct_meta = TRUE,
    meta_daily_source = "episodes"
  )
  diagnostics <- attr(second, "meta_reconstruction_diagnostics")

  expect_equal(nrow(second$episode), 0)
  expect_equal(nrow(second$daily), 0)
  expect_equal(diagnostics$n_dropped_unmatched_events, 1)
  expect_equal(diagnostics$n_dropped_unmatched_starts, 1)
  expect_equal(diagnostics$dropped_unmatched_event_proportion, 1)
})

test_that("meta summary and episode daily disagreement is quantified", {
  first <- list(
    meta_events = synthetic_meta_events(c(1, 2), c(0, 10000)),
    meta_summary = synthetic_meta_summary(duration_ms = 15000)
  )

  second <- make_second_level_appusage(
    first,
    reconstruct_meta = TRUE,
    meta_daily_source = "both"
  )

  expect_equal(nrow(second$daily), 2)
  expect_setequal(second$daily$daily_source, c("meta_summary", "meta_episodes"))
  expect_true(all(second$daily$duration_agreement_status == "matched_with_difference"))
  expect_true(all(second$daily$summary_duration_ms == 15000))
  expect_true(all(second$daily$episode_duration_ms == 10000))
  expect_true(all(second$daily$duration_diff_ms == -5000))
  expect_true(all(second$daily$duration_diff_pct == -5000 / 15000))
})

test_that("line day app and meta daily outputs remain bindable with provenance columns", {
  line <- parse_line(testthat::test_path("fixtures", "line_sample.txt"))
  day <- parse_day(testthat::test_path("fixtures", "day_sample.txt"))
  app <- parse_app(testthat::test_path("fixtures", "app_sample.txt"))
  meta <- list(
    meta_events = synthetic_meta_events(c(1, 2), c(0, 10000)),
    meta_summary = synthetic_meta_summary(duration_ms = 10000)
  )

  line_second <- make_second_level_appusage(list(line = line))
  day_second <- make_second_level_appusage(list(day = day))
  app_second <- make_second_level_appusage(list(app = app))
  meta_second <- make_second_level_appusage(meta, reconstruct_meta = TRUE, meta_daily_source = "both")

  expect_identical(names(day_second$daily), names(line_second$daily))
  expect_identical(names(app_second$daily), names(line_second$daily))
  expect_identical(names(meta_second$daily), names(line_second$daily))
  combined <- dplyr::bind_rows(
    line_second$daily,
    day_second$daily,
    app_second$daily,
    meta_second$daily
  )
  expect_equal(
    sort(unique(stats::na.omit(combined$daily_source))),
    sort(c("line_episodes", "day_export", "app_export", "meta_summary", "meta_episodes"))
  )
  expect_true(all(c(
    "summary_duration_ms",
    "episode_duration_ms",
    "duration_diff_ms",
    "complete_episode_count",
    "unmatched_start_count",
    "unmatched_end_count",
    "invalid_pair_count",
    "reconstruction_warning_count"
  ) %in% names(combined)))
})

test_that("proc-2 metadata records meta daily comparison summaries", {
  first <- list(
    meta_events = synthetic_meta_events(c(1, 2), c(0, 10000)),
    meta_summary = synthetic_meta_summary(duration_ms = 15000)
  )
  output_dir <- file.path(tempdir(), paste0("appusage_meta_daily_", as.integer(runif(1, 1, 1e8))))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  first_file <- file.path(output_dir, "sub-3003_type-meta_proc-1.rda")
  data <- first
  save(data, file = first_file)

  second_file <- write_second_level_appusage(
    first_file,
    overwrite = TRUE,
    reconstruct_meta = TRUE,
    meta_daily_source = "both"
  )
  metadata_file <- sub("[.]rda$", ".json", second_file)
  loaded <- load(second_file)
  metadata <- jsonlite::read_json(metadata_file, simplifyVector = TRUE)

  expect_equal(loaded, "data")
  expect_equal(nrow(data$daily), 2)
  expect_equal(metadata$second_level$parameters$meta_daily_source, "both")
  expect_equal(metadata$meta_daily_comparison$selected_source, "both")
  expect_true(metadata$meta_daily_comparison$reconstruction_used_for_daily)
  expect_equal(metadata$meta_daily_comparison$n_matched_summary_episode_keys, 1)
  expect_equal(metadata$meta_daily_comparison$total_abs_duration_diff_ms, 5000)
  expect_equal(metadata$meta_daily_comparison$max_abs_duration_diff_ms, 5000)
})
