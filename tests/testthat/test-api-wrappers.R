wrapper_qc_daily <- function(day_offsets, participant_id = "p1") {
  dates <- as.Date("2024-10-07") + day_offsets
  tibble::tibble(
    participant_id = participant_id,
    date = dates,
    weekday = weekdays(dates),
    app_name = "Example App",
    package_name = "com.example.app",
    duration_ms = rep(1000, length(dates)),
    duration_min = rep(1000 / 60000, length(dates)),
    open_count = rep(1L, length(dates)),
    notification_count = rep(0L, length(dates)),
    split_screen_ms = rep(0, length(dates)),
    is_all_apps = FALSE,
    is_collection_app = FALSE,
    parse_warning = NA_character_
  )
}

test_that("read_appusage_text handles file, text, and lines input", {
  path <- testthat::test_path("fixtures", "line_sample.txt")
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  text <- paste(lines, collapse = "\n")

  file_out <- read_appusage_text(path)
  text_out <- read_appusage_text(text, input = "text")
  lines_out <- read_appusage_text(lines, input = "lines")

  expect_s3_class(file_out, "appusage_text")
  expect_equal(file_out$input, "file")
  expect_equal(file_out$source_file, "line_sample.txt")
  expect_equal(file_out$n_lines, length(file_out$lines))
  expect_equal(text_out$lines, lines_out$lines)
  expect_equal(text_out$n_lines, length(lines))
})

test_that("run_first_level_appusage dispatches to current parsers", {
  line_path <- testthat::test_path("fixtures", "line_sample.txt")
  meta_path <- testthat::test_path("fixtures", "meta_sample.txt")
  day_path <- testthat::test_path("fixtures", "day_sample.txt")
  app_path <- testthat::test_path("fixtures", "app_sample.txt")

  line <- run_first_level_appusage(line_path, participant_id = "line_id")
  meta <- run_first_level_appusage(meta_path, participant_id = "meta_id")
  day <- run_first_level_appusage(day_path, participant_id = "day_id")
  app <- run_first_level_appusage(app_path, participant_id = "app_id")

  expect_equal(line$type, "line")
  expect_equal(nrow(line$parsed), nrow(parse_line(line_path, participant_id = "line_id")))
  expect_equal(meta$type, "meta")
  expect_equal(nrow(meta$parsed$summary), nrow(parse_meta(meta_path, participant_id = "meta_id")$summary))
  expect_equal(nrow(meta$parsed$events), nrow(parse_meta(meta_path, participant_id = "meta_id")$events))
  expect_equal(day$type, "day")
  expect_equal(nrow(day$parsed), nrow(parse_day(day_path, participant_id = "day_id")))
  expect_equal(app$type, "app")
  expect_equal(nrow(app$parsed), nrow(parse_app(app_path, participant_id = "app_id")))
})

test_that("run_first_level_appusage writes existing proc-1 cache format", {
  path <- testthat::test_path("fixtures", "line_sample.txt")
  output_dir <- file.path(
    tempdir(),
    paste0("appusage_first_wrapper_", as.integer(runif(1, 1, 1e8)))
  )

  first <- run_first_level_appusage(
    path,
    participant_id = "wrap_line",
    output_dir = output_dir,
    overwrite = TRUE
  )

  expect_equal(first$status, "success")
  expect_match(basename(first$data_file), "^sub-wrap-line_type-line_proc-1[.]rda$")
  expect_true(file.exists(first$data_file))
  expect_true(file.exists(first$metadata_file))
  loaded <- load(first$data_file)
  expect_equal(loaded, "data")
  expect_named(data, "line")
})

test_that("run_second_level_appusage returns standard grains with default meta reconstruction", {
  line <- run_first_level_appusage(
    testthat::test_path("fixtures", "line_sample.txt"),
    participant_id = "line_id"
  )
  meta <- run_first_level_appusage(
    testthat::test_path("fixtures", "meta_sample.txt"),
    participant_id = "meta_id"
  )

  line_second <- run_second_level_appusage(line)
  meta_second <- run_second_level_appusage(meta)

  expect_s3_class(line_second, "appusage_second_level")
  expect_named(line_second$data, c("event", "episode", "daily"))
  expect_equal(nrow(line_second$data$episode), 1)
  expect_equal(nrow(meta_second$data$event), 2)
  expect_equal(nrow(meta_second$data$episode), 1)
  expect_equal(nrow(meta_second$data$daily), 1)
})

test_that("run_second_level_appusage writes proc-2 cache from first-level result", {
  first_dir <- file.path(
    tempdir(),
    paste0("appusage_second_wrapper_first_", as.integer(runif(1, 1, 1e8)))
  )
  second_dir <- file.path(
    tempdir(),
    paste0("appusage_second_wrapper_second_", as.integer(runif(1, 1, 1e8)))
  )
  first <- run_first_level_appusage(
    testthat::test_path("fixtures", "line_sample.txt"),
    participant_id = "wrap_line",
    output_dir = first_dir,
    overwrite = TRUE
  )

  second <- run_second_level_appusage(
    first,
    output_dir = second_dir,
    overwrite = TRUE
  )

  expect_true(file.exists(second$data_file))
  expect_true(file.exists(second$metadata_file))
  expect_match(basename(second$data_file), "^sub-wrap-line_type-line_proc-2[.]rda$")
  expect_named(second$data, c("event", "episode", "daily"))
})

test_that("run_qc_appusage preserves existing in-memory daily QC rules", {
  qc <- run_qc_appusage(wrapper_qc_daily(0:6), progress = FALSE)

  expect_true(qc$pass_qc[[1]])
  expect_equal(qc$n_nonempty_days[[1]], 7)
})

test_that("extract_uncoded_apps reports missing dictionary categories only", {
  data <- list(
    daily = tibble::tibble(
      app_name = c("Coded", "Uncoded", "Uncoded"),
      package_name = c("coded.pkg", "uncoded.pkg", "uncoded.pkg"),
      duration_ms = c(100, 200, 300),
      source_export_type = "day",
      Level_1_Category = c("Tools", NA_character_, NA_character_),
      Level_2_Category = c("Utility", NA_character_, NA_character_)
    )
  )

  uncoded <- extract_uncoded_apps(data)

  expect_equal(nrow(uncoded), 1)
  expect_equal(uncoded$package_name[[1]], "uncoded.pkg")
  expect_equal(uncoded$n_rows[[1]], 2)
  expect_equal(uncoded$total_duration_ms[[1]], 500)
})

test_that("run_appusage_workflow writes proc-1 and proc-2 only", {
  files <- c(
    testthat::test_path("fixtures", "line_sample.txt"),
    testthat::test_path("fixtures", "day_sample.txt")
  )
  output_dir <- file.path(
    tempdir(),
    paste0("appusage_workflow_wrapper_", as.integer(runif(1, 1, 1e8)))
  )

  result <- run_appusage_workflow(
    files,
    output_dir = output_dir,
    ids = c("line_id", "day_id"),
    project_name = "WrapperWF",
    project_id = "a1b2",
    progress = FALSE
  )

  expect_s3_class(result, "appusage_workflow_result")
  expect_true(dir.exists(file.path(result$project_dir, "proclevel-1")))
  expect_true(dir.exists(file.path(result$project_dir, "proclevel-2")))
  expect_false(dir.exists(file.path(result$project_dir, "proclevel-3")))
  expect_false(dir.exists(file.path(result$project_dir, "proclevel-tmp")))
  expect_true(file.exists(file.path(result$project_dir, "analytic_summary_table_proclevel-2.csv")))
})

test_that("old low-level exported functions still work", {
  path <- testthat::test_path("fixtures", "line_sample.txt")

  expect_equal(detect_appusage_type(path), "line")
  expect_equal(nrow(parse_line(path)), 1)
  expect_named(make_second_level_appusage(list(line = parse_line(path))), c("event", "episode", "daily"))
})
