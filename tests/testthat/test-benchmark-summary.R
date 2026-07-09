test_that("preprocessing benchmark summary aggregates first and second level profiling", {
  source_a <- tempfile(fileext = ".txt")
  source_b <- tempfile(fileext = ".txt")
  writeLines("a", source_a)
  writeLines("bb", source_b)
  first <- tibble::tibble(
    source_file = c(source_a, source_b),
    status = c("success", "error"),
    detected_type = c("line", "meta"),
    elapsed_sec = c(2, 125),
    n_rows = c(10L, NA_integer_),
    first_level_requested_workers = 8L,
    first_level_selected_workers = 6L,
    first_level_worker_cap_reason = "test_cap",
    first_level_worker_cap_override = FALSE
  )
  manifest <- tibble::tibble(
    source_file = c(source_a, source_b),
    file_size = c(100, 250)
  )
  second <- tibble::tibble(
    status = c("success", "skipped"),
    detected_type = c("line", "meta"),
    second_level_total_elapsed_sec = c(4, NA_real_),
    n_event_rows = c(0L, NA_integer_),
    n_episode_rows = c(3L, NA_integer_),
    n_daily_rows = c(2L, NA_integer_),
    first_level_rda_size_bytes = c(1000, 2000),
    second_level_worker_pid = c(111L, 111L)
  )
  qc <- tibble::tibble(
    qc_status = c("success", "not_run"),
    detected_type = c("line", "meta"),
    second_level_inline_qc_elapsed_sec = c(0.5, NA_real_)
  )

  benchmark <- build_preprocessing_benchmark_summary(
    first = first,
    second = second,
    qc = qc,
    manifest = manifest
  )

  first_line <- benchmark[benchmark$stage == "first_level" &
    benchmark$status == "success" &
    benchmark$detected_type == "line", ]
  expect_equal(first_line$n_records, 1L)
  expect_equal(first_line$elapsed_sec_total, 2)
  expect_equal(first_line$source_size_bytes_total, 100)
  expect_equal(first_line$n_rows_total, 10)
  expect_equal(first_line$first_level_selected_workers, 6L)

  first_meta <- benchmark[benchmark$stage == "first_level" &
    benchmark$status == "error" &
    benchmark$detected_type == "meta", ]
  expect_equal(first_meta$n_slow_gt_60s, 1L)

  second_line <- benchmark[benchmark$stage == "second_level" &
    benchmark$status == "success" &
    benchmark$detected_type == "line", ]
  expect_equal(second_line$n_episode_rows_total, 3)
  expect_equal(second_line$n_daily_rows_total, 2)
  expect_equal(second_line$n_worker_pids, 1L)
})

test_that("preprocessing benchmark writer handles missing summaries", {
  out_dir <- file.path(tempdir(), paste0("appusage_benchmark_", sample.int(1e8, 1)))
  result <- write_preprocessing_benchmark_summary(
    project_dir = out_dir,
    first = NULL,
    second = NULL,
    qc = NULL,
    first_level_worker_decision = list(
      requested_workers = 4L,
      selected_workers = 2L,
      cap_reason = "test",
      worker_cap_override = FALSE
    )
  )

  expect_true(file.exists(result$file))
  read_back <- utils::read.csv(result$file, stringsAsFactors = FALSE, check.names = FALSE)
  expect_equal(sort(unique(read_back$stage)), c("first_level", "qc", "second_level"))
  expect_true(all(read_back$status == "missing"))
  expect_true(all(read_back$n_records == 0L))
  expect_true(all(read_back$first_level_selected_workers == 2L))
})

test_that("project workflow writes preprocessing benchmark artifact", {
  raw_root <- file.path(tempdir(), paste0("appusage_benchmark_raw_", sample.int(1e8, 1)))
  project_dir <- file.path(raw_root, "ProjectName-StudyA_ProjectID-123")
  dir.create(project_dir, recursive = TRUE)
  file.copy(
    testthat::test_path("fixtures", "line_sample.txt"),
    file.path(project_dir, "1001_AppUsage_line_2024_1_2_3_4_5.txt")
  )
  output_root <- file.path(tempdir(), paste0("appusage_benchmark_project_", sample.int(1e8, 1)))

  result <- run_appusage_project_workflow(
    project_dir = project_dir,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    run_second_level = FALSE,
    run_qc = FALSE,
    overwrite = TRUE,
    progress = FALSE
  )

  expect_true(file.exists(result$benchmark_summary_file))
  benchmark <- utils::read.csv(result$benchmark_summary_file, stringsAsFactors = FALSE, check.names = FALSE)
  expect_true("first_level" %in% benchmark$stage)
  expect_true("second_level" %in% benchmark$stage)
  expect_true(any(benchmark$stage == "first_level" & benchmark$status == "success"))
  expect_equal(normalizePath(result$benchmark_summary_file, winslash = "/"),
    normalizePath(file.path(result$project_dir, "preprocessing_benchmark_summary.csv"), winslash = "/")
  )
})
