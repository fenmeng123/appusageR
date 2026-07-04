test_that("read_appusage_batch writes caches and invisibly returns summary", {
  paths <- testthat::test_path("fixtures", c("day_sample.txt", "app_sample.txt"))
  output_dir <- file.path(tempdir(), paste0("appusage_batch_", as.integer(runif(1, 1, 1e8))))

  result <- withVisible(read_appusage_batch(
    paths,
    ids = c("day_id", "app_id"),
    output_dir = output_dir,
    progress = FALSE
  ))

  expect_false(result$visible)
  summary <- result$value
  expect_s3_class(summary, "tbl_df")
  expect_equal(summary$status, c("success", "success"))
  expect_true(all(file.exists(summary$data_file)))
  expect_true(all(file.exists(summary$metadata_file)))
  expect_true(all(summary$n_rows > 0))
  expect_true(all(basename(dirname(summary$data_file)) == "proclevel-1"))
  expect_true(file.exists(file.path(unique(summary$project_root), "dataset_descriptions.json")))
  expect_true(file.exists(file.path(unique(summary$project_root), "analytic_summary_table_proclevel-1.csv")))

  loaded_names <- load(summary$data_file[[1]])
  expect_equal(loaded_names, "data")
  expect_false(any(c("participant_id", "source_file") %in% names(data[[1]])))

  metadata <- jsonlite::read_json(summary$metadata_file[[1]], simplifyVector = TRUE)
  expect_equal(metadata$parser_diagnostics$export_type, "day")
  expect_true(metadata$parser_diagnostics$data_presence$has_required_headers)
  expect_true(metadata$parser_diagnostics$data_presence$has_data_rows)
  expect_true(metadata$parser_diagnostics$parse_quality$n_rows_out > 0)
})

test_that("read_appusage_batch captures malformed input diagnostics", {
  paths <- testthat::test_path("fixtures", c("day_sample.txt", "malformed.txt"))
  output_dir <- file.path(tempdir(), paste0("appusage_batch_", as.integer(runif(1, 1, 1e8))))

  summary <- read_appusage_batch(
    paths,
    ids = c("ok", "bad"),
    output_dir = output_dir,
    progress = FALSE
  )

  expect_equal(summary$status, c("success", "error"))
  expect_equal(summary$detected_type[[2]], "unknown")
  expect_match(summary$error_message[[2]], "not supported|unknown")
  expect_false(is.na(summary$error_class[[2]]))
  expect_false(is.na(summary$traceback[[2]]))
  expect_true(file.exists(summary$metadata_file[[2]]))
})

test_that("read_appusage_batch parses line and meta types", {
  paths <- testthat::test_path("fixtures", c("line_sample.txt", "meta_sample.txt"))
  output_dir <- file.path(tempdir(), paste0("appusage_batch_", as.integer(runif(1, 1, 1e8))))

  summary <- read_appusage_batch(
    paths,
    ids = c("line_id", "meta_id"),
    output_dir = output_dir,
    progress = FALSE
  )

  expect_equal(summary$status, c("success", "success"))
  expect_equal(summary$detected_type, c("line", "meta"))
  expect_true(all(file.exists(summary$data_file)))
  expect_true(all(file.exists(summary$metadata_file)))
  expect_equal(summary$n_parse_warnings, c(0, 0))
})

test_that("read_appusage_batch records empty recognized exports without writing RDA", {
  empty_meta <- file.path(tempdir(), "AppUsage_meta_2024_10_07_8_0_0.txt")
  writeLines(
    c(
      paste0(
        "2024-10-07\u8868\u4e00,\u5e94\u7528\u540d\u79f0,\u5e94\u7528\u5305\u540d,",
        "\u5f00\u59cb\u65f6\u95f4,\u7ed3\u675f\u65f6\u95f4,\u6700\u540e\u4e00\u6b21,",
        "\u603b\u65f6\u95f4,\u5f00\u59cb\u65f6\u95f4\uff08ms\uff09,",
        "\u7ed3\u675f\u65f6\u95f4\uff08ms\uff09,\u6700\u540e\u4e00\u6b21\uff08ms\uff09,",
        "\u603b\u65f6\u95f4\uff08ms\uff09"
      ),
      paste0(
        "2024-10-07\u8868\u4e8c,\u5e94\u7528\u540d\u79f0,\u5e94\u7528\u5305\u540d,",
        "\u5177\u4f53\u9875\u9762,\u65f6\u95f4,\u65f6\u95f4\u6233,\u7c7b\u578b,\u914d\u7f6e"
      )
    ),
    empty_meta,
    useBytes = TRUE
  )
  output_dir <- file.path(tempdir(), paste0("appusage_empty_batch_", as.integer(runif(1, 1, 1e8))))

  summary <- read_appusage_batch(
    empty_meta,
    output_dir = output_dir,
    progress = FALSE
  )

  expect_equal(summary$detected_type[[1]], "meta")
  expect_equal(summary$status[[1]], "error")
  expect_true(is.na(summary$data_file[[1]]))
  expect_true(file.exists(summary$metadata_file[[1]]))
  expect_match(summary$error_class[[1]], "appusage_empty_raw_data")
  expect_match(summary$error_message[[1]], "parsed raw data are empty")

  metadata <- jsonlite::read_json(summary$metadata_file[[1]], simplifyVector = TRUE)
  expect_equal(metadata$processing$first_level_status, "error")
  expect_equal(metadata$processing$first_level_failure_reason, "empty_raw_data")
  expect_equal(metadata$export$native_export_type_from_filename, "meta")
  expect_true(is.null(metadata$outputs$first_level_rda) || is.na(metadata$outputs$first_level_rda))
  expect_equal(metadata$parser_diagnostics$export_type, "meta")
  expect_true(metadata$parser_diagnostics$data_presence$has_required_headers)
  expect_false(metadata$parser_diagnostics$data_presence$has_data_rows)
  expect_equal(metadata$parser_diagnostics$data_presence$empty_reason, "headers_only_or_no_record_rows")
  expect_true(metadata$parser_diagnostics$format_specific$table1_empty)
  expect_true(metadata$parser_diagnostics$format_specific$table2_empty)

  second <- write_second_level_batch(summary, overwrite = TRUE, progress = FALSE)
  project_root <- unique(summary$project_root)
  expect_equal(second$status[[1]], "skipped")
  expect_equal(second$skip_reason[[1]], "upstream_first_level_error")
  expect_equal(second$first_level_error_message[[1]], summary$error_message[[1]])
  expect_true(is.na(second$error_message[[1]]))
  expect_true(is.na(second$second_level_data_file[[1]]))
  expect_true(is.na(second$second_level_metadata_file[[1]]))
  expect_true(is.na(second$second_level_rda[[1]]))
  expect_true(is.na(second$metadata_json[[1]]))
  expect_false(dir.exists(file.path(project_root, "proclevel-2")))
})

test_that("read_appusage_batch uses native filename type before content detection", {
  source <- testthat::test_path("fixtures", "line_sample.txt")
  misleading <- file.path(tempdir(), "AppUsage_meta_2024_10_07_8_0_1.txt")
  file.copy(source, misleading, overwrite = TRUE)
  output_dir <- file.path(tempdir(), paste0("appusage_filename_type_", as.integer(runif(1, 1, 1e8))))

  summary <- read_appusage_batch(
    misleading,
    output_dir = output_dir,
    progress = FALSE
  )

  expect_equal(summary$detected_type[[1]], "meta")
  expect_equal(summary$status[[1]], "error")
  expect_true(is.na(summary$data_file[[1]]))
  expect_true(file.exists(summary$metadata_file[[1]]))
  expect_true(isTRUE(summary$export_type_match[[1]]))
  expect_match(summary$error_message[[1]], "meta-format APP Usage table markers")
})

test_that("parse_day and parse_app output dates in chronological order", {
  day <- parse_day(testthat::test_path("fixtures", "day_sample.txt"))
  app <- parse_app(testthat::test_path("fixtures", "app_sample.txt"))

  expect_true(all(diff(as.numeric(day$date)) >= 0))
  expect_true(all(diff(as.numeric(app$date)) >= 0))
})

test_that("BIDS-like filename helpers round trip entities", {
  filename <- build_appusage_filename(
    participant_id = "p 001",
    export_type = "line",
    proc = 1,
    extension = "rda"
  )
  entities <- parse_appusage_filename(filename)

  expect_equal(filename, "sub-p-001_type-line_proc-1.rda")
  expect_equal(entities$sub, "p-001")
  expect_equal(entities$type, "line")
  expect_equal(entities$proc, "1")
  expect_equal(entities$extension, "rda")
})

test_that("native and Wenjuanxing filename parsers extract provenance metadata", {
  native <- parse_native_appusage_filename("AppUsage_line_2023_12_15_19_36_5.txt")
  expect_equal(native$native_export_type_from_filename, "line")
  expect_equal(native$native_filename_parse_status, "success")
  expect_match(native$native_export_created_at, "2023-12-15T19:36:05")

  wrapped <- parse_wenjuanxing_upload_filename(
    "\u5e8f\u53f73821_div style=tex_AppUsage_meta_2023_10_25_8_31_20.txt"
  )
  expect_equal(wrapped$wenjuanxing_sequence_id, 3821L)
  expect_equal(wrapped$native_export_type_from_filename, "meta")
  expect_equal(wrapped$filename_parse_status, "success")

  malformed <- parse_wenjuanxing_upload_filename(
    "\u5e8f\u53f73834_div style=tex_run_ver3.txt"
  )
  expect_equal(malformed$wenjuanxing_sequence_id, 3834L)
  expect_equal(malformed$filename_parse_status, "partial")
  expect_true(is.na(malformed$native_export_type_from_filename))

  unlock <- parse_wenjuanxing_upload_filename(
    "1001_AppUsage_Unlock_2024_10_7_8_0_0.txt"
  )
  expect_equal(unlock$wenjuanxing_sequence_id, 1001L)
  expect_equal(unlock$native_export_type_from_filename, "unknown")
  expect_equal(unlock$native_export_type_raw, "unlock")
  expect_equal(unlock$filename_parse_status, "unsupported")

  long_wjx <- parse_wenjuanxing_upload_filename(
    "序号1_手机使用时间的习惯 （安卓系统_AppUsage_line_2024_10_8_16_5_58.txt"
  )
  expect_equal(long_wjx$wenjuanxing_sequence_id, 1L)
  expect_equal(long_wjx$uploaded_file_name, "AppUsage_line_2024_10_8_16_5_58.txt")
  expect_equal(long_wjx$native_export_type_from_filename, "line")
})

test_that("Unlock native filenames are treated as unsupported first-level input", {
  source <- testthat::test_path("fixtures", "line_sample.txt")
  unlock_file <- file.path(tempdir(), "1001_AppUsage_Unlock_2024_10_7_8_0_0.txt")
  file.copy(source, unlock_file, overwrite = TRUE)
  output_dir <- file.path(tempdir(), paste0("appusage_unlock_batch_", as.integer(runif(1, 1, 1e8))))

  summary <- read_appusage_batch(
    unlock_file,
    output_dir = output_dir,
    progress = FALSE
  )

  expect_equal(summary$participant_id[[1]], "1001")
  expect_equal(summary$filename_export_type[[1]], "unknown")
  expect_equal(summary$detected_type[[1]], "unknown")
  expect_equal(summary$status[[1]], "error")
  expect_match(summary$error_class[[1]], "appusage_unsupported_type")
  expect_true(is.na(summary$data_file[[1]]))
})

test_that("batch summary and metadata include filename-derived ID fields", {
  source <- testthat::test_path("fixtures", "line_sample.txt")
  wrapped <- file.path(
    tempdir(),
    "\u5e8f\u53f73821_div style=tex_AppUsage_line_2023_12_15_19_36_5.txt"
  )
  file.copy(source, wrapped, overwrite = TRUE)
  output_dir <- file.path(tempdir(), paste0("appusage_batch_", as.integer(runif(1, 1, 1e8))))

  summary <- read_appusage_batch(
    wrapped,
    output_dir = output_dir,
    progress = FALSE
  )

  expect_equal(summary$participant_id, "3821")
  expect_equal(summary$participant_id_source, "filename_wenjuanxing")
  expect_equal(summary$wenjuanxing_sequence_id, 3821L)
  expect_equal(summary$filename_export_type, "line")
  expect_true(summary$export_type_match)

  metadata <- jsonlite::read_json(summary$metadata_file[[1]], simplifyVector = TRUE)
  expect_equal(metadata$identity$wenjuanxing_sequence_id, 3821L)
  expect_equal(metadata$export$native_export_type_from_filename, "line")
  expect_true(metadata$export$export_type_match)
})

test_that("batch fallback IDs remain valid filename entity values", {
  source <- testthat::test_path("fixtures", "line_sample.txt")
  renamed <- file.path(tempdir(), "not_an_appusage_name.txt")
  file.copy(source, renamed, overwrite = TRUE)
  output_dir <- file.path(tempdir(), paste0("appusage_batch_", as.integer(runif(1, 1, 1e8))))

  summary <- read_appusage_batch(
    renamed,
    output_dir = output_dir,
    progress = FALSE
  )

  expect_equal(summary$participant_id, "record-000001")
  expect_equal(summary$participant_id_source, "fallback")
  expect_match(basename(summary$data_file), "^sub-record-000001_type-line_proc-1[.]rda$")
})

test_that("batch output uses project folders and manual project identifiers", {
  paths <- testthat::test_path("fixtures", c("line_sample.txt", "malformed.txt"))
  parent_dir <- file.path(tempdir(), paste0("appusage_project_", as.integer(runif(1, 1, 1e8))))

  summary <- read_appusage_batch(
    paths,
    output_dir = parent_dir,
    project_name = "MyStudy",
    project_id = "a1b2",
    progress = FALSE
  )

  project_root <- file.path(parent_dir, "MyStudy_a1b2")
  expect_true(dir.exists(project_root))
  expect_true(dir.exists(file.path(project_root, "proclevel-1")))
  expect_false(dir.exists(file.path(project_root, "proclevel-2")))
  expect_false(dir.exists(file.path(project_root, "proclevel-3")))
  expect_false(dir.exists(file.path(project_root, "proclevel-tmp")))
  expect_true(all(dirname(summary$data_file[summary$status == "success"]) == normalizePath(file.path(project_root, "proclevel-1"), winslash = "/", mustWork = FALSE)))
  expect_true(file.exists(file.path(project_root, "dataset_descriptions.json")))
  expect_true(file.exists(file.path(project_root, "analytic_summary_table_proclevel-1.csv")))

  description <- jsonlite::read_json(file.path(project_root, "dataset_descriptions.json"), simplifyVector = TRUE)
  expect_equal(description$project_name, "MyStudy")
  expect_equal(description$project_id, "a1b2")
  expect_equal(description$latest_proclevel, 1)
  expect_equal(description$appusage_files$n_recognized_appusage_files, 1)
  expect_equal(description$appusage_files$n_line, 1)
  expect_equal(description$appusage_files$n_meta, 0)
  expect_true(is.null(description$appusage_files$native_export_date_min) || is.na(description$appusage_files$native_export_date_min))
})

test_that("write_second_level_batch writes project-level proclevel-2 outputs", {
  paths <- testthat::test_path("fixtures", c("line_sample.txt", "meta_sample.txt"))
  parent_dir <- file.path(tempdir(), paste0("appusage_project_", as.integer(runif(1, 1, 1e8))))
  first <- read_appusage_batch(
    paths,
    output_dir = parent_dir,
    project_name = "SecondStudy",
    project_id = "c3d4",
    progress = FALSE
  )

  second <- write_second_level_batch(first, overwrite = TRUE, progress = FALSE)
  project_root <- unique(first$project_root)

  expect_equal(second$status, c("success", "success"))
  expect_false(dir.exists(file.path(project_root, "proclevel-3")))
  expect_false(dir.exists(file.path(project_root, "proclevel-tmp")))
  expect_true(all(file.exists(second$second_level_data_file)))
  expect_true(all(file.exists(second$metadata_json)))
  expect_true(all(basename(dirname(second$second_level_data_file)) == "proclevel-2"))
  expect_true(all(basename(dirname(second$metadata_json)) == "proclevel-2"))
  expect_true(file.exists(file.path(project_root, "analytic_summary_table_proclevel-2.csv")))
  expect_equal(second$qc_status, c("success", "success"))
  expect_true(all(!is.na(second$second_level_total_elapsed_sec)))

  loaded <- load(second$second_level_data_file[[1]])
  expect_equal(loaded, "data")
  expect_named(data, c("event", "episode", "daily"))

  metadata <- jsonlite::read_json(second$metadata_json[[1]], simplifyVector = TRUE)
  expect_equal(metadata$processing$second_level_status, "success")
  expect_equal(metadata$processing$qc_status, "success")
  expect_equal(metadata$outputs$second_level_rda, second$second_level_data_file[[1]])
  expect_true(!is.null(metadata$second_level$profiling$total_elapsed_sec))
  expect_equal(metadata$qc$qc_function, "qc_appusage_day_inline")
})

test_that("dataset description summarizes recognized appusage types and dates", {
  source <- testthat::test_path("fixtures", "line_sample.txt")
  wrapped_line <- file.path(
    tempdir(),
    "\u5e8f\u53f71001_div style=tex_AppUsage_line_2023_12_15_19_36_5.txt"
  )
  wrapped_meta <- file.path(
    tempdir(),
    "\u5e8f\u53f71002_div style=tex_AppUsage_meta_2023_12_17_8_31_20.txt"
  )
  file.copy(source, wrapped_line, overwrite = TRUE)
  file.copy(source, wrapped_meta, overwrite = TRUE)
  parent_dir <- file.path(tempdir(), paste0("appusage_project_", as.integer(runif(1, 1, 1e8))))

  summary <- read_appusage_batch(
    c(wrapped_line, wrapped_meta),
    output_dir = parent_dir,
    project_name = "DateStudy",
    project_id = "d5e6",
    progress = FALSE
  )

  description <- jsonlite::read_json(
    file.path(unique(summary$project_root), "dataset_descriptions.json"),
    simplifyVector = TRUE
  )
  expect_equal(description$appusage_files$n_recognized_appusage_files, 2)
  expect_equal(description$appusage_files$n_line, 1)
  expect_equal(description$appusage_files$n_meta, 1)
  expect_equal(description$appusage_files$native_export_date_min, "2023-12-15")
  expect_equal(description$appusage_files$native_export_date_max, "2023-12-17")
})

test_that("parallel core settings are validated", {
  path <- testthat::test_path("fixtures", "line_sample.txt")
  output_dir <- file.path(tempdir(), paste0("appusage_batch_", as.integer(runif(1, 1, 1e8))))
  max_cores <- parallel::detectCores(logical = TRUE)
  testthat::skip_if(is.na(max_cores), "available cores could not be detected")
  expect_error(
    read_appusage_batch(
      path,
      output_dir = output_dir,
      parallel = TRUE,
      n_cores = max_cores + 1,
      progress = FALSE
    ),
    "cannot exceed available cores"
  )
})

test_that("second-level worker cap logic is bounded and explicit", {
  expect_equal(
    resolve_appusage_parallel_workers(
      parallel = TRUE,
      n_cores = 13,
      max_workers = 12,
      available_cores = 16,
      stage = "second-level"
    ),
    12L
  )
  expect_error(
    resolve_appusage_parallel_workers(
      parallel = TRUE,
      n_cores = 17,
      max_workers = 12,
      available_cores = 16,
      stage = "second-level"
    ),
    "cannot exceed available cores"
  )
  expect_equal(
    suppressMessages(resolve_appusage_parallel_workers(
      parallel = FALSE,
      n_cores = 4,
      max_workers = 12,
      available_cores = 16,
      stage = "second-level"
    )),
    1L
  )
})

load_proc2_data_for_test <- function(path) {
  env <- new.env(parent = emptyenv())
  expect_equal(load(path, envir = env), "data")
  env$data
}

test_that("second-level parallel mode preserves serial outputs on fixtures", {
  detected_cores <- parallel::detectCores(logical = TRUE)
  testthat::skip_if(is.na(detected_cores) || detected_cores < 2L)
  paths <- testthat::test_path("fixtures", c("line_sample.txt", "day_sample.txt", "app_sample.txt"))
  parent_dir <- file.path(tempdir(), paste0("appusage_second_parallel_", as.integer(runif(1, 1, 1e8))))
  first <- read_appusage_batch(
    paths,
    ids = c("line", "day", "app"),
    output_dir = parent_dir,
    progress = FALSE
  )
  serial_dir <- file.path(parent_dir, "serial-proclevel-2")
  parallel_dir <- file.path(parent_dir, "parallel-proclevel-2")

  serial <- write_second_level_batch(first,
    output_dir = serial_dir,
    overwrite = TRUE,
    progress = FALSE,
    parallel = FALSE
  )
  messages <- capture.output(
    parallel_summary <- write_second_level_batch(first,
      output_dir = parallel_dir,
      overwrite = TRUE,
      progress = TRUE,
      parallel = TRUE,
      n_cores = 2
    ),
    type = "message"
  )

  expect_true(any(grepl("parallel workers", messages)))
  expect_equal(parallel_summary$status, serial$status)
  expect_equal(parallel_summary$detected_type, serial$detected_type)
  for (i in seq_len(nrow(serial))) {
    serial_data <- load_proc2_data_for_test(serial$second_level_data_file[[i]])
    parallel_data <- load_proc2_data_for_test(parallel_summary$second_level_data_file[[i]])
    expect_equal(vapply(serial_data, nrow, integer(1)), vapply(parallel_data, nrow, integer(1)))
    expect_identical(names(serial_data$daily), names(parallel_data$daily))
    expect_equal(sum(serial_data$daily$duration_ms, na.rm = TRUE), sum(parallel_data$daily$duration_ms, na.rm = TRUE))
  }
})

test_that("second-level size-aware scheduling prioritizes larger line caches", {
  files <- file.path(tempdir(), paste0("appusage_sched_", 1:4, ".rda"))
  for (i in seq_along(files)) {
    writeBin(as.raw(rep(i, i * 10)), files[[i]])
  }
  batch <- tibble::tibble(
    index = 1:4,
    status = c("success", "success", "error", "success"),
    data_file = files,
    n_rows = c(1, 100, 999, 10),
    detected_type = c("app", "line", "line", "day")
  )

  order <- second_level_processing_order(batch)

  expect_setequal(order, 1:4)
  expect_equal(order[[1]], 2L)
  expect_equal(order[[length(order)]], 3L)
})

test_that("second-level resume skips valid proc-2 RDA JSON pairs", {
  path <- testthat::test_path("fixtures", "line_sample.txt")
  parent_dir <- file.path(tempdir(), paste0("appusage_second_resume_", as.integer(runif(1, 1, 1e8))))
  first <- read_appusage_batch(path,
    ids = "line",
    output_dir = parent_dir,
    progress = FALSE
  )

  initial <- write_second_level_batch(first, overwrite = TRUE, progress = FALSE)
  rda <- initial$second_level_data_file[[1]]
  metadata <- initial$metadata_json[[1]]
  rda_mtime <- file.info(rda)$mtime
  json_mtime <- file.info(metadata)$mtime
  Sys.sleep(1.1)

  resumed <- write_second_level_batch(first,
    resume = TRUE,
    overwrite = FALSE,
    progress = FALSE
  )

  expect_equal(resumed$status[[1]], "success")
  expect_equal(file.info(rda)$mtime, rda_mtime)
  expect_equal(file.info(metadata)$mtime, json_mtime)
  expect_true(file.exists(rda))
  expect_true(file.exists(metadata))
})

test_that("second-level resume does not treat JSON-only or RDA-only partial caches as complete", {
  path <- testthat::test_path("fixtures", "line_sample.txt")
  parent_dir <- file.path(tempdir(), paste0("appusage_second_partial_", as.integer(runif(1, 1, 1e8))))
  first <- read_appusage_batch(path,
    ids = "line",
    output_dir = parent_dir,
    progress = FALSE
  )

  json_only_dir <- file.path(parent_dir, "json-only-proclevel-2")
  dir.create(json_only_dir, recursive = TRUE)
  json_only_paths <- second_level_expected_paths(first$data_file[[1]], json_only_dir)
  writeLines("{}", json_only_paths$json_file)
  json_result <- write_second_level_batch(first,
    output_dir = json_only_dir,
    resume = TRUE,
    overwrite = FALSE,
    progress = FALSE
  )
  expect_equal(json_result$status[[1]], "success")
  expect_true(file.exists(json_only_paths$rda_file))
  expect_true(file.exists(json_only_paths$json_file))

  rda_only_dir <- file.path(parent_dir, "rda-only-proclevel-2")
  dir.create(rda_only_dir, recursive = TRUE)
  rda_only_paths <- second_level_expected_paths(first$data_file[[1]], rda_only_dir)
  data <- list(event = data.frame(), episode = data.frame(), daily = data.frame())
  save(data, file = rda_only_paths$rda_file)
  rda_result <- write_second_level_batch(first,
    output_dir = rda_only_dir,
    resume = TRUE,
    overwrite = FALSE,
    progress = FALSE
  )
  expect_equal(rda_result$status[[1]], "success")
  metadata <- jsonlite::read_json(rda_only_paths$json_file, simplifyVector = TRUE)
  expect_equal(metadata$processing$second_level_status, "success")
})

test_that("second-level parallel worker errors are captured in non-strict summaries", {
  detected_cores <- parallel::detectCores(logical = TRUE)
  testthat::skip_if(is.na(detected_cores) || detected_cores < 2L)
  path <- testthat::test_path("fixtures", "line_sample.txt")
  parent_dir <- file.path(tempdir(), paste0("appusage_second_error_", as.integer(runif(1, 1, 1e8))))
  first <- read_appusage_batch(path,
    ids = "line",
    output_dir = parent_dir,
    progress = FALSE
  )
  bad_file <- file.path(dirname(first$data_file[[1]]), "sub-bad_type-line_proc-1.rda")
  not_data <- list(bad = TRUE)
  save(not_data, file = bad_file)
  bad <- first[1, , drop = FALSE]
  bad$index <- 2L
  bad$participant_id <- "bad"
  bad$data_file <- bad_file
  bad$metadata_file <- NA_character_
  batch <- rbind(first, bad)

  second <- write_second_level_batch(batch,
    overwrite = TRUE,
    progress = FALSE,
    parallel = TRUE,
    n_cores = 2
  )

  expect_true(any(second$status == "success"))
  expect_true(any(second$second_level_status == "error" | second$status == "error", na.rm = TRUE))
  expect_true(any(!is.na(second$error_message) & nzchar(second$error_message)))
})
