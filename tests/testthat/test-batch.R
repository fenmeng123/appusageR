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

test_that("first-level cache summary rebuild detects complete, error, incomplete, and not-processed rows", {
  raw_dir <- file.path(tempdir(), paste0("appusage_rebuild_raw_", sample.int(1e8, 1)))
  dir.create(raw_dir, recursive = TRUE)
  source_day <- file.path(raw_dir, "seq101_div style=tex_AppUsage_day_2024_1_2_3_4_5.txt")
  source_app <- file.path(raw_dir, "seq102_div style=tex_AppUsage_app_2024_1_2_3_4_5.txt")
  source_bad <- file.path(raw_dir, "seq103_div style=tex_AppUsage_meta_2024_1_2_3_4_5.txt")
  source_not_processed <- file.path(raw_dir, "seq104_div style=tex_AppUsage_day_2024_1_2_3_4_5.txt")
  file.copy(testthat::test_path("fixtures", "day_sample.txt"), source_day)
  file.copy(testthat::test_path("fixtures", "app_sample.txt"), source_app)
  writeLines("not an app usage export", source_bad)
  file.copy(testthat::test_path("fixtures", "day_sample.txt"), source_not_processed)

  output_dir <- file.path(tempdir(), paste0("appusage_rebuild_out_", sample.int(1e8, 1)))
  first <- read_appusage_batch(
    c(source_day, source_app, source_bad),
    output_dir = output_dir,
    progress = FALSE
  )
  project_root <- unique(first$project_root)
  manifest <- build_appusage_project_manifest(raw_dir)

  unlink(file.path(project_root, "analytic_summary_table_proclevel-1.csv"))
  app_rda <- first$data_file[first$detected_type == "app"]
  unlink(app_rda)
  data <- list()
  save(data, file = file.path(project_root, "proclevel-1", "sub-orphan_type-line_proc-1.rda"))

  rebuilt <- rebuild_first_level_summary_from_cache(
    project_root,
    manifest = manifest,
    write = TRUE
  )

  expect_true(file.exists(file.path(project_root, "analytic_summary_table_proclevel-1.csv")))
  expect_true(any(rebuilt$status == "success"))
  expect_true(any(rebuilt$status == "error"))
  expect_true(any(rebuilt$status == "incomplete"))
  expect_true(any(rebuilt$status == "not_processed"))
  expect_true(any(rebuilt$cache_rebuild_status == "success_json_missing_rda"))
  expect_true(any(rebuilt$cache_rebuild_status == "rda_missing_json"))
  expect_true(any(rebuilt$cache_rebuild_status == "recorded_error"))
  expect_true(any(rebuilt$summary_source == "reconstructed_proc1_cache"))
})

test_that("read_appusage_batch resumes from first-level checkpoint without reparsing existing caches", {
  paths <- testthat::test_path("fixtures", c("day_sample.txt", "app_sample.txt"))
  output_dir <- file.path(tempdir(), paste0("appusage_checkpoint_", sample.int(1e8, 1)))

  first <- read_appusage_batch(
    paths,
    output_dir = output_dir,
    project_name = "StudyCheckpoint",
    project_id = "cp1",
    progress = FALSE,
    checkpoint_every = 1
  )
  project_root <- unique(first$project_root)
  unlink(file.path(project_root, "analytic_summary_table_proclevel-1.csv"))

  resumed <- read_appusage_batch(
    paths,
    output_dir = output_dir,
    project_name = "StudyCheckpoint",
    project_id = "cp1",
    progress = FALSE,
    resume = TRUE
  )

  expect_equal(resumed$status, first$status)
  expect_equal(normalizePath(resumed$data_file, winslash = "/"), normalizePath(first$data_file, winslash = "/"))
  expect_true(file.exists(file.path(project_root, "analytic_summary_table_proclevel-1.csv")))
})

test_that("read_appusage_batch resumes missing rows from rebuilt first-level summary", {
  raw_dir <- file.path(tempdir(), paste0("appusage_resume_rebuilt_raw_", sample.int(1e8, 1)))
  dir.create(raw_dir, recursive = TRUE)
  source_day <- file.path(raw_dir, "1001_AppUsage_day_2024_1_2_3_4_5.txt")
  source_app <- file.path(raw_dir, "1002_AppUsage_app_2024_1_2_3_4_5.txt")
  file.copy(testthat::test_path("fixtures", "day_sample.txt"), source_day)
  file.copy(testthat::test_path("fixtures", "app_sample.txt"), source_app)
  output_dir <- file.path(tempdir(), paste0("appusage_resume_rebuilt_", sample.int(1e8, 1)))

  first <- read_appusage_batch(
    source_day,
    output_dir = output_dir,
    project_name = "StudyResume",
    project_id = "rb1",
    progress = FALSE
  )
  project_root <- unique(first$project_root)
  manifest <- build_appusage_project_manifest(raw_dir)
  unlink(file.path(project_root, "analytic_summary_table_proclevel-1.csv"))
  unlink(file.path(project_root, "analytic_summary_table_proclevel-1.checkpoint.csv"))
  rebuilt <- rebuild_first_level_summary_from_cache(
    project_root,
    manifest = manifest,
    write = TRUE
  )
  expect_true(any(rebuilt$status == "not_processed"))

  resumed <- read_appusage_batch(
    c(source_day, source_app),
    output_dir = output_dir,
    project_name = "StudyResume",
    project_id = "rb1",
    progress = FALSE,
    resume = TRUE
  )

  expect_equal(resumed$status, c("success", "success"))
  expect_equal(normalizePath(resumed$data_file[[1]], winslash = "/"), normalizePath(first$data_file[[1]], winslash = "/"))
  expect_true(file.exists(resumed$data_file[[2]]))
  expect_false(any(resumed$status == "not_processed"))
})

test_that("read_appusage_batch retries memory-allocation rows from rebuilt summary", {
  path <- testthat::test_path("fixtures", "day_sample.txt")
  output_dir <- file.path(tempdir(), paste0("appusage_memory_seed_", sample.int(1e8, 1)))
  project_root <- file.path(output_dir, "StudyMemory_mem1")
  proc1 <- file.path(project_root, "proclevel-1")
  dir.create(proc1, recursive = TRUE)
  seed <- data.frame(
    index = 1L,
    source_file = path,
    status = "error",
    detected_type = "day",
    data_file = NA_character_,
    metadata_file = file.path(proc1, "sub-record-000001_type-day_proc-1.json"),
    error_class = "simpleError",
    error_message = "cannot allocate vector of size 143 Kb",
    failure_family = "memory_allocation",
    stringsAsFactors = FALSE
  )
  utils::write.csv(
    seed,
    file.path(project_root, "analytic_summary_table_proclevel-1.csv"),
    row.names = FALSE,
    na = ""
  )

  resumed <- read_appusage_batch(
    path,
    output_dir = output_dir,
    project_name = "StudyMemory",
    project_id = "mem1",
    progress = FALSE,
    resume = TRUE,
    retry_memory_allocation = TRUE,
    memory_retry_workers = 1L
  )

  expect_equal(resumed$status[[1]], "success")
  expect_equal(resumed$retry_attempt[[1]], 1L)
  expect_equal(resumed$retry_worker_count[[1]], 1L)
  expect_equal(resumed$original_failure_family[[1]], "memory_allocation")
  expect_match(resumed$original_error_message[[1]], "cannot allocate vector")
})

test_that("first-level memory allocation diagnostics include Windows R allocation messages", {
  string_buffer_error <- simpleError(
    "could not allocate memory (0 Mb) in C function 'R_AllocStringBuffer'"
  )
  realloc_error <- "Failed to realloc working memory stack to 100000*4bytes"

  expect_equal(first_level_failure_reason(string_buffer_error), "memory_allocation")
  expect_true(appusage_is_memory_allocation_text(conditionMessage(string_buffer_error)))
  expect_true(appusage_is_memory_allocation_text(realloc_error))
  expect_equal(
    appusage_classify_failure_family("simpleError", conditionMessage(string_buffer_error)),
    "memory_allocation"
  )
  expect_equal(
    appusage_classify_failure_family("simpleError", realloc_error),
    "memory_allocation"
  )
})

test_that("first-level adaptive worker selector records low-risk and high-risk decisions", {
  high_memory <- list(
    detected_total_memory_bytes = 64 * 1024^3,
    detected_free_memory_bytes = 48 * 1024^3,
    memory_detected = TRUE,
    memory_source = "test"
  )
  unknown_memory <- list(
    detected_total_memory_bytes = NA_real_,
    detected_free_memory_bytes = NA_real_,
    memory_detected = FALSE,
    memory_source = "test_unknown"
  )
  low_risk_size <- list(
    total_source_size_bytes = 100 * 1024^2,
    max_source_size_bytes = 1 * 1024^2,
    average_source_size_bytes = 20 * 1024,
    n_sized_files = 6000L
  )
  high_risk_size <- list(
    total_source_size_bytes = 500 * 1024^2,
    max_source_size_bytes = 250 * 1024^2,
    average_source_size_bytes = 1 * 1024^2,
    n_sized_files = 6000L
  )

  expect_equal(
    appusage_resolve_first_level_workers(
      parallel = FALSE,
      n_cores = 99,
      x = letters[1:3],
      available_cores = 16,
      memory_info = unknown_memory
    ),
    1L
  )
  low_risk <- appusage_first_level_worker_decision(
    parallel = TRUE,
    n_cores = 12,
    x = as.character(seq_len(6000)),
    available_cores = 32,
    memory_info = high_memory,
    size_info = low_risk_size
  )
  expect_gt(low_risk$selected_workers, 6L)
  expect_equal(low_risk$selected_workers, 12L)
  expect_equal(low_risk$cap_reason, "adaptive_low_memory_risk")

  expect_equal(
    appusage_first_level_worker_decision(
      parallel = TRUE,
      n_cores = 20,
      x = as.character(seq_len(100)),
      available_cores = 16,
      memory_info = high_memory,
      size_info = low_risk_size
    )$selected_workers,
    12L
  )
  high_risk <- appusage_first_level_worker_decision(
    parallel = TRUE,
    n_cores = 12,
    x = as.character(seq_len(6000)),
    available_cores = 32,
    memory_info = high_memory,
    size_info = high_risk_size
  )
  expect_equal(high_risk$selected_workers, 4L)
  expect_equal(high_risk$cap_reason, "large_file_risk")

  unknown <- appusage_first_level_worker_decision(
    parallel = TRUE,
    n_cores = 12,
    x = as.character(seq_len(6000)),
    available_cores = 32,
    memory_info = unknown_memory,
    size_info = low_risk_size
  )
  expect_equal(unknown$selected_workers, 6L)
  expect_equal(unknown$cap_reason, "conservative_large_project_memory_unknown")

  override <- appusage_first_level_worker_decision(
    parallel = TRUE,
    n_cores = 20,
    x = as.character(seq_len(6000)),
    available_cores = 32,
    max_workers = 12,
    worker_cap_override = TRUE,
    memory_info = unknown_memory,
    size_info = high_risk_size
  )
  expect_equal(override$selected_workers, 20L)
  expect_true(override$worker_cap_override)
  expect_equal(override$cap_reason, "explicit_worker_cap_override")
})

test_that("read_appusage_batch exposes first-level worker decision columns", {
  path <- testthat::test_path("fixtures", "day_sample.txt")
  output_dir <- file.path(tempdir(), paste0("appusage_worker_decision_", sample.int(1e8, 1)))
  summary <- read_appusage_batch(
    path,
    output_dir = output_dir,
    project_name = "WorkerDecision",
    project_id = "wd1",
    progress = FALSE,
    parallel = TRUE,
    n_cores = 2,
    max_workers = 2
  )

  expect_true(all(c(
    "first_level_requested_workers",
    "first_level_selected_workers",
    "first_level_worker_cap_reason",
    "first_level_worker_cap_override"
  ) %in% names(summary)))
  expect_equal(summary$first_level_requested_workers[[1]], 2L)
  expect_equal(summary$first_level_selected_workers[[1]], 1L)
  expect_false(summary$first_level_worker_cap_override[[1]])
  expect_type(attr(summary, "first_level_worker_decision"), "list")
})

test_that("first-level worker tasks carry compact per-record payloads", {
  x <- list(
    c("record one line 1", "record one line 2"),
    c("record two line 1", "record two line 2")
  )
  id_plan <- tibble::tibble(
    participant_id = c("p1", "p2"),
    source_index = 1:2,
    source_file = c("one.txt", "two.txt")
  )

  tasks <- appusage_make_first_level_worker_tasks(
    indices = 2L,
    x = x,
    id_plan = id_plan,
    type = "auto",
    input = "lines",
    output_dir = tempdir(),
    tz = "Asia/Shanghai",
    encoding = "auto",
    overwrite_plan = c(FALSE, TRUE)
  )

  expect_length(tasks, 1L)
  expect_equal(tasks[[1]]$index, 2L)
  expect_equal(tasks[[1]]$x, x[[2]])
  expect_false(identical(tasks[[1]]$x, x))
  expect_equal(nrow(tasks[[1]]$id_info), 1L)
  expect_equal(tasks[[1]]$id_info$participant_id[[1]], "p2")
  expect_true(tasks[[1]]$overwrite)
})

test_that("first-level parallel mode preserves serial outputs and recovery columns", {
  detected_cores <- parallel::detectCores(logical = TRUE)
  testthat::skip_if(is.na(detected_cores) || detected_cores < 2L)
  paths <- testthat::test_path("fixtures", c("line_sample.txt", "day_sample.txt", "app_sample.txt"))
  parent_dir <- file.path(tempdir(), paste0("appusage_first_parallel_", sample.int(1e8, 1)))

  serial <- read_appusage_batch(
    paths,
    ids = c("line", "day", "app"),
    output_dir = file.path(parent_dir, "serial"),
    project_name = "SerialFirst",
    project_id = "s1",
    progress = FALSE,
    parallel = FALSE,
    checkpoint_every = 1
  )
  parallel_summary <- read_appusage_batch(
    paths,
    ids = c("line", "day", "app"),
    output_dir = file.path(parent_dir, "parallel"),
    project_name = "ParallelFirst",
    project_id = "p1",
    progress = FALSE,
    parallel = TRUE,
    n_cores = 2,
    max_workers = 2,
    checkpoint_every = 1
  )

  expect_equal(parallel_summary$participant_id, serial$participant_id)
  expect_equal(parallel_summary$status, serial$status)
  expect_equal(parallel_summary$detected_type, serial$detected_type)
  expect_equal(parallel_summary$n_rows, serial$n_rows)
  expect_equal(parallel_summary$n_parse_warnings, serial$n_parse_warnings)
  expect_true(all(c(
    "failure_family",
    "retry_attempt",
    "retry_worker_count",
    "original_failure_family",
    "worker_pid"
  ) %in% names(parallel_summary)))
  expect_true(all(!is.na(parallel_summary$worker_pid)))
  expect_true(file.exists(file.path(
    unique(parallel_summary$project_root),
    "analytic_summary_table_proclevel-1.checkpoint.csv"
  )))

  for (i in seq_len(nrow(serial))) {
    serial_env <- new.env(parent = emptyenv())
    parallel_env <- new.env(parent = emptyenv())
    expect_equal(load(serial$data_file[[i]], envir = serial_env), "data")
    expect_equal(load(parallel_summary$data_file[[i]], envir = parallel_env), "data")
    expect_equal(
      vapply(serial_env$data, nrow, integer(1)),
      vapply(parallel_env$data, nrow, integer(1))
    )
  }
})

test_that("first-level parallel progress and worker errors are structured", {
  detected_cores <- parallel::detectCores(logical = TRUE)
  testthat::skip_if(is.na(detected_cores) || detected_cores < 2L)
  paths <- testthat::test_path("fixtures", c("line_sample.txt", "malformed.txt"))
  output_dir <- file.path(tempdir(), paste0("appusage_first_progress_", sample.int(1e8, 1)))

  messages <- capture.output(
    summary <- read_appusage_batch(
      paths,
      ids = c("ok", "bad"),
      output_dir = output_dir,
      project_name = "FirstProgress",
      project_id = "fp1",
      progress = TRUE,
      parallel = TRUE,
      n_cores = 2,
      max_workers = 2,
      checkpoint_every = 1
    ),
    type = "message"
  )

  expect_true(any(grepl("\\[parallel-progress\\] first-level", messages)))
  expect_true(any(grepl("status=success", messages)))
  expect_true(any(grepl("status=error", messages)))
  expect_true(all(c(
    "worker_stage",
    "worker_task_index",
    "worker_pid",
    "failure_family",
    "retry_attempt",
    "retry_worker_count"
  ) %in% names(summary)))
  error_row <- summary[summary$status == "error", , drop = FALSE]
  expect_equal(nrow(error_row), 1L)
  expect_equal(error_row$worker_stage[[1]], "first-level")
  expect_equal(error_row$worker_task_index[[1]], 2L)
  expect_false(is.na(error_row$worker_pid[[1]]))
  expect_true(!is.na(error_row$error_class[[1]]) && nzchar(error_row$error_class[[1]]))
  expect_true(!is.na(error_row$error_message[[1]]) && nzchar(error_row$error_message[[1]]))
  expect_true(file.exists(file.path(
    unique(summary$project_root),
    "analytic_summary_table_proclevel-1.checkpoint.csv"
  )))
})

test_that("memory allocation failures are classified and can be retried", {
  row <- data.frame(
    status = "error",
    error_class = "simpleError",
    error_message = "cannot allocate vector of size 143 Kb",
    stringsAsFactors = FALSE
  )
  row <- appusage_annotate_first_level_row(row)

  expect_equal(row$failure_family[[1]], "memory_allocation")

  retry <- appusage_retry_memory_row(
    row,
    retry_fun = function() {
      data.frame(
        status = "success",
        error_class = NA_character_,
        error_message = NA_character_,
        data_file = "ok.rda",
        stringsAsFactors = FALSE
      )
    },
    retry_worker_count = 1L
  )

  expect_equal(retry$status[[1]], "success")
  expect_equal(retry$retry_attempt[[1]], 1L)
  expect_equal(retry$retry_worker_count[[1]], 1L)
  expect_equal(retry$original_failure_family[[1]], "memory_allocation")
  expect_match(retry$original_error_message[[1]], "cannot allocate vector")
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

test_that("inline second-level QC matches explicit QC-only rerun semantics", {
  paths <- testthat::test_path("fixtures", "line_sample.txt")
  parent_inline <- file.path(tempdir(), paste0("appusage_inline_qc_", as.integer(runif(1, 1, 1e8))))
  parent_explicit <- file.path(tempdir(), paste0("appusage_explicit_qc_", as.integer(runif(1, 1, 1e8))))
  first_inline <- read_appusage_batch(
    paths,
    output_dir = parent_inline,
    project_name = "InlineQC",
    project_id = "i1",
    progress = FALSE
  )
  first_explicit <- read_appusage_batch(
    paths,
    output_dir = parent_explicit,
    project_name = "ExplicitQC",
    project_id = "e1",
    progress = FALSE
  )

  inline <- write_second_level_batch(
    first_inline,
    overwrite = TRUE,
    progress = FALSE,
    min_nonempty_days = 1,
    require_all_weekdays = FALSE
  )
  explicit_initial <- write_second_level_batch(
    first_explicit,
    overwrite = TRUE,
    progress = FALSE,
    inline_qc = FALSE,
    min_nonempty_days = 1,
    require_all_weekdays = FALSE
  )
  explicit_project <- unique(first_explicit$project_root)
  explicit <- write_qc_metadata_batch(
    explicit_project,
    progress = FALSE,
    overwrite = TRUE,
    min_nonempty_days = 1,
    require_all_weekdays = FALSE
  )

  expect_equal(explicit_initial$qc_status[[1]], "not_run")
  expect_equal(inline$qc_status[[1]], "success")
  expect_equal(explicit$qc_status[[1]], "success")
  for (field in c(
    "pass_qc", "n_recorded_days", "n_nonempty_days",
    "weekdays_covered", "analysis_eligible_episode",
    "analysis_eligible_daily", "n_daily_rows"
  )) {
    expect_equal(inline[[field]][[1]], explicit[[field]][[1]], info = field)
  }
  inline_metadata <- jsonlite::read_json(inline$metadata_json[[1]], simplifyVector = TRUE)
  explicit_metadata <- jsonlite::read_json(explicit$metadata_json[[1]], simplifyVector = TRUE)
  expect_equal(inline_metadata$qc$qc_function, "qc_appusage_day_inline")
  expect_equal(explicit_metadata$qc$qc_function, "qc_appusage_day")
  expect_equal(inline_metadata$qc$pass_qc, explicit_metadata$qc$pass_qc)
  expect_equal(inline_metadata$qc$n_nonempty_days, explicit_metadata$qc$n_nonempty_days)
  expect_false(dir.exists(file.path(unique(first_inline$project_root), "proclevel-3")))
  expect_false(dir.exists(file.path(explicit_project, "proclevel-3")))
})

test_that("second-level inline QC preserves skipped rows without proc-3 output", {
  paths <- testthat::test_path("fixtures", c("line_sample.txt", "malformed.txt"))
  parent_dir <- file.path(tempdir(), paste0("appusage_inline_qc_skip_", as.integer(runif(1, 1, 1e8))))
  first <- read_appusage_batch(
    paths,
    output_dir = parent_dir,
    project_name = "InlineSkip",
    project_id = "s1",
    progress = FALSE
  )

  second <- write_second_level_batch(
    first,
    overwrite = TRUE,
    progress = FALSE,
    min_nonempty_days = 1,
    require_all_weekdays = FALSE
  )
  project_root <- unique(first$project_root)
  skipped <- second[second$status == "skipped", , drop = FALSE]
  success <- second[second$status == "success", , drop = FALSE]

  expect_equal(nrow(success), 1L)
  expect_equal(success$qc_status[[1]], "success")
  expect_equal(nrow(skipped), 1L)
  expect_equal(skipped$qc_status[[1]], "not_run")
  expect_equal(skipped$skip_reason[[1]], "upstream_first_level_error")
  expect_false(dir.exists(file.path(project_root, "proclevel-3")))
  expect_false(dir.exists(file.path(project_root, "proclevel-tmp")))
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

test_that("first-level parallel core settings are capped", {
  path <- testthat::test_path("fixtures", "line_sample.txt")
  output_dir <- file.path(tempdir(), paste0("appusage_batch_", as.integer(runif(1, 1, 1e8))))
  max_cores <- parallel::detectCores(logical = TRUE)
  testthat::skip_if(is.na(max_cores), "available cores could not be detected")
  summary <- read_appusage_batch(
    path,
    output_dir = output_dir,
    parallel = TRUE,
    n_cores = max_cores + 1,
    progress = FALSE
  )
  expect_equal(summary$status[[1]], "success")
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
  expect_equal(parallel_summary$participant_id, serial$participant_id)
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

test_that("second-level size-aware scheduling prioritizes heavier eligible caches", {
  files <- file.path(tempdir(), paste0("appusage_sched_", sample.int(1e8, 1), "_", 1:6, ".rda"))
  for (i in seq_along(files)) {
    writeBin(as.raw(rep(i, i * 10)), files[[i]])
  }
  batch <- tibble::tibble(
    index = 1:6,
    status = c("success", "success", "error", "success", "success", "success"),
    data_file = c(files[1:4], file.path(tempdir(), "appusage_missing_sched.rda"), files[6]),
    first_level_rda_size_bytes = c(10, 5e7, 1e9, NA, 9e7, NA),
    source_file_size_bytes = c(10, 1e5, 1e9, 20, 1e9, NA),
    n_rows = c(1, 100, 999, 10, 999, NA),
    n_event_rows = c(NA, NA, NA, NA, NA, 2),
    n_episode_rows = c(NA, NA, NA, NA, NA, 3),
    n_daily_rows = c(NA, NA, NA, NA, NA, 4),
    detected_type = c("line", "line", "line", "day", "meta", "app")
  )

  order <- second_level_processing_order(batch)
  score <- second_level_processing_score(batch)

  expect_setequal(order, 1:6)
  expect_equal(order[[1]], 2L)
  expect_true(max(match(c(1L, 2L, 4L, 6L), order)) < min(match(c(3L, 5L), order)))
  expect_true(score$eligible[[2]])
  expect_false(score$eligible[[3]])
  expect_false(score$eligible[[5]])
  expect_equal(score$row_signal[[6]], 9)
  expect_gt(score$file_size_bytes[[2]], score$file_size_bytes[[1]])
})

test_that("second-level scheduling handles missing profile metadata fallbacks", {
  files <- file.path(tempdir(), paste0("appusage_sched_fallback_", sample.int(1e8, 1), "_", 1:3, ".rda"))
  for (i in seq_along(files)) {
    writeBin(as.raw(rep(1L, 5L)), files[[i]])
  }
  batch <- tibble::tibble(
    index = 1:3,
    status = c("success", "success", "error"),
    data_file = files,
    detected_type = c(NA_character_, "unknown", "line")
  )

  order <- second_level_processing_order(batch)
  score <- second_level_processing_score(batch)

  expect_setequal(order, 1:3)
  expect_setequal(order[1:2], 1:2)
  expect_equal(order[[3]], 3L)
  expect_equal(score$row_signal, c(0, 0, 0))
  expect_true(all(score$file_size_bytes > 0))
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

test_that("second-level project subset rerun rebuilds only filtered rows", {
  paths <- testthat::test_path("fixtures", c("line_sample.txt", "meta_sample.txt", "malformed.txt"))
  parent_dir <- file.path(tempdir(), paste0("appusage_second_subset_", as.integer(runif(1, 1, 1e8))))
  first <- read_appusage_batch(paths,
    ids = c("line", "meta", "bad"),
    output_dir = parent_dir,
    progress = FALSE
  )
  first$detected_type[first$participant_id == "bad"] <- "meta"
  initial <- write_second_level_batch(first, overwrite = TRUE, progress = FALSE)
  project_root <- unique(first$project_root)
  initial$self_report_match_status <- paste0("match-", initial$participant_id)
  initial$self_report_sequence_id <- seq_len(nrow(initial))
  utils::write.csv(initial,
    file.path(project_root, "analytic_summary_table_proclevel-2.csv"),
    row.names = FALSE,
    na = ""
  )
  line_rda <- initial$second_level_data_file[initial$detected_type == "line"][[1]]
  meta_rda <- initial$second_level_data_file[initial$detected_type == "meta"][[1]]
  skipped <- initial[initial$status == "skipped", , drop = FALSE]
  expect_equal(nrow(skipped), 1L)
  line_mtime <- file.info(line_rda)$mtime
  meta_mtime <- file.info(meta_rda)$mtime
  Sys.sleep(1.1)

  result <- rerun_second_level_project_subset(
    project_root,
    filter = list(detected_type = "meta"),
    overwrite = TRUE,
    progress = FALSE
  )

  expect_s3_class(result, "appusage_second_level_subset_rerun")
  expect_equal(result$n_selected, 1L)
  expect_equal(result$selected_first_level$detected_type, "meta")
  expect_true(all(result$selected_first_level$status == "success"))
  expect_equal(file.info(line_rda)$mtime, line_mtime)
  expect_gt(file.info(meta_rda)$mtime, meta_mtime)
  expect_true(file.exists(result$configuration_file))
  expect_equal(nrow(result$summary), nrow(initial))
  expect_true(any(result$summary$status == "skipped"))
  expect_true(all(!is.na(result$summary$self_report_match_status)))
  expect_true(all(!is.na(result$summary$self_report_sequence_id)))
})

test_that("second-level subset summary merge restores matching fields by key after reordering", {
  first <- tibble::tibble(
    data_file = c("first-line.rda", "first-meta.rda", "first-day.rda"),
    participant_id = c("line", "meta", "day"),
    detected_type = c("line", "meta", "day"),
    filename_export_type = c("line", "meta", "day"),
    wenjuanxing_sequence_id = c(101L, 102L, 103L)
  )
  old_summary <- tibble::tibble(
    first_level_data_file = first$data_file,
    participant_id = first$participant_id,
    detected_type = first$detected_type,
    filename_export_type = first$filename_export_type,
    wenjuanxing_sequence_id = first$wenjuanxing_sequence_id,
    status = "success",
    self_report_match_status = paste0("old-", first$participant_id),
    self_report_sequence_id = first$wenjuanxing_sequence_id
  )
  new_summary <- old_summary[2, ]
  new_summary$self_report_match_status <- NA_character_
  new_summary$self_report_sequence_id <- NA_integer_
  new_summary$status <- "success-rebuilt"

  merged <- merge_subset_second_level_summary(
    old_summary = old_summary[c(3, 1, 2), ],
    new_summary = new_summary,
    first_summary = first
  )

  expect_equal(nrow(merged), nrow(old_summary))
  expect_equal(merged$participant_id, first$participant_id)
  expect_equal(
    merged$self_report_match_status,
    paste0("old-", first$participant_id)
  )
  expect_equal(merged$self_report_sequence_id, first$wenjuanxing_sequence_id)
  expect_equal(merged$status[merged$participant_id == "meta"], "success-rebuilt")
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

  messages <- capture.output(
    second <- write_second_level_batch(batch,
      overwrite = TRUE,
      progress = TRUE,
      parallel = TRUE,
      n_cores = 2
    ),
    type = "message"
  )

  expect_true(any(grepl("\\[parallel-progress\\] second-level", messages)))
  expect_true(any(grepl("status=success", messages)))
  expect_true(any(grepl("status=error", messages)))
  expect_true(any(second$status == "success"))
  expect_true(any(second$second_level_status == "error" | second$status == "error", na.rm = TRUE))
  expect_true(any(!is.na(second$error_message) & nzchar(second$error_message)))
  is_error <- second$status == "error" |
    (!is.na(second$second_level_status) & second$second_level_status == "error")
  error_row <- second[is_error, , drop = FALSE]
  expect_true("worker_stage" %in% names(second))
  expect_true("worker_task_index" %in% names(second))
  expect_true("worker_pid" %in% names(second))
  expect_true("error_class" %in% names(second))
  expect_equal(error_row$worker_stage[[1]], "second-level")
  expect_equal(error_row$worker_task_index[[1]], 2L)
  expect_false(is.na(error_row$worker_pid[[1]]))
  expect_true(!is.na(error_row$error_class[[1]]) && nzchar(error_row$error_class[[1]]))
  expect_true(file.exists(error_row$second_level_metadata_file[[1]]))
})
