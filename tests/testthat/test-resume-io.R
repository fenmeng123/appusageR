resume_io_fixture <- function() {
  root <- tempfile("resume-io-")
  dir.create(root)
  path <- file.path(root, "1_AppUsage_line_2024_1_2_3_4_5.txt")
  file.copy(test_path("fixtures", "line_sample.txt"), path)
  project <- file.path(root, "project")
  result <- run_appusage_workflow(path, project_dir = project,
    config = appusage_config(execution = list(progress = FALSE)))
  list(project = project, result = result,
    json = second_level_metadata_path(appusage_summary_proc2_paths(result$second_level)[[1]]))
}

test_that("content-checked CSV reuse preserves values and detects restored-mtime changes", {
  root <- tempfile("summary-bytes-")
  dir.create(root)
  path <- file.path(root, "summary.csv")
  write.csv(data.frame(key = "a", amount = 1), path, row.names = FALSE)
  context <- appusage_runtime_context()
  context$enabled <- TRUE
  withr::local_options(list(appusageR.runtime_context = context))
  original <- appusage_read_summary_csv(path)
  expect_identical(appusage_read_summary_csv(path), original)
  stamp <- file.info(path)
  write.csv(data.frame(key = "b", amount = 1), path, row.names = FALSE)
  Sys.setFileTime(path, stamp$mtime)
  expect_equal(file.info(path)$size, stamp$size)
  expect_identical(appusage_read_summary_csv(path)$key, "b")
  metrics <- appusage_runtime_metrics(context)
  expect_equal(metrics$calls[metrics$operation == "summary_csv_read"], 2)
  expect_equal(metrics$calls[metrics$operation == "summary_csv_reuse"], 1)
})

test_that("compact JSON reuse drops details and detects same-size external changes", {
  f <- resume_io_fixture()
  full <- appusage_read_json(f$json, simplifyVector = TRUE)
  full$private_qc_details <- as.list(seq_len(10000L))
  write_metadata_json(full, f$json)
  context <- appusage_runtime_context()
  context$enabled <- TRUE
  withr::local_options(list(appusageR.runtime_context = context))
  compact <- appusage_read_validation_json(f$json)
  expect_null(compact$private_qc_details)
  expect_identical(qc_summary_row_from_loaded_metadata(compact, f$json),
    qc_summary_row_from_loaded_metadata(full, f$json))
  expect_equal(appusage_daily_self_check_summary_values(metadata = compact),
    appusage_daily_self_check_summary_values(metadata = full))
  expect_equal(appusage_source_qc_summary_from_metadata(compact),
    appusage_source_qc_summary_from_metadata(full))
  before <- tools::md5sum(f$json)
  expect_error(write_metadata_json(compact, f$json), "cannot replace")
  expect_identical(tools::md5sum(f$json), before)
  expect_identical(appusage_read_validation_json(f$json), compact)
  stamp <- file.info(f$json)
  full$identity$participant_id <- "2"
  jsonlite::write_json(full, f$json, auto_unbox = TRUE, pretty = TRUE, na = "null")
  Sys.setFileTime(f$json, stamp$mtime)
  expect_equal(file.info(f$json)$size, stamp$size)
  expect_identical(appusage_read_validation_json(f$json)$identity$participant_id, "2")
  expect_equal(sum(appusage_runtime_metrics(context)$calls[
    appusage_runtime_metrics(context)$operation == "json_parse"]), 2)
})

test_that("damaged decoding indexes fall back to owner content without silent reuse", {
  f <- resume_io_fixture()
  control <- file.path(f$project, "appusage_projection_state.rds")
  state <- readRDS(control)
  expect_true(length(state$resume_index$payload$validation_json) > 0)
  key <- f$json
  expect_true(key %in% names(state$resume_index$payload$validation_json))
  state$resume_index$payload$validation_json[[key]]$value$participant_id <- "wrong"
  saveRDS(state, control)
  context <- appusage_runtime_context()
  context$enabled <- TRUE
  withr::local_options(list(appusageR.runtime_context = context))
  appusage_resume_index_load(f$project)
  expect_length(as.list(context$validation_json), 0)
  metadata <- appusage_read_validation_json(key)
  expect_false(identical(metadata$participant_id, "wrong"))
  expect_equal(sum(appusage_runtime_metrics(context)$calls[
    appusage_runtime_metrics(context)$operation == "json_parse"]), 1)
})

test_that("unchanged recovery checks bytes without decoding owners or calculating science", {
  f <- resume_io_fixture()
  withr::local_options(list(appusageR.diagnostics = TRUE))
  paths <- c(f$result$first_level$data_file, f$result$first_level$metadata_file,
    appusage_summary_proc2_paths(f$result$second_level), f$json)
  before <- tools::md5sum(paths)
  result <- run_appusage_workflow(project_dir = f$project)
  expect_true(all(result$run_record$tasks$actual_action %in% c("reuse", "disabled")))
  expect_identical(tools::md5sum(paths), before)
  expect_false(any(result$run_record$metrics$operation %in%
    c("json_parse", "summary_csv_read", "parse", "research_data", "scientific_qc", "rda_load")))
})

test_that("same-size RDA damage invalidates indexed reuse and repairs only that source", {
  f <- resume_io_fixture()
  path <- appusage_summary_proc2_paths(f$result$second_level)[[1]]
  expected <- load_appusage_data_object(path)
  protected <- c(f$result$first_level$data_file, f$result$first_level$metadata_file)
  before <- tools::md5sum(protected)
  stamp <- file.info(path)
  bytes <- readBin(path, "raw", n = stamp$size)
  bytes[[1L]] <- as.raw(bitwXor(as.integer(bytes[[1L]]), 1L))
  writeBin(bytes, path)
  Sys.setFileTime(path, stamp$mtime)
  expect_equal(file.info(path)$size, stamp$size)
  withr::local_options(list(appusageR.diagnostics = TRUE))
  result <- run_appusage_workflow(project_dir = f$project)
  expect_equal(result$plan$tasks$action[result$plan$tasks$stage == "research_data"], "run")
  expect_identical(load_appusage_data_object(path), expected)
  expect_identical(tools::md5sum(protected), before)
  counts <- result$run_record$metrics
  expect_equal(counts$calls[counts$operation == "research_data"], 1)
})
