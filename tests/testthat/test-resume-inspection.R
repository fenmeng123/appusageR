resume_fixture <- function(n = 2L) {
  root <- tempfile("resume-inspection-")
  dir.create(root)
  files <- file.path(root, sprintf("%d_AppUsage_line_2024_1_2_3_4_5.txt", seq_len(n)))
  for (path in files) file.copy(test_path("fixtures", "line_sample.txt"), path)
  list(files = files, project = file.path(root, "project"),
    config = appusage_config(execution = list(progress = FALSE)))
}

test_that("canonical comparison agrees with the previous hash oracle", {
  normalize <- function(x) {
    if (is.list(x)) return(lapply(x, normalize))
    if (is.numeric(x)) return(as.numeric(x))
    x
  }
  oracle <- function(x, y) !is.null(x) && identical(
    appusage_object_fingerprint(normalize(x)), appusage_object_fingerprint(normalize(y)))
  objects <- list(NULL, list(), list(b = 1L, a = c(NA_real_, Inf)),
    list(a = c(NA_real_, Inf), b = 1), list(b = 2, a = c(NA_real_, Inf)),
    list(x = NA), list(x = NA_character_), as.Date("2024-01-01"),
    as.POSIXct("2024-01-01", tz = "UTC"), c(a = "A", b = "NA"),
    list(x = c("", "\u4e2d\u6587")), structure(1, unit = "ms"))
  for (x in objects) for (y in objects)
    expect_identical(appusage_contract_equal(x, y), oracle(x, y))
})

test_that("plans provide stable identities, project tasks and precise differences", {
  f <- resume_fixture()
  before <- plan_appusage_workflow(f$files, f$project, f$config)
  expect_true(all(is.na(before$tasks$source_record_key)))
  expect_identical(before$project_tasks$stage, c("matching", "summary"))
  reordered <- plan_appusage_workflow(rev(f$files), f$project, f$config)
  expect_setequal(before$tasks$task_id, reordered$tasks$task_id)
  result <- run_appusage_workflow(plan = before)
  f$config$qc$min_nonempty_days <- 2
  changed <- plan_appusage_workflow(f$files, f$project, f$config)
  expect_true(all(changed$tasks$action[changed$tasks$stage == "research_data"] == "reuse"))
  expect_true(all(grepl("min_nonempty_days: 7 -> 2", changed$tasks$details[changed$tasks$stage == "qc"], fixed = TRUE)))
  expect_true(all(c("planned_action", "actual_action", "final_status", "deviation_reason",
    "started_at", "finished_at", "elapsed_sec") %in% names(result$run_record$tasks)))
  expect_true(all(vapply(result$run_record$stages, function(x) x$elapsed_sec >= 0, logical(1))))
})

test_that("metadata previews and object methods never load scientific or Excel payloads", {
  f <- resume_fixture(1L)
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  local_mocked_bindings(appusage_source_fingerprint = function(...) stop("raw read"),
    load_appusage_data_object = function(...) stop("RDA load"),
    appusage_read_self_report_workbook = function(...) stop("Excel read"))
  plan <- plan_appusage_workflow(project_dir = f$project)
  expect_false(any(plan$tasks$source_verified))
  expect_true(all(summary(plan)$artifacts$state == "unverified"))
  expect_output(print(plan), "not verified")
  expect_output(print(result), "Coverage QC")
  expect_named(summary(result), c("status", "stages", "tasks", "suggested_calls", "overview"))
})

test_that("unchanged matching and missing export reuse the relationship owner", {
  f <- resume_fixture(1L)
  survey <- file.path(dirname(f$project), "ProjectID-123_WJXraw-456.xlsx")
  appusage_write_xlsx(data.frame(seq = c(1L, 99L), upload = c(basename(f$files), "absent.txt")), survey)
  f$config$matching <- modifyList(f$config$matching, list(enabled = TRUE,
    self_report_file = survey, sequence_col = "seq", upload_col = "upload"))
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  owner <- file.path(f$project, "self_report_link_result.rds")
  before <- tools::md5sum(owner)
  export <- appusage_matching_export_path(f$project, f$config)
  unlink(export)
  plan <- plan_appusage_workflow(project_dir = f$project, verify = "content")
  expect_identical(plan$project_tasks$action, c("reuse", "run"))
  updated <- run_appusage_workflow(plan = plan)
  expect_identical(tools::md5sum(owner), before)
  expect_true(isTRUE(attr(updated$matching, "stage_reused")))
  expect_true(file.exists(export))
  expect_equal(nrow(updated$matching$matched_self_report), 2L)
})

test_that("preview actions are revalidated and independent stages record scope", {
  f <- resume_fixture()
  run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  plan <- plan_appusage_workflow(project_dir = f$project, verify = "content")
  writeLines("unsupported", f$files[[1]])
  result <- run_appusage_workflow(plan = plan)
  expect_equal(result$first_level$status, c("error", "success"))
  f$config$qc$min_nonempty_days <- 2
  stage <- run_appusage_stage("qc", f$project, f$config, x = f$files[[2]])
  record <- attr(stage, "run_record")
  expect_equal(record$requested_stage, "qc")
  expect_true(all(record$tasks$actual_action[record$tasks$stage == "parse"] == "disabled"))
  expect_true(any(record$tasks$actual_action[record$tasks$stage == "qc"] == "run"))
})

test_that("execution success is separate from coverage and unavailable grains", {
  first <- data.frame(status = c("success", "success", "error"))
  second <- data.frame(second_level_status = c("success", "success", "skipped"),
    qc_status = c("success", "not_run", "skipped"), detected_type = c("line", "app", "unknown"),
    pass_qc = c(FALSE, FALSE, FALSE), n_event_rows = c(0L, 0L, NA),
    n_episode_rows = c(3L, 0L, NA), n_daily_rows = c(1L, 1L, NA),
    analysis_eligible_daily = c(FALSE, FALSE, FALSE))
  overview <- appusage_workflow_overview(first, second)
  expect_equal(overview$coverage_qc$n, c(0L, 1L, 2L))
  expect_equal(overview$grains$not_applicable[overview$grains$grain == "event"], 2)
  expect_equal(overview$grains$ineligible[overview$grains$grain == "daily"], 1)
  expect_true(all(overview$grains$denominator == 3L))
})

test_that("missing summaries recover without science and diagnostics count no-change reuse", {
  f <- resume_fixture()
  withr::local_options(appusageR.diagnostics = TRUE)
  run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  resumed <- run_appusage_workflow(project_dir = f$project)
  counts <- resumed$run_record$metrics
  expect_false(any(counts$operation %in% c("parse", "research_data", "reconstruction",
    "daily_aggregation", "scientific_qc", "rda_load")))
  expect_true(all(c("source_fingerprint", "file_md5", "validation_json_reuse") %in% counts$operation))
  expect_false("json_parse" %in% counts$operation)
  expect_true(all(resumed$plan$project_tasks$action %in% c("reuse", "disabled")))
  csv <- file.path(f$project, c("analytic_summary_table_proclevel-1.csv", "analytic_summary_table_proclevel-2.csv"))
  unlink(csv)
  preview <- plan_appusage_workflow(project_dir = f$project, verify = "content")
  expect_false(any(file.exists(csv)))
  expect_true(all(preview$tasks$action %in% c("reuse", "disabled")))
  result <- run_appusage_stage("summary", f$project)
  expect_true(all(file.exists(csv)))
  expect_equal(nrow(result), 2L)
  expect_false(any(attr(result, "run_record")$metrics$operation %in% c("parse", "research_data", "scientific_qc", "rda_load")))
})

test_that("duration labels and coverage changes preserve category ownership", {
  f <- resume_fixture(1L)
  f$config$category <- list(enabled = TRUE, overwrite = TRUE,
    dictionary = data.frame(App_UUID = "example.app", App_Name = "Example",
      Level_1_Category = "Manual", Level_2_Category = "Manual"))
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  path <- appusage_summary_proc2_paths(result$second_level)[[1]]
  old <- load_appusage_data_object(path)
  f$config$qc$min_nonempty_days <- 1
  f$config$qc$max_episode_ms <- 1
  changed <- run_appusage_workflow(project_dir = f$project, config = f$config)
  fresh <- load_appusage_data_object(path)
  expect_identical(fresh$episode$duration_ms, old$episode$duration_ms)
  expect_identical(fresh$episode$Level_1_Category, old$episode$Level_1_Category)
  json <- appusage_read_json(second_level_metadata_path(path), simplifyVector = TRUE)
  csv <- appusage_read_summary_csv(file.path(f$project, "analytic_summary_table_proclevel-2.csv"))
  expect_equal(csv$pass_qc, json$qc$pass_qc)
  expect_equal(json$module_state$qc$configuration$max_episode_ms, 1)
  expect_true(all(fresh$episode$anomaly_extreme_duration[fresh$episode$duration_ms > 1]))
  expect_equal(changed$plan$tasks$action[changed$plan$tasks$stage == "research_data"], "reuse")
})

test_that("meta threshold refresh updates existing warning and eligibility annotations", {
  f <- resume_fixture(1L)
  file.copy(test_path("fixtures", "meta_sample.txt"), f$files, overwrite = TRUE)
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  f$config$qc$max_episode_ms <- f$config$qc$max_daily_app_ms <- 1
  run_appusage_stage("qc", f$project, f$config)
  raw <- load_appusage_data_object(result$first_level$data_file[[1]])
  actual <- load_appusage_data_object(appusage_summary_proc2_paths(result$second_level)[[1]])
  expected <- make_second_level_appusage(raw, max_episode_ms = 1, max_daily_app_ms = 1)
  expect_identical(actual, expected)
  expect_equal(actual$episode$reconstruction_warning, "overlong_episode")
  expect_false(actual$daily$analysis_eligible_daily)
  json <- appusage_read_json(second_level_metadata_path(appusage_summary_proc2_paths(result$second_level)[[1]]), simplifyVector = TRUE)
  expect_equal(json$meta_reconstruction$n_reconstruction_warnings,
    attr(expected, "meta_reconstruction_diagnostics")$n_reconstruction_warnings)
  expect_equal(json$meta_daily_comparison$n_reconstruction_warnings,
    summarize_meta_daily_comparison(expected, "summary", TRUE)$n_reconstruction_warnings)
})

test_that("shared matching projection preserves diagnostic column order", {
  f <- resume_fixture(1L)
  f$config$matching <- modifyList(f$config$matching, list(enabled = TRUE,
    self_report = data.frame(seq = 1L, upload = basename(f$files)),
    sequence_col = "seq", upload_col = "upload"))
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  summary <- as.data.frame(result$second_level)
  summary$self_report_match_status <- summary$self_report_sequence_id <- NULL
  summary$diagnostic_report <- "report.md"
  summary$diagnostic_json <- "report.json"
  projected <- appusage_apply_match_projection(summary, result$matching$file_matches)
  expect_identical(tail(names(projected), 4L), c("self_report_match_status",
    "self_report_sequence_id", "diagnostic_report", "diagnostic_json"))
  expect_identical(projected$diagnostic_report, "report.md")
  expect_identical(projected$self_report_match_status, "matched")
})

test_that("merged meta duration warnings refresh without relaxing parse diagnostics", {
  for (daily_source in c("summary", "episodes", "both")) {
    f <- resume_fixture(1L)
    lines <- readLines(test_path("fixtures", "meta_sample.txt"), encoding = "UTF-8")
    writeLines(c(lines,
      ",微信,com.tencent.mm,com.tencent.mm.Main,2024-10-07 08:30:10:000,T:1728261010000,1,NULL",
      ",微信,com.tencent.mm,com.tencent.mm.Main,2024-10-07 08:40:10:000,T:1728261610000,2,NULL"), f$files)
    f$config$daily$meta_daily_source <- daily_source
    result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
    path <- appusage_summary_proc2_paths(result$second_level)[[1]]
    original <- load_appusage_data_object(path)
    expect_equal(original$episode$source_episode_count, 2)
    changed <- f$config
    changed$qc$max_episode_ms <- changed$qc$max_daily_app_ms <- 1
    run_appusage_stage("qc", f$project, changed)
    actual <- load_appusage_data_object(path)
    raw <- load_appusage_data_object(result$first_level$data_file[[1]])
    expected <- make_second_level_appusage(raw, max_episode_ms = 1,
      max_daily_app_ms = 1, meta_daily_source = daily_source)
    expect_identical(actual, expected)
    expect_equal(actual$episode$parse_warning, "overlong_episode")
    run_appusage_stage("qc", f$project, f$config)
    expect_identical(load_appusage_data_object(path), original)
    if (daily_source == "summary") {
      corrupted <- original
      corrupted$episode$parse_warning <- "synthetic_unrelated_warning"
      appusage_save_second_level_data(corrupted, path)
      json_path <- second_level_metadata_path(path)
      metadata <- appusage_read_json(json_path, simplifyVector = TRUE)
      before <- tools::md5sum(c(path, json_path))
      expect_error(appusage_sync_qc_labels(metadata, json_path, 1, 1),
        "Parse diagnostics changed")
      expect_identical(tools::md5sum(c(path, json_path)), before)
    }
  }
  expect_identical(appusage_duration_parse_warnings(c(NA, "overlong_episode",
    "raw_warning; overlong_episode", "overlong_episode; raw_warning")),
    c(NA_character_, NA_character_, "raw_warning", "raw_warning"))
  expect_false(identical(appusage_duration_parse_warnings("raw_warning"),
    appusage_duration_parse_warnings("different_warning; overlong_episode")))
})

test_that("metadata projection rebuild preserves every field of upstream failures", {
  f <- resume_fixture()
  writeLines("Unsupported synthetic export", f$files[[2]])
  withr::local_options(appusageR.diagnostics = TRUE)
  run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  path <- file.path(f$project, "analytic_summary_table_proclevel-2.csv")
  before <- read.csv(path, check.names = FALSE, na.strings = "")
  unlink(path)
  result <- run_appusage_stage("summary", f$project)
  after <- read.csv(path, check.names = FALSE, na.strings = "")
  expect_identical(after, before)
  expect_equal(after$qc_status, c("success", "not_run"))
  expect_false(any(attr(result, "run_record")$metrics$operation %in%
    c("parse", "research_data", "scientific_qc", "rda_load")))
})

test_that("projection implementation changes rerun only project projection", {
  f <- resume_fixture(1L)
  withr::local_options(appusageR.diagnostics = TRUE)
  run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  local_mocked_bindings(appusage_projection_implementation = function() "changed-projection")
  plan <- plan_appusage_workflow(project_dir = f$project, verify = "content")
  expect_true(all(plan$tasks$action %in% c("reuse", "disabled")))
  expect_identical(plan$project_tasks$reason_code[plan$project_tasks$stage == "summary"], "implementation_changed")
  result <- run_appusage_workflow(project_dir = f$project)
  expect_false(any(result$run_record$metrics$operation %in%
    c("parse", "research_data", "reconstruction", "scientific_qc", "rda_load")))
  expect_identical(appusage_projection_state(f$project)$summary$implementation, "changed-projection")
})
