workflow_fixture <- function(n = 2L) {
  root <- tempfile("workflow-plan-")
  dir.create(root)
  files <- file.path(root, sprintf("%d_AppUsage_line_2024_1_2_3_4_5.txt", seq_len(n)))
  for (f in files) file.copy(test_path("fixtures", "line_sample.txt"), f)
  list(files = files, project = file.path(root, "project"),
    config = appusage_config(execution = list(progress = FALSE)))
}

test_that("generic manifest plans are read-only and no-change resume invokes no science", {
  f <- workflow_fixture()
  plan <- plan_appusage_workflow(f$files, f$project, f$config)
  expect_false(dir.exists(f$project))
  expect_true(all(plan$tasks$action[plan$tasks$stage == "parse"] == "run"))
  result <- run_appusage_workflow(plan = plan)
  expect_equal(nrow(result$first_level), 2L)
  paths <- appusage_summary_proc2_paths(result$second_level)
  before <- tools::md5sum(paths)
  plan <- plan_appusage_workflow(project_dir = f$project, verify = "content")
  expect_true(all(plan$tasks$action %in% c("reuse", "disabled")))
  never_science <- function(...) stop("Unexpected science")
  formals(never_science) <- formals(make_second_level_appusage)
  local_mocked_bindings(make_second_level_appusage = never_science,
    preprocess_one_appusage = function(...) stop("Unexpected parse"),
    appusage_research_implementation_fingerprint = function() result$implementation_provenance$research_implementation_fingerprint)
  resumed <- run_appusage_workflow(project_dir = f$project)
  expect_true(attr(resumed$first_level, "stage_reused"))
  expect_identical(tools::md5sum(paths), before)
})

test_that("QC-only configuration changes preserve research RDA and raw caches", {
  f <- workflow_fixture(1L)
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  paths <- c(result$first_level$data_file, appusage_summary_proc2_paths(result$second_level))
  before <- tools::md5sum(paths)
  f$config$qc$min_nonempty_days <- 2
  plan <- plan_appusage_workflow(f$files, f$project, f$config)
  expect_equal(plan$tasks$action[plan$tasks$stage == "qc"], "run")
  expect_equal(plan$tasks$action[plan$tasks$stage == "research_data"], "reuse")
  run_appusage_workflow(plan = plan)
  expect_identical(tools::md5sum(paths), before)
  expect_true(all(plan_appusage_workflow(project_dir = f$project)$tasks$action %in% c("reuse", "disabled")))
})

test_that("corrupt RDA is repaired only for its source", {
  f <- workflow_fixture()
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  paths <- appusage_summary_proc2_paths(result$second_level)
  good <- tools::md5sum(paths[[2]])
  writeBin(charToRaw("corrupt"), paths[[1]])
  plan <- plan_appusage_workflow(project_dir = f$project, verify = "content")
  expect_equal(plan$tasks$action[plan$tasks$stage == "research_data"], c("run", "reuse"))
  run_appusage_workflow(plan = plan)
  expect_identical(tools::md5sum(paths[[2]]), good)
  expect_named(load_appusage_data_object(paths[[1]]), c("event", "episode", "daily"))
})

test_that("cache-only downstream execution never requires raw inputs", {
  f <- workflow_fixture(1L)
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  unlink(f$files)
  f$config$execution$source_verification <- "cache_only"
  f$config$qc$min_nonempty_days <- 3
  resumed <- run_appusage_workflow(project_dir = f$project, config = f$config)
  expect_equal(nrow(resumed$qc), 1L)
  expect_true(attr(resumed$first_level, "stage_reused"))
})

test_that("matching module retains zero and unmatched questionnaire rows", {
  manifest <- data.frame(source_file = "1_AppUsage_line_2024_1_2_3_4_5.txt")
  survey <- data.frame(seq = c(1L, 2L), upload = c("missing.txt", "other.txt"))
  result <- match_appusage_self_report(survey, manifest, tempdir(), "seq", "upload")
  expect_equal(nrow(result$matched_self_report), 2L)
  empty <- match_appusage_self_report(survey[0, ], manifest, tempdir(), "seq", "upload")
  expect_equal(nrow(empty$matched_self_report), 0L)
})

test_that("standalone threshold QC synchronizes RDA labels and retains numeric data", {
  f <- workflow_fixture(1L)
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  path <- appusage_summary_proc2_paths(result$second_level)[[1]]
  old <- load_appusage_data_object(path)
  f$config$qc$max_episode_ms <- 1
  f$config$qc$max_daily_app_ms <- 1
  run_appusage_stage("qc", f$project, config = f$config)
  fresh <- load_appusage_data_object(path)
  expect_identical(fresh$episode$duration_ms, old$episode$duration_ms)
  expect_identical(fresh$daily$duration_ms, old$daily$duration_ms)
  expect_true(all(fresh$episode$anomaly_extreme_duration[fresh$episode$duration_ms > 1]))
  expect_true(all(fresh$daily$anomaly_extreme_duration[fresh$daily$duration_ms > 1]))
  metadata <- jsonlite::read_json(second_level_metadata_path(path), simplifyVector = TRUE)
  expect_equal(metadata$module_state$qc$configuration$max_episode_ms, 1)
})

test_that("subset workflow and summary rebuild retain unselected and failed sources", {
  f <- workflow_fixture(2L)
  writeLines("unsupported export", f$files[[2]])
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  expect_equal(nrow(result$second_level), 2L)
  subset <- run_appusage_workflow(f$files[[1]], project_dir = f$project, config = f$config)
  expect_true(all(subset$overview$grains$denominator == 2L))
  expect_true(all(subset$overview$grains$not_generated >= 0))
  unlink(file.path(f$project, "analytic_summary_table_proclevel-2.csv"))
  summary <- run_appusage_stage("summary", f$project)
  expect_equal(nrow(summary), 2L)
  expect_equal(sum(summary$first_level_status == "error"), 1L)
})

test_that("configuration rejects mixed entry settings and research changes invalidate only downstream", {
  f <- workflow_fixture(1L)
  expect_error(run_appusage_workflow(f$files, project_dir = f$project,
    config = f$config, tz = "UTC"), "do not mix")
  run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  f$config$reconstruction$meta_episode_merge_gap_ms <- 100
  plan <- plan_appusage_workflow(f$files, f$project, f$config)
  expect_equal(plan$tasks$action, c("reuse", "run", "run", "disabled"))
  f$config$reconstruction$meta_episode_merge_gap_ms <- 30000
  f$config$execution$workers <- 2L
  plan <- plan_appusage_workflow(f$files, f$project, f$config)
  expect_true(all(plan$tasks$action %in% c("reuse", "disabled")))
})

test_that("generic workflow supports the existing two-worker route", {
  skip_on_cran()
  f <- workflow_fixture(2L)
  f$config$execution$parallel <- TRUE
  f$config$execution$workers <- 2L
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  expect_true(all(result$first_level$status == "success"))
  expect_true(all(result$second_level$second_level_status == "success"))
  versions <- vapply(result$first_level$metadata_file, function(path)
    jsonlite::read_json(path, simplifyVector = TRUE)$package_version, character(1))
  expect_true(all(versions == as.character(utils::packageVersion("appusageR"))))
  expect_true(all(result$first_level$worker_pid != Sys.getpid()))
  serial <- make_second_level_appusage(load_appusage_data_object(result$first_level$data_file[[1]]))
  actual <- load_appusage_data_object(appusage_summary_proc2_paths(result$second_level)[[1]])
  expect_identical(actual, serial)
})

test_that("category and matching owners survive standalone QC and projection", {
  f <- workflow_fixture(1L)
  f$config$category <- list(enabled = TRUE, overwrite = TRUE,
    dictionary = data.frame(App_UUID = "example.app", App_Name = "Example",
      Level_1_Category = "Manual", Level_2_Category = "Manual"))
  f$config$matching <- modifyList(f$config$matching, list(enabled = TRUE,
    self_report = data.frame(seq = c(1L, 999L), upload = c(basename(f$files), "absent.txt")),
    sequence_col = "seq", upload_col = "upload"))
  result <- run_appusage_workflow(f$files, project_dir = f$project, config = f$config)
  expect_equal(nrow(result$matching$matched_self_report), 2L)
  expect_equal(sum(result$matching$matched_self_report$moSens_match_status == "matched"), 1L)
  path <- appusage_summary_proc2_paths(result$second_level)[[1]]
  before <- tools::md5sum(path)
  plan <- plan_appusage_workflow(project_dir = f$project, verify = "content")
  expect_true(all(plan$tasks$action == "reuse"))
  run_appusage_workflow(plan = plan)
  expect_identical(tools::md5sum(path), before)
  f$config$qc$min_nonempty_days <- 3
  run_appusage_stage("qc", f$project, f$config)
  summary <- run_appusage_stage("summary", f$project)
  expect_equal(summary$self_report_match_status, "matched")
  expect_identical(tools::md5sum(path), before)
  expect_equal(nrow(readRDS(file.path(f$project, "self_report_link_result.rds"))$matched_self_report), 2L)
})

test_that("stage scope does not overwrite desired downstream switches and force plans are honest", {
  f <- workflow_fixture(1L)
  run_appusage_stage("parse", f$project, f$config, x = f$files)
  expect_true(readRDS(file.path(f$project, "appusage_configuration.rds"))$qc$enabled)
  result <- run_appusage_workflow(project_dir = f$project)
  expect_equal(result$qc$qc_status, "success")
  f$config$execution$overwrite <- TRUE
  plan <- plan_appusage_workflow(project_dir = f$project, config = f$config)
  expect_equal(plan$tasks$action, c("run", "run", "run", "disabled"))
})
