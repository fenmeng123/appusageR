project_workflow_fixture <- function(include_good = FALSE) {
  root <- file.path(tempdir(), paste0("appusage_project_root_", sample.int(1e8, 1)))
  project <- file.path(root, "ProjectName-StudyA_ProjectID-123")
  dir.create(project, recursive = TRUE)
  excel <- file.path(root, "WJXraw_ProjectID-123.xlsx")
  file.create(excel)
  writeLines("not an app usage export", file.path(
    project,
    "seq101_div style=tex_AppUsage_day_2024_1_2_3_4_5.txt"
  ))
  file.create(file.path(
    project,
    "seq102_div style=tex_AppUsage_meta_2024_1_2_3_4_5.txt"
  ))
  writeLines("notes", file.path(project, "notes.csv"))
  if (isTRUE(include_good)) {
    file.copy(
      testthat::test_path("fixtures", "line_sample.txt"),
      file.path(project, "seq103_div style=tex_AppUsage_line_2024_1_2_3_4_5.txt")
    )
  }
  list(root = root, project = project, excel = excel)
}

project_workflow_root_fixture <- function() {
  testthat::skip_if_not_installed("openxlsx")
  root <- file.path(tempdir(), paste0("appusage_raw_root_", sample.int(1e8, 1)))
  project <- file.path(root, "ProjectName-StudyA_ProjectID-123")
  dir.create(project, recursive = TRUE)
  source_file <- file.path(project, "1001_AppUsage_line_2024_1_2_3_4_5.txt")
  file.copy(testthat::test_path("fixtures", "line_sample.txt"), source_file)
  excel <- file.path(root, "ProjectID-123-WJXraw-demo.xlsx")
  self_report <- data.frame(
    "序号" = 1001L,
    upload = "uploaded: https://example.test/files/1001_AppUsage_line_2024_1_2_3_4_5.txt",
    "提交答卷时间" = "2024-01-02 03:05:30",
    check.names = FALSE
  )
  openxlsx::write.xlsx(self_report, excel, overwrite = TRUE)
  file.create(file.path(root, "~$ProjectID-123-WJXraw-demo.xlsx"))
  list(root = root, project = project, source_file = source_file, excel = excel)
}

project_workflow_match_fixture <- function() {
  project_root <- file.path(tempdir(), paste0("appusage_match_root_", sample.int(1e8, 1)))
  proc2 <- file.path(project_root, "proclevel-2")
  dir.create(proc2, recursive = TRUE)
  rda_line <- file.path(proc2, "sub-1001_type-line_proc-2.rda")
  rda_meta <- file.path(proc2, "sub-1001_type-meta_proc-2.rda")
  data <- list(event = data.frame(), episode = data.frame(), daily = data.frame())
  save(data, file = rda_line)
  save(data, file = rda_meta)
  line_source <- file.path(project_root, "1001_AppUsage_line_2024_1_2_3_4_5.txt")
  meta_source <- file.path(project_root, "1001_AppUsage_meta_2024_1_2_3_4_5.txt")
  manifest <- tibble::tibble(
    project_name = "StudyA",
    study_id = "StudyA",
    project_id = "123",
    project_label = "ProjectName-StudyA",
    project_dir = project_root,
    self_report_file = NA_character_,
    index = 1:2,
    source_file = c(line_source, meta_source),
    source_basename = basename(c(line_source, meta_source)),
    extension = "txt",
    is_txt = TRUE,
    is_zero_byte = FALSE,
    is_zero_byte_txt = FALSE,
    file_size = 1,
    modified_at = NA_character_,
    wenjuanxing_sequence_id = c(1001L, 1001L),
    candidate_participant_id = "1001",
    filename_parse_status = "success",
    filename_parse_warning = NA_character_,
    native_export_file_name = c(
      "AppUsage_line_2024_1_2_3_4_5.txt",
      "AppUsage_meta_2024_1_2_3_4_5.txt"
    ),
    uploaded_file_name = c(
      "AppUsage_line_2024_1_2_3_4_5.txt",
      "AppUsage_meta_2024_1_2_3_4_5.txt"
    ),
    filename_export_type = c("line", "meta"),
    native_export_created_at = c(
      "2024-01-02T03:04:05+0800",
      "2024-01-02T03:04:05+0800"
    ),
    project_n_files = 2L,
    project_n_txt_files = 2L,
    project_n_zero_byte_txt = 0L,
    project_n_non_txt_files = 0L
  )
  first <- tibble::tibble(
    index = 1:2,
    source_file = c(line_source, meta_source),
    data_file = c("first-line.rda", "first-meta.rda"),
    participant_id = c("1001", "1001"),
    detected_type = c("line", "meta")
  )
  second <- tibble::tibble(
    first_level_data_file = c("first-line.rda", "first-meta.rda"),
    second_level_rda = c(rda_line, rda_meta),
    participant_id = c("1001", "1001"),
    detected_type = c("line", "meta")
  )
  list(project_root = project_root, manifest = manifest, first = first, second = second)
}

test_that("scan_appusage_project_root detects projects and paired Excel files", {
  fixture <- project_workflow_fixture()

  scan <- scan_appusage_project_root(fixture$root)

  expect_equal(nrow(scan), 1L)
  expect_equal(scan$project_name[[1]], "StudyA")
  expect_equal(scan$study_id[[1]], "StudyA")
  expect_equal(scan$project_id[[1]], "123")
  expect_equal(scan$excel_count[[1]], 1L)
  expect_equal(normalizePath(scan$excel_path[[1]], winslash = "/"), normalizePath(fixture$excel, winslash = "/"))
  expect_equal(scan$n_txt_files[[1]], 2L)
  expect_equal(scan$n_zero_byte_txt[[1]], 1L)
  expect_equal(scan$n_non_txt_files[[1]], 1L)
})

test_that("project workflow resume rebuilds missing first-level summary from proc-1 cache", {
  raw_root <- file.path(tempdir(), paste0("appusage_project_resume_raw_", sample.int(1e8, 1)))
  project_dir <- file.path(raw_root, "ProjectName-StudyA_ProjectID-123")
  dir.create(project_dir, recursive = TRUE)
  file.copy(
    testthat::test_path("fixtures", "line_sample.txt"),
    file.path(project_dir, "1001_AppUsage_line_2024_1_2_3_4_5.txt")
  )
  output_root <- file.path(tempdir(), paste0("appusage_project_resume_", sample.int(1e8, 1)))

  first_run <- run_appusage_project_workflow(
    project_dir = project_dir,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    run_second_level = FALSE,
    run_qc = FALSE,
    overwrite = TRUE,
    progress = FALSE
  )
  project_root <- first_run$project_dir
  first_summary <- file.path(project_root, "analytic_summary_table_proclevel-1.csv")
  checkpoint <- file.path(project_root, "analytic_summary_table_proclevel-1.checkpoint.csv")
  expect_true(file.exists(first_summary))
  unlink(first_summary)
  unlink(checkpoint)

  resumed <- run_appusage_project_workflow(
    project_dir = project_dir,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    run_second_level = FALSE,
    run_qc = FALSE,
    resume = TRUE,
    overwrite = FALSE,
    progress = FALSE
  )

  expect_true(file.exists(first_summary))
  expect_true(isTRUE(resumed$resumed))
  expect_equal(resumed$first_level$status[[1]], "success")
  expect_true(any(resumed$first_level$summary_source == "reconstructed_proc1_cache"))
})

test_that("project workflow continues first-level when rebuilt summary has not-processed rows", {
  raw_root <- file.path(tempdir(), paste0("appusage_project_incomplete_raw_", sample.int(1e8, 1)))
  project_dir <- file.path(raw_root, "ProjectName-StudyA_ProjectID-123")
  dir.create(project_dir, recursive = TRUE)
  source_line <- file.path(project_dir, "1001_AppUsage_line_2024_1_2_3_4_5.txt")
  source_day <- file.path(project_dir, "1002_AppUsage_day_2024_1_2_3_4_5.txt")
  file.copy(testthat::test_path("fixtures", "line_sample.txt"), source_line)
  file.copy(testthat::test_path("fixtures", "day_sample.txt"), source_day)
  output_root <- file.path(tempdir(), paste0("appusage_project_incomplete_", sample.int(1e8, 1)))

  first_run <- run_appusage_project_workflow(
    project_dir = project_dir,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    max_files = 1,
    run_second_level = FALSE,
    run_qc = FALSE,
    overwrite = TRUE,
    progress = FALSE
  )
  project_root <- first_run$project_dir
  unlink(file.path(project_root, "workflow_configuration.rds"))
  unlink(file.path(project_root, "analytic_summary_table_proclevel-1.csv"))
  unlink(file.path(project_root, "analytic_summary_table_proclevel-1.checkpoint.csv"))
  rebuilt <- rebuild_first_level_summary_from_cache(
    project_root,
    manifest = build_appusage_project_manifest(project_dir),
    write = TRUE
  )
  expect_true(any(rebuilt$status == "not_processed"))

  resumed <- run_appusage_project_workflow(
    project_dir = project_dir,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    run_second_level = FALSE,
    run_qc = FALSE,
    resume = TRUE,
    overwrite = FALSE,
    progress = FALSE
  )

  expect_false(any(resumed$first_level$status == "not_processed"))
  expect_equal(sum(resumed$first_level$status == "success"), 2L)
})

test_that("project workflow can skip complete first-level summary and run second-level", {
  raw_root <- file.path(tempdir(), paste0("appusage_project_complete_raw_", sample.int(1e8, 1)))
  project_dir <- file.path(raw_root, "ProjectName-StudyA_ProjectID-123")
  dir.create(project_dir, recursive = TRUE)
  file.copy(
    testthat::test_path("fixtures", "line_sample.txt"),
    file.path(project_dir, "1001_AppUsage_line_2024_1_2_3_4_5.txt")
  )
  output_root <- file.path(tempdir(), paste0("appusage_project_complete_", sample.int(1e8, 1)))

  first_run <- run_appusage_project_workflow(
    project_dir = project_dir,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    run_second_level = FALSE,
    run_qc = FALSE,
    overwrite = TRUE,
    progress = FALSE
  )
  unlink(file.path(first_run$project_dir, "workflow_configuration.rds"))

  resumed <- run_appusage_project_workflow(
    project_dir = project_dir,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    run_second_level = TRUE,
    run_qc = FALSE,
    resume = TRUE,
    overwrite = FALSE,
    progress = FALSE
  )

  expect_true(isTRUE(resumed$resumed))
  expect_equal(resumed$first_level$status[[1]], "success")
  expect_equal(resumed$second_level$status[[1]], "success")
})

test_that("project workflow resumes from an early config without completed stage", {
  raw_root <- file.path(tempdir(), paste0("appusage_project_early_config_raw_", sample.int(1e8, 1)))
  project_dir <- file.path(raw_root, "ProjectName-StudyA_ProjectID-123")
  dir.create(project_dir, recursive = TRUE)
  file.copy(
    testthat::test_path("fixtures", "line_sample.txt"),
    file.path(project_dir, "1001_AppUsage_line_2024_1_2_3_4_5.txt")
  )
  output_root <- file.path(tempdir(), paste0("appusage_project_early_config_", sample.int(1e8, 1)))
  first_run <- run_appusage_project_workflow(
    project_dir = project_dir,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    run_second_level = FALSE,
    run_qc = FALSE,
    overwrite = TRUE,
    progress = FALSE
  )
  config <- readRDS(first_run$configuration_file)
  config$workflow_state$run_status <- "initialized"
  config$workflow_state$current_stage <- "initialized"
  config$workflow_state$last_completed_stage <- NA_character_
  config$workflow_state$stage_completed_at <- list()
  config$workflow_state$stage_status <- list()
  appusage_write_workflow_configuration(config)

  resumed <- run_appusage_project_workflow(
    project_dir = project_dir,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    run_second_level = FALSE,
    run_qc = FALSE,
    resume = TRUE,
    overwrite = FALSE,
    progress = FALSE
  )

  expect_true(isTRUE(resumed$resumed))
  expect_equal(resumed$first_level$status[[1]], "success")
  resumed_config <- readRDS(resumed$configuration_file)
  expect_equal(resumed_config$workflow_state$run_status, "completed")
})

test_that("project workflow records first-level worker controls and decision", {
  raw_root <- file.path(tempdir(), paste0("appusage_project_worker_raw_", sample.int(1e8, 1)))
  project_dir <- file.path(raw_root, "ProjectName-StudyA_ProjectID-123")
  dir.create(project_dir, recursive = TRUE)
  file.copy(
    testthat::test_path("fixtures", "line_sample.txt"),
    file.path(project_dir, "1001_AppUsage_line_2024_1_2_3_4_5.txt")
  )
  output_root <- file.path(tempdir(), paste0("appusage_project_worker_", sample.int(1e8, 1)))

  result <- run_appusage_project_workflow(
    project_dir = project_dir,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    dry_run = TRUE,
    progress = FALSE,
    parallel = TRUE,
    n_cores = 8,
    first_level_max_workers = 7,
    first_level_worker_cap_override = TRUE,
    first_level_checkpoint_every = 11,
    retry_memory_allocation = TRUE,
    memory_retry_workers = 1
  )
  config <- readRDS(result$configuration_file)

  expect_equal(config$first_level_options$first_level_max_workers, 7)
  expect_true(config$first_level_options$first_level_worker_cap_override)
  expect_equal(config$first_level_options$first_level_checkpoint_every, 11)
  expect_true(config$first_level_options$retry_memory_allocation)
  expect_equal(config$first_level_options$memory_retry_workers, 1)
  expect_equal(config$first_level_options$worker_decision$requested_workers, 8L)
  expect_equal(config$first_level_options$worker_decision$selected_workers, 1L)
  expect_true(config$first_level_options$worker_decision$worker_cap_override)
  expect_equal(result$first_level_worker_decision$cap_reason, "explicit_worker_cap_override")
})

test_that("workflow config exists before first-level work and records failures", {
  raw_root <- file.path(tempdir(), paste0("appusage_config_early_raw_", sample.int(1e8, 1)))
  project_dir <- file.path(raw_root, "ProjectName-StudyA_ProjectID-123")
  dir.create(project_dir, recursive = TRUE)
  file.copy(
    testthat::test_path("fixtures", "line_sample.txt"),
    file.path(project_dir, "1001_AppUsage_line_2024_1_2_3_4_5.txt")
  )
  output_root <- file.path(tempdir(), paste0("appusage_config_early_", sample.int(1e8, 1)))
  project_root <- file.path(output_root, "ProjectName-StudyA_ProjectID-123")
  config_file <- file.path(project_root, "workflow_configuration.rds")
  observed_before_work <- FALSE
  original <- simpleError("synthetic first-level failure")
  testthat::local_mocked_bindings(
    read_appusage_batch = function(...) {
      observed_before_work <<- file.exists(config_file) &&
        identical(readRDS(config_file)$workflow_state$current_stage, "first_level")
      stop(original)
    },
    .package = "appusageR"
  )

  observed <- tryCatch(
    run_appusage_project_workflow(
      project_dir = project_dir,
      project_id = "123",
      project_name = "StudyA",
      output_root = output_root,
      run_second_level = FALSE,
      run_qc = FALSE,
      overwrite = TRUE,
      progress = FALSE
    ),
    error = identity
  )

  expect_identical(observed, original)
  expect_true(observed_before_work)
  config <- readRDS(config_file)
  expect_equal(config$workflow_state$run_status, "failed")
  expect_equal(config$workflow_state$current_stage, "first_level")
  expect_equal(config$workflow_state$stage_status$first_level, "failed")
  expect_equal(config$workflow_state$last_error$condition_message, conditionMessage(original))
})

test_that("second-level failures leave first-level completed and preserve condition", {
  raw_root <- file.path(tempdir(), paste0("appusage_config_second_raw_", sample.int(1e8, 1)))
  project_dir <- file.path(raw_root, "ProjectName-StudyA_ProjectID-123")
  dir.create(project_dir, recursive = TRUE)
  file.copy(
    testthat::test_path("fixtures", "line_sample.txt"),
    file.path(project_dir, "1001_AppUsage_line_2024_1_2_3_4_5.txt")
  )
  output_root <- file.path(tempdir(), paste0("appusage_config_second_", sample.int(1e8, 1)))
  original <- simpleError("synthetic second-level failure")
  testthat::local_mocked_bindings(
    write_second_level_batch = function(...) stop(original),
    .package = "appusageR"
  )

  observed <- tryCatch(
    run_appusage_project_workflow(
      project_dir = project_dir,
      project_id = "123",
      project_name = "StudyA",
      output_root = output_root,
      run_second_level = TRUE,
      run_qc = FALSE,
      overwrite = TRUE,
      progress = FALSE
    ),
    error = identity
  )

  expect_identical(observed, original)
  config_file <- file.path(
    output_root,
    "ProjectName-StudyA_ProjectID-123",
    "workflow_configuration.rds"
  )
  config <- readRDS(config_file)
  expect_equal(config$workflow_state$run_status, "failed")
  expect_equal(config$workflow_state$current_stage, "second_level")
  expect_equal(config$workflow_state$last_completed_stage, "first_level")
})

test_that("successful workflow state records workers stages and checkpoints", {
  fixture <- project_workflow_fixture(include_good = TRUE)
  output_root <- file.path(tempdir(), paste0("appusage_config_success_", sample.int(1e8, 1)))

  result <- run_appusage_project_workflow(
    project_dir = fixture$project,
    project_name = "StudyA",
    project_id = "123",
    output_root = output_root,
    overwrite = TRUE,
    progress = FALSE,
    parallel = TRUE,
    n_cores = 2,
    diagnostic_verbosity = "none"
  )
  config <- readRDS(result$configuration_file)
  state <- config$workflow_state

  expect_equal(state$run_status, "completed")
  expect_equal(state$current_stage, "completed")
  expect_equal(state$last_completed_stage, "completed")
  expect_equal(state$requested_shared_n_cores, 2L)
  expect_true(!is.null(state$first_level_worker_decision$selected_workers))
  expect_equal(state$second_level_worker_decision$requested_workers, 2L)
  expect_true(all(c(
    "first_level", "second_level", "qc",
    "self_report_matching", "completed"
  ) %in% names(state$stage_completed_at)))
  expect_true(file.exists(state$checkpoints$first_level$path))
  expect_gt(state$checkpoints$first_level$row_count, 0L)
  expect_true(file.exists(state$checkpoints$second_level$path))
  expect_gt(state$checkpoints$second_level$row_count, 0L)
})

test_that("atomic workflow config promotion failure preserves prior config", {
  project_root <- file.path(
    tempdir(),
    paste0("appusage_config_atomic_", sample.int(1e8, 1))
  )
  config <- list(
    output_study_dir = project_root,
    marker = "original",
    workflow_state = list(checkpoints = list())
  )
  config_file <- appusage_write_workflow_configuration(config)
  promote <- appusage_promote_workflow_file
  failed_once <- FALSE
  testthat::local_mocked_bindings(
    appusage_promote_workflow_file = function(from, to) {
      if (!failed_once && appusage_normalized_paths_equal(to, config_file)) {
        failed_once <<- TRUE
        return(FALSE)
      }
      promote(from, to)
    },
    .package = "appusageR"
  )
  changed <- config
  changed$marker <- "changed"

  expect_error(
    appusage_write_workflow_configuration(changed),
    "Could not promote"
  )

  expect_equal(readRDS(config_file)$marker, "original")
  leftovers <- list.files(
    project_root,
    pattern = "[.]appusage-(tmp|backup)-",
    full.names = TRUE,
    all.files = TRUE
  )
  expect_length(leftovers, 0L)
})

test_that("checkpoint config refresh is durable and failure-safe", {
  project_root <- file.path(
    tempdir(),
    paste0("appusage_config_checkpoint_", sample.int(1e8, 1))
  )
  config <- list(
    output_study_dir = project_root,
    workflow_state = list(checkpoints = list())
  )
  appusage_write_workflow_configuration(config)
  checkpoint <- file.path(project_root, "analytic_summary_table_proclevel-1.checkpoint.csv")
  utils::write.csv(data.frame(index = 1:3), checkpoint, row.names = FALSE)

  expect_true(appusage_refresh_workflow_checkpoint_safely(
    project_root,
    "first_level",
    checkpoint,
    3L
  ))
  refreshed <- readRDS(file.path(project_root, "workflow_configuration.rds"))
  expect_equal(refreshed$workflow_state$checkpoints$first_level$row_count, 3L)
  expect_equal(
    refreshed$workflow_state$checkpoints$first_level$path,
    normalizePath(checkpoint, winslash = "/")
  )
  testthat::local_mocked_bindings(
    appusage_write_workflow_configuration = function(config) {
      stop("synthetic config refresh failure")
    },
    .package = "appusageR"
  )

  expect_false(appusage_refresh_workflow_checkpoint_safely(
    project_root,
    "first_level",
    checkpoint,
    4L
  ))
  unchanged <- readRDS(file.path(project_root, "workflow_configuration.rds"))
  expect_equal(
    unchanged$workflow_state$checkpoints$first_level$row_count,
    3L
  )
  diagnostic_files <- list.files(
    file.path(project_root, "diagnostics"),
    pattern = "^workflow_configuration_.*[.]json$",
    full.names = TRUE
  )
  expect_length(diagnostic_files, 1L)
})

test_that("scan_appusage_project_root does not count project subdirectories as files", {
  fixture <- project_workflow_fixture()
  dir.create(file.path(fixture$project, "nested"))

  scan <- scan_appusage_project_root(fixture$root)

  expect_equal(scan$n_files[[1]], 3L)
  expect_equal(scan$n_txt_files[[1]], 2L)
  expect_equal(scan$n_non_txt_files[[1]], 1L)
})

test_that("root ProjectID resolver ignores unrelated project contents and Excel lock files", {
  root <- file.path(tempdir(), paste0("appusage_raw_root_", sample.int(1e8, 1)))
  selected <- file.path(root, "ProjectName-StudyA_ProjectID-123")
  unrelated <- file.path(root, "ProjectName-Other_ProjectID-999")
  dir.create(selected, recursive = TRUE)
  dir.create(unrelated, recursive = TRUE)
  dir.create(file.path(unrelated, "sentinel_subfolder"))
  invisible(vapply(seq_len(100), function(i) {
    file.create(file.path(unrelated, sprintf("unrelated_%03d.txt", i)))
  }, logical(1)))
  real_excel <- file.path(root, "ProjectID-123-WJXraw-real.xlsx")
  lock_excel <- file.path(root, "~$ProjectID-123-WJXraw-real.xlsx")
  file.create(real_excel)
  file.create(lock_excel)

  resolved <- appusage_resolve_project_by_id(root, "123")

  expect_equal(normalizePath(resolved$project_dir, winslash = "/"), normalizePath(selected, winslash = "/"))
  expect_equal(normalizePath(resolved$excel_path, winslash = "/"), normalizePath(real_excel, winslash = "/"))
  expect_false(grepl("^~\\$", basename(resolved$excel_path)))
})

test_that("root ProjectID resolver prefers sequence-order WJXraw Excel files", {
  root <- file.path(tempdir(), paste0("appusage_raw_root_", sample.int(1e8, 1)))
  selected <- file.path(root, "ProjectName-StudyA_ProjectID-123")
  dir.create(selected, recursive = TRUE)
  sequence_excel <- file.path(root, "ProjectID-123-WJXraw-按序号-demo.xlsx")
  text_excel <- file.path(root, "ProjectID-123-WJXraw-按文本SMAIPAQ-demo.xlsx")
  unrelated_excel <- file.path(root, "ProjectID-123-other.xlsx")
  file.create(sequence_excel)
  file.create(text_excel)
  file.create(unrelated_excel)

  resolved <- appusage_resolve_project_by_id(root, "123")

  expect_equal(
    normalizePath(resolved$excel_path, winslash = "/"),
    normalizePath(sequence_excel, winslash = "/")
  )
})

test_that("build_appusage_project_manifest records file and filename metadata without parsing text", {
  fixture <- project_workflow_fixture()

  manifest <- build_appusage_project_manifest(fixture$project,
    self_report_file = fixture$excel
  )

  expect_equal(nrow(manifest), 3L)
  expect_equal(unique(manifest$project_n_txt_files), 2L)
  expect_equal(unique(manifest$project_n_zero_byte_txt), 1L)
  expect_equal(unique(manifest$project_n_non_txt_files), 1L)
  day_row <- manifest[manifest$wenjuanxing_sequence_id %in% 101L, , drop = FALSE]
  expect_equal(day_row$extension[[1]], "txt")
  expect_false(day_row$is_zero_byte_txt[[1]])
  expect_equal(day_row$filename_export_type[[1]], "day")
  zero_row <- manifest[manifest$wenjuanxing_sequence_id %in% 102L, , drop = FALSE]
  expect_true(zero_row$is_zero_byte_txt[[1]])
  expect_equal(zero_row$file_size[[1]], 0)
  expect_true(any(manifest$extension == "csv"))
})

test_that("run_appusage_project_workflow dry run writes manifest only", {
  fixture <- project_workflow_fixture()
  output_root <- file.path(tempdir(), paste0("appusage_project_output_", sample.int(1e8, 1)))

  result <- run_appusage_project_workflow(
    fixture$project,
    output_root = output_root,
    project_name = "StudyB",
    project_id = "999",
    dry_run = TRUE,
    progress = FALSE,
    diagnostic_verbosity = "none"
  )

  expect_true(result$dry_run)
  expect_equal(basename(result$project_dir), "ProjectName-StudyB_ProjectID-999")
  expect_true(file.exists(result$manifest_file))
  expect_false(dir.exists(file.path(result$project_dir, "proclevel-1")))
  expect_false(dir.exists(file.path(result$project_dir, "proclevel-2")))
  expect_false(dir.exists(file.path(result$project_dir, "proclevel-3")))
  expect_false(dir.exists(file.path(result$project_dir, "proclevel-tmp")))
  config <- readRDS(result$configuration_file)
  expect_equal(config$workflow_state$run_status, "dry_run")
  expect_equal(config$workflow_state$current_stage, "dry_run")
  expect_false(identical(config$workflow_state$last_completed_stage, "completed"))
})

test_that("project workflow non-strict mode continues after bad files and writes diagnostics", {
  fixture <- project_workflow_fixture(include_good = TRUE)
  output_root <- file.path(tempdir(), paste0("appusage_project_output_", sample.int(1e8, 1)))

  expect_message(
    result <- run_appusage_project_workflow(
      fixture$project,
      output_root = output_root,
      strict = FALSE,
      overwrite = TRUE,
      progress = FALSE,
      diagnostic_verbosity = "summary"
    ),
    "Source data context"
  )

  expect_true(any(result$first_level$status == "success"))
  expect_true(any(result$first_level$status == "error"))
  expect_false(is.null(result$second_level))
  upstream_skipped <- result$second_level$skip_reason %in% "upstream_first_level_error"
  expect_true(any(upstream_skipped))
  expect_true(all(is.na(result$second_level$error_message[upstream_skipped])))
  expect_true(all(is.na(result$second_level$diagnostic_report[upstream_skipped])))
  expect_true(any(result$qc$skip_reason %in% "upstream_first_level_error"))
  expect_true(all(is.na(result$qc$error_message[result$qc$skip_reason %in% "upstream_first_level_error"])))
  report <- stats::na.omit(result$first_level$diagnostic_report)
  expect_gt(length(report), 0)
  expect_true(file.exists(report[[1]]))
  issue <- paste(readLines(report[[1]], warn = FALSE), collapse = "\n")
  expect_match(issue, "## Source data context")
  expect_match(issue, "## Traceback")
  expect_lt(regexpr("## Source data context", issue)[[1]], regexpr("## Traceback", issue)[[1]])
  expect_false(dir.exists(file.path(result$project_dir, "proclevel-3")))
  expect_false(dir.exists(file.path(result$project_dir, "proclevel-tmp")))
})

test_that("project workflow strict mode stops after writing diagnostics", {
  fixture <- project_workflow_fixture(include_good = TRUE)
  output_root <- file.path(tempdir(), paste0("appusage_project_output_", sample.int(1e8, 1)))
  expected_project <- file.path(output_root, "ProjectName-StudyA_ProjectID-123")

  expect_error(
    run_appusage_project_workflow(
      fixture$project,
      output_root = output_root,
      strict = TRUE,
      overwrite = TRUE,
      progress = FALSE,
      diagnostic_verbosity = "none"
    ),
    "Project workflow failed during first_level"
  )
  reports <- list.files(file.path(expected_project, "diagnostics", "error_reports"),
    pattern = "[.]md$",
    full.names = TRUE
  )
  expect_gt(length(reports), 0)
  expect_false(dir.exists(file.path(expected_project, "proclevel-3")))
  expect_false(dir.exists(file.path(expected_project, "proclevel-tmp")))
})

test_that("diagnose and format appusage errors put source context first", {
  source_file <- tempfile(fileext = ".txt")
  writeLines("bad", source_file)

  context <- diagnose_appusage_error(
    simpleError("synthetic failure"),
    source_file = source_file,
    stage = "first_level",
    context = list(
      project_name = "Demo",
      project_id = "123",
      index = 1L,
      function_name = "parse_line",
      traceback = "parse_line()"
    )
  )
  report <- format_appusage_issue_report(context)

  expect_equal(context$source_basename, basename(source_file))
  expect_equal(context$stage, "first_level")
  expect_match(report, "source_file")
  expect_lt(regexpr("## Source data context", report)[[1]], regexpr("## Traceback", report)[[1]])
})

test_that("preflight_self_report_matching reports missing columns and duplicate values", {
  self_report <- data.frame(
    seq = c("1", "1", NA),
    upload = c("file-a.txt", "file-a.txt", ""),
    stringsAsFactors = FALSE
  )

  result <- preflight_self_report_matching(
    self_report,
    sequence_col = "seq",
    upload_col = "upload",
    participant_id_col = "pid"
  )

  expect_equal(result$status[[1]], "error")
  expect_match(result$missing_required_columns[[1]], "pid")
  expect_equal(result$n_missing_sequence[[1]], 1L)
  expect_equal(result$n_duplicate_sequence[[1]], 1L)
  expect_equal(result$n_missing_upload[[1]], 1L)
  expect_equal(result$n_duplicate_upload[[1]], 1L)
})

test_that("numeric-prefix raw filenames parse sequence, type, and timestamp", {
  parsed <- parse_wenjuanxing_upload_filename("1001_AppUsage_line_2023_9_21_8_45_57.txt")

  expect_equal(parsed$wenjuanxing_sequence_id, 1001L)
  expect_equal(parsed$uploaded_file_name, "AppUsage_line_2023_9_21_8_45_57.txt")
  expect_equal(parsed$native_export_type_from_filename, "line")
  expect_match(parsed$native_export_created_at, "2023-09-21T08:45:57")
})

test_that("upload-cell extraction handles complex text and URLs", {
  candidates <- extract_wenjuanxing_upload_filenames(
    "first file: https://example.test/upload/1001_AppUsage_line_2024_1_2_3_4_5.txt; second=1001_AppUsage_meta_2024_1_2_3_4_5.txt"
  )

  expect_true("1001_AppUsage_line_2024_1_2_3_4_5.txt" %in% candidates)
  expect_true("1001_AppUsage_meta_2024_1_2_3_4_5.txt" %in% candidates)
})

test_that("upload-cell extraction handles Wenjuanxing URL filename parameters", {
  old_url <- paste0(
    "http://pubuserqiniu.paperol.cn/233166970_1_q84_1694568839kheadJ.txt?",
    "attname=2_84_run_ver3.txt&e=1708439689&token=abc"
  )
  new_url <- paste0(
    "https://alifilezx.sojump.cn/278254757_1_q29_20241008160617894Z3W3JE.txt?",
    "Expires=1736349214&response-content-disposition=",
    "attachment%3Bfilename%3D1_29_AppUsage_line_2024_10_8_16_5_58.txt"
  )

  expect_equal(extract_wenjuanxing_upload_filenames(old_url), "2_84_run_ver3.txt")
  new_candidates <- extract_wenjuanxing_upload_filenames(new_url)
  expect_true("1_29_AppUsage_line_2024_10_8_16_5_58.txt" %in% new_candidates)
  expect_true("AppUsage_line_2024_10_8_16_5_58.txt" %in% new_candidates)
  expect_false("29_AppUsage_line_2024_10_8_16_5_58.txt" %in% new_candidates)
  expect_equal(extract_wenjuanxing_upload_filenames("-3"), character())
})

test_that("self-report matching requires both sequence and upload filename", {
  fixture <- project_workflow_match_fixture()
  self_report <- data.frame(
    "序号" = c(1001L, 1001L, 9999L),
    upload = c(
      "1001_AppUsage_line_2024_1_2_3_4_5.txt",
      "1001_AppUsage_day_2024_1_2_3_4_5.txt",
      "1001_AppUsage_line_2024_1_2_3_4_5.txt"
    ),
    check.names = FALSE
  )

  matched <- appusage_match_self_report_table(
    self_report,
    manifest = fixture$manifest,
    project_root = fixture$project_root,
    first = fixture$first,
    second = fixture$second,
    sequence_col = "序号",
    upload_col = "upload",
    project_id = "123",
    project_name = "StudyA"
  )$matched_self_report

  expect_equal(matched$moSens_match_status[[1]], "matched")
  expect_equal(matched$moSens_match_status[[2]], "unmatched_filename")
  expect_equal(matched$moSens_match_status[[3]], "unmatched_sequence")
  expect_false("moSens_metadata_json" %in% names(matched))
  expect_false(grepl("^/", matched$moSens_data_dir[[1]]))
})

test_that("self-report matching status keeps filename and sequence mismatches distinct", {
  fixture <- project_workflow_match_fixture()
  missing_sequence_manifest <- fixture$manifest[1, ]
  missing_sequence_manifest$wenjuanxing_sequence_id <- NA_integer_
  self_report_filename_only <- data.frame(
    "\u5e8f\u53f7" = 1001L,
    upload = "1001_AppUsage_line_2024_1_2_3_4_5.txt",
    check.names = FALSE
  )

  filename_only <- appusage_match_self_report_table(
    self_report_filename_only,
    manifest = missing_sequence_manifest,
    project_root = fixture$project_root,
    first = fixture$first[1, ],
    second = fixture$second[1, ],
    sequence_col = "\u5e8f\u53f7",
    upload_col = "upload",
    project_id = "123",
    project_name = "StudyA"
  )$matched_self_report

  expect_equal(filename_only$moSens_match_status[[1]], "unmatched_sequence")

  self_report_sequence_only <- data.frame(
    "\u5e8f\u53f7" = 1001L,
    upload = "1001_AppUsage_day_2024_1_2_3_4_5.txt",
    check.names = FALSE
  )
  sequence_only <- appusage_match_self_report_table(
    self_report_sequence_only,
    manifest = fixture$manifest,
    project_root = fixture$project_root,
    first = fixture$first,
    second = fixture$second,
    sequence_col = "\u5e8f\u53f7",
    upload_col = "upload",
    project_id = "123",
    project_name = "StudyA"
  )$matched_self_report

  expect_equal(sequence_only$moSens_match_status[[1]], "unmatched_filename")
})

test_that("self-report matching uses keyed lookup when inputs are reordered", {
  fixture <- project_workflow_match_fixture()
  self_report <- data.frame(
    sequence = 1001L,
    upload = "1001_AppUsage_line_2024_1_2_3_4_5.txt",
    check.names = FALSE
  )
  names(self_report)[[1]] <- "\u5e8f\u53f7"

  matched <- appusage_match_self_report_table(
    self_report,
    manifest = fixture$manifest[c(2, 1), ],
    project_root = fixture$project_root,
    first = fixture$first[c(2, 1), ],
    second = fixture$second[c(2, 1), ],
    sequence_col = "\u5e8f\u53f7",
    upload_col = "upload",
    project_id = "123",
    project_name = "StudyA"
  )$matched_self_report

  expect_equal(matched$moSens_match_status[[1]], "matched")
  expect_equal(matched$moSens_appusage_export_type[[1]], "line")
  expect_match(basename(matched$moSens_data_dir[[1]]), "sub-1001_type-line_proc-2[.]rda")
  expect_false("moSens_metadata_json" %in% names(matched))
})

test_that("self-report match summary refresh uses keyed proc-2 paths", {
  fixture <- project_workflow_match_fixture()
  summary_file <- file.path(fixture$project_root, "analytic_summary_table_proclevel-2.csv")
  summary <- fixture$second[c(2, 1), ]
  utils::write.csv(summary, summary_file, row.names = FALSE, na = "")
  manifest <- appusage_manifest_with_proc2_paths(
    fixture$manifest,
    fixture$first,
    fixture$second,
    fixture$project_root
  )
  file_matches <- appusage_init_file_match_summary(manifest)
  file_matches$self_report_match_status <- c("matched", "unmatched_appusage_upload")
  file_matches$self_report_sequence_id <- c(1001L, NA_integer_)

  appusage_refresh_match_summary(fixture$project_root, file_matches)
  refreshed <- utils::read.csv(summary_file, stringsAsFactors = FALSE)

  expect_equal(nrow(refreshed), nrow(summary))
  expect_equal(
    refreshed$self_report_match_status[refreshed$detected_type == "line"],
    "matched"
  )
  expect_equal(
    refreshed$self_report_match_status[refreshed$detected_type == "meta"],
    "unmatched_appusage_upload"
  )
  expect_equal(
    refreshed$self_report_sequence_id[refreshed$detected_type == "line"],
    1001L
  )
})

test_that("self-report matching can use native APP Usage filename postfix", {
  fixture <- project_workflow_match_fixture()
  fixture$manifest$wenjuanxing_sequence_id[[1]] <- 1L
  fixture$manifest$candidate_participant_id[[1]] <- "1"
  fixture$manifest$source_basename[[1]] <- "序号1_手机使用时间_AppUsage_line_2024_10_8_16_5_58.txt"
  fixture$manifest$source_file[[1]] <- file.path(fixture$project_root, fixture$manifest$source_basename[[1]])
  fixture$manifest$uploaded_file_name[[1]] <- "AppUsage_line_2024_10_8_16_5_58.txt"
  fixture$manifest$native_export_file_name[[1]] <- "AppUsage_line_2024_10_8_16_5_58.txt"
  fixture$manifest$filename_export_type[[1]] <- "line"
  fixture$manifest$native_export_created_at[[1]] <- "2024-10-08T16:05:58+0800"
  fixture$first$source_file[[1]] <- fixture$manifest$source_file[[1]]
  self_report <- data.frame(
    "序号" = 1L,
    upload = paste0(
      "https://alifilezx.sojump.cn/x.txt?response-content-disposition=",
      "attachment%3Bfilename%3D1_29_AppUsage_line_2024_10_8_16_5_58.txt"
    ),
    check.names = FALSE
  )

  matched <- appusage_match_self_report_table(
    self_report,
    manifest = fixture$manifest[1, ],
    project_root = fixture$project_root,
    first = fixture$first[1, ],
    second = fixture$second[1, ],
    sequence_col = "序号",
    upload_col = "upload",
    project_id = "123",
    project_name = "StudyA"
  )$matched_self_report

  expect_equal(matched$moSens_match_status[[1]], "matched")
})

test_that("duplicate candidates resolve by export-type priority", {
  fixture <- project_workflow_match_fixture()
  self_report <- data.frame(
    "序号" = 1001L,
    upload = "1001_AppUsage_line_2024_1_2_3_4_5.txt 1001_AppUsage_meta_2024_1_2_3_4_5.txt",
    check.names = FALSE
  )

  matched <- appusage_match_self_report_table(
    self_report,
    manifest = fixture$manifest,
    project_root = fixture$project_root,
    first = fixture$first,
    second = fixture$second,
    sequence_col = "序号",
    upload_col = "upload",
    export_type_priority = c("meta", "line"),
    project_id = "123",
    project_name = "StudyA"
  )$matched_self_report

  expect_equal(matched$moSens_appusage_export_type[[1]], "meta")
  expect_match(matched$moSens_match_warning[[1]], "export_type_priority")
})

test_that("duplicate same-type candidates resolve by nearest submit time", {
  fixture <- project_workflow_match_fixture()
  second_line <- fixture$manifest[1, ]
  second_line$source_file <- file.path(fixture$project_root, "1001_AppUsage_line_2024_1_2_4_0_0.txt")
  second_line$source_basename <- basename(second_line$source_file)
  second_line$native_export_file_name <- "AppUsage_line_2024_1_2_4_0_0.txt"
  second_line$uploaded_file_name <- "AppUsage_line_2024_1_2_4_0_0.txt"
  second_line$native_export_created_at <- "2024-01-02T04:00:00+0800"
  second_line$index <- 3L
  rda_late <- file.path(fixture$project_root, "proclevel-2", "sub-1001_type-line_proc-2-late.rda")
  data <- list(event = data.frame(), episode = data.frame(), daily = data.frame())
  save(data, file = rda_late)
  manifest <- rbind(fixture$manifest[1, ], second_line)
  first <- tibble::tibble(
    index = 1:2,
    source_file = manifest$source_file,
    data_file = c("first-early.rda", "first-late.rda"),
    participant_id = c("1001", "1001"),
    detected_type = c("line", "line")
  )
  second <- tibble::tibble(
    first_level_data_file = c("first-early.rda", "first-late.rda"),
    second_level_rda = c(fixture$second$second_level_rda[[1]], rda_late),
    participant_id = c("1001", "1001"),
    detected_type = c("line", "line")
  )
  self_report <- data.frame(
    "序号" = 1001L,
    upload = "1001_AppUsage_line_2024_1_2_3_4_5.txt 1001_AppUsage_line_2024_1_2_4_0_0.txt",
    submit = "2024-01-02 03:59:59",
    check.names = FALSE
  )

  matched <- appusage_match_self_report_table(
    self_report,
    manifest = manifest,
    project_root = fixture$project_root,
    first = first,
    second = second,
    sequence_col = "序号",
    upload_col = "upload",
    submit_time_col = "submit",
    project_id = "123",
    project_name = "StudyA"
  )$matched_self_report

  expect_match(matched$moSens_appusage_source_file[[1]], "4_0_0")
  expect_match(matched$moSens_match_warning[[1]], "nearest_submit_time")
})

test_that("root-based workflow discovers ProjectID, writes Study output, matched Excel, and resumes", {
  testthat::skip_if_not_installed("openxlsx")
  fixture <- project_workflow_root_fixture()
  output_root <- file.path(tempdir(), paste0("appusage_project_output_", sample.int(1e8, 1)))
  self_report_names <- names(openxlsx::read.xlsx(fixture$excel))
  sequence_col <- self_report_names[[1]]
  submit_time_col <- self_report_names[[3]]

  result <- run_appusage_project_workflow(
    raw_data_root = fixture$root,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    sequence_col = sequence_col,
    upload_col = "upload",
    submit_time_col = submit_time_col,
    overwrite = TRUE,
    progress = FALSE,
    diagnostic_verbosity = "none"
  )

  expect_equal(basename(result$project_dir), "Study-StudyA_ProjectID-123")
  expect_true(file.exists(result$configuration_file))
  expect_true(file.exists(result$matched_self_report_file))
  matched <- openxlsx::read.xlsx(result$matched_self_report_file)
  expect_equal(nrow(matched), 1L)
  expect_equal(matched$moSens_match_status[[1]], "matched")
  expect_true(file.exists(file.path(result$project_dir, matched$moSens_data_dir[[1]])))
  expect_false("moSens_metadata_json" %in% names(matched))
  rda <- file.path(result$project_dir, matched$moSens_data_dir[[1]])
  mtime <- file.info(rda)$mtime
  Sys.sleep(1.1)

  resumed <- run_appusage_project_workflow(
    raw_data_root = fixture$root,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    sequence_col = sequence_col,
    upload_col = "upload",
    submit_time_col = submit_time_col,
    resume = TRUE,
    overwrite = FALSE,
    progress = FALSE,
    diagnostic_verbosity = "none"
  )

  expect_true(resumed$resumed)
  expect_equal(file.info(rda)$mtime, mtime)

  proc2_summary <- file.path(result$project_dir, "analytic_summary_table_proclevel-2.csv")
  unlink(proc2_summary)
  expect_false(file.exists(proc2_summary))
  Sys.sleep(1.1)
  partial_resumed <- run_appusage_project_workflow(
    raw_data_root = fixture$root,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    sequence_col = sequence_col,
    upload_col = "upload",
    submit_time_col = submit_time_col,
    resume = TRUE,
    overwrite = FALSE,
    progress = FALSE,
    diagnostic_verbosity = "none"
  )

  expect_true(partial_resumed$resumed)
  expect_true(file.exists(proc2_summary))
  expect_equal(file.info(rda)$mtime, mtime)
  expect_false(dir.exists(file.path(result$project_dir, "proclevel-3")))
  expect_false(dir.exists(file.path(result$project_dir, "proclevel-tmp")))
})

test_that("project console start banner contains project metadata", {
  project <- list(project_id = "288816384", project_name = "ZJW1reup")
  start_time <- as.POSIXct("2026-06-30 10:00:00", tz = "Asia/Shanghai")

  output <- capture.output(
    appusage_console_project_start(
      progress = TRUE,
      project = project,
      n_appusage = 559L,
      n_survey = 321L,
      start_time = start_time
    ),
    type = "message"
  )
  text <- paste(output, collapse = "\n")

  expect_match(text, "Project ID: 288816384", fixed = TRUE)
  expect_match(text, "Project Name: ZJW1reup", fixed = TRUE)
  expect_match(text, "N_appusage: 559", fixed = TRUE)
  expect_match(text, "N_survey: 321", fixed = TRUE)
  expect_match(text, "Project Preprocessing Start Time: 2026-06-30 10:00:00", fixed = TRUE)
})

test_that("project console file-stage lines report success and failure", {
  line_time <- as.POSIXct("2026-06-30 10:00:00", tz = "Asia/Shanghai")

  success <- capture.output(
    appusage_console_file_stage(
      progress = TRUE,
      index = 17L,
      total = 559L,
      stage_label = "first-level",
      file_label = "sub-17_type-line",
      status = "success",
      time = line_time
    ),
    type = "message"
  )
  failure <- capture.output(
    appusage_console_file_stage(
      progress = TRUE,
      index = 17L,
      total = 559L,
      stage_label = "first-level",
      file_label = "sub-17_type-line",
      status = "failed",
      diagnostic = "diagnostics/error_reports/example.md",
      time = line_time
    ),
    type = "message"
  )

  expect_match(
    paste(success, collapse = "\n"),
    "[2026-06-30 10:00:00] 17/559 | first-level | sub-17_type-line | Done!",
    fixed = TRUE
  )
  expect_match(paste(failure, collapse = "\n"), "Failed!", fixed = TRUE)
  expect_match(
    paste(failure, collapse = "\n"),
    "diagnostic: diagnostics/error_reports/example.md",
    fixed = TRUE
  )
})

test_that("project console sample-size flow counts and percentages are coherent", {
  first <- tibble::tibble(status = c("success", "success", "error"))
  second <- tibble::tibble(status = c("success", "skipped"))
  qc <- tibble::tibble(qc_status = c("pass", "fail", "skipped"))
  matched <- data.frame(moSens_match_status = c("matched", "unmatched_sequence", "unmatched_filename"))

  flow <- appusage_console_sample_size_flow(
    first = first,
    second = second,
    qc = qc,
    matched = matched,
    n_appusage = 4L,
    n_survey = 3L
  )

  for (stage in names(flow)) {
    expect_equal(sum(unname(flow[[stage]]$counts)), flow[[stage]]$total)
    if (flow[[stage]]$total > 0) {
      expect_equal(sum(unname(flow[[stage]]$percents)), 100)
    }
  }
  first_row <- appusage_console_percent_row("first-level", flow$first_level)
  expect_match(first_row, "TOTAL: 4", fixed = TRUE)
  expect_match(first_row, "Skipped: 1", fixed = TRUE)
})

test_that("project console progress can be suppressed", {
  project <- list(project_id = "288816384", project_name = "ZJW1reup")
  start_time <- as.POSIXct("2026-06-30 10:00:00", tz = "Asia/Shanghai")

  output <- capture.output(
    appusage_console_project_start(
      progress = FALSE,
      project = project,
      n_appusage = 559L,
      n_survey = 321L,
      start_time = start_time
    ),
    type = "message"
  )

  expect_equal(output, character())
})

test_that("matching-only resume prints matching output without false preprocessing rerun lines", {
  testthat::skip_if_not_installed("openxlsx")
  fixture <- project_workflow_root_fixture()
  output_root <- file.path(tempdir(), paste0("appusage_project_output_", sample.int(1e8, 1)))
  self_report_names <- names(openxlsx::read.xlsx(fixture$excel))
  sequence_col <- self_report_names[[1]]
  submit_time_col <- self_report_names[[3]]

  initial <- run_appusage_project_workflow(
    raw_data_root = fixture$root,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    sequence_col = sequence_col,
    upload_col = "upload",
    submit_time_col = submit_time_col,
    overwrite = TRUE,
    progress = FALSE,
    diagnostic_verbosity = "none"
  )
  expect_false(is.null(initial$matched_self_report_file))

  output <- capture.output(
    resumed <- run_appusage_project_workflow(
      raw_data_root = fixture$root,
      project_id = "123",
      project_name = "StudyA",
      output_root = output_root,
      sequence_col = sequence_col,
      upload_col = "upload",
      submit_time_col = submit_time_col,
      resume = TRUE,
      overwrite = FALSE,
      progress = TRUE,
      diagnostic_verbosity = "none"
    ),
    type = "message"
  )
  text <- paste(output, collapse = "\n")

  expect_true(resumed$resumed)
  expect_match(text, "Project Preprocessing Start Time", fixed = TRUE)
  expect_match(text, "self-report matching", fixed = TRUE)
  expect_false(grepl("\\| first-level \\| sub-", text))
  expect_false(grepl("\\| second-level \\| sub-", text))
  expect_false(grepl("\\| QC-daily-qc-v1 \\| sub-", text))
})

appusage_mixed_type_workbook_fixture <- function() {
  testthat::skip_if_not_installed("openxlsx")
  path <- tempfile(fileext = ".xlsx")
  workbook <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(workbook, "survey")
  openxlsx::writeData(
    workbook,
    "survey",
    data.frame(mixed_value = rep(TRUE, 1405L)),
    colNames = TRUE
  )
  openxlsx::writeData(
    workbook,
    "survey",
    "late text value",
    startCol = 1L,
    startRow = 1406L,
    colNames = FALSE
  )
  openxlsx::saveWorkbook(workbook, path, overwrite = TRUE)
  path
}

test_that("self-report reader guesses across the full selected sheet", {
  workbook <- appusage_mixed_type_workbook_fixture()
  diagnostics_dir <- tempfile("self-report-diagnostics-")

  result <- appusage_read_self_report_workbook(
    workbook,
    diagnostics_dir = diagnostics_dir,
    emit_warning = FALSE
  )

  expect_equal(nrow(result$data), 1405L)
  expect_equal(result$data$mixed_value[[1405]], "late text value")
  expect_identical(result$diagnostics$effective_guess_max, "Inf")
  expect_equal(result$diagnostics$warning_count, 0L)
  expect_true(file.exists(result$diagnostics_file))
})

test_that("legacy NA self-report guess_max remains unspecified", {
  expect_identical(
    appusage_effective_self_report_guess_max(
      n_max = Inf,
      guess_max = NA_real_
    ),
    Inf
  )
  expect_identical(
    appusage_effective_self_report_guess_max(
      n_max = 250,
      guess_max = NA_real_
    ),
    250L
  )
})

test_that("legacy empty self-report col_types remains unspecified", {
  workbook <- appusage_mixed_type_workbook_fixture()

  result <- appusage_read_self_report_workbook(
    workbook,
    col_types = character(),
    emit_warning = FALSE
  )

  expect_equal(nrow(result$data), 1405L)
  expect_null(result$diagnostics$explicit_col_types)
})

test_that("explicit incompatible self-report col_types records one concise warning", {
  workbook <- appusage_mixed_type_workbook_fixture()
  diagnostics_dir <- tempfile("self-report-coercion-")
  package_warnings <- character()

  result <- withCallingHandlers(
    appusage_read_self_report_workbook(
      workbook,
      col_types = "logical",
      diagnostics_dir = diagnostics_dir,
      emit_warning = TRUE
    ),
    warning = function(w) {
      package_warnings <<- c(package_warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_length(package_warnings, 1L)
  expect_match(package_warnings[[1]], "Self-report workbook read produced")
  expect_true(is.na(result$data$mixed_value[[1405]]))
  expect_gt(result$diagnostics$warning_count, 0L)
  expect_true(all(vapply(
    result$diagnostics$warnings,
    function(x) all(c("timestamp", "message", "category") %in% names(x)),
    logical(1)
  )))
  expect_true(any(vapply(
    result$diagnostics$warnings,
    function(x) identical(x$category, "type_coercion"),
    logical(1)
  )))
  persisted <- jsonlite::read_json(result$diagnostics_file, simplifyVector = TRUE)
  expect_equal(persisted$read_status, "success")
  expect_equal(persisted$warning_count, result$diagnostics$warning_count)
})

test_that("self-report read failure writes diagnostics and preserves the condition", {
  workbook <- appusage_mixed_type_workbook_fixture()
  diagnostics_dir <- tempfile("self-report-read-error-")
  original <- structure(
    simpleError("synthetic workbook read failure"),
    class = c("appusage_test_workbook_error", "error", "condition")
  )
  testthat::local_mocked_bindings(
    appusage_read_excel_impl = function(...) stop(original)
  )

  expect_error(
    appusage_read_self_report_workbook(
      workbook,
      diagnostics_dir = diagnostics_dir
    ),
    "synthetic workbook read failure",
    class = "appusage_test_workbook_error"
  )
  diagnostic_file <- file.path(diagnostics_dir, "self_report_read.json")
  expect_true(file.exists(diagnostic_file))
  persisted <- jsonlite::read_json(diagnostic_file, simplifyVector = TRUE)
  expect_equal(persisted$read_status, "error")
  expect_match(persisted$error_condition_message, "synthetic workbook read failure")
})

test_that("project workflow reads the self-report workbook once and reuses its table", {
  testthat::skip_if_not_installed("openxlsx")
  fixture <- project_workflow_root_fixture()
  output_root <- tempfile("appusage-single-workbook-read-")
  original_reader <- appusage_read_excel_impl
  read_count <- 0L
  testthat::local_mocked_bindings(
    appusage_read_excel_impl = function(...) {
      read_count <<- read_count + 1L
      original_reader(...)
    }
  )

  result <- run_appusage_project_workflow(
    raw_data_root = fixture$root,
    project_id = "123",
    project_name = "StudyA",
    output_root = output_root,
    sequence_col = names(openxlsx::read.xlsx(fixture$excel))[[1]],
    upload_col = "upload",
    submit_time_col = names(openxlsx::read.xlsx(fixture$excel))[[3]],
    overwrite = TRUE,
    progress = FALSE,
    diagnostic_verbosity = "none"
  )

  expect_equal(read_count, 1L)
  expect_equal(nrow(result$matched_self_report), 1L)
  expect_equal(result$matched_self_report$moSens_match_status[[1]], "matched")
  expect_false("moSens_metadata_json" %in% names(result$matched_self_report))
  expect_equal(result$self_report_read_diagnostics$n_rows, 1L)
  config <- readRDS(result$configuration_file)
  expect_equal(config$self_report_sheet, 1)
  expect_true(is.na(config$self_report_guess_max))
  expect_identical(config$self_report_col_types, character())
  expect_equal(config$self_report_read$warning_count, 0L)
})

test_that("workflow configuration compatibility includes workbook read controls", {
  current <- list(
    self_report_sheet = 1,
    self_report_guess_max = NA_real_,
    self_report_col_types = character()
  )
  expect_length(appusage_workflow_config_differences(list(), current), 0L)

  changed_sheet <- current
  changed_sheet$self_report_sheet <- "Survey"
  expect_true("self_report_sheet" %in%
    appusage_workflow_config_differences(current, changed_sheet))

  changed_guess <- current
  changed_guess$self_report_guess_max <- 1500
  expect_true("self_report_guess_max" %in%
    appusage_workflow_config_differences(current, changed_guess))

  changed_types <- current
  changed_types$self_report_col_types <- c("text", "numeric")
  expect_true("self_report_col_types" %in%
    appusage_workflow_config_differences(current, changed_types))
})
