collection_fixture <- function(n = 2L, matching = FALSE) {
  root <- tempfile("appusage-collection-")
  dir.create(root)
  ids <- sprintf("%03d", seq_len(n))
  raw <- file.path(root, paste0("raw-", ids))
  out <- file.path(root, paste0("output-", ids))
  paths <- character(n)
  for (i in seq_len(n)) {
    dir.create(raw[[i]])
    paths[[i]] <- file.path(raw[[i]], "1_AppUsage_line_2024_1_2_3_4_5.txt")
    file.copy(test_path("fixtures", "line_sample.txt"), paths[[i]])
  }
  registry <- data.frame(project_id = ids, project_name = paste0("Study", ids),
    project_dir = raw, output_dir = out, stringsAsFactors = FALSE)
  if (matching) {
    workbooks <- file.path(root, paste0("ProjectID-", ids, "_WJXraw-test.xlsx"))
    for (i in seq_len(n)) appusage_write_xlsx(data.frame(seq = c(1L, 99L),
      upload = c(basename(paths[[i]]), "absent.txt"), note = c("A", "B")), workbooks[[i]])
    registry$self_report_file <- workbooks
    registry$sequence_col <- "seq"; registry$upload_col <- "upload"
  }
  list(root = root, registry = registry, paths = paths,
    batch = file.path(root, "batch"), config = appusage_config(execution = list(progress = FALSE)))
}

test_that("delivery shares compact metadata without weakening changed-content checks", {
  f <- collection_fixture(n = 1)
  run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  run_appusage_workflow(project_dir = f$registry$output_dir)
  withr::local_options(appusageR.diagnostics = TRUE)
  context <- appusage_runtime_context()
  withr::local_options(appusageR.runtime_context = context)
  report <- validate_appusage_project(f$registry$output_dir, verify = "content")
  expect_true(report$complete, info = paste(report$checks$detail[report$checks$status == "failed"], collapse = "; "))
  metrics <- appusage_runtime_metrics(context)
  expect_false(any(metrics$operation == "json_parse" & metrics$calls > 0))
  expect_true(any(metrics$operation == "validation_json_reuse" & metrics$calls > 0))
  second <- appusage_read_summary_csv(file.path(f$registry$output_dir,
    "analytic_summary_table_proclevel-2.csv"))
  path <- second_level_metadata_path(appusage_summary_proc2_paths(second)[[1]])
  metadata <- appusage_read_json_safely(path)
  metadata$counts$n_episode_rows <- metadata$counts$n_episode_rows + 1L
  appusage_atomic_write_metadata_json(metadata, path)
  changed <- validate_appusage_project(f$registry$output_dir, verify = "content")
  expect_false(changed$complete)
  expect_true(any(changed$checks$check == "scientific_payload_contract" & changed$checks$status == "failed"))
})

test_that("Excel comparison follows writer serialization without trimming source text", {
  values <- c("plain", "a\nb", "a\rb", "a\r\nb", "a\r\r\nb",
    "_x000D_", "_x005F_x000D_", "_x4E2D_", "literal \\n", "&gt;",
    "  spaces  ", "NA", " ", "\t", "\r", "\n", "\r\n", "_x0020_",
    "\u00a0", NA_character_, "")
  path <- tempfile(fileext = ".xlsx")
  appusage_write_xlsx(data.frame(sequence = seq_along(values), value = values), path)
  actual <- readxl::read_excel(path, col_types = "text", trim_ws = FALSE)$value
  desired <- appusage_delivery_excel_text(values)
  actual[is.na(actual)] <- ""; desired[is.na(desired)] <- ""
  expect_identical(actual, desired)
})

test_that("delivery preserves multiline workbook headers and literal text cells", {
  f <- collection_fixture(1, matching = TRUE)
  questionnaire <- data.frame(seq = c(1L, 99L),
    upload = c(basename(f$paths), "absent.txt"),
    note = c("first\r\nsecond", "_x000D_"),
    literal = c("NA", "  spaces  "), entity = c("&gt;", "&amp;"), check.names = FALSE)
  names(questionnaire)[[3]] <- "Question\r\ncontinued"
  appusage_write_xlsx(questionnaire, f$registry$self_report_file)
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_identical(result$projects$status, "completed")
  out <- f$registry$output_dir
  expect_true(validate_appusage_project(out, verify = "content")$complete)
  cfg <- readRDS(file.path(out, "appusage_configuration.rds"))
  link <- readRDS(file.path(out, "self_report_link_result.rds"))$matched_self_report
  export <- appusage_matching_export_path(out, cfg)
  changed <- link
  changed[[3]][[1]] <- "changed content"
  appusage_write_xlsx(changed, export)
  report <- validate_appusage_project(out, verify = "content")
  expect_false(report$complete)
  expect_true(any(report$checks$check == "questionnaire_export" & report$checks$status == "failed"))
  appusage_write_xlsx(link, export)
  names(link)[[3]] <- "changed header"
  appusage_write_xlsx(link, export)
  report <- validate_appusage_project(out, verify = "content")
  expect_false(report$complete)
  expect_true(any(report$checks$check == "questionnaire_export" & report$checks$status == "failed"))
})

test_that("read-only projects report missing delivery without modifying existing outputs", {
  f <- collection_fixture(1, matching = TRUE)
  run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  cfg <- readRDS(file.path(f$registry$output_dir, "appusage_configuration.rds"))
  export <- appusage_matching_export_path(f$registry$output_dir, cfg)
  unlink(export)
  files <- list.files(f$registry$output_dir, recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(files)
  f$registry$read_only <- TRUE
  local_mocked_bindings(appusage_execute_pipeline = function(...) stop("unexpected read-only repair"))
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_identical(result$projects$status, "blocked")
  expect_match(result$projects$error, "Read-only project")
  expect_false(file.exists(export))
  expect_identical(tools::md5sum(files), before)
})

test_that("simultaneously missing summary and Excel preserve legacy diagnostic layout", {
  f <- collection_fixture(1, matching = TRUE)
  run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  out <- f$registry$output_dir
  cfg <- readRDS(file.path(out, "appusage_configuration.rds"))
  path <- file.path(out, "analytic_summary_table_proclevel-2.csv")
  previous <- appusage_read_summary_csv(path)
  previous$diagnostic_report <- NA_character_
  previous$diagnostic_json <- NA_character_
  appusage_publish_project(out, cfg, fresh = previous, force = TRUE)
  previous <- appusage_read_summary_csv(path)
  export <- appusage_matching_export_path(out, cfg)
  cells <- readxl::read_excel(export, col_types = "text", .name_repair = "minimal")
  artifacts <- unlist(lapply(c("proclevel-1", "proclevel-2"), function(stage)
    list.files(file.path(out, stage), pattern = "[.](rda|json)$", full.names = TRUE)))
  before <- tools::md5sum(artifacts)
  unlink(c(path, export))
  withr::local_options(appusageR.diagnostics = TRUE)
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_identical(result$projects$status, "completed")
  expect_identical(appusage_read_summary_csv(path), previous)
  expect_identical(readxl::read_excel(export, col_types = "text", .name_repair = "minimal"), cells)
  expect_identical(tools::md5sum(artifacts), before)
  record <- readRDS(file.path(out, "appusage_run_record.rds"))
  expect_false(any(record$tasks$stage %in% c("parse", "research_data", "qc", "matching") &
    record$tasks$actual_action == "run"))
  expect_equal(sum(record$metrics$calls[record$metrics$operation %in%
    c("parse", "research_data", "scientific_qc", "rda_load", "matching")]), 0)
})

test_that("legacy unspecified questionnaire read options remain reusable", {
  f <- collection_fixture(1, matching = TRUE)
  run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  out <- f$registry$output_dir
  cfg <- readRDS(file.path(out, "appusage_configuration.rds"))
  path <- file.path(out, "workflow_configuration.rds")
  adapter <- readRDS(path)
  adapter$self_report_guess_max <- NA_real_
  adapter$self_report_col_types <- character()
  saveRDS(adapter, path)
  first <- appusage_read_summary_csv(file.path(out, "analytic_summary_table_proclevel-1.csv"))
  second <- appusage_read_summary_csv(file.path(out, "analytic_summary_table_proclevel-2.csv"))
  state <- appusage_projection_state(out)
  state$matching$request <- appusage_matching_request(out, cfg, first, second, adapter = adapter)
  saveRDS(state, file.path(out, "appusage_projection_state.rds"))
  entry <- appusage_collection_registry(f$registry, f$config)
  requested <- appusage_collection_adapter(entry, entry$config[[1]])
  expect_identical(requested$self_report_guess_max, NA_real_)
  expect_identical(requested$self_report_col_types, character())
  files <- list.files(out, full.names = TRUE, recursive = TRUE)
  before <- tools::md5sum(files)
  f$registry$read_only <- TRUE
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_identical(result$projects$status, "skipped_complete")
  expect_equal(result$projects$workflow_elapsed_sec, 0)
  expect_identical(tools::md5sum(files), before)
})

test_that("collection keeps identical participant and filenames independent across projects", {
  f <- collection_fixture(matching = TRUE)
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_true(all(result$projects$status == "completed"))
  expect_identical(result$projects$project_id, c("001", "002"))
  expect_false(result$project_parallel)
  for (out in f$registry$output_dir) {
    report <- validate_appusage_project(out, verify = "content")
    expect_true(report$complete, info = paste(report$checks$detail[report$checks$status == "failed"], collapse = "; "))
    link <- readRDS(file.path(out, "self_report_link_result.rds"))
    expect_equal(nrow(link$matched_self_report), 2)
    expect_equal(sum(link$matched_self_report$moSens_match_status == "matched"), 1)
  }
  before <- tools::md5sum(unlist(lapply(f$registry$output_dir, function(out)
    list.files(out, pattern = "[.](rda|json)$", full.names = TRUE, recursive = TRUE))))
  withr::local_options(appusageR.diagnostics = TRUE)
  local_mocked_bindings(appusage_execute_pipeline = function(...) stop("unexpected workflow"))
  resumed <- run_appusage_projects(f$registry[2:1, ], f$batch, f$config, workers = 1)
  expect_true(all(resumed$projects$status == "skipped_complete"))
  expect_identical(tools::md5sum(names(before)), before)
  expect_identical(resumed$projects$project_key[match(result$projects$project_id,
    resumed$projects$project_id)], result$projects$project_key)
  only_one <- run_appusage_projects(f$registry[1, ], f$batch, f$config, workers = 1)
  expect_setequal(only_one$projects$project_id, c("001", "002"))
})

test_that("registry errors are rejected before writing outputs", {
  f <- collection_fixture()
  bad <- f$registry; bad$project_id[[2]] <- bad$project_id[[1]]
  expect_error(run_appusage_projects(bad, f$batch), "unique")
  bad <- f$registry; bad$project_id <- as.numeric(bad$project_id)
  expect_error(run_appusage_projects(bad, f$batch), "character")
  bad <- f$registry; bad$output_dir[[2]] <- file.path(bad$output_dir[[1]], "nested")
  expect_error(run_appusage_projects(bad, f$batch), "nonoverlapping")
  bad <- f$registry; bad$output_dir[[1]] <- bad$project_dir[[2]]
  expect_error(run_appusage_projects(bad, f$batch), "Raw and output")
  expect_error(run_appusage_projects(f$registry, f$batch, workers = 0), "positive integer")
  expect_false(any(dir.exists(f$registry$output_dir)))
  expect_false(dir.exists(f$batch))
  f <- collection_fixture(matching = TRUE)
  f$registry$upload_col[[2]] <- "missing-column"
  expect_error(run_appusage_projects(f$registry, f$batch), "uniquely")
  expect_false(any(dir.exists(f$registry$output_dir)))
})

test_that("project failure preserves a checkpoint and leaves other projects runnable", {
  f <- collection_fixture()
  original <- appusage_execute_pipeline
  local_mocked_bindings(appusage_execute_pipeline = function(..., project_id = NULL) {
    if (project_id == "001") stop("synthetic interruption")
    original(..., project_id = project_id)
  })
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_equal(result$projects$status, c("failed", "completed"))
  saved <- readRDS(file.path(f$batch, "appusage_projects.rds"))
  expect_equal(saved$projects$error[[1]], "synthetic interruption")
  expect_false("first_level" %in% names(saved))
  stopped <- run_appusage_projects(f$registry, file.path(f$root, "stop-batch"), f$config,
    workers = 1, continue_on_error = FALSE)
  expect_identical(stopped$projects$project_id, c("001", "002"))
  expect_identical(stopped$projects$status, c("failed", "pending"))
})

test_that("manifest inputs and relative aliases cannot overlap output or checkpoints", {
  f <- collection_fixture()
  bad <- f$registry
  bad$manifest <- list(data.frame(source_file = file.path(f$registry$output_dir[[2]], "source.txt")),
    data.frame(source_file = f$paths[[2]]))
  expect_error(run_appusage_projects(bad, f$batch), "Manifest sources")
  bad$manifest[[1]]$source_file <- file.path(f$batch, "source.txt")
  expect_error(run_appusage_projects(bad, f$batch), "Manifest sources")
  expect_false(dir.exists(f$batch))
  expect_false(any(dir.exists(f$registry$output_dir)))
  withr::local_dir(f$root)
  bad <- f$registry
  bad$output_dir[[1]] <- "raw-001/../raw-001/new-output"
  expect_error(run_appusage_projects(bad, f$batch), "Raw and output")
})

test_that("metadata checks and report methods do not open raw, RDA or questionnaire cells", {
  f <- collection_fixture(1, matching = TRUE)
  run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  local_mocked_bindings(appusage_source_fingerprint = function(...) stop("raw read"),
    load_appusage_data_object = function(...) stop("RDA load"),
    appusage_read_self_report_workbook = function(...) stop("questionnaire read"))
  report <- validate_appusage_project(f$registry$output_dir, verify = "metadata")
  expect_false(report$complete)
  expect_identical(report$status, "unverified")
  expect_output(print(report), "unverified")
  expect_equal(summary(report)$n_sources, 1)
})

test_that("delivery catches new sources, corrupt signatures, typed payloads and exports", {
  f <- collection_fixture(1, matching = TRUE)
  run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  out <- f$registry$output_dir
  new <- file.path(f$registry$project_dir, "2_AppUsage_line_2024_1_2_3_4_5.txt")
  file.copy(f$paths, new)
  report <- validate_appusage_project(out, x = c(f$paths, new), verify = "content")
  expect_false(report$complete)
  expect_true(any(report$checks$check == "first_source_inventory" & report$checks$status == "failed"))
  paths <- list.files(file.path(out, "proclevel-2"), pattern = "[.]rda$", full.names = TRUE)
  original <- readBin(paths, "raw", n = file.info(paths)$size)
  data <- load_appusage_data_object(paths)
  data$daily$duration_ms <- as.character(data$daily$duration_ms)
  save(data, file = paths)
  report <- validate_appusage_project(out, verify = "content")
  expect_false(report$complete)
  expect_true(any(report$checks$check == "scientific_payload_contract" & report$checks$status == "failed"))
  writeBin(original, paths)
  cfg <- readRDS(file.path(out, "appusage_configuration.rds"))
  export <- appusage_matching_export_path(out, cfg)
  appusage_write_xlsx(data.frame(wrong = "changed"), export)
  report <- validate_appusage_project(out, verify = "content")
  expect_false(report$complete)
  expect_true(any(report$checks$check == "questionnaire_export" & report$checks$status == "failed"))
})

test_that("diagnosed upstream failures remain completed delivery with source issues", {
  f <- collection_fixture(1)
  writeLines("unsupported export", f$paths)
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_identical(result$projects$status, "completed_with_source_issues")
  expect_equal(result$projects$source_issues, 1)
  report <- validate_appusage_project(f$registry$output_dir, verify = "content")
  expect_true(report$complete)
  expect_equal(report$n_sources, 1)
})

test_that("existing legacy evidence is blocked rather than silently rebuilding scientific data", {
  f <- collection_fixture(1)
  run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  out <- f$registry$output_dir
  first <- read.csv(file.path(out, "analytic_summary_table_proclevel-1.csv"))
  metadata <- appusage_read_json_safely(first$metadata_file)
  metadata$module_state$parse <- NULL
  appusage_atomic_write_metadata_json(metadata, first$metadata_file)
  before <- tools::md5sum(first$data_file)
  local_mocked_bindings(read_appusage_batch = function(...) stop("unexpected legacy rebuild"))
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_identical(result$projects$status, "blocked")
  expect_identical(tools::md5sum(first$data_file), before)
  report <- readRDS(file.path(f$batch, "delivery-001.rds"))
  expect_identical(report$status, "legacy_unverified")
})

test_that("missing first summaries preserve legacy and orphan science protection", {
  for (remove_json in c(FALSE, TRUE)) {
    f <- collection_fixture(1)
    run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
    out <- f$registry$output_dir
    path <- file.path(out, "analytic_summary_table_proclevel-1.csv")
    first <- read.csv(path)
    if (remove_json) {
      unlink(first$metadata_file)
    } else {
      meta <- appusage_read_json_safely(first$metadata_file)
      meta$module_state$parse <- NULL
      appusage_atomic_write_metadata_json(meta, first$metadata_file)
    }
    unlink(path)
    files <- list.files(out, pattern = "[.]rda$", recursive = TRUE, full.names = TRUE)
    before <- tools::md5sum(files)
    result <- with_mocked_bindings(
      run_appusage_projects(f$registry, f$batch, f$config, workers = 1),
      appusage_execute_pipeline = function(...) stop("unexpected unverified rebuild"))
    expect_identical(result$projects$status, "blocked")
    expect_match(result$projects$error, "explicit rebuild approval")
    expect_false(file.exists(path))
    expect_identical(tools::md5sum(files), before)
  }
})

test_that("a missing compatible first summary rebuilds only its projection", {
  f <- collection_fixture(1)
  run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  out <- f$registry$output_dir
  path <- file.path(out, "analytic_summary_table_proclevel-1.csv")
  expected <- appusage_read_summary_csv(path)
  metadata_before <- appusage_read_json_safely(expected$metadata_file)
  artifacts <- list.files(out, pattern = "[.]rda$", recursive = TRUE, full.names = TRUE)
  before <- tools::md5sum(artifacts)
  unlink(path)
  withr::local_options(appusageR.diagnostics = TRUE)
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_identical(result$projects$status, "completed")
  # The existing recovery contract reconstructs business fields from metadata;
  # historical worker/timing columns are not stored in every source JSON.
  fields <- c("source_file", "participant_id", "detected_type", "status",
    "source_record_key", "source_fingerprint", "metadata_file", "data_file")
  expect_identical(appusage_read_summary_csv(path)[fields], expected[fields])
  expect_identical(tools::md5sum(artifacts), before)
  metadata_after <- appusage_read_json_safely(expected$metadata_file)
  expect_identical(appusage_metadata_source_identity(metadata_after),
    appusage_metadata_source_identity(metadata_before))
  expect_identical(metadata_after$module_state, metadata_before$module_state)
  record <- readRDS(file.path(out, "appusage_run_record.rds"))
  expect_false(any(record$tasks$stage %in% c("parse", "research_data", "qc") &
    record$tasks$actual_action == "run"))
  expect_equal(sum(record$metrics$calls[record$metrics$operation %in%
    c("parse", "research_data", "scientific_qc", "rda_load")]), 0)
})

test_that("a changed questionnaire and missing export refresh only their owner", {
  f <- collection_fixture(matching = TRUE)
  run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  sources <- unlist(lapply(f$registry$output_dir, function(out)
    list.files(file.path(out, "proclevel-2"), full.names = TRUE)))
  before <- tools::md5sum(sources)
  appusage_write_xlsx(data.frame(seq = c(1L, 99L, 100L),
    upload = c(basename(f$paths[[1]]), "absent.txt", "missing.txt"), note = c("A", "B", "C")),
    f$registry$self_report_file[[1]])
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_equal(result$projects$status, c("completed", "skipped_complete"))
  expect_identical(tools::md5sum(sources), before)
  cfg <- readRDS(file.path(f$registry$output_dir[[1]], "appusage_configuration.rds"))
  export <- appusage_matching_export_path(f$registry$output_dir[[1]], cfg)
  unlink(export)
  implementation <- appusage_matching_implementation()
  local_mocked_bindings(appusage_match_self_report_table = function(...) stop("unexpected rematch"),
    appusage_matching_implementation = function(...) implementation)
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_true(file.exists(export))
  expect_identical(tools::md5sum(sources), before)
})

test_that("explicit worker override is propagated without changing scientific configuration", {
  config <- appusage_config(execution = list(workers = 8, parallel = TRUE))
  expect_identical(appusage_config_second_options(config), appusage_config_second_options(appusage_config()))
  f <- collection_fixture(1)
  original <- appusage_execute_pipeline
  options_seen <- NULL
  local_mocked_bindings(appusage_execute_pipeline = function(..., first_options = list()) {
    options_seen <<- first_options
    original(..., first_options = first_options)
  })
  result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 8)
  expect_identical(result$projects$status, "completed")
  expect_true(options_seen$worker_cap_override)
})

test_that("worksheet selection is revalidated against the requested registry", {
  f <- collection_fixture(1, matching = TRUE)
  book <- list(First = data.frame(seq = c(1L, 99L), upload = c(basename(f$paths), "absent.txt")),
    Second = data.frame(seq = c(1L, 99L, 100L), upload = c(basename(f$paths), "absent.txt", "missing.txt")))
  openxlsx::write.xlsx(book, f$registry$self_report_file, overwrite = TRUE)
  initial <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_identical(initial$projects$status, "completed")
  files <- list.files(f$registry$output_dir, pattern = "[.]rda$", full.names = TRUE, recursive = TRUE)
  before <- tools::md5sum(files)
  f$registry$sheet <- "First"
  equivalent <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_identical(equivalent$projects$status, "skipped_complete")
  f$registry$sheet <- "Second"
  changed <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
  expect_identical(changed$projects$status, "completed")
  expect_identical(tools::md5sum(files), before)
  link <- readRDS(file.path(f$registry$output_dir, "self_report_link_result.rds"))
  expect_equal(nrow(link$matched_self_report), 3L)
})

test_that("delivery covers every raw format and rejects scientific setting changes", {
  for (type in c("line", "meta", "day", "app")) {
    f <- collection_fixture(1)
    file.copy(test_path("fixtures", paste0(type, "_sample.txt")), f$paths, overwrite = TRUE)
    result <- run_appusage_projects(f$registry, f$batch, f$config, workers = 1)
    expect_identical(result$projects$status, "completed", info = type)
    before <- tools::md5sum(list.files(f$registry$output_dir,
      pattern = "[.](rda|json)$", recursive = TRUE, full.names = TRUE))
    changed <- appusage_config(time = list(tz = "UTC"), execution = list(progress = FALSE))
    result <- run_appusage_projects(f$registry, f$batch, changed, workers = 1)
    expect_identical(result$projects$status, "blocked", info = type)
    expect_identical(tools::md5sum(names(before)), before)
  }
})
