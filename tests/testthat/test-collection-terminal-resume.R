test_that("collection resume retains all-failed, partial-failed and successful deliveries", {
  demo_env <- new.env(parent = baseenv())
  data("appusage_demo", package = "appusageR", envir = demo_env)
  sources <- demo_env$appusage_demo$sources
  input <- system.file("extdata", "synth", package = "appusageR", mustWork = TRUE)
  root <- tempfile("collection-resume-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  cases <- list(
    all_failed = which(sources$expected_status == "error"),
    partial_failed = which(sources$scenario %in% c("line_seven_days", "mixed_line_meta",
      "empty_mixed_export", "zero_byte_upload")),
    successful = which(sources$scenario %in% c("line_seven_days", "mixed_line_meta")))
  registry <- data.frame(project_id = c("001", "002", "003"),
    project_name = names(cases), project_dir = file.path(root, paste0("raw-", names(cases))),
    output_dir = file.path(root, paste0("output-", names(cases))))
  for (i in seq_along(cases)) {
    dir.create(registry$project_dir[i])
    expect_true(all(file.copy(file.path(input, sources$source_relpath[cases[[i]]]),
                              registry$project_dir[i])))
  }
  config <- appusage_config(execution = list(progress = FALSE))
  batch <- file.path(root, "batch")
  initial <- run_appusage_projects(registry, batch, config, workers = 1L)
  expect_identical(initial$projects$status,
    c("completed_with_source_issues", "completed_with_source_issues", "completed"))
  expect_equal(initial$projects$source_issues, c(5, 2, 0))
  expected_success <- c(0L, 2L, 2L)
  for (i in seq_along(cases)) {
    result <- run_appusage_workflow(project_dir = registry$output_dir[i])
    wanted <- sources[cases[[i]], ]
    at <- match(basename(result$first_level$source_file), wanted$source_basename)
    expect_false(anyNA(at))
    expect_identical(result$first_level$status, wanted$expected_status[at])
    expect_equal(sum(result$first_level$status == "success"), expected_success[i])
    expect_equal(sum(result$qc$qc_status == "success"), expected_success[i])
    expect_equal(sum(result$qc$pass_qc %in% TRUE), expected_success[i])
    expect_equal(sum(result$qc$qc_status == "not_run"), nrow(wanted) - expected_success[i])
    expect_true(all(is.na(result$qc$pass_qc[result$qc$qc_status == "not_run"])))
    expect_true(validate_appusage_project(registry$output_dir[i], verify = "content")$complete)
  }
  # CSV inference makes a wholly missing path column logical. Keep that real
  # representation in this public-entry regression rather than coercing input.
  first <- appusage_read_summary_csv(file.path(registry$output_dir[1L],
    "analytic_summary_table_proclevel-1.csv"))
  expect_type(first$data_file, "logical")
  expect_true(all(is.na(first$data_file)))
  snapshot <- function() {
    paths <- sort(unlist(lapply(registry$output_dir, list.files,
      full.names = TRUE, recursive = TRUE), use.names = FALSE))
    tools::md5sum(paths)
  }
  before <- snapshot()
  # No pipeline stage, including parsing, scientific processing, QC or matching,
  # should be entered by either unchanged recovery call.
  local_mocked_bindings(appusage_execute_pipeline = function(...) stop("unexpected pipeline execution"))
  for (read_only in c(FALSE, TRUE)) {
    registry$read_only <- read_only
    resumed <- run_appusage_projects(registry, batch, config, workers = 1L,
      continue_on_error = FALSE)
    expect_identical(resumed$projects$status, rep("skipped_complete", 3L))
    expect_identical(resumed$projects$action, rep("reuse", 3L))
    expect_equal(resumed$projects$source_issues, c(5, 2, 0))
    expect_equal(resumed$projects$workflow_elapsed_sec, rep(0, 3L))
    expect_identical(snapshot(), before)
  }
})

test_that("missing path values do not remove protection for existing scientific artifacts", {
  root <- tempfile("collection-protection-")
  dir.create(root)
  withr::defer(unlink(root, recursive = TRUE))
  path <- file.path(root, "saved.rda")
  data <- list(synthetic = TRUE)
  save(data, file = path)
  report <- list(tasks = data.frame(source_file = "source.txt", stage = "parse",
    action = "run", reason_code = "implementation_changed"))
  for (missing_path in list(NA, NA_character_)) {
    failed <- data.frame(status = "error", data_file = missing_path, source_file = "failed.txt")
    expect_identical(appusage_collection_protected_sources(report, failed, root), character())
    success <- data.frame(status = "success", data_file = path, source_file = "source.txt")
    expect_identical(appusage_collection_protected_sources(report, rbind(failed, success), root), "source.txt")
    orphan <- data.frame(status = "incomplete", data_file = path, source_file = NA_character_)
    expect_identical(appusage_collection_protected_sources(report, rbind(failed, orphan), root),
      "unidentified_scientific_artifacts")
  }
  dir.create(file.path(root, "proclevel-1"))
  file.copy(path, file.path(root, "proclevel-1", "orphan.rda"))
  expect_identical(appusage_collection_protected_sources(report, data.frame(), root),
    "unidentified_scientific_artifacts")
})
