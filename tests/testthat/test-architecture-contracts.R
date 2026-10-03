architecture_project <- function(n = 1L) {
  root <- tempfile("architecture-")
  source <- file.path(root, "ProjectName-Synthetic_ProjectID-036")
  dir.create(source, recursive = TRUE)
  files <- file.path(source, sprintf("%d_AppUsage_line_2024_1_2_3_4_5.txt", seq_len(n)))
  for (path in files) file.copy(test_path("fixtures", "line_sample.txt"), path)
  list(source = source, files = files, output = file.path(root, "output"))
}

architecture_run <- function(fixture, ...) {
  run_appusage_project_workflow(
    project_dir = fixture$source, output_root = fixture$output,
    progress = FALSE, diagnostic_verbosity = "none", ...
  )
}

test_that("single and batch entry points share source cache identity", {
  f <- architecture_project()
  single <- run_first_level_appusage(f$files, participant_id = "1",
    output_dir = file.path(f$output, "single"))
  batch <- read_appusage_batch(f$files, ids = "1", output_dir = f$output,
    project_name = "Batch", project_id = "036", progress = FALSE)
  expect_identical(basename(single$data_file), basename(batch$data_file))
  expect_identical(single$metadata$identity$source_record_key, batch$source_record_key[[1]])
})

test_that("project timezone reaches scientific data, not just configuration", {
  f <- architecture_project()
  result <- architecture_run(f, tz = "UTC")
  paths <- list.files(file.path(result$project_dir, "proclevel-2"), "[.]rda$", full.names = TRUE)
  expect_length(paths, 1L)
  data <- load_appusage_data_object(paths[[1]])
  expect_identical(attr(data, "effective_timezone"), "UTC")
  expect_identical(attr(data$episode$start_datetime, "tzone"), "UTC")
})

test_that("QC switch disables routine QC while retaining research data", {
  f <- architecture_project()
  result <- architecture_run(f, run_qc = FALSE)
  paths <- list.files(file.path(result$project_dir, "proclevel-2"), "[.]json$", full.names = TRUE)
  metadata <- jsonlite::read_json(paths[[1]], simplifyVector = TRUE)
  expect_false(identical(metadata$processing$qc_status, "success"))
  expect_null(result$qc)
  expect_true(file.exists(metadata$outputs$second_level_rda))
})

test_that("project resume repairs one missing pair even when summary exists", {
  f <- architecture_project(2L)
  result <- architecture_run(f)
  paths <- list.files(file.path(result$project_dir, "proclevel-2"), "[.]json$", full.names = TRUE)
  intact <- paths[[2]]
  before <- tools::md5sum(intact)
  unlink(paths[[1]])
  resumed <- architecture_run(f)
  expect_true(file.exists(paths[[1]]))
  expect_identical(tools::md5sum(intact), before)
  expect_equal(nrow(resumed$second_level), 2L)
})

test_that("first level resume joins by source rather than prior position", {
  f <- architecture_project(2L)
  initial <- read_appusage_batch(f$files, ids = c("1", "2"),
    output_dir = f$output, project_name = "Batch", project_id = "036", progress = FALSE)
  resumed <- read_appusage_batch(rev(f$files), ids = c("2", "1"),
    output_dir = f$output, project_name = "Batch", project_id = "036", progress = FALSE,
    resume = TRUE)
  expect_identical(normalizePath(resumed$source_file, winslash = "/"),
    normalizePath(rev(f$files), winslash = "/"))
  expect_identical(resumed$source_record_key, rev(initial$source_record_key))
})

test_that("independent QC preserves source matching annotations", {
  f <- architecture_project()
  result <- architecture_run(f)
  path <- file.path(result$project_dir, "analytic_summary_table_proclevel-2.csv")
  summary <- appusage_read_summary_csv(path)
  summary$self_report_match_status <- "matched"
  summary$self_report_sequence_id <- 1L
  utils::write.csv(summary, path, row.names = FALSE, na = "")
  qc <- write_qc_metadata_batch(result$project_dir, progress = FALSE)
  expect_identical(qc$self_report_match_status, "matched")
  expect_equal(qc$self_report_sequence_id, 1L)
})
