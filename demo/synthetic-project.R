library(appusageR)

# Fifteen fictional uploads: ten usable logs and five diagnosed input failures.
demo_input <- system.file("extdata", "synth", package = "appusageR",
                          mustWork = TRUE)
demo_output <- tempfile("appusage-synthetic-")

# Files within this project use two PSOCK workers. QC uses the package defaults.
demo_result <- run_appusage_project_workflow(
  raw_data_root = demo_input, project_id = "009", output_root = demo_output,
  self_report_sheet = "Survey", sequence_col = "sequence_id",
  upload_col = "upload", submit_time_col = "submitted_at",
  parallel = TRUE, n_cores = 2L, progress = FALSE, strict = FALSE
)
data.frame(source = basename(demo_result$first_level$source_file),
           status = demo_result$first_level$status,
           worker_pid = demo_result$first_level$worker_pid)
demo_result$qc[, c("participant_id", "qc_status", "pass_qc")]
demo_result$matched_self_report[, c("survey_id", "moSens_match_status",
                                   "moSens_data_dir")]

# Both keys (upload sequence and filename) are required. All survey rows survive.
expected_links <- read.csv(file.path(demo_input, "expected-links.csv"),
                           colClasses = "character")
stopifnot(
  identical(demo_result$matched_self_report$survey_id, expected_links$survey_id),
  identical(demo_result$matched_self_report$moSens_match_status,
            expected_links$expected_match_status)
)

# Read a successful paired cache in its own environment.
cache <- new.env(parent = emptyenv())
load(demo_result$second_level$second_level_data_file[[1L]], envir = cache)
head(cache$data$daily[, c("date", "app_name", "duration_ms")])

# Keep the temporary results for inspection; remove them when finished:
# unlink(demo_output, recursive = TRUE)
