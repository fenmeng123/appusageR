# Verify no-change reuse and a single selected missing proc-2 JSON recovery.
# Usage: Rscript audit_resume.R /private/run-settings.rds /private/project /private/audit
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
library(appusageR)
settings <- readRDS(args[[1]])
project <- normalizePath(args[[2]], winslash = "/", mustWork = TRUE)
destination <- args[[3]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
paths <- unlist(lapply(c("proclevel-1", "proclevel-2"), function(stage)
  list.files(file.path(project, stage), pattern = "[.](rda|json)$", full.names = TRUE)), use.names = FALSE)
before <- tools::md5sum(paths)
settings$resume <- TRUE
settings$overwrite <- FALSE
settings$progress <- FALSE
start <- Sys.time()
result <- do.call(run_appusage_project_workflow, settings)
elapsed <- as.numeric(difftime(Sys.time(), start, units = "secs"))
stopifnot(identical(before, tools::md5sum(paths)))
first <- result$first_level
chosen <- which(first$status == "success")[[1]]
ns <- asNamespace("appusageR")
pair <- get("second_level_expected_paths", ns)(first$data_file[[chosen]], file.path(project, "proclevel-2"))
backup <- file.path(destination, "selected_proc2_before_recovery.json")
stopifnot(file.copy(pair$json_file, backup, overwrite = FALSE))
# This is the disposable output project's one selected generated JSON, never raw input.
stopifnot(startsWith(normalizePath(pair$json_file, winslash = "/"), paste0(project, "/proclevel-2/")))
stopifnot(file.remove(pair$json_file))
repaired <- tryCatch(do.call(run_appusage_project_workflow, settings), error = function(e) {
  if (!file.exists(pair$json_file)) file.copy(backup, pair$json_file)
  stop(e)
})
after <- tools::md5sum(paths)
other <- !paths %in% c(pair$rda_file, pair$json_file)
stopifnot(identical(before[other], after[other]), file.exists(pair$json_file))
env <- new.env(parent = emptyenv())
stopifnot(identical(load(pair$rda_file, env), "data"))
source <- get("load_appusage_data_object", ns)(first$data_file[[chosen]])
config <- readRDS(file.path(project, "appusage_configuration.rds"))
options <- get("appusage_config_second_options", ns)(config)
scientific <- options[intersect(names(options), names(formals(make_second_level_appusage)))]
expected <- do.call(make_second_level_appusage,
  c(list(data = source, export_type = first$detected_type[[chosen]]), scientific))
stopifnot(identical(env$data, expected))
jsonlite::write_json(list(no_change_artifacts = length(paths), no_change_elapsed_sec = elapsed,
  targeted_sources = 1L, untouched_other_artifacts = sum(other),
  recovered_research_values_identical = TRUE, status = "passed"),
  file.path(destination, "resume_audit.json"), pretty = TRUE, auto_unbox = TRUE)
cat("No-change and targeted resume audits passed.\n")
