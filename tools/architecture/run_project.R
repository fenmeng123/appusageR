# Run only the explicitly configured project; never discover a corpus here.
# Usage: Rscript run_project.R /private/run-settings.rds /private/results
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
settings <- readRDS(args[[1]])
destination <- args[[2]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
library(appusageR)
stopifnot(as.character(packageVersion("appusageR")) == "0.3.6")
allowed <- names(formals(run_appusage_project_workflow))
stopifnot(all(names(settings) %in% allowed))
started <- Sys.time()
writeLines(c(R.version.string, find.package("appusageR"), capture.output(sessionInfo())),
  file.path(destination, "runtime.txt"))
result <- do.call(run_appusage_project_workflow, settings)
saveRDS(result, file.path(destination, "workflow_result.rds"))
saveRDS(list(started_at = started, finished_at = Sys.time(),
  elapsed_sec = as.numeric(difftime(Sys.time(), started, units = "secs")),
  project_dir = result$project_dir), file.path(destination, "run_timing.rds"))
cat("Workflow finished. Sources:", nrow(result$first_level), "\n")
print(table(result$first_level$status, useNA = "ifany"))
print(table(result$second_level$second_level_status, useNA = "ifany"))
print(table(result$qc$qc_status, useNA = "ifany"))
