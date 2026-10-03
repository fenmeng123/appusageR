# Evidence used by the measurement gate; run only after the final checks finish.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- args[[1L]]
library(testthat)
tests <- as.data.frame(readRDS(file.path(root, "test-results.rds")))
counts <- colSums(tests[c("failed", "warning", "skipped", "error", "passed")])
stopifnot(counts[["failed"]] == 0, counts[["warning"]] == 0, counts[["error"]] == 0)
check_log <- readLines(file.path(root, "check-final/appusageR.Rcheck/00check.log"), warn = FALSE)
stopifnot(any(grepl("^Status: OK", check_log)))
ip <- installed.packages()
ip <- ip[!duplicated(ip[, "Package"]), , drop = FALSE]
# stringr is deliberately included to pin the old version's dependency closure.
packages <- unique(c("cli", "jsonlite", "openxlsx", "readxl", "stringi", "tibble", "stringr"))
packages <- sort(unique(c(packages, unlist(tools::package_dependencies(packages, db = ip,
  which = c("Depends", "Imports", "LinkingTo"), recursive = TRUE)))))
versions <- vapply(packages, function(p) as.character(packageVersion(p)), character(1))
jsonlite::write_json(as.list(versions), file.path(root, "dependency-lock.json"), auto_unbox = TRUE, pretty = TRUE)
jsonlite::write_json(as.list(counts), file.path(root, "validation-counts.json"), auto_unbox = TRUE, pretty = TRUE)
print(counts)
