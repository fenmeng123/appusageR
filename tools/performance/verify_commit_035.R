# Verify commit preparation against the measured implementation without raw I/O.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
work <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
snapshot <- file.path(work, "candidate-plus-v2-source")
files <- sort(list.files("R", pattern = "[.]R$", full.names = TRUE))
stopifnot(identical(basename(files), sort(list.files(file.path(snapshot, "R"), pattern = "[.]R$"))))
for (file in files) {
  stopifnot(identical(parse(file, keep.source = FALSE),
    parse(file.path(snapshot, file), keep.source = FALSE)))
}
devtools::load_all(quiet = TRUE)
expected <- jsonlite::read_json(file.path(work, "latest-only/summary.json"))
stopifnot(identical(appusageR:::appusage_parser_implementation_fingerprint(), expected$parser_fingerprint),
  identical(appusageR:::appusage_second_level_implementation_fingerprint(), expected$second_fingerprint))
cat(length(files), "R files have identical parsed expressions to the measured snapshot; both fingerprints match\n")
result <- testthat::test_file("tests/testthat/test-text-engine.R", reporter = "summary")
counts <- as.data.frame(result)
stopifnot(sum(counts$failed) == 0, !any(counts$error), sum(counts$warning) == 0)
jsonlite::write_json(list(status = "success", identical_r_files = length(files),
  parser_fingerprint = expected$parser_fingerprint, second_fingerprint = expected$second_fingerprint,
  text_expectations = sum(counts$passed), failures = sum(counts$failed), warnings = sum(counts$warning)),
  file.path(work, "commit-verification-2026-10-03.json"), auto_unbox = TRUE, pretty = TRUE)
