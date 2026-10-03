# Reproduce the user-approved exception without any real input files.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
root <- args[[1L]]
pkgload::load_all(file.path(root, "candidate-source"), quiet = TRUE)
old <- new.env(parent = globalenv())
for (file in list.files(file.path(root, "baseline-source/R"), pattern = "[.]R$", full.names = TRUE)) sys.source(file, old)
rows <- list()
for (type in c("line", "meta", "day", "app")) {
  fixture <- file.path(root, "candidate-source/tests/testthat/fixtures", paste0(type, "_sample.txt"))
  text <- paste(readLines(fixture, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  for (encoding in c("GBK", "GB18030", "CP936")) {
    file <- tempfile(fileext = ".txt")
    writeBin(stringi::stri_encode(text, from = "UTF-8", to = encoding, to_raw = TRUE)[[1L]], file)
    before <- old$run_first_level_appusage(file, encoding = encoding, participant_id = "synthetic")
    automatic <- old$run_first_level_appusage(file, encoding = "auto", participant_id = "synthetic")
    after <- run_first_level_appusage(file, encoding = encoding, participant_id = "synthetic")
    stopifnot(automatic$status == "success", after$status == "success", identical(automatic$data, after$data))
    rows[[length(rows)+1L]] <- data.frame(type = type, encoding = encoding,
      old_explicit_status = before$status, old_auto_status = automatic$status,
      candidate_explicit_status = after$status, exact_data_equals_old_auto = TRUE)
    unlink(file)
  }
}
write.csv(do.call(rbind, rows), file.path(root, "approved-encoding-exception.csv"), row.names = FALSE)
cat("12 synthetic explicit-encoding entries equal the old automatic-decoding data\n")
