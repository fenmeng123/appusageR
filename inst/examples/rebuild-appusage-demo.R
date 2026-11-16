# Reconstruct appusage_demo from the bundled fictional project (GPL-3).
# Run with an installed appusageR package. This script creates an in-memory
# object only; it does not write to the package installation or create caches.
local({
  root <- system.file("extdata", "synth", package = "appusageR", mustWork = TRUE)
  sources <- utils::read.csv(file.path(root, "scenarios.csv"),
                      colClasses = c(sequence_id = "character"),
                      stringsAsFactors = FALSE)
  sources$source_relpath <- file.path("ProjectName-Demo_ProjectID-009",
                                     sources$source_basename)
  selected <- which(sources$expected_status == "success")
  records <- lapply(selected, function(i) {
    first <- appusageR::run_first_level_appusage(
      file.path(root, sources$source_relpath[i]), strict = TRUE,
      tz = "Asia/Shanghai")
    appusageR::run_second_level_appusage(first, tz = "Asia/Shanghai")
  })
  names(records) <- sources$scenario[selected]
  survey <- utils::read.csv(file.path(root, "survey.csv"), na.strings = "",
    colClasses = c("character", "character", "numeric", "numeric", "numeric",
                   "character", "character"), stringsAsFactors = FALSE)
  expected_links <- utils::read.csv(file.path(root, "expected-links.csv"),
                             colClasses = "character")
  result <- list(
    info = list(project_id = "009", scientific_schema = "0.3.5",
      timezone = "Asia/Shanghai", source_root = "extdata/synth",
      origin = "Deterministic fictional teaching logs and questionnaire",
      license = "GPL-3"),
    sources = sources, records = records, survey = survey,
    expected_links = expected_links)
  assign("appusage_demo", result, envir = parent.env(environment()))
})
