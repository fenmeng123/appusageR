test_that("the installed demonstration reproduces the same synthetic sources", {
  env <- new.env(parent = baseenv())
  data("appusage_demo", package = "appusageR", envir = env)
  demo <- env$appusage_demo
  expect_identical(demo$info$project_id, "009")
  expect_identical(demo$info$scientific_schema, "0.3.5")
  expect_identical(nrow(demo$sources), 15L)
  expect_identical(length(demo$records), 10L)
  expect_identical(nrow(demo$survey), 17L)
  expect_identical(demo$survey$survey_id, demo$expected_links$survey_id)
  expect_identical(sum(demo$expected_links$expected_match_status == "matched"), 8L)
  expect_true(is.character(demo$sources$sequence_id))
  expect_true(is.na(demo$survey$sleep_hours[6L]))
  expect_identical(demo$survey$phone_minutes_day[16L], 0)
  root <- system.file("extdata", "synth", package = "appusageR", mustWork = TRUE)
  selected <- which(demo$sources$expected_status == "success")
  for (i in selected) {
    record <- demo$records[[demo$sources$scenario[i]]]
    fresh <- run_second_level_appusage(run_first_level_appusage(
      file.path(root, demo$sources$source_relpath[i]), strict = TRUE))
    expect_identical(record, fresh)
    expect_true(is.na(record$data_file))
    expect_true(is.na(record$metadata_file))
    qc <- assess_appusage_qc(record, metadata = record$metadata,
                            participant_id = demo$sources$sequence_id[i])
    expect_identical(qc$qc$pass_qc, demo$sources$expected_pass_qc[i] == "true")
  }
  # The public reproduction script works without the development tools tree.
  # Inherit the test's package lookup in both pkgload and installed execution.
  regenerated <- new.env(parent = environment())
  sys.source(system.file("examples", "rebuild-appusage-demo.R",
                         package = "appusageR", mustWork = TRUE), envir = regenerated)
  expect_identical(regenerated$appusage_demo, demo)
})

test_that("demo data are portable and preserve empty-grain and date evidence", {
  env <- new.env(parent = baseenv())
  data("appusage_demo", package = "appusageR", envir = env)
  demo <- env$appusage_demo
  # Aggregate portability assertions avoid an expectation per data cell.
  unsafe <- character()
  walk <- function(x) {
    if (is.environment(x) || is.function(x) || typeof(x) == "externalptr")
      unsafe <<- c(unsafe, "runtime object")
    if (is.character(x) && any(grepl("^[A-Za-z]:[/\\\\]|^/|^\\\\\\\\", x), na.rm = TRUE))
      unsafe <<- c(unsafe, "absolute path")
    if (is.list(x)) invisible(lapply(x, walk))
    if (length(attributes(x))) invisible(lapply(attributes(x), walk))
  }
  walk(demo)
  expect_length(unsafe, 0L)
  daily_only <- demo$records$app_partial_coverage$data
  expect_identical(nrow(daily_only$event), 0L)
  expect_identical(nrow(daily_only$episode), 0L)
  expect_s3_class(daily_only$daily$date, "Date")
  mixed <- demo$records$mixed_line_meta$data
  expect_s3_class(mixed$event$event_datetime, "POSIXct")
  expect_identical(attr(mixed$event$event_datetime, "tzone"), "Asia/Shanghai")
  audit <- audit_appusage_days(demo$records$recorded_blank_absent_days,
                              window = as.Date(c("2025-10-01", "2025-10-04")))
  expect_equal(audit$denominator_days, 2)
  expect_equal(audit$mean_ms, 150000)
})
