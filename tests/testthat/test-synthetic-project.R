synthetic_teaching_root <- function() {
  system.file("extdata", "synth", package = "appusageR", mustWork = TRUE)
}

test_that("the fictional questionnaire preserves values, missingness and identifiers", {
  root <- synthetic_teaching_root()
  csv <- read.csv(file.path(root, "survey.csv"), colClasses = "character",
                  na.strings = "", check.names = FALSE)
  book <- as.data.frame(readxl::read_excel(
    file.path(root, "ProjectID-009-WJXraw-synthetic.xlsx"), sheet = "Survey"))
  expect_identical(names(book), names(csv))
  expect_identical(nrow(book), 17L)
  expect_identical(book$sequence_id, csv$sequence_id)
  expect_identical(book$survey_id, csv$survey_id)
  expect_identical(book$upload, csv$upload)
  for (field in c("age_years", "sleep_hours", "phone_minutes_day")) {
    expect_equal(book[[field]], as.numeric(csv[[field]]))
  }
  expect_identical(format(book$submitted_at, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                   csv$submitted_at)
  expect_true(is.na(book$sleep_hours[6]))
  expect_identical(book$phone_minutes_day[16], 0)
  expect_identical(sum(is.na(book$upload)), 1L)
})

test_that("synthetic formats reproduce independently specified counts and durations", {
  root <- synthetic_teaching_root()
  expected <- read.csv(file.path(root, "scenarios.csv"), stringsAsFactors = FALSE)
  project <- file.path(root, "ProjectName-Demo_ProjectID-009")
  expect_setequal(list.files(project, pattern = "[.]txt$"), expected$source_basename)
  expect_identical(nrow(expected), 15L)
  parsed <- list()
  for (i in seq_len(nrow(expected))) {
    path <- file.path(project, expected$source_basename[i])
    if (expected$expected_status[i] == "error") {
      expect_error(run_first_level_appusage(path, strict = TRUE),
                    class = expected$expected_failure_class[i])
      next
    }
    first <- run_first_level_appusage(path, strict = TRUE)
    second <- run_second_level_appusage(first)
    expect_identical(second$status, "success")
    for (grain in c("event", "episode", "daily")) {
      expect_equal(nrow(second$data[[grain]]), expected[[paste0("expected_", grain, "_rows")]][i])
    }
    daily <- second$data$daily
    expect_equal(sum(daily$duration_ms[daily$package_name != "ALL"]),
                  expected$expected_app_duration_ms[i])
    daily$participant_id <- as.character(expected$sequence_id[i])
    qc <- qc_appusage_day(daily)
    expect_equal(qc$n_nonempty_days, expected$expected_nonempty_days[i])
    expect_identical(qc$pass_qc, expected$expected_pass_qc[i] == "true")
    parsed[[expected$scenario[i]]] <- second
  }
  mixed <- parsed$mixed_line_meta
  expect_equal(mixed$data$event$event_type, rep(c(1, 2, 1, 2), 7))
  expect_true(all(diff(mixed$data$event$source_line) > 0))
  expect_true(all(mixed$data$event$configuration_tail ==
                    "orientation=portrait,demo=synthetic"))
  expect_identical(mixed$data$episode$package_name,
                   parsed$line_seven_days$data$episode$package_name)
  expect_equal(mixed$data$episode$duration_ms,
                parsed$line_seven_days$data$episode$duration_ms)
  midnight <- parsed$cross_midnight$data$daily
  expect_equal(midnight$duration_ms[order(midnight$date)], c(300000, 420000))
  overlap <- audit_appusage_overlap(parsed$overlapping_intervals)
  expect_equal(unname(overlap$totals[["endpoint_union"]]), 14000)
  coverage <- parsed$recorded_blank_absent_days
  window <- as.Date(c("2025-10-01", "2025-10-04"))
  observed <- audit_appusage_days(coverage, window = window)
  zero <- audit_appusage_days(coverage, window = window, strategy = "zero_window")
  expect_equal(observed$numerator_ms, 300000)
  expect_equal(observed$denominator_days, 2)
  expect_equal(observed$mean_ms, 150000)
  expect_equal(zero$denominator_days, 4)
  expect_equal(zero$mean_ms, 75000)
})
