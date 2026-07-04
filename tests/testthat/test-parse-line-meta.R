test_that("parse_line computes duration from timestamps and sorts chronologically", {
  path <- testthat::test_path("fixtures", "line_sample.txt")
  out <- parse_line(path)

  expect_s3_class(out, "tbl_df")
  expect_equal(out$duration_ms[[1]], 12556)
  expect_equal(out$duration_text[[1]], "少于一分钟")
  expect_true(all(diff(out$start_ts_ms) >= 0))
})

test_that("parse_meta splits Table 1 and Table 2", {
  path <- testthat::test_path("fixtures", "meta_sample.txt")
  out <- parse_meta(path)

  expect_type(out, "list")
  expect_s3_class(out$summary, "tbl_df")
  expect_s3_class(out$events, "tbl_df")
  expect_equal(nrow(out$summary), 1)
  expect_equal(nrow(out$events), 2)
  expect_equal(out$summary$total_duration_ms, 1800000)
})

test_that("parse_meta maps event type 1 and 2", {
  path <- testthat::test_path("fixtures", "meta_sample.txt")
  out <- parse_meta(path)

  expect_equal(
    out$events$event_type_label,
    c("ACTIVITY_RESUMED_OR_MOVE_TO_FOREGROUND", "ACTIVITY_PAUSED_OR_MOVE_TO_BACKGROUND")
  )
  expect_true(all(is.na(out$events$configuration)))
})

test_that("parse_meta handles empty input lines", {
  out <- parse_meta(c("", ""), input = "lines")

  expect_equal(nrow(out$summary), 0)
  expect_equal(nrow(out$events), 0)
})

test_that("label_event_type preserves legacy foreground/background labels", {
  expect_equal(label_event_type(1), "ACTIVITY_RESUMED_OR_MOVE_TO_FOREGROUND")
  expect_equal(label_event_type(2), "ACTIVITY_PAUSED_OR_MOVE_TO_BACKGROUND")
})

test_that("label_event_type maps Android UsageEvents labels", {
  values <- c(0, 5, 7, 11, 15, 16, 17, 18, 19, 20, 23, 26, 27, 31)
  expected <- c(
    "NONE",
    "CONFIGURATION_CHANGE",
    "USER_INTERACTION",
    "STANDBY_BUCKET_CHANGED",
    "SCREEN_INTERACTIVE",
    "SCREEN_NON_INTERACTIVE",
    "KEYGUARD_SHOWN",
    "KEYGUARD_HIDDEN",
    "FOREGROUND_SERVICE_START",
    "FOREGROUND_SERVICE_STOP",
    "ACTIVITY_STOPPED",
    "DEVICE_SHUTDOWN",
    "DEVICE_STARTUP",
    "APP_COMPONENT_USED"
  )

  expect_equal(label_event_type(values), expected)
})

test_that("label_event_type preserves unknown labels and vector shape", {
  values <- c(1, 5, 999, NA, "")
  labels <- label_event_type(values)

  expect_length(labels, length(values))
  expect_equal(
    labels,
    c(
      "ACTIVITY_RESUMED_OR_MOVE_TO_FOREGROUND",
      "CONFIGURATION_CHANGE",
      "EVENT_TYPE_999",
      NA_character_,
      NA_character_
    )
  )
})

test_that("parse_meta does not reconstruct episodes", {
  parsed <- parse_meta(character(), input = "lines")

  expect_type(parsed, "list")
  expect_setequal(names(parsed), c("summary", "events"))
  expect_false(any(c("episode", "episodes") %in% names(parsed)))
})
