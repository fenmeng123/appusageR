test_that("parse_day returns canonical day-level columns", {
  path <- testthat::test_path("fixtures", "day_sample.txt")
  out <- parse_day(path, participant_id = "p1")

  expect_s3_class(out, "tbl_df")
  expect_named(out, c(
    "participant_id", "source_file", "export_type", "date", "weekday",
    "app_name", "package_name", "duration_text", "duration_ms",
    "duration_min", "open_count", "notification_count", "split_screen_ms",
    "is_all_apps", "is_collection_app", "parse_warning"
  ))
  expect_true(any(out$is_all_apps))
  expect_true(any(out$is_collection_app))
})

test_that("parse_day parses ALL row, counts, and split-screen duration", {
  path <- testthat::test_path("fixtures", "day_sample.txt")
  out <- parse_day(path)
  all_row <- out[out$is_all_apps & out$date == as.Date("2024-10-07"), ]

  expect_equal(all_row$duration_ms, 10800000)
  expect_equal(all_row$open_count, 20)
  expect_equal(all_row$notification_count, 5)
  expect_equal(all_row$split_screen_ms, 38541)
})

test_that("parse_app converts scientific notation duration_ms", {
  path <- testthat::test_path("fixtures", "app_sample.txt")
  out <- parse_app(path)

  expect_equal(out$duration_ms[[1]], 11447863)
  expect_false(out$is_all_apps[[1]])
  expect_equal(out$package_name[[1]], "com.tencent.mm")
})

test_that("day and app parsers do not use duration_text as numeric source", {
  day <- parse_day(testthat::test_path("fixtures", "day_sample.txt"))
  app <- parse_app(testthat::test_path("fixtures", "app_sample.txt"))

  expect_equal(day$duration_ms[day$app_name == "APP Usage"], 5000)
  expect_equal(app$duration_ms[[1]], 11447863)
})
