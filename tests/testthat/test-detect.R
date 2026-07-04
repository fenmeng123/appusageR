test_that("detect_appusage_type detects all four formats", {
  fixture <- testthat::test_path("fixtures")
  expect_equal(detect_appusage_type(file.path(fixture, "line_sample.txt")), "line")
  expect_equal(detect_appusage_type(file.path(fixture, "meta_sample.txt")), "meta")
  expect_equal(detect_appusage_type(file.path(fixture, "day_sample.txt")), "day")
  expect_equal(detect_appusage_type(file.path(fixture, "app_sample.txt")), "app")
  expect_equal(detect_appusage_type(file.path(fixture, "malformed.txt")), "unknown")
})
