test_that("safe date conversion retains vector positions with malformed dates", {
  x <- c("4674/19-0", "2024/2/29", NA_character_, "2023-02-29", "2025-9-27",
    "2025/29-0", "unrelated text", "record 2021-04-01 annotation", "2024-00-01")
  expected <- as.Date(c(NA, "2024-02-29", NA, NA, "2025-09-27", NA, NA,
    "2021-04-01", NA), format = "%Y-%m-%d")
  expect_equal(safe_as_date(x), expected)
  expect_equal(safe_as_date(c("4674/19-0", "3686/1-0")),
    as.Date(c(NA_character_, NA_character_)))
  expect_equal(safe_as_date(character()), as.Date(character()))
  expect_s3_class(safe_as_date("4674/19-0"), "Date")
  expect_equal(safe_as_date(factor(c("2024-2-29", "invalid"))),
    as.Date(c("2024-02-29", NA), format = "%Y-%m-%d"))
})

test_that("unknown content with malformed dates retains its input failure", {
  lines <- c("asset/4674/19-0", "asset/3686/1-0", "modified 2021-04-01")
  expect_equal(detect_appusage_type(lines, input = "lines"), "unknown")
  root <- tempfile("invalid-date-preflight-")
  dir.create(root)
  path <- file.path(root, "invalid_date_tokens.txt")
  writeLines(lines, path, useBytes = TRUE)
  result <- read_appusage_batch(path, ids = "synthetic", output_dir = root,
    project_name = "Synthetic", project_id = "dates", progress = FALSE)
  expect_equal(result$status, "error")
  expect_equal(result$failure_family, "source_unknown_content")
  expect_match(result$error_class, "appusage_unknown_content")
  expect_true(is.na(result$data_file))
  expect_true(file.exists(result$metadata_file))
})

test_that("malformed unrelated date tokens do not obscure valid line exports", {
  lines <- readLines(test_path("fixtures", "line_sample.txt"), warn = FALSE,
    encoding = "UTF-8")
  expect_equal(detect_appusage_type(c("asset/4674/19-0", lines), input = "lines"), "line")
})
