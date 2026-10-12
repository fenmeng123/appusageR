test_that("numeric Excel cells accept equivalent notation without a tolerance", {
  equal <- appusage_delivery_excel_cells_equal
  expect_true(equal("0.0000001", 1e-7))
  expect_true(equal("1e-07", 1e-7))
  expect_true(equal(c("2.5e+00", "-1.25E+01", "0007"), c(2.5, -12.5, 7)))
  expect_true(equal(c("0.0000001", NA_character_), c(1e-7, NA_real_)))
  expect_true(equal(character(), numeric()))
  expect_false(equal("0.0000002", 1e-7))
  expect_false(equal("1.00000000000001", 1))
  expect_false(equal("", 1e-7))
  expect_false(equal("0", NA_real_))
  expect_false(equal(" 0.0000001", 1e-7))
  expect_false(equal("0.0000001\n", 1e-7))
  expect_false(equal("1e999", 1))
  expect_false(equal("not numeric", 1))
  expect_false(equal(c("1", "2"), 1))
})

test_that("literal strings and date/time cells retain their existing comparisons", {
  equal <- appusage_delivery_excel_cells_equal
  expect_false(equal("0.0000001", "1e-07"))
  expect_false(equal("0.0000001", factor("1e-07")))
  expect_true(equal("1e-07", "1e-07"))
  date <- as.Date("2024-01-01")
  expect_true(equal(appusage_delivery_excel_text(date), date))
  expect_false(equal("4.5292e4", date))
  time <- as.POSIXct("2024-01-01 00:00:00", tz = "Asia/Shanghai")
  expect_true(equal(appusage_delivery_excel_text(time), time))
  expect_false(equal("4.5292e4", time))
})

test_that("real workbook roundtrips accept small numbers and reject changed values", {
  values <- data.frame(row_id = seq_len(3L), number = c(1e-7, 1.25, NA_real_),
    text = c("1e-07", "literal", NA_character_), stringsAsFactors = FALSE)
  path <- tempfile(fileext = ".xlsx")
  appusage_write_xlsx(values, path)
  cells <- readxl::read_excel(path, col_types = "text", trim_ws = FALSE)
  expect_true(appusage_delivery_excel_cells_equal(cells$number, values$number))
  expect_true(appusage_delivery_excel_cells_equal(cells$text, values$text))
  cells$number[[1]] <- "0.0000002"
  expect_false(appusage_delivery_excel_cells_equal(cells$number, values$number))
  cells$text[[1]] <- "0.0000001"
  expect_false(appusage_delivery_excel_cells_equal(cells$text, values$text))
})

test_that("project delivery accepts numeric notation and detects export corruption", {
  root <- tempfile("numeric-delivery-"); dir.create(root)
  raw <- file.path(root, "raw"); dir.create(raw)
  name <- "1_AppUsage_line_2024_1_2_3_4_5.txt"
  file.copy(test_path("fixtures", "line_sample.txt"), file.path(raw, name))
  questionnaire <- file.path(root, "ProjectID-123_WJXraw-test.xlsx")
  appusage_write_xlsx(data.frame(seq = c(1L, 2L), upload = c(name, "missing.txt"),
    aversion = c(1e-7, 1.25)), questionnaire)
  out <- file.path(root, "output")
  registry <- data.frame(project_id = "123", project_name = "NumericCells",
    project_dir = raw, output_dir = out, self_report_file = questionnaire,
    sequence_col = "seq", upload_col = "upload", stringsAsFactors = FALSE)
  result <- run_appusage_projects(registry, file.path(root, "batch"),
    appusage_config(execution = list(progress = FALSE)), workers = 1L)
  expect_identical(result$projects$status, "completed")
  report <- validate_appusage_project(out, verify = "content")
  expect_true(report$complete)
  config <- readRDS(file.path(out, "appusage_configuration.rds"))
  link <- readRDS(file.path(out, "self_report_link_result.rds"))
  export <- appusage_matching_export_path(out, config)
  link$matched_self_report$aversion[[1]] <- 2e-7
  appusage_write_xlsx(link$matched_self_report, export)
  report <- validate_appusage_project(out, verify = "content")
  expect_false(report$complete)
  expect_true(any(report$checks$check == "questionnaire_export" & report$checks$status == "failed"))
})
