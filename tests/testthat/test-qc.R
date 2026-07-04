test_that("qc_appusage_day uses summed app rows instead of likely total ALL rows", {
  data <- tibble::tibble(
    participant_id = "p1",
    date = as.Date("2024-10-07") + 0:6,
    app_name = "所有应用",
    package_name = "ALL",
    duration_ms = rep(3000, 7),
    is_all_apps = TRUE,
    is_collection_app = FALSE
  )
  app_rows <- tibble::tibble(
    participant_id = "p1",
    date = rep(as.Date("2024-10-07") + 0:6, each = 2),
    app_name = rep(c("App A", "App B"), 7),
    package_name = rep(c("com.a", "com.b"), 7),
    duration_ms = rep(c(1000, 2000), 7),
    is_all_apps = FALSE,
    is_collection_app = FALSE
  )
  qc <- qc_appusage_day(rbind(data, app_rows))

  expect_equal(qc$n_nonempty_days, 7)
  expect_true(qc$pass_qc)
})

test_that("qc_appusage_day can exclude collection app from totals", {
  data <- tibble::tibble(
    participant_id = "p1",
    date = as.Date("2024-10-07") + 0:6,
    app_name = "屏幕使用时间",
    package_name = "com.w.appusage",
    duration_ms = rep(1000, 7),
    is_all_apps = FALSE,
    is_collection_app = TRUE
  )

  qc_included <- qc_appusage_day(data, include_collection_app = TRUE)
  qc_excluded <- qc_appusage_day(data, include_collection_app = FALSE)

  expect_equal(qc_included$n_nonempty_days, 7)
  expect_equal(qc_excluded$n_nonempty_days, 0)
})

test_that("qc_appusage_day flags fewer than seven non-empty days", {
  data <- tibble::tibble(
    participant_id = "p1",
    date = as.Date("2024-10-07") + 0:5,
    duration_ms = rep(1000, 6),
    is_all_apps = FALSE,
    is_collection_app = FALSE
  )
  qc <- qc_appusage_day(data)

  expect_false(qc$pass_min_days)
  expect_false(qc$pass_qc)
})

test_that("qc_appusage_day flags missing weekday coverage", {
  data <- tibble::tibble(
    participant_id = "p1",
    date = as.Date("2024-10-07") + c(0:5, 7),
    duration_ms = rep(1000, 7),
    is_all_apps = FALSE,
    is_collection_app = FALSE
  )
  qc <- qc_appusage_day(data)

  expect_true(qc$pass_min_days)
  expect_false(qc$pass_all_weekdays)
  expect_false(qc$pass_qc)
})

test_that("filter_valid_appusage keeps only passing participants", {
  data <- tibble::tibble(
    participant_id = c("p1", "p2"),
    date = as.Date("2024-10-07"),
    duration_ms = c(1000, 1000)
  )
  qc <- tibble::tibble(
    participant_id = c("p1", "p2"),
    pass_qc = c(TRUE, FALSE)
  )

  out <- filter_valid_appusage(data, qc)
  expect_equal(out$participant_id, "p1")
})
