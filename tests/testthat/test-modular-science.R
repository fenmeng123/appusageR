test_that("configuration distinguishes scientific rules and execution controls", {
  cfg <- appusage_config(time = list(tz = "UTC"), execution = list(workers = 2L))
  expect_s3_class(cfg, "appusage_config")
  expect_identical(cfg$time$tz, "UTC")
  expect_error(appusage_config(qc = list(misspelled = TRUE)), "Unknown qc")
  expect_error(appusage_config(execution = list(workers = 0)), "positive integers")
  expect_error(appusage_config(reconstruction = list(reconstruct_meta = FALSE),
    daily = list(meta_daily_source = "episodes")), "require meta reconstruction")
  expect_error(appusage_config(category = list(enabled = TRUE)), "dictionary")
})

test_that("independent scientific modules reproduce the composed line and meta output", {
  for (type in c("line", "meta", "day", "app")) {
    first <- run_first_level_appusage(test_path("fixtures", paste0(type, "_sample.txt")))
    standardized <- standardize_appusage(first$data, tz = "UTC")
    episodes <- if (type == "meta") reconstruct_meta_episodes(standardized$meta_events,
      tz = "UTC") else NULL
    daily <- build_appusage_daily(standardized, episodes)
    composed <- make_second_level_appusage(first$data, tz = "UTC")
    expect_equal(appusage_order_daily(daily$daily), composed$daily)
    expect_identical(attr(composed, "effective_timezone"), "UTC")
    expect_identical(attr(composed$episode$start_datetime, "tzone"), "UTC")
    expect_identical(attr(composed, "daily_aggregation_self_check")$status, "success")
  }
})

test_that("complete in-memory QC agrees with the inline metadata result", {
  root <- tempfile("qc-module-")
  first <- run_first_level_appusage(test_path("fixtures", "line_sample.txt"),
    output_dir = file.path(root, "proclevel-1"))
  second <- write_second_level_appusage(first$data_file)
  data <- load_appusage_data_object(second)
  metadata <- jsonlite::read_json(second_level_metadata_path(second), simplifyVector = TRUE)
  result <- assess_appusage_qc(data, metadata = first$metadata)
  expect_equal(result$qc$pass_qc, metadata$qc$pass_qc)
  expect_equal(result$counts$n_episode_rows, metadata$counts$n_episode_rows)
  expect_equal(result$anomaly_qc$n_anomalies_total, metadata$anomaly_qc$n_anomalies_total)
})
