mixed_line_header <- function(date = "2024-01-01") {
  paste(
    date, "\u5f00\u59cb\u65f6\u95f4\uff08ms\uff09", "\u5f00\u59cb\u65f6\u95f4",
    "\u5e94\u7528\u540d\u79f0", "\u5e94\u7528\u6807\u8bc6", "\u4f7f\u7528\u65f6\u957f",
    "\u7ed3\u675f\u65f6\u95f4\uff08ms\uff09", "\u7ed3\u675f\u65f6\u95f4",
    sep = ","
  )
}

mixed_line_record <- function(app = "LineApp", package = "org.example.line",
                              start = 1704067200000, end = 1704067260000) {
  paste(
    "", paste0("T:", start), "00:00:00", app, package, "1 minute",
    if (is.na(end)) "" else paste0("T:", end), "00:01:00",
    sep = ","
  )
}

mixed_meta_lines <- function(with_records = TRUE) {
  table1 <- paste(
    "2024-01-01\u8868\u4e00", "\u5e94\u7528\u540d\u79f0", "\u5e94\u7528\u5305\u540d",
    "\u5f00\u59cb\u65f6\u95f4", "\u7ed3\u675f\u65f6\u95f4", "\u6700\u540e\u65f6\u95f4",
    "\u603b\u65f6\u957f", "\u5f00\u59cb\u65f6\u95f4\uff08ms\uff09",
    "\u7ed3\u675f\u65f6\u95f4\uff08ms\uff09", "\u6700\u540e\u65f6\u95f4\uff08ms\uff09",
    "\u603b\u65f6\u957f\uff08ms\uff09", sep = ","
  )
  table1_record <- paste(
    "", "MetaApp", "org.example.meta", "2024-01-01 00:00:00",
    "2024-01-01 00:30:00", "2024-01-01 00:30:00", "30 minutes",
    "T:1704067200000", "T:1704069000000", "T:1704069000000", "1800000",
    sep = ","
  )
  table2 <- paste(
    "2024-01-01\u8868\u4e8c", "\u5e94\u7528\u540d\u79f0", "\u5e94\u7528\u5305\u540d",
    "\u5177\u4f53\u9875\u9762", "\u65f6\u95f4", "\u65f6\u95f4\u6233",
    "\u7c7b\u578b", "\u914d\u7f6e", sep = ","
  )
  table2_record <- paste(
    "", "MetaApp", "org.example.meta", "MainActivity",
    "2024-01-01 00:00:00", "T:1704067200000", "1", "", sep = ","
  )
  if (isTRUE(with_records)) {
    c(table1, table1_record, table2, table2_record)
  } else {
    c(table1, table2)
  }
}

mixed_content_lines <- function(meta_records = TRUE) {
  c(mixed_line_header(), mixed_line_record(), mixed_meta_lines(meta_records))
}

test_that("mixed content selects filename component and bounds line parsing", {
  lines <- mixed_content_lines(TRUE)
  line_preflight <- appusage_source_preflight(
    lines, input = "lines", filename_type = "line"
  )
  expect_equal(line_preflight$status, "ok")
  expect_true(line_preflight$mixed_content)
  expect_setequal(line_preflight$detected_components, c("line", "meta"))
  expect_equal(line_preflight$selected_component, "line")
  expect_equal(
    line_preflight$selection_rule,
    "filename_within_detected_components"
  )

  parsed_line <- parse_line(lines, input = "lines", strict = TRUE)
  expect_equal(nrow(parsed_line), 1L)
  expect_equal(parsed_line$app_name, "LineApp")
  expect_false(any(grepl("\u8868\u4e00|\u8868\u4e8c|MetaApp", parsed_line$app_name)))

  meta_preflight <- appusage_source_preflight(
    lines, input = "lines", filename_type = "meta"
  )
  expect_equal(meta_preflight$selected_component, "meta")
  expect_equal(meta_preflight$selection_rule, "filename_within_detected_components")
  parsed_meta <- parse_meta(lines, input = "lines", strict = TRUE)
  expect_equal(nrow(parsed_meta$summary), 1L)
  expect_equal(nrow(parsed_meta$events), 1L)

  root <- tempfile("appusage-mixed-selection-")
  dir.create(root)
  line_file <- file.path(root, "AppUsage_line_2024_1_1_1_0_0.txt")
  meta_file <- file.path(root, "AppUsage_meta_2024_1_1_2_0_0.txt")
  writeLines(lines, line_file, useBytes = TRUE)
  writeLines(lines, meta_file, useBytes = TRUE)
  line_summary <- read_appusage_batch(
    line_file, ids = "mixed-line", output_dir = root,
    project_name = "MixedLine", project_id = "C1L", progress = FALSE
  )
  expect_equal(line_summary$status, "success")
  expect_equal(line_summary$detected_type, "line")
  expect_true(line_summary$mixed_content)
  expect_equal(
    line_summary$component_selection_rule,
    "filename_within_detected_components"
  )
  line_data <- load_appusage_data_object(line_summary$data_file[[1]])
  expect_setequal(names(line_data), "line")
  expect_equal(nrow(line_data$line), 1L)
  line_metadata <- jsonlite::read_json(
    line_summary$metadata_file[[1]], simplifyVector = TRUE
  )
  expect_setequal(line_metadata$export$detected_components, c("line", "meta"))
  expect_equal(line_metadata$export$selected_component, "line")
  expect_true(line_metadata$export$mixed_content)
  expect_gt(length(line_metadata$export$boundary_diagnostics$boundaries), 0L)

  meta_summary <- read_appusage_batch(
    meta_file, ids = "mixed-meta", output_dir = root,
    project_name = "MixedMeta", project_id = "C1M", progress = FALSE
  )
  expect_equal(meta_summary$status, "success")
  expect_equal(meta_summary$detected_type, "meta")
  expect_true(meta_summary$mixed_content)
  meta_data <- load_appusage_data_object(meta_summary$data_file[[1]])
  expect_setequal(names(meta_data), c("meta_summary", "meta_events"))
})

test_that("content wins single-component filename disagreement and unknown filename", {
  lines <- c(mixed_line_header(), mixed_line_record())
  disagreement <- appusage_source_preflight(
    lines, input = "lines", filename_type = "meta"
  )
  expect_equal(disagreement$selected_component, "line")
  expect_true(disagreement$filename_content_disagreement)
  expect_equal(disagreement$selection_rule, "single_content_component")

  unknown <- appusage_source_preflight(
    lines, input = "lines", filename_type = NA_character_
  )
  expect_equal(unknown$status, "ok")
  expect_equal(unknown$selected_component, "line")

  root <- tempfile("appusage-content-authoritative-")
  dir.create(root)
  mismatched_file <- file.path(root, "AppUsage_meta_2024_1_1_0_0_0.txt")
  writeLines(lines, mismatched_file, useBytes = TRUE)
  summary <- read_appusage_batch(
    mismatched_file, ids = "content", progress = FALSE
  )
  expect_equal(summary$status, "success")
  expect_equal(summary$detected_type, "line")
  expect_true(summary$filename_content_disagreement)
  expect_match(summary$warning_messages, "content-selected component")

  unknown_file <- file.path(root, "unknown-upload.txt")
  writeLines(lines, unknown_file, useBytes = TRUE)
  unknown_summary <- read_appusage_batch(
    unknown_file, ids = "unknown", progress = FALSE
  )
  expect_equal(unknown_summary$status, "success")
  expect_equal(unknown_summary$detected_type, "line")
})

test_that("ambiguous mixed content without safe selector fails precisely", {
  lines <- mixed_content_lines(TRUE)
  preflight <- appusage_source_preflight(
    lines, input = "lines", filename_type = NA_character_
  )
  expect_equal(preflight$status, "mixed_content_ambiguous")
  expect_equal(preflight$selection_rule, "ambiguous_mixed_content")
  expect_false(is_present_string(preflight$selected_component))
  error <- appusage_source_preflight_error(preflight)
  expect_s3_class(error, "appusage_mixed_content_source")
  expect_match(conditionMessage(error), "no unambiguous bounded component")
})

test_that("line blocks stop at every structural boundary", {
  header <- mixed_line_header()
  record <- mixed_line_record()
  repeated <- parse_line(c(header, record, header, record), input = "lines", strict = TRUE)
  expect_equal(nrow(repeated), 2L)

  boundaries <- list(
    meta = mixed_meta_lines(FALSE)[[1]],
    day = paste(
      "2024-01-02", "\u5e94\u7528\u540d\u79f0", "\u5e94\u7528\u6807\u8bc6",
      "\u683c\u5f0f\u5316\u65f6\u95f4", "\u4f7f\u7528\u65f6\u957f\uff08ms\uff09",
      "\u542f\u52a8\u6b21\u6570", "\u901a\u77e5\u6b21\u6570", sep = ","
    ),
    app = paste(
      "Target", "org.example.target", "\u65e5\u671f", "\u683c\u5f0f\u5316\u65f6\u95f4",
      "\u4f7f\u7528\u65f6\u957f\uff08ms\uff09", "\u542f\u52a8\u6b21\u6570",
      "\u901a\u77e5\u6b21\u6570", sep = ","
    ),
    step = "\u5f00\u673a\u81f3\u4eca\u6b65\u6570\u4fe1\u606f,1000",
    device = "\u8bbe\u5907\u4fe1\u606f,Example Phone",
    system = "\u7cfb\u7edf\u4fe1\u606f,Android"
  )
  for (name in names(boundaries)) {
    parsed <- parse_line(
      c(header, record, boundaries[[name]], mixed_line_record(app = "ShouldNotParse")),
      input = "lines", strict = TRUE
    )
    expect_equal(nrow(parsed), 1L, info = name)
    expect_equal(parsed$app_name, "LineApp", info = name)
  }
})

test_that("line structural quality metrics apply documented thresholds", {
  clean <- parse_line(
    c(mixed_line_header(), mixed_line_record()), input = "lines", strict = TRUE
  )
  clean_quality <- line_structural_quality(clean, 1L)
  expect_equal(clean_quality$status, "pass")

  contaminated <- clean
  contaminated$app_name <- "\u5e94\u7528\u540d\u79f0"
  contamination_quality <- line_structural_quality(contaminated, 1L)
  expect_equal(contamination_quality$status, "critical")
  expect_equal(contamination_quality$n_header_token_contamination, 1L)

  twenty <- clean[rep(1L, 20L), , drop = FALSE]
  twenty$start_ts_ms <- clean$start_ts_ms[[1]] + seq_len(20L) * 100000
  twenty$end_ts_ms <- twenty$start_ts_ms + 60000
  twenty$duration_ms <- 60000
  twenty$start_datetime <- ms_to_datetime(twenty$start_ts_ms, tz = "Asia/Shanghai")
  twenty$end_datetime <- ms_to_datetime(twenty$end_ts_ms, tz = "Asia/Shanghai")
  warning_rows <- twenty
  warning_rows$start_ts_ms[[1]] <- NA_real_
  warning_rows$duration_ms[[1]] <- NA_real_
  warning_quality <- line_structural_quality(warning_rows, 20L)
  expect_equal(warning_quality$valid_interval_ratio, 0.95)
  expect_equal(warning_quality$status, "warning")

  critical_rows <- warning_rows
  critical_rows$start_ts_ms[[2]] <- NA_real_
  critical_rows$duration_ms[[2]] <- NA_real_
  expect_equal(line_structural_quality(critical_rows, 20L)$status, "critical")

  hundred <- clean[rep(1L, 100L), , drop = FALSE]
  hundred$start_ts_ms <- clean$start_ts_ms[[1]] + seq_len(100L) * 100000
  hundred$end_ts_ms <- hundred$start_ts_ms + 60000
  hundred$duration_ms <- 60000
  hundred$start_datetime <- ms_to_datetime(hundred$start_ts_ms, tz = "Asia/Shanghai")
  hundred$end_datetime <- ms_to_datetime(hundred$end_ts_ms, tz = "Asia/Shanghai")
  hundred[100, ] <- hundred[1, ]
  duplicate_warning <- line_structural_quality(hundred, 100L)
  expect_equal(duplicate_warning$n_exact_duplicates, 1L)
  expect_equal(duplicate_warning$status, "warning")
  hundred[99, ] <- hundred[1, ]
  expect_equal(line_structural_quality(hundred, 100L)$status, "critical")

  malformed <- clean
  malformed$package_name <- "not a package"
  malformed$source_table_date <- as.Date("2024-01-02")
  expect_identical(malformed$date, clean$date)
  malformed_quality <- line_structural_quality(malformed, 1L)
  expect_equal(malformed_quality$n_malformed_identity, 1L)
  expect_equal(malformed_quality$n_source_date_timestamp_mismatch, 1L)
  expect_equal(malformed_quality$status, "warning")
})

test_that("critical line structural quality is a first-level terminal failure", {
  root <- tempfile("appusage-structural-gate-")
  dir.create(root)
  path <- file.path(root, "AppUsage_line_2024_1_1_0_0_0.txt")
  lines <- c(
    mixed_line_header(),
    mixed_line_record(),
    mixed_line_record(app = "Broken", package = "org.example.broken", end = NA_real_)
  )
  writeLines(lines, path, useBytes = TRUE)
  summary <- read_appusage_batch(
    path, ids = "structural", output_dir = root,
    project_name = "Structural", project_id = "C3", progress = FALSE
  )
  expect_equal(summary$status, "error")
  expect_equal(summary$failure_family, "structural_quality")
  expect_equal(summary$structural_quality_status, "critical")
  expect_true(summary$structural_quality_critical)
  expect_true(is.na(summary$data_file))
  metadata <- jsonlite::read_json(summary$metadata_file, simplifyVector = TRUE)
  expect_equal(metadata$structural_quality$status, "critical")
  expect_equal(
    metadata$structural_quality$thresholds$valid_interval_critical_min,
    0.95
  )
  expect_equal(metadata$processing$first_level_failure_reason, "structural_quality_critical")

  second <- write_second_level_batch(summary, progress = FALSE)
  expect_equal(second$second_level_status, "skipped")
  expect_equal(second$skip_reason, "upstream_first_level_error")
})

test_that("clean line fixture remains semantically unchanged", {
  parsed <- parse_line(testthat::test_path("fixtures", "line_sample.txt"))
  expect_equal(parsed$duration_ms[[1]], 12556)
  expect_true(all(diff(parsed$start_ts_ms) >= 0))
  quality <- parser_diagnostics(parsed)$format_specific$structural_quality
  expect_false(isTRUE(quality$critical))
})
