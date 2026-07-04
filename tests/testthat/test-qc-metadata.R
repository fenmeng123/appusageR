qc_metadata_daily <- function(day_offsets, participant_id = "p1") {
  dates <- as.Date("2024-10-07") + day_offsets
  tibble::tibble(
    date = dates,
    weekday = weekdays(dates),
    app_name = "Example App",
    package_name = "com.example.app",
    duration_ms = rep(1000, length(dates)),
    duration_min = rep(1000 / 60000, length(dates)),
    open_count = rep(1L, length(dates)),
    notification_count = rep(0L, length(dates)),
    split_screen_ms = rep(0, length(dates)),
    episode_count = rep(NA_integer_, length(dates)),
    event_count = rep(NA_integer_, length(dates)),
    source_export_type = "day",
    is_all_apps = FALSE,
    is_collection_app = FALSE,
    parse_warning = NA_character_,
    anomaly_any = FALSE
  )
}

qc_metadata_project <- function(daily, participant_id = "p1",
                                export_type = "day",
                                second_level = c("valid", "missing", "malformed")) {
  second_level <- match.arg(second_level)
  root <- file.path(
    tempdir(),
    paste0("appusage_qc_project_", as.integer(runif(1, 1, 1e8))),
    "QCStudy_a1b2"
  )
  proclevel_1 <- file.path(root, "proclevel-1")
  proclevel_2 <- file.path(root, "proclevel-2")
  dir.create(proclevel_1, recursive = TRUE, showWarnings = FALSE)
  dir.create(proclevel_2, recursive = TRUE, showWarnings = FALSE)

  first_json <- file.path(
    proclevel_1,
    build_appusage_filename(participant_id, export_type, proc = 1, extension = "json")
  )
  first_rda <- file.path(
    proclevel_1,
    build_appusage_filename(participant_id, export_type, proc = 1, extension = "rda")
  )
  second_rda <- file.path(
    proclevel_2,
    build_appusage_filename(participant_id, export_type, proc = 2, extension = "rda")
  )
  second_json <- file.path(
    proclevel_2,
    build_appusage_filename(participant_id, export_type, proc = 2, extension = "json")
  )

  first_data <- list(day = daily)
  data <- first_data
  save(data, file = first_rda)

  if (identical(second_level, "valid")) {
    data <- list(
      event = tibble::tibble(),
      episode = tibble::tibble(),
      daily = daily
    )
    save(data, file = second_rda)
  } else if (identical(second_level, "malformed")) {
    writeLines("not an RDA file", second_rda)
  }

  metadata <- list(
    schema_version = "0.2.0",
    package_version = as.character(utils::packageVersion("appusageR")),
    parser_version = as.character(utils::packageVersion("appusageR")),
    created_at = "2024-01-01T00:00:00.000+0800",
    updated_at = "2024-01-01T00:00:00.000+0800",
    participant_id = participant_id,
    participant_id_source = "manual",
    identity = list(
      participant_id = participant_id,
      participant_id_source = "manual",
      wenjuanxing_sequence_id = NA_integer_
    ),
    source = list(file_name = "synthetic.txt"),
    export = list(
      detected_type = export_type,
      content_detected_export_type = export_type,
      native_export_type_from_filename = NA_character_,
      export_type_match = NA
    ),
    processing = list(
      first_level_status = "success",
      second_level_status = "pending",
      qc_status = "pending"
    ),
    outputs = list(
      metadata_json = first_json,
      first_level_rda = first_rda,
      second_level_rda = NA_character_
    ),
    counts = list(n_parse_warnings = 0L),
    qc = list(pass_qc = NA),
    anomalies = list(),
    errors = list()
  )
  jsonlite::write_json(metadata,
    path = first_json,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )

  second_metadata <- metadata
  second_metadata$processing$second_level_status <- "success"
  second_metadata$processing$qc_status <- "not_run"
  second_metadata$processing$app_category_status <- "not_run"
  second_metadata$outputs$metadata_json <- second_json
  second_metadata$outputs$first_level_metadata_json <- first_json
  second_metadata$outputs$first_level_rda <- first_rda
  second_metadata$outputs$second_level_metadata_json <- second_json
  second_metadata$outputs$second_level_rda <- second_rda
  second_metadata$counts <- list(
    n_event_rows = 0L,
    n_episode_rows = 0L,
    n_daily_rows = if (identical(second_level, "missing")) NA_integer_ else nrow(daily),
    n_anomalies = 0L,
    n_parse_warnings = 0L
  )
  second_metadata$category_dictionary <- list(app_category_status = "not_run")
  jsonlite::write_json(second_metadata,
    path = second_json,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  root
}

qc_metadata_proc2_file <- function(project, participant_id = "p1",
                                   export_type = "day") {
  file.path(
    project,
    "proclevel-2",
    build_appusage_filename(participant_id, export_type, proc = 2, extension = "json")
  )
}

qc_metadata_legacy_proc3_file <- function(project, participant_id = "p1",
                                          export_type = "day") {
  file.path(
    project,
    "proclevel-3",
    build_appusage_filename(participant_id, export_type, proc = 3, extension = "json")
  )
}

write_legacy_proc3_metadata <- function(project, participant_id = "p1",
                                        export_type = "day", qc_status = "success",
                                        pass_qc = TRUE, n_recorded_days = 7L,
                                        n_nonempty_days = 7L,
                                        malformed = FALSE) {
  legacy_file <- qc_metadata_legacy_proc3_file(project, participant_id, export_type)
  dir.create(dirname(legacy_file), recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(malformed)) {
    writeLines("{not-valid-json", legacy_file)
    return(legacy_file)
  }

  info <- list(
    schema_version = "0.2.0",
    package_version = as.character(utils::packageVersion("appusageR")),
    parser_version = as.character(utils::packageVersion("appusageR")),
    created_at = "2024-01-01T00:00:00.000+0800",
    updated_at = "2024-01-01T00:00:00.000+0800",
    participant_id = participant_id,
    participant_id_source = "manual",
    identity = list(
      participant_id = participant_id,
      participant_id_source = "manual",
      wenjuanxing_sequence_id = NA_integer_
    ),
    source = list(file_name = "synthetic.txt"),
    export = list(
      detected_type = export_type,
      content_detected_export_type = export_type,
      native_export_type_from_filename = NA_character_,
      export_type_match = NA
    ),
    processing = list(
      first_level_status = "success",
      second_level_status = "success",
      qc_status = qc_status
    ),
    outputs = list(
      metadata_json = legacy_file,
      first_level_rda = file.path(project, "proclevel-1", "synthetic_proc-1.rda"),
      second_level_rda = file.path(project, "proclevel-2", "synthetic_proc-2.rda"),
      qc_metadata_json = legacy_file
    ),
    counts = list(
      n_event_rows = 0L,
      n_episode_rows = 0L,
      n_daily_rows = n_recorded_days,
      n_anomalies = 0L,
      n_parse_warnings = 0L
    ),
    qc = list(
      qc_status = qc_status,
      pass_qc = pass_qc,
      qc_error_message = NA_character_,
      n_recorded_days = n_recorded_days,
      n_nonempty_days = n_nonempty_days,
      weekdays_covered = "Monday; Tuesday; Wednesday; Thursday; Friday; Saturday; Sunday",
      analysis_eligible_event = FALSE,
      analysis_eligible_episode = FALSE,
      analysis_eligible_daily = pass_qc
    ),
    errors = list()
  )
  jsonlite::write_json(info,
    path = legacy_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  legacy_file
}

write_legacy_proc3_from_proc2 <- function(project, proc2_file,
                                          participant_id = "p1",
                                          export_type = "day") {
  legacy_file <- qc_metadata_legacy_proc3_file(project, participant_id, export_type)
  dir.create(dirname(legacy_file), recursive = TRUE, showWarnings = FALSE)
  info <- jsonlite::read_json(proc2_file, simplifyVector = TRUE)
  info$outputs$metadata_json <- legacy_file
  info$outputs$qc_metadata_json <- legacy_file
  jsonlite::write_json(info,
    path = legacy_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  legacy_file
}

test_that("write_qc_metadata_batch updates proc-2 QC metadata and summary columns", {
  project <- qc_metadata_project(qc_metadata_daily(0:6))
  expect_false(dir.exists(file.path(project, "proclevel-3")))

  summary <- write_qc_metadata_batch(project, progress = FALSE)

  required <- c(
    "participant_id", "participant_id_source", "wenjuanxing_sequence_id",
    "detected_type", "filename_export_type", "export_type_match",
    "first_level_status", "second_level_status", "qc_status", "pass_qc",
    "analysis_eligible_event", "analysis_eligible_episode",
    "analysis_eligible_daily", "n_recorded_days", "n_nonempty_days",
    "weekdays_covered", "n_event_rows", "n_episode_rows", "n_daily_rows",
    "n_anomalies", "n_critical_anomalies", "n_warning_anomalies",
    "has_critical_anomaly", "has_warning_anomaly", "n_episode_anomalies",
    "n_event_anomalies", "n_daily_anomalies", "n_export_span_anomalies",
    "n_meta_duration_disagreements", "max_abs_meta_duration_diff_ms",
    "max_daily_total_ms_observed", "max_observed_export_lookback_days",
    "n_parse_warnings", "n_category_matched_apps",
    "category_match_rate", "error_message", "metadata_json",
    "first_level_rda", "second_level_rda", "qc_metadata_source",
    "legacy_proc3_json", "legacy_qc_fallback_used",
    "legacy_qc_conflict", "legacy_qc_conflict_fields",
    "legacy_qc_error_message"
  )
  expect_false(dir.exists(file.path(project, "proclevel-3")))
  expect_true(file.exists(file.path(project, "analytic_summary_table_proclevel-2.csv")))
  expect_true(all(required %in% names(summary)))
  expect_equal(summary$qc_status[[1]], "success")
  expect_equal(summary$qc_metadata_source[[1]], "proc-2")
  expect_false(summary$legacy_qc_fallback_used[[1]])
  expect_false(summary$legacy_qc_conflict[[1]])
  expect_true(summary$pass_qc[[1]])
  expect_true(summary$analysis_eligible_daily[[1]])
  expect_match(basename(summary$metadata_json[[1]]), "^sub-p1_type-day_proc-2[.]json$")

  metadata <- jsonlite::read_json(summary$metadata_json[[1]], simplifyVector = TRUE)
  expect_true(metadata$qc$pass_qc)
  expect_equal(metadata$qc$n_nonempty_days, 7)
  expect_equal(metadata$processing$qc_status, "success")
  expect_equal(metadata$anomaly_qc$status, "success")
  expect_equal(metadata$anomaly_qc$rule_version, "0.2.8")
})

test_that("write_qc_metadata_batch writes anomaly section and is idempotent", {
  project <- qc_metadata_project(qc_metadata_daily(0:6))
  proc2_rda <- file.path(
    project,
    "proclevel-2",
    "sub-p1_type-day_proc-2.rda"
  )
  load(proc2_rda)
  data$episode <- tibble::tibble(
    start_ts_ms = NA_real_,
    end_ts_ms = NA_real_,
    duration_ms = -1000,
    date = as.Date("2024-01-01"),
    anomaly_any = TRUE
  )
  save(data, file = proc2_rda)

  first <- write_qc_metadata_batch(project, progress = FALSE)
  second <- write_qc_metadata_batch(project, progress = FALSE)
  metadata <- jsonlite::read_json(qc_metadata_proc2_file(project), simplifyVector = TRUE)

  expect_equal(metadata$anomaly_qc$status, "success")
  expect_gt(metadata$anomaly_qc$n_critical_anomalies, 0)
  expect_gt(metadata$anomaly_qc$n_episode_anomalies, 0)
  expect_true(first$has_critical_anomaly[[1]])
  expect_true(first$pass_qc[[1]])
  expect_equal(first$n_anomalies[[1]], second$n_anomalies[[1]])
  expect_false(dir.exists(file.path(project, "proclevel-3")))
  expect_false(dir.exists(file.path(project, "proclevel-tmp")))
})

test_that("proc-2 summary refresh tolerates malformed metadata files", {
  project <- qc_metadata_project(qc_metadata_daily(0:6))
  metadata_file <- qc_metadata_proc2_file(project)
  writeLines("{not-json", metadata_file, useBytes = TRUE)

  summary <- build_qc_summary_from_metadata(metadata_file, project_dir = project)

  expect_equal(nrow(summary), 1L)
  expect_equal(summary$qc_status[[1]], "error")
  expect_match(summary$error_message[[1]], "could not be read")
  expect_true("n_critical_anomalies" %in% names(summary))
})

test_that("write_qc_metadata_batch records fewer-than-seven-day QC failure", {
  project <- qc_metadata_project(qc_metadata_daily(0:5))

  summary <- write_qc_metadata_batch(project, progress = FALSE)
  metadata <- jsonlite::read_json(summary$metadata_json[[1]], simplifyVector = TRUE)

  expect_equal(summary$qc_status[[1]], "success")
  expect_false(summary$pass_qc[[1]])
  expect_equal(summary$n_nonempty_days[[1]], 6)
  expect_false(metadata$qc$pass_min_days)
})

test_that("write_qc_metadata_batch records incomplete weekday coverage", {
  project <- qc_metadata_project(qc_metadata_daily(c(0:5, 7)))

  summary <- write_qc_metadata_batch(project, progress = FALSE)
  metadata <- jsonlite::read_json(summary$metadata_json[[1]], simplifyVector = TRUE)

  expect_equal(summary$qc_status[[1]], "success")
  expect_false(summary$pass_qc[[1]])
  expect_true(metadata$qc$pass_min_days)
  expect_false(metadata$qc$pass_all_weekdays)
})

test_that("write_qc_metadata_batch captures missing second-level RDA in non-strict mode", {
  project <- qc_metadata_project(qc_metadata_daily(0:6), second_level = "missing")

  summary <- write_qc_metadata_batch(project, progress = FALSE, strict = FALSE)
  metadata <- jsonlite::read_json(summary$metadata_json[[1]], simplifyVector = TRUE)

  expect_equal(summary$qc_status[[1]], "error")
  expect_equal(summary$second_level_status[[1]], "missing")
  expect_false(summary$pass_qc[[1]])
  expect_true(nzchar(summary$error_message[[1]]))
  expect_match(metadata$qc$qc_error_message, "Second-level RDA file does not exist")
})

test_that("first-, second-level, and routine QC functions do not create proclevel-3", {
  path <- testthat::test_path("fixtures", "line_sample.txt")
  parent_dir <- file.path(tempdir(), paste0("appusage_qc_full_", as.integer(runif(1, 1, 1e8))))
  first <- read_appusage_batch(
    path,
    ids = "line_id",
    output_dir = parent_dir,
    project_name = "QCFull",
    project_id = "c3d4",
    progress = FALSE
  )
  project <- unique(first$project_root)
  second <- write_second_level_batch(first, overwrite = TRUE, progress = FALSE)

  expect_equal(second$status[[1]], "success")
  expect_false(dir.exists(file.path(project, "proclevel-3")))

  qc <- write_qc_metadata_batch(project, progress = FALSE)
  expect_equal(qc$qc_status[[1]], "success")
  expect_false(dir.exists(file.path(project, "proclevel-3")))
  expect_false(dir.exists(file.path(project, "proclevel-tmp")))
  expect_true(file.exists(file.path(project, "analytic_summary_table_proclevel-2.csv")))
})

test_that("QC summary reads legacy proc-3 metadata when proc-2 is absent", {
  project <- file.path(
    tempdir(),
    paste0("appusage_legacy_only_", as.integer(runif(1, 1, 1e8))),
    "LegacyStudy_a1b2"
  )
  legacy_file <- write_legacy_proc3_metadata(project)

  summary <- build_qc_summary_from_metadata(character(), project_dir = project)

  expect_equal(nrow(summary), 1)
  expect_equal(summary$qc_metadata_source[[1]], "legacy_proc-3")
  expect_equal(normalizePath(summary$legacy_proc3_json[[1]], winslash = "/", mustWork = FALSE), normalizePath(legacy_file, winslash = "/", mustWork = FALSE))
  expect_true(summary$legacy_qc_fallback_used[[1]])
  expect_false(summary$legacy_qc_conflict[[1]])
  expect_true(summary$pass_qc[[1]])
  expect_equal(summary$n_nonempty_days[[1]], 7)
})

test_that("QC summary falls back to legacy proc-3 when proc-2 QC is not run", {
  project <- qc_metadata_project(qc_metadata_daily(0:6))
  proc2_file <- qc_metadata_proc2_file(project)
  legacy_file <- write_legacy_proc3_metadata(project)

  summary <- build_qc_summary_from_metadata(proc2_file, project_dir = project)

  expect_equal(summary$qc_metadata_source[[1]], "proc-2_with_legacy_fallback")
  expect_equal(normalizePath(summary$metadata_json[[1]], winslash = "/", mustWork = FALSE), normalizePath(proc2_file, winslash = "/", mustWork = FALSE))
  expect_equal(normalizePath(summary$legacy_proc3_json[[1]], winslash = "/", mustWork = FALSE), normalizePath(legacy_file, winslash = "/", mustWork = FALSE))
  expect_true(summary$legacy_qc_fallback_used[[1]])
  expect_true(summary$pass_qc[[1]])
  expect_equal(summary$n_nonempty_days[[1]], 7)
})

test_that("QC summary prefers proc-2 when proc-2 and legacy proc-3 agree", {
  project <- qc_metadata_project(qc_metadata_daily(0:6))
  proc2_summary <- write_qc_metadata_batch(project, progress = FALSE)
  write_legacy_proc3_from_proc2(project, proc2_summary$metadata_json[[1]])

  summary <- build_qc_summary_from_metadata(proc2_summary$metadata_json, project_dir = project)

  expect_equal(summary$qc_metadata_source[[1]], "proc-2")
  expect_false(summary$legacy_qc_fallback_used[[1]])
  expect_false(summary$legacy_qc_conflict[[1]])
  expect_true(summary$pass_qc[[1]])
  expect_equal(summary$n_nonempty_days[[1]], 7)
})

test_that("QC summary reports conflicts while keeping proc-2 values", {
  project <- qc_metadata_project(qc_metadata_daily(0:6))
  proc2_summary <- write_qc_metadata_batch(project, progress = FALSE)
  write_legacy_proc3_metadata(project, pass_qc = FALSE, n_nonempty_days = 3L)

  summary <- build_qc_summary_from_metadata(proc2_summary$metadata_json, project_dir = project)
  conflict_fields <- strsplit(summary$legacy_qc_conflict_fields[[1]], ";", fixed = TRUE)[[1]]

  expect_equal(summary$qc_metadata_source[[1]], "proc-2")
  expect_false(summary$legacy_qc_fallback_used[[1]])
  expect_true(summary$legacy_qc_conflict[[1]])
  expect_true(summary$pass_qc[[1]])
  expect_equal(summary$n_nonempty_days[[1]], 7)
  expect_true(all(c("pass_qc", "n_nonempty_days") %in% conflict_fields))
})

test_that("malformed legacy proc-3 JSON is summarized without aborting", {
  project <- file.path(
    tempdir(),
    paste0("appusage_malformed_legacy_", as.integer(runif(1, 1, 1e8))),
    "LegacyStudy_c3d4"
  )
  write_legacy_proc3_metadata(project, malformed = TRUE)

  summary <- build_qc_summary_from_metadata(character(), project_dir = project)

  expect_equal(nrow(summary), 1)
  expect_equal(summary$qc_metadata_source[[1]], "none")
  expect_false(summary$legacy_qc_fallback_used[[1]])
  expect_false(summary$legacy_qc_conflict[[1]])
  expect_true(nzchar(summary$error_message[[1]]))
  expect_true(nzchar(summary$legacy_qc_error_message[[1]]))
})
