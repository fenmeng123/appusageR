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

test_that("Table 1 accepts a repeated display quartet in the native millisecond positions", {
  canonical <- mixed_meta_lines()
  repeated <- canonical
  repeated[1] <- gsub("\uff08ms\uff09", "", repeated[1], fixed = TRUE)
  for (prefix in list(character(), c(mixed_line_header(), mixed_line_record()))) {
    read <- function(meta) run_first_level_appusage(c(prefix, meta), input = "lines")
    reference <- read(canonical)
    result <- read(repeated)
    expect_identical(result$status, "success")
    for (table in names(reference$data)) {
      expect_identical(as.list(result$data[[table]]), as.list(reference$data[[table]]))
    }
    expect_equal(result$data$meta_summary$total_duration_ms, 1800000)
    expect_equal(result$data$meta_summary$start_ts_ms, 1704067200000)
    expect_identical(run_second_level_appusage(result)$data,
                     run_second_level_appusage(reference)$data)
  }
  bad <- repeated
  cells <- strsplit(bad[1], ",", fixed = TRUE)[[1]]
  cells[c(8L, 9L)] <- cells[c(9L, 8L)]
  bad[1] <- paste(cells, collapse = ",")
  expect_identical(run_first_level_appusage(bad, input = "lines")$status, "error")
})

test_that("mixed defaults survive project execution, saved configuration and resume", {
  root <- tempfile("mix")
  dir.create(root)
  file <- file.path(root, "AppUsage_meta_2024_1_1_2_0_0.txt")
  writeLines(mixed_content_lines(), file, useBytes = TRUE)
  project <- file.path(root, "p")
  result <- run_appusage_workflow(file, project_dir = project, progress = FALSE)
  first <- result$first_level
  second <- appusage_summary_proc2_paths(result$second_level)
  pair <- c(first$data_file, first$metadata_file, second, second_level_metadata_path(second))
  original <- tools::md5sum(pair)
  info <- appusage_read_json(pair[4], simplifyVector = TRUE)
  first_info <- appusage_read_json(pair[2], simplifyVector = TRUE)
  for (metadata in list(first_info, info)) {
    expect_identical(metadata$schema_version, "0.3.5")
    expect_identical(metadata$implementation_provenance$output_schema_version, "0.3.5")
  }
  expect_identical(first_info$module_state$parse$output_schema_version, "0.3.5")
  expect_identical(info$module_state$research_data$output_schema_version, "0.3.5")
  expect_identical(info$second_level$meta_argument_sources,
                   list(reconstruct_meta = "default", meta_daily_source = "default"))
  expect_false(info$meta_reconstruction$requested)
  expect_identical(info$second_level$grain_sources,
                   list(event = "meta_events", episode = "line", daily = "line"))
  expect_equal(info$module_state$upstream_components$tables,
               list(line = 1L, meta_summary = 1L, meta_events = 1L))
  expect_equal(load_appusage_data_object(second)$daily$duration_ms, 60000)
  raw <- load_appusage_data_object(pair[1])
  bad <- write_second_level_batch(first, output_dir = dirname(second), progress = FALSE,
                                  resume = TRUE, meta_daily_source = "summary")
  expect_identical(bad$second_level_status, "error")
  expect_match(bad$error_class, "appusage_mixed_argument_conflict")
  expect_identical(tools::md5sum(pair), original)
  good <- write_second_level_batch(first, output_dir = dirname(second), progress = FALSE, resume = TRUE)
  expect_identical(good$second_level_status, "success")
  expect_identical(tools::md5sum(pair), original)
  expect_true(appusage_delivery_raw_contract(raw, first$detected_type, info$module_state$upstream_components))
  expect_error(build_appusage_daily(standardize_appusage(raw), meta_daily_source = "summary"),
               class = "appusage_mixed_argument_conflict")
  saved <- readRDS(file.path(project, "appusage_configuration.rds"))
  expect_false(appusage_meta_explicit(appusage_validate_config(saved))$meta_daily_source)
  run_appusage_workflow(project_dir = project)
  expect_identical(tools::md5sum(pair), original)
  unlink(second)
  run_appusage_stage("research_data", project)
  expect_identical(tools::md5sum(pair[1:2]), original[1:2])
  expect_equal(load_appusage_data_object(second)$daily$duration_ms, 60000)
  for (args in list(list(reconstruct_meta = TRUE), list(meta_daily_source = "summary"),
                    list(meta_daily_source = "episodes"), list(meta_daily_source = "both"))) {
    expect_error(do.call(run_second_level_appusage, c(list(x = load_appusage_data_object(pair[1])), args)),
                 class = "appusage_mixed_argument_conflict")
  }
  explicit <- appusage_config(reconstruction = list(reconstruct_meta = FALSE), execution = list(progress = FALSE))
  result <- run_appusage_workflow(file, project_dir = project, config = explicit)
  info <- appusage_read_json(pair[4], simplifyVector = TRUE)
  expect_identical(info$second_level$meta_argument_sources$reconstruct_meta, "explicit")
  expect_identical(tools::md5sum(pair[1:2]), original[1:2])
  expect_equal(load_appusage_data_object(second)$daily$duration_ms, 60000)

  # A pre-component config has the expanded values but no evidence of their origin.
  attr(saved, "meta_explicit") <- NULL
  expect_identical(appusage_validate_config(saved)$parse$type, "legacy")
  saveRDS(saved, file.path(project, "appusage_configuration.rds"))
  legacy <- run_appusage_workflow(project_dir = project)
  expect_identical(legacy$first_level$detected_type, "meta")
  expect_named(load_appusage_data_object(legacy$first_level$data_file), c("meta_summary", "meta_events"))
  expect_identical(legacy$first_level$component_selection_rule, "filename_within_detected_components")
  legacy_json <- second_level_metadata_path(appusage_summary_proc2_paths(legacy$second_level))
  info <- appusage_read_json(legacy_json, simplifyVector = TRUE)
  expect_identical(info$second_level$meta_argument_sources,
                   list(reconstruct_meta = "legacy_unknown", meta_daily_source = "legacy_unknown"))
  tasks <- plan_appusage_workflow(project_dir = project, verify = "content")$tasks
  expect_true(all(tasks$action[tasks$stage %in% c("parse", "research_data")] == "reuse"))
  expect_error(run_appusage_workflow(project_dir = project, type = "auto"), "new appusage_config")
  config <- appusage_config(parse = list(type = "legacy"))
  expect_identical(config$parse$type, "legacy")
  ambiguous <- run_first_level_appusage(mixed_content_lines(), input = "lines", type = "legacy")
  expect_identical(ambiguous$status, "error")
  one <- run_first_level_appusage(c(mixed_line_header(), mixed_line_record(), mixed_meta_lines()[3:4]),
                                 input = "lines", type = "legacy")
  expect_named(one$data, "line")
})

test_that("mixed component receipts gate cache-only use and repair damaged owners", {
  root <- tempfile("mc")
  dir.create(root)
  source <- file.path(root, "synthetic.txt")
  writeLines(mixed_content_lines(), source, useBytes = TRUE)
  project <- file.path(root, "p")
  config <- appusage_config(execution = list(progress = FALSE))
  result <- run_appusage_workflow(source, project_dir = project, config = config)
  first <- result$first_level
  second <- appusage_summary_proc2_paths(result$second_level)
  json2 <- second_level_metadata_path(second)
  original <- load_appusage_data_object(second)
  plan <- function(verify) plan_appusage_workflow(project_dir = project, verify = verify)$tasks
  metadata <- appusage_read_json(first$metadata_file, simplifyVector = FALSE)
  no_parse <- metadata
  no_parse$module_state$parse <- NULL
  write_metadata_json(no_parse, first$metadata_file)
  expect_error(write_second_level_appusage(first$data_file, overwrite = TRUE),
               class = "appusage_first_cache_unverified")
  metadata$module_state$components <- NULL
  write_metadata_json(metadata, first$metadata_file)
  rebuilt <- rebuild_first_level_summary_from_cache(project, write = FALSE)
  expect_identical(rebuilt$cache_rebuild_status, "component_verification_required")
  for (scope in c("metadata", "cache_only")) {
    tasks <- plan(scope)
    parse <- tasks[tasks$stage == "parse", ]
    expect_identical(parse$reason_code, "component_verification_required")
    expect_identical(parse$action, if (scope == "cache_only") "blocked" else "run")
  }
  expect_error(write_second_level_appusage(first$data_file, overwrite = TRUE),
               class = "appusage_first_cache_unverified")
  run_appusage_workflow(project_dir = project)
  expect_true(appusage_components_complete(appusage_read_json(first$metadata_file, simplifyVector = TRUE)))
  metadata <- appusage_read_json(json2, simplifyVector = FALSE)
  metadata$module_state$upstream_components <- NULL
  write_metadata_json(metadata, json2)
  tasks <- plan("cache_only")
  expect_identical(tasks$action[tasks$stage == "parse"], "reuse")
  expect_identical(tasks$action[tasks$stage == "research_data"], "run")
  run_appusage_workflow(project_dir = project)
  for (path in c(first$data_file, first$metadata_file, second, json2)) {
    writeBin(charToRaw("broken"), path)
    tasks <- plan("content")
    expect_identical(tasks$action[tasks$stage == "parse"],
                     if (path %in% c(first$data_file, first$metadata_file)) "run" else "reuse")
    expect_identical(tasks$action[tasks$stage == "research_data"], "run")
    run_appusage_workflow(project_dir = project)
    expect_identical(load_appusage_data_object(second), original)
    tasks <- plan("content")
    expect_true(all(tasks$action[tasks$stage %in% c("parse", "research_data")] == "reuse"))
  }
})

test_that("a damaged mixed component retains its diagnostic and never seeds a success", {
  source <- tempfile(fileext = ".txt")
  lines <- mixed_content_lines()
  writeLines(lines[-5L], source, useBytes = TRUE)
  partial <- run_first_level_appusage(source)
  expect_error(run_second_level_appusage(partial), class = "appusage_first_cache_unverified")
  expect_equal(nrow(parse_line(source)), 1L)
  project <- tempfile("partial")
  config <- appusage_config(execution = list(progress = FALSE))
  result <- run_appusage_workflow(source, project_dir = project, config = config)
  expect_identical(result$first_level$status, "error")
  info <- appusage_read_json(result$first_level$metadata_file, simplifyVector = TRUE)
  expect_identical(info$module_state$components, list(status = "incomplete",
                   states = list(line = "not_run", meta = "invalid_header")))
  expect_identical(result$second_level$skip_reason, "upstream_first_level_error")
  tasks <- plan_appusage_workflow(project_dir = project, verify = "cache_only")$tasks
  expect_identical(tasks$action[tasks$stage == "parse"], "blocked")
  writeLines(lines, source, useBytes = TRUE)
  repaired <- run_appusage_workflow(project_dir = project)
  expect_identical(repaired$first_level$status, "success")
  expect_equal(nrow(load_appusage_data_object(appusage_summary_proc2_paths(repaired$second_level))$event), 1L)
})

test_that("a parser exception after line parsing cannot publish a partial pair", {
  source <- tempfile(fileext = ".txt")
  writeLines(mixed_content_lines(), source, useBytes = TRUE)
  project <- tempfile("pf")
  local_mocked_bindings(appusage_parse_meta_context = function(...) stop("Injected table 2 failure"))
  first <- read_appusage_batch(source, project_dir = project, progress = FALSE)
  info <- appusage_read_json(first$metadata_file, simplifyVector = TRUE)
  expect_identical(first$status, "error")
  expect_identical(info$module_state$components$states, list(line = "parsed", meta = "failed"))
  expect_false(file.exists(sub("[.]json$", ".rda", first$metadata_file)))
  expect_identical(write_second_level_batch(first, progress = FALSE)$skip_reason, "upstream_first_level_error")
})

test_that("proc-1 rolls back the previously published pair if promotion fails", {
  source <- tempfile(fileext = ".txt")
  writeLines(mixed_content_lines(), source, useBytes = TRUE)
  project <- tempfile("rb")
  first <- read_appusage_batch(source, project_dir = project, progress = FALSE)
  pair <- c(first$data_file, first$metadata_file)
  before <- tools::md5sum(pair)
  promote <- appusage_promote_file
  local_mocked_bindings(appusage_promote_file = function(from, to) {
    if (identical(basename(to), basename(pair[2])) && grepl("tmp-", basename(from))) return(FALSE)
    promote(from, to)
  })
  failed <- read_appusage_batch(source, project_dir = project, progress = FALSE, overwrite = TRUE)
  expect_identical(failed$status, "error")
  expect_identical(tools::md5sum(pair), before)
})

test_that("bounded line/meta content retains both components for either filename", {
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
    "bounded_line_meta"
  )

  parsed_line <- parse_line(lines, input = "lines", strict = TRUE)
  expect_equal(nrow(parsed_line), 1L)
  expect_equal(parsed_line$app_name, "LineApp")
  expect_false(any(grepl("\u8868\u4e00|\u8868\u4e8c|MetaApp", parsed_line$app_name)))

  meta_preflight <- appusage_source_preflight(
    lines, input = "lines", filename_type = "meta"
  )
  expect_equal(meta_preflight$selected_component, "line")
  expect_equal(meta_preflight$selection_rule, "bounded_line_meta")
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
    "bounded_line_meta"
  )
  line_data <- load_appusage_data_object(line_summary$data_file[[1]])
  expect_setequal(names(line_data), c("line", "meta_summary", "meta_events"))
  expect_equal(nrow(line_data$line), 1L)
  line_metadata <- jsonlite::read_json(
    line_summary$metadata_file[[1]], simplifyVector = TRUE
  )
  expect_setequal(line_metadata$export$detected_components, c("line", "meta"))
  expect_equal(line_metadata$export$selected_component, "line")
  expect_true(line_metadata$export$mixed_content)
  expect_gt(length(line_metadata$export$boundary_diagnostics$boundaries), 0L)
  second <- run_second_level_appusage(line_summary$data_file, output_dir = root)
  receipt <- jsonlite::read_json(second$metadata_file)
  expect_identical(receipt$second_level$grain_sources,
                   list(event = "meta_events", episode = "line", daily = "line"))
  expect_false(receipt$meta_reconstruction$requested)
  expect_identical(receipt$meta_daily_comparison$selected_source, "line")
  expect_equal(second$data$daily$duration_ms, 60000)

  meta_summary <- read_appusage_batch(
    meta_file, ids = "mixed-meta", output_dir = root,
    project_name = "MixedMeta", project_id = "C1M", progress = FALSE
  )
  expect_equal(meta_summary$status, "success")
  expect_equal(meta_summary$detected_type, "line")
  expect_true(meta_summary$mixed_content)
  meta_data <- load_appusage_data_object(meta_summary$data_file[[1]])
  expect_setequal(names(meta_data), names(line_data))
  expect_identical(meta_data$meta_events$source_line, line_data$meta_events$source_line)
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

test_that("mixed formats outside line/meta still require a safe selector", {
  lines <- c(mixed_content_lines(TRUE), readLines(test_path("fixtures", "day_sample.txt"), encoding = "UTF-8"))
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

test_that("a line streaming section marker is not an unparsed app record", {
  marker <- "\u6d41\u5a92\u4f53"
  name <- paste0("Example (", marker, ")")
  record <- mixed_line_record(app = name)
  lines <- c(mixed_line_header(), record, marker, mixed_line_header("2024-01-02"),
    paste0(" ", marker, ",,"), mixed_line_header("2024-01-03"),
    mixed_line_record(app = name, start = 1704240000000, end = 1704240060000), "damaged")
  parsed <- run_first_level_appusage(lines, input = "lines", participant_id = "demo")
  blocks <- appusage_day_blocks(parsed$metadata)
  expect_identical(blocks$state, c("recorded", "explicit_empty", "partial"))
  expect_equal(blocks$candidate_rows_in_block, c(1L, 0L, 2L))
  expect_identical(parsed$data$line$app_name, rep(name, 2L))
  # The suffix denotes background activity. Check all activities here.
  audit <- audit_appusage_days(parsed, activity = "all")
  expect_equal(audit$denominator_days, 1L)
  expect_equal(audit$mean_ms, 60000)
  ctx <- appusage_parse_context(c(mixed_line_header(), marker))
  expect_identical(appusage_failed_coverage(ctx)$blocks$state, "explicit_empty")
})

test_that("documented mixed line quality failures are terminal while incomplete parses retry", {
  root <- tempfile("mixed-quality-")
  dir.create(root)
  paths <- file.path(root, paste0(1:4, "_AppUsage_line_2024_1_1_0_0_0.txt"))
  cases <- list(mixed_line_record(), rep(mixed_line_record(), 2),
    mixed_line_record(app = "demo\u65e5\u671f"), mixed_line_record(end = NA_real_))
  for (i in seq_along(paths))
    writeLines(c(mixed_line_header(), cases[[i]], mixed_meta_lines()), paths[i], useBytes = TRUE)
  project <- file.path(root, "project")
  run <- run_appusage_workflow(paths, project_dir = project,
    config = appusage_config(execution = list(progress = FALSE)))
  one <- run$first_level
  expect_identical(one$status, c("success", rep("error", 3)))
  expect_true(all(run$second_level$second_level_status[-1] == "skipped"))
  expect_true(validate_appusage_project(project, verify = "content")$complete)
  row <- one[2, , drop = FALSE]
  path <- row$metadata_file[[1]]
  meta <- appusage_read_cache_json(path)
  expect_true(appusage_mixed_quality_failure(row, meta))
  verify <- function(m) appusage_parse_evidence(row, m, meta$module_state$parse, paths[2])$reason
  absent <- meta
  absent$structural_quality$n_exact_duplicates <- NULL
  expect_identical(verify(absent), "incomplete_parse")
  absent <- meta
  absent$module_state$components <- NULL
  expect_identical(verify(absent), "incomplete_parse")
  absent$export$component_selection_rule <- NULL
  expect_identical(verify(absent), "incomplete_parse")
  absent <- meta
  absent$module_state$components$states$meta <- "unverified"
  expect_identical(verify(absent), "incomplete_parse")
  absent <- meta
  absent$errors <- NULL
  expect_identical(verify(absent), "incomplete_parse")
  cat("\nchanged", file = paths[2], append = TRUE)
  expect_identical(verify(meta), "source_changed")
})

test_that("interleaved blocks retain source order, repeated events and decoded tails", {
  meta <- mixed_meta_lines()
  line <- c(mixed_line_header(), mixed_line_record())
  earlier <- sub("T:1704067200000", "T:1704067100000", meta[4], fixed = TRUE)
  earlier <- sub(",1,$", ",32,", earlier)
  tail <- ' cfg=a, "quoted", \u8868\u4e00,\u8868\u4e8c  '
  malformed <- sub(",1,$", ",v2, ", meta[4])
  # The repeated Table 2 uses two extra leading columns.
  lines <- c(meta[1:4], line, paste0(",,", c(meta[3], paste0(earlier, tail),
                                             paste0(earlier, tail), malformed)))
  first <- run_first_level_appusage(lines, input = "lines", participant_id = "demo")
  expect_identical(first$status, "success")
  second <- run_second_level_appusage(first)
  for (events in list(first$data$meta_events, second$data$event)) {
    expect_identical(events$source_line, c(4L, 8L, 9L, 10L))
    expect_identical(events$event_ts_ms, c(1704067200000, 1704067100000, 1704067100000, 1704067200000))
    expect_identical(events$event_type, c(1, 32, 32, NA_real_))
    expect_identical(events$event_type_raw, c("1", "32", "32", "v2"))
    expect_identical(events$configuration_tail, c("", tail, tail, " "))
    expect_identical(events$configuration_extra_fields, c(FALSE, TRUE, TRUE, FALSE))
  }
  expect_equal(nrow(first$data$meta_summary), 1L)
  expect_equal(nrow(second$data$episode), 1L)
  expect_identical(second$data$daily$duration_ms, 60000)
})

test_that("standalone table 2 requires a line block in auto mode", {
  line <- c(mixed_line_header(), mixed_line_record())
  meta <- mixed_meta_lines()
  for (input in list(meta[1:2], meta[3:4], c(line, meta[1:2]))) {
    check <- appusage_source_preflight(input, input = "lines")
    expect_identical(check$status, "incomplete_meta_blocks")
    expect_identical(detect_appusage_type(input, input = "lines"), "unknown")
  }
  mixed <- c(line, meta[3:4])
  first <- run_first_level_appusage(mixed, input = "lines", participant_id = "demo")
  expect_identical(names(first$data), c("line", "meta_events"))
  expect_identical(first$data$meta_events$source_line, 4L)
  expect_identical(detect_appusage_type(mixed, input = "lines"), "line")
  for (type in c("line", "meta")) {
    one <- run_first_level_appusage(mixed, input = "lines", type = type, participant_id = "demo")
    expect_identical(one$type, type)
    expect_identical(one$metadata$export$component_selection_rule, "explicit_component")
    expect_true(xor("line" %in% names(one$data), "meta_events" %in% names(one$data)))
  }
})

test_that("empty and repeated meta blocks remain distinct at line boundaries", {
  line <- c(mixed_line_header(), mixed_line_record())
  meta <- mixed_meta_lines()
  empty <- run_first_level_appusage(c(line, meta[3L]), input = "lines", participant_id = "demo")
  second <- run_second_level_appusage(empty)
  expect_identical(second$data$event$source_line, integer())
  expect_identical(second$data$event$configuration_tail, character())
  expect_identical(second$data$daily$duration_ms, 60000)

  # A repeated Table 1 has no right to read through the following line block.
  missing <- ",System App,,Main,,bad-ms,,"
  lines <- c(meta[1:2], line, meta[1:3], paste0(meta[4], meta[3]), missing)
  parsed <- parse_meta(lines, input = "lines", strict = TRUE)
  expect_identical(parsed$summary$package_name, rep("org.example.meta", 2L))
  expect_identical(parsed$events$source_line, 8:9)
  expect_identical(parsed$events$configuration_tail, c(meta[3], ""))
  expect_true(is.na(parsed$events$package_name[2L]))
  expect_true(is.na(parsed$events$event_ts_ms[2L]))
  expect_true(is.na(parsed$events$event_type[2L]))
})
