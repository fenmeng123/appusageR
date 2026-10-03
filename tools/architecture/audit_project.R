# Exhaustive per-source audit with one source's payloads in memory at a time.
# Usage: Rscript audit_project.R /private/project /private/audit
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 2L)
library(appusageR)
ns <- asNamespace("appusageR")
internal <- function(name) get(name, ns, inherits = FALSE)
project <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
destination <- args[[2]]
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
first <- read.csv(file.path(project, "analytic_summary_table_proclevel-1.csv"),
  stringsAsFactors = FALSE, check.names = FALSE)
second <- read.csv(file.path(project, "analytic_summary_table_proclevel-2.csv"),
  stringsAsFactors = FALSE, check.names = FALSE)
stopifnot(nrow(first) == nrow(second), !anyDuplicated(first$source_record_key),
  !anyDuplicated(second$source_record_key), setequal(first$source_record_key, second$source_record_key))
config <- readRDS(file.path(project, "appusage_configuration.rds"))
options <- internal("appusage_config_second_options")(config)
records <- vector("list", nrow(first))
artifact_paths <- character()
json_roundtrip <- function(x) jsonlite::fromJSON(jsonlite::toJSON(x, auto_unbox = TRUE,
  null = "null", na = "null", digits = 4), simplifyVector = TRUE)
assert_json <- function(x, y, label) {
  if (!internal("appusage_contract_equal")(json_roundtrip(x), json_roundtrip(y))) {
    stop(paste(label, paste(all.equal(json_roundtrip(x), json_roundtrip(y)), collapse = "; ")))
  }
}
read_data <- function(path) {
  env <- new.env(parent = emptyenv())
  loaded <- load(path, envir = env)
  stopifnot(identical(loaded, "data"), identical(ls(env), "data"))
  env$data
}
assert_raw_contract <- function(data, type, tz) {
  prototypes <- switch(type,
    line = list(line = internal("empty_line_tibble")()),
    meta = list(meta_summary = internal("empty_meta_summary_tibble")(),
      meta_events = internal("empty_meta_events_tibble")()),
    day = list(day = internal("empty_day_tibble")()),
    app = list(app = internal("empty_day_tibble")()))
  stopifnot(is.list(data), identical(names(data), names(prototypes)))
  for (name in names(prototypes)) {
    expected <- internal("strip_individual_columns")(prototypes[[name]])
    actual <- data[[name]]
    stopifnot(is.data.frame(actual), identical(names(actual), names(expected)),
      identical(lapply(actual, class), lapply(expected, class)))
    if (all(c("duration_ms", "duration_min") %in% names(actual))) {
      stopifnot(identical(actual$duration_min, actual$duration_ms / 60000))
    }
  }
  if (type == "line") {
    line <- data$line
    stopifnot(identical(line$duration_ms, line$end_ts_ms - line$start_ts_ms),
      identical(line$start_datetime, internal("ms_to_datetime")(line$start_ts_ms, tz)),
      identical(line$end_datetime, internal("ms_to_datetime")(line$end_ts_ms, tz)),
      identical(line$date, internal("appusage_date_from_datetime")(line$start_datetime, tz = tz)))
  }
  if (type == "meta" && nrow(data$meta_events) > 0L) {
    events <- data$meta_events
    stopifnot(identical(events$event_datetime,
      internal("ms_to_datetime")(events$event_ts_ms, tz)),
      identical(events$date, internal("appusage_date_from_datetime")(events$event_datetime, tz = tz)))
  }
  invisible(TRUE)
}
started <- Sys.time()
for (i in seq_len(nrow(first))) {
  row <- first[i, , drop = FALSE]
  stage <- "source_metadata"
  status <- "success"
  n_event <- n_episode <- n_daily <- NA_integer_
  failure <- tryCatch({
    meta1 <- jsonlite::read_json(row$metadata_file, simplifyVector = TRUE)
    artifact_paths <- c(artifact_paths, row$metadata_file)
    stopifnot(identical(meta1$identity$source_record_key, row$source_record_key),
      identical(row$source_fingerprint, internal("appusage_source_fingerprint")(row$source_file)),
      identical(meta1$source$source_fingerprint, row$source_fingerprint),
      identical(meta1$export$detected_type, row$detected_type),
      identical(meta1$processing$first_level_status, row$status),
      internal("appusage_normalized_paths_equal")(meta1$outputs$metadata_json, row$metadata_file))
    row2 <- second[match(row$source_record_key, second$source_record_key), , drop = FALSE]
    if (row$status == "success") {
      stage <- "proc1_payload"
      raw <- read_data(row$data_file)
      assert_raw_contract(raw, row$detected_type, config$time$tz)
      artifact_paths <- c(artifact_paths, row$data_file)
      stopifnot(!internal("first_level_is_empty")(raw),
        internal("appusage_artifact_valid")(row$data_file, meta1$module_state$artifact),
        internal("appusage_normalized_paths_equal")(meta1$outputs$first_level_rda, row$data_file))
      assert_json(meta1$counts, internal("data_counts")(raw, meta1$warning_messages),
        "Raw table counts differ")
      stopifnot(row2$second_level_status == "success")
      path2 <- internal("appusage_summary_proc2_paths")(row2)[[1]]
      json2 <- internal("second_level_metadata_path")(path2)
      stage <- "proc2_payload"
      data <- read_data(path2)
      meta2 <- jsonlite::read_json(json2, simplifyVector = TRUE)
      internal("appusage_validate_second_level_success_metadata")(json2, path2, json2)
      artifact_paths <- c(artifact_paths, path2, json2)
      stopifnot(identical(names(data), c("event", "episode", "daily")),
        all(vapply(data, is.data.frame, logical(1))),
        internal("appusage_artifact_valid")(path2, meta2$module_state$artifact),
        identical(meta2$identity$source_record_key, row$source_record_key),
        identical(meta2$source$source_fingerprint, row$source_fingerprint),
        meta2$processing$second_level_status == "success")
      n_event <- nrow(data$event); n_episode <- nrow(data$episode); n_daily <- nrow(data$daily)
      # Unavailable grains must be empty. Supported grains can legitimately
      # have no rows when the source/explicit configuration supplies none;
      # exact recomputation below checks their full values and types.
      if (row$detected_type == "line") stopifnot(n_event == 0L)
      if (row$detected_type %in% c("day", "app")) stopifnot(n_event == 0L, n_episode == 0L)
      stage <- "exact_research_recomputation"
      scientific <- options[intersect(names(options), names(formals(make_second_level_appusage)))]
      expected <- do.call(make_second_level_appusage,
        c(list(data = raw, export_type = row$detected_type), scientific))
      if (!identical(data, expected)) stop(paste(all.equal(data, expected), collapse = "; "))
      check <- internal("appusage_validate_second_level_daily")(data, tz = config$time$tz)
      stopifnot(check$status != "error")
      stage <- "full_qc_recomputation"
      result <- assess_appusage_qc(data, metadata = meta2,
        participant_id = as.character(row$participant_id), config = config)
      expected_meta <- internal("update_qc_metadata")(meta2, json2, json2, result,
        started_at = Sys.time(), finished_at = Sys.time())
      actual_qc <- meta2$qc
      predicted_qc <- expected_meta$qc
      actual_qc$qc_created_at <- predicted_qc$qc_created_at <- NULL
      # The established contract names inline and independent execution
      # separately. Only this enumerated method label and generation time vary.
      stopifnot(actual_qc$qc_function %in% c("qc_appusage_day", "qc_appusage_day_inline"),
        predicted_qc$qc_function == "qc_appusage_day")
      actual_qc$qc_function <- predicted_qc$qc_function <- NULL
      assert_json(actual_qc, predicted_qc, "QC values differ")
      meta2$anomaly_qc$created_at <- expected_meta$anomaly_qc$created_at <- NULL
      meta2$anomaly_qc$source_anomaly_qc$created_at <-
        expected_meta$anomaly_qc$source_anomaly_qc$created_at <- NULL
      meta2$source_anomaly_qc$created_at <- expected_meta$source_anomaly_qc$created_at <- NULL
      assert_json(meta2$anomaly_qc, expected_meta$anomaly_qc, "Anomaly QC differs")
      assert_json(meta2$source_anomaly_qc, expected_meta$source_anomaly_qc, "Source QC differs")
      stopifnot(result$qc_status == meta2$processing$qc_status,
        meta2$counts$n_event_rows == n_event, meta2$counts$n_episode_rows == n_episode,
        meta2$counts$n_daily_rows == n_daily)
      rm(raw, data, expected, result, expected_meta)
    } else {
      stage <- "explained_upstream_failure"
      stopifnot(row2$second_level_status == "skipped", length(meta1$errors) > 0L,
        !is.na(row$failure_family), nzchar(row$failure_family))
    }
    NULL
  }, error = function(e) conditionMessage(e))
  records[[i]] <- data.frame(index = i, source_record_key = row$source_record_key,
    type = row$detected_type, first_status = row$status,
    audit_status = if (is.null(failure)) "passed" else "failed",
    last_check = stage, error = if (is.null(failure)) NA_character_ else failure,
    n_event = n_event, n_episode = n_episode, n_daily = n_daily,
    stringsAsFactors = FALSE)
  write.csv(do.call(rbind, records[seq_len(i)]), file.path(destination, "source_audit.csv"),
    row.names = FALSE, na = "")
  if (i %% 10L == 0L || i == nrow(first)) {
    cat(sprintf("Audited %d/%d sources; current %s\n", i, nrow(first), records[[i]]$audit_status))
    gc(verbose = FALSE)
  }
}
audit <- do.call(rbind, records)
all_rda <- unlist(lapply(c("proclevel-1", "proclevel-2"), function(stage)
  list.files(file.path(project, stage), pattern = "[.]rda$", full.names = TRUE)), use.names = FALSE)
stopifnot(setequal(normalizePath(all_rda, winslash = "/"),
  normalizePath(artifact_paths[stringi::stri_endswith_fixed(artifact_paths, ".rda")], winslash = "/")))
all_json <- unlist(lapply(c("proclevel-1", "proclevel-2"), function(stage)
  list.files(file.path(project, stage), pattern = "[.]json$", full.names = TRUE)), use.names = FALSE)
stopifnot(setequal(normalizePath(all_json, winslash = "/"),
  normalizePath(artifact_paths[stringi::stri_endswith_fixed(artifact_paths, ".json")], winslash = "/")))
owner <- file.path(project, "self_report_link_result.rds")
link_audit <- NULL
if (file.exists(owner)) {
  links <- readRDS(owner)
  matching <- links$matched_self_report
  available <- matching$moSens_match_status == "matched"
  stopifnot(all(file.exists(file.path(project, matching$moSens_data_dir[available]))))
  adapter_path <- file.path(project, "workflow_configuration.rds")
  if (file.exists(adapter_path)) {
    adapter <- readRDS(adapter_path)
    survey <- internal("appusage_read_self_report_workbook")(
      adapter$resolved_self_report_file, sheet = adapter$self_report_sheet,
      n_max = adapter$self_report_n_max,
      guess_max = if (is.na(adapter$self_report_guess_max)) NULL else adapter$self_report_guess_max,
      col_types = if (length(adapter$self_report_col_types)) adapter$self_report_col_types else NULL)$data
    manifest <- read.csv(file.path(project, "diagnostics", "project_manifest.csv"),
      stringsAsFactors = FALSE, check.names = FALSE, na.strings = "")
    repeated <- internal("appusage_match_self_report_table")(survey, manifest,
      project_root = project, first = first, second = second,
      sequence_col = adapter$sequence_col, upload_col = adapter$upload_col,
      submit_time_col = if (is.na(adapter$submit_time_col)) NULL else adapter$submit_time_col,
      export_type_priority = adapter$export_type_priority,
      project_id = adapter$project_id, project_name = adapter$project_name)
    assert_json(links$matched_self_report, repeated$matched_self_report, "Questionnaire relationship recomputation differs")
    assert_json(links$file_matches, repeated$file_matches, "Source relationship recomputation differs")
    stopifnot(nrow(matching) == nrow(survey))
  }
  link_audit <- list(questionnaire_rows = nrow(matching), matched = sum(available),
    unmatched = sum(!available), source_rows = nrow(links$file_matches))
}
fingerprints <- tools::md5sum(unique(artifact_paths))
write.csv(data.frame(path = names(fingerprints), md5 = unname(fingerprints)),
  file.path(destination, "artifact_fingerprints.csv"), row.names = FALSE)
report <- list(sources = nrow(first), first_status = as.list(table(first$status)),
  second_status = as.list(table(second$second_level_status)),
  qc_status = as.list(table(second$qc_status, useNA = "ifany")),
  audit_status = as.list(table(audit$audit_status)), rda_count = length(all_rda),
  json_count = length(all_json),
  links = link_audit, elapsed_sec = as.numeric(difftime(Sys.time(), started, units = "secs")))
jsonlite::write_json(report, file.path(destination, "audit_summary.json"),
  pretty = TRUE, auto_unbox = TRUE, na = "null")
print(report)
if (any(audit$audit_status != "passed")) stop("Artifact audit found failures; see source_audit.csv")
