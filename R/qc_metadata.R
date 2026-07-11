#' Update second-level QC metadata for an APP Usage project
#'
#' Runs daily-data QC one second-level cache at a time, updates matching
#' `proc-2` JSON metadata under `proclevel-2`, and refreshes
#' `analytic_summary_table_proclevel-2.csv` from those metadata files.
#'
#' @param project_dir Project folder produced by [read_appusage_batch()] and
#'   [write_second_level_batch()].
#' @param output_dir Deprecated. Routine QC updates `proc-2` JSON and does not
#'   create a separate output directory.
#' @param strict If `TRUE`, stop after the first per-file QC error. If `FALSE`,
#'   record the error in metadata and continue.
#' @param overwrite Whether existing QC fields in `proc-2` JSON may be updated.
#' @param progress Whether to print simple progress messages.
#' @param require_all_weekdays Whether Monday through Sunday must be covered.
#' @param min_nonempty_days Minimum number of non-empty recorded days.
#' @param use_all_apps_row Passed to [qc_appusage_day()].
#' @param include_collection_app Passed to [qc_appusage_day()].
#' @param drop_likely_total_all_rows Passed to [qc_appusage_day()].
#' @param all_row_tolerance Passed to [qc_appusage_day()].
#' @param max_episode_ms,max_daily_app_ms,max_daily_total_ms Numeric anomaly
#'   thresholds in milliseconds.
#' @param max_export_lookback_days Maximum expected lookback in days from native
#'   export timestamp to observed record dates.
#' @param meta_diff_abs_ms,meta_diff_ratio Thresholds for meta summary-vs-episode
#'   duration disagreement checks.
#'
#' @return Invisibly returns the refreshed second-level analytic summary tibble.
#' @export
write_qc_metadata_batch <- function(project_dir, output_dir = NULL,
                                    strict = FALSE, overwrite = TRUE,
                                    progress = TRUE,
                                    require_all_weekdays = TRUE,
                                    min_nonempty_days = 7,
                                    use_all_apps_row = FALSE,
                                    include_collection_app = TRUE,
                                    drop_likely_total_all_rows = TRUE,
                                    all_row_tolerance = 0.10,
                                    max_episode_ms = 24 * 60 * 60 * 1000,
                                    max_daily_app_ms = 24 * 60 * 60 * 1000,
                                    max_daily_total_ms = 24 * 60 * 60 * 1000,
                                    max_export_lookback_days = 31,
                                    meta_diff_abs_ms = 60 * 1000,
                                    meta_diff_ratio = 0.20) {
  if (length(project_dir) != 1 || is.na(project_dir) || !dir.exists(project_dir)) {
    cli::cli_abort("`project_dir` must be an existing project directory.")
  }
  if (!is.null(output_dir)) {
    cli::cli_abort("`output_dir` is no longer used; routine QC updates `proc-2` JSON metadata.")
  }
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  proclevel_2 <- file.path(project_dir, "proclevel-2")
  if (!dir.exists(proclevel_2)) {
    cli::cli_abort("Project is missing {.path proclevel-2}.")
  }
  metadata_files <- ensure_second_level_metadata_files(project_dir)
  if (length(metadata_files) == 0) {
    cli::cli_abort("No second-level metadata JSON files were found in {.path {proclevel_2}}.")
  }

  for (i in seq_along(metadata_files)) {
    if (isTRUE(progress) && (i == 1 || i == length(metadata_files))) {
      message(sprintf("Writing QC metadata file %d/%d", i, length(metadata_files)))
    }
    metadata_files[[i]] <- write_qc_metadata_one(
      metadata_file = metadata_files[[i]],
      overwrite = overwrite,
      require_all_weekdays = require_all_weekdays,
      min_nonempty_days = min_nonempty_days,
      use_all_apps_row = use_all_apps_row,
      include_collection_app = include_collection_app,
      drop_likely_total_all_rows = drop_likely_total_all_rows,
      all_row_tolerance = all_row_tolerance,
      max_episode_ms = max_episode_ms,
      max_daily_app_ms = max_daily_app_ms,
      max_daily_total_ms = max_daily_total_ms,
      max_export_lookback_days = max_export_lookback_days,
      meta_diff_abs_ms = meta_diff_abs_ms,
      meta_diff_ratio = meta_diff_ratio
    )
    if (isTRUE(strict)) {
      qc_metadata <- jsonlite::read_json(metadata_files[[i]], simplifyVector = TRUE)
      qc_status <- qc_metadata_value(qc_metadata, c("processing", "qc_status"))
      if (identical(qc_status, "error")) {
        message <- qc_metadata_value(qc_metadata, c("qc", "qc_error_message"))
        cli::cli_abort("QC metadata failed at record {i}: {message}")
      }
    }
  }

  summary <- build_qc_summary_from_metadata(metadata_files)
  summary_file <- file.path(project_dir, "analytic_summary_table_proclevel-2.csv")
  utils::write.csv(summary, summary_file, row.names = FALSE, na = "")
  write_dataset_description_json(
    project_info_from_root(project_dir),
    summary = summary,
    proclevel = 2,
    summary_file = summary_file,
    status = "success"
  )

  invisible(summary)
}

ensure_second_level_metadata_files <- function(project_dir) {
  proclevel_2 <- file.path(project_dir, "proclevel-2")
  metadata_files <- sort(list.files(
    proclevel_2,
    pattern = "_proc-2[.]json$",
    full.names = TRUE
  ))
  if (length(metadata_files) > 0) {
    return(metadata_files)
  }

  rda_files <- sort(list.files(
    proclevel_2,
    pattern = "_proc-2[.]rda$",
    full.names = TRUE
  ))
  if (length(rda_files) == 0) {
    return(character())
  }
  vapply(rda_files, create_missing_second_level_metadata, character(1), project_dir = project_dir)
}

create_missing_second_level_metadata <- function(second_level_rda, project_dir) {
  started_at <- Sys.time()
  entities <- parse_appusage_filename(second_level_rda)
  first_level_rda <- file.path(
    project_dir,
    "proclevel-1",
    build_appusage_filename(
      participant_id = entities$sub %||% "unknown",
      export_type = entities$type %||% "unknown",
      proc = 1,
      extension = "rda"
    )
  )
  first_level_json <- first_level_metadata_path(first_level_rda)
  result <- tryCatch(
    {
      data <- load_appusage_data_object(second_level_rda)
      list(data = data, error = NULL, status = "success")
    },
    error = function(e) list(data = NULL, error = e, status = "error")
  )
  first_metadata <- if (file.exists(first_level_json)) {
    read_first_level_metadata(first_level_json)$metadata
  } else {
    read_first_level_metadata_for_second(first_level_rda)
  }
  metadata_file <- second_level_metadata_path(second_level_rda)
  metadata <- build_second_level_metadata(
    first_metadata = first_metadata,
    first_level_rda = first_level_rda,
    first_level_metadata_file = first_level_json,
    second_level_rda = second_level_rda,
    second_level_metadata_file = metadata_file,
    second_level_data = result$data,
    status = result$status,
    error = result$error,
    include_collection_app = NA,
    max_episode_ms = NA_real_,
    max_daily_app_ms = NA_real_,
    started_at = started_at,
    finished_at = Sys.time()
  )
  write_metadata_json(metadata, metadata_file)
  normalizePath(metadata_file, winslash = "/", mustWork = FALSE)
}

write_qc_metadata_one <- function(metadata_file, overwrite,
                                  require_all_weekdays,
                                  min_nonempty_days, use_all_apps_row,
                                  include_collection_app,
                                  drop_likely_total_all_rows,
                                  all_row_tolerance,
                                  max_episode_ms,
                                  max_daily_app_ms,
                                  max_daily_total_ms,
                                  max_export_lookback_days,
                                  meta_diff_abs_ms,
                                  meta_diff_ratio) {
  started_at <- Sys.time()
  metadata_read <- read_first_level_metadata(metadata_file)
  metadata <- metadata_read$metadata
  participant_id <- qc_metadata_participant_id(metadata, metadata_file)
  if (!isTRUE(overwrite)) {
    existing_status <- qc_metadata_value(metadata, c("processing", "qc_status"))
    if (!identical(existing_status, "not_run") && !is.na(existing_status)) {
      cli::cli_abort("QC metadata already exists in {.path {metadata_file}}")
    }
  }

  if (!is.null(metadata_read$error)) {
    result <- qc_error_result(
      metadata = metadata,
      second_level_rda = NA_character_,
      message = paste(
        "First-level metadata JSON could not be read:",
        conditionMessage(metadata_read$error)
      )
    )
  } else {
    second_level_rda <- infer_second_level_rda_path(
      metadata = metadata,
      metadata_file = metadata_file,
      second_level_dir = dirname(metadata_file)
    )
    result <- run_qc_for_second_level_file(
      second_level_rda = second_level_rda,
      participant_id = participant_id,
      require_all_weekdays = require_all_weekdays,
      min_nonempty_days = min_nonempty_days,
      use_all_apps_row = use_all_apps_row,
      include_collection_app = include_collection_app,
      drop_likely_total_all_rows = drop_likely_total_all_rows,
      all_row_tolerance = all_row_tolerance,
      metadata = metadata,
      max_episode_ms = max_episode_ms,
      max_daily_app_ms = max_daily_app_ms,
      max_daily_total_ms = max_daily_total_ms,
      max_export_lookback_days = max_export_lookback_days,
      meta_diff_abs_ms = meta_diff_abs_ms,
      meta_diff_ratio = meta_diff_ratio
    )
  }

  finished_at <- Sys.time()
  metadata <- update_qc_metadata(
    metadata = metadata,
    metadata_file = metadata_file,
    qc_file = metadata_file,
    result = result,
    started_at = started_at,
    finished_at = finished_at
  )
  write_metadata_json(metadata, metadata_file)
  normalizePath(metadata_file, winslash = "/", mustWork = FALSE)
}

read_first_level_metadata <- function(metadata_file) {
  tryCatch(
    list(
      metadata = jsonlite::read_json(metadata_file, simplifyVector = TRUE),
      error = NULL
    ),
    error = function(e) {
      list(
        metadata = minimal_qc_metadata(metadata_file),
        error = e
      )
    }
  )
}

minimal_qc_metadata <- function(metadata_file) {
  entities <- parse_appusage_filename(metadata_file)
  participant_id <- entities$sub %||% "unknown"
  export_type <- entities$type %||% "unknown"
  list(
    schema_version = "0.3.0",
    package_version = as.character(utils::packageVersion("appusageR")),
    parser_version = as.character(utils::packageVersion("appusageR")),
    created_at = NA_character_,
    updated_at = NA_character_,
    participant_id = participant_id,
    participant_id_source = NA_character_,
    identity = list(
      participant_id = participant_id,
      participant_id_source = NA_character_,
      wenjuanxing_sequence_id = NA_integer_
    ),
    source = list(),
    export = list(
      detected_type = export_type,
      content_detected_export_type = export_type,
      native_export_type_from_filename = NA_character_,
      export_type_match = NA
    ),
    processing = list(
      first_level_status = "error",
      second_level_status = "pending",
      qc_status = "pending"
    ),
    outputs = list(
      first_level_metadata_json = normalizePath(metadata_file, winslash = "/", mustWork = FALSE),
      first_level_rda = NA_character_,
      second_level_rda = NA_character_
    ),
    counts = list(n_parse_warnings = NA_integer_),
    qc = list(),
    errors = list()
  )
}

infer_second_level_rda_path <- function(metadata, metadata_file, second_level_dir) {
  recorded <- qc_metadata_value(metadata, c("outputs", "second_level_rda"))
  if (is_present_string(recorded)) {
    return(normalizePath(recorded, winslash = "/", mustWork = FALSE))
  }
  entities <- parse_appusage_filename(metadata_file)
  participant_id <- qc_metadata_participant_id(metadata, metadata_file)
  export_type <- qc_metadata_export_type(metadata, metadata_file)
  file.path(
    second_level_dir,
    build_appusage_filename(
      participant_id = entities$sub %||% participant_id,
      export_type = entities$type %||% export_type,
      proc = 2,
      extension = "rda"
    )
  )
}

run_qc_for_second_level_file <- function(second_level_rda, participant_id,
                                         require_all_weekdays,
                                         min_nonempty_days,
                                         use_all_apps_row,
                                         include_collection_app,
                                         drop_likely_total_all_rows,
                                         all_row_tolerance,
                                         metadata = NULL,
                                         max_episode_ms = 24 * 60 * 60 * 1000,
                                         max_daily_app_ms = 24 * 60 * 60 * 1000,
                                         max_daily_total_ms = 24 * 60 * 60 * 1000,
                                         max_export_lookback_days = 31,
                                         meta_diff_abs_ms = 60 * 1000,
                                         meta_diff_ratio = 0.20) {
  tryCatch(
    {
      if (!is_present_string(second_level_rda) || !file.exists(second_level_rda)) {
        cli::cli_abort("Second-level RDA file does not exist: {.path {second_level_rda}}")
      }
      data <- load_appusage_data_object(second_level_rda)
      run_qc_for_second_level_data(
        data = data,
        second_level_rda = second_level_rda,
        participant_id = participant_id,
        require_all_weekdays = require_all_weekdays,
        min_nonempty_days = min_nonempty_days,
        use_all_apps_row = use_all_apps_row,
        include_collection_app = include_collection_app,
        drop_likely_total_all_rows = drop_likely_total_all_rows,
        all_row_tolerance = all_row_tolerance,
        metadata = metadata,
        max_episode_ms = max_episode_ms,
        max_daily_app_ms = max_daily_app_ms,
        max_daily_total_ms = max_daily_total_ms,
        max_export_lookback_days = max_export_lookback_days,
        meta_diff_abs_ms = meta_diff_abs_ms,
        meta_diff_ratio = meta_diff_ratio
      )
    },
    error = function(e) {
      counts <- if (is_present_string(second_level_rda) && file.exists(second_level_rda)) {
        tryCatch(
          {
            data <- load_appusage_data_object(second_level_rda)
            counts <- second_level_qc_counts(data)
            anomaly_qc <- qc_appusage_anomalies(
              data,
              metadata = metadata,
              max_episode_ms = max_episode_ms,
              max_daily_app_ms = max_daily_app_ms,
              max_daily_total_ms = max_daily_total_ms,
              max_export_lookback_days = max_export_lookback_days,
              meta_diff_abs_ms = meta_diff_abs_ms,
              meta_diff_ratio = meta_diff_ratio
            )
            counts$n_anomalies <- anomaly_qc$n_anomalies_total
            counts
          },
          error = function(e2) empty_qc_counts()
        )
      } else {
        empty_qc_counts()
      }
      anomaly_qc <- appusage_empty_anomaly_qc(
        status = "error",
        thresholds = appusage_anomaly_thresholds(
          max_episode_ms = max_episode_ms,
          max_daily_app_ms = max_daily_app_ms,
          max_daily_total_ms = max_daily_total_ms,
          max_export_lookback_days = max_export_lookback_days,
          meta_diff_abs_ms = meta_diff_abs_ms,
          meta_diff_ratio = meta_diff_ratio
        ),
        error_message = conditionMessage(e)
      )
      qc_error_result(
        metadata = NULL,
        second_level_rda = second_level_rda,
        message = conditionMessage(e),
        counts = counts,
        anomaly_qc = anomaly_qc
      )
    }
  )
}

run_qc_for_second_level_data <- function(data, second_level_rda, participant_id,
                                         require_all_weekdays,
                                         min_nonempty_days,
                                         use_all_apps_row,
                                         include_collection_app,
                                         drop_likely_total_all_rows,
                                         all_row_tolerance,
                                         metadata = NULL,
                                         max_episode_ms = 24 * 60 * 60 * 1000,
                                         max_daily_app_ms = 24 * 60 * 60 * 1000,
                                         max_daily_total_ms = 24 * 60 * 60 * 1000,
                                         max_export_lookback_days = 31,
                                         meta_diff_abs_ms = 60 * 1000,
                                         meta_diff_ratio = 0.20) {
  counts <- second_level_qc_counts(data)
  anomaly_qc <- qc_appusage_anomalies(
    data,
    metadata = metadata,
    max_episode_ms = max_episode_ms,
    max_daily_app_ms = max_daily_app_ms,
    max_daily_total_ms = max_daily_total_ms,
    max_export_lookback_days = max_export_lookback_days,
    meta_diff_abs_ms = meta_diff_abs_ms,
    meta_diff_ratio = meta_diff_ratio
  )
  counts$n_anomalies <- anomaly_qc$n_anomalies_total
  if (is.null(data$daily) || !is.data.frame(data$daily)) {
    cli::cli_abort("Second-level RDA does not contain `data$daily`.")
  }
  if (nrow(data$daily) == 0) {
    cli::cli_abort("Second-level `data$daily` has zero rows.")
  }

  daily <- data$daily
  if (!"participant_id" %in% names(daily)) {
    daily$participant_id <- participant_id
  }
  qc <- qc_appusage_day(
    daily,
    participant_col = "participant_id",
    require_all_weekdays = require_all_weekdays,
    min_nonempty_days = min_nonempty_days,
    use_all_apps_row = use_all_apps_row,
    include_collection_app = include_collection_app,
    drop_likely_total_all_rows = drop_likely_total_all_rows,
    all_row_tolerance = all_row_tolerance
  )
  if (nrow(qc) == 0) {
    cli::cli_abort("Daily QC returned no rows.")
  }
  qc_row <- qc[1, , drop = FALSE]
  pass_qc <- isTRUE(qc_row$pass_qc[[1]])
  source_eligibility <- anomaly_qc$source_anomaly_qc$eligibility %||% list()
  episode_eligible <- pass_qc && counts$n_episode_rows > 0 &&
    !isTRUE(source_eligibility$episode_ineligible)
  daily_eligible <- pass_qc && counts$n_daily_rows > 0 &&
    !isTRUE(source_eligibility$daily_ineligible)
  list(
    qc_status = "success",
    second_level_status = "success",
    second_level_rda = normalizePath(second_level_rda, winslash = "/", mustWork = FALSE),
    counts = counts,
    anomaly_qc = anomaly_qc,
    qc = list(
      pass_qc = pass_qc,
      qc_error_message = NA_character_,
      n_recorded_days = as.integer(qc_row$n_recorded_days[[1]]),
      n_nonempty_days = as.integer(qc_row$n_nonempty_days[[1]]),
      weekdays_covered = qc_row$weekdays_covered[[1]],
      has_monday = as.logical(qc_row$has_monday[[1]]),
      has_tuesday = as.logical(qc_row$has_tuesday[[1]]),
      has_wednesday = as.logical(qc_row$has_wednesday[[1]]),
      has_thursday = as.logical(qc_row$has_thursday[[1]]),
      has_friday = as.logical(qc_row$has_friday[[1]]),
      has_saturday = as.logical(qc_row$has_saturday[[1]]),
      has_sunday = as.logical(qc_row$has_sunday[[1]]),
      pass_min_days = as.logical(qc_row$pass_min_days[[1]]),
      pass_all_weekdays = as.logical(qc_row$pass_all_weekdays[[1]]),
      analysis_eligible_event = pass_qc && counts$n_event_rows > 0,
      analysis_eligible_episode = episode_eligible,
      analysis_eligible_daily = daily_eligible,
      analysis_ineligible_episode_reasons = source_eligibility$episode_reasons %||% NA_character_,
      analysis_ineligible_daily_reasons = source_eligibility$daily_reasons %||% NA_character_
    )
  )
}

load_appusage_data_object <- function(path) {
  env <- new.env(parent = emptyenv())
  loaded <- load(path, envir = env)
  if (!identical(loaded, "data")) {
    cli::cli_abort("RDA file must contain exactly one object named `data`.")
  }
  env$data
}

second_level_qc_counts <- function(data) {
  if (!is.list(data)) {
    cli::cli_abort("Second-level `data` object must be a list.")
  }
  frames <- list(
    event = data$event,
    episode = data$episode,
    daily = data$daily
  )
  list(
    n_event_rows = count_frame_rows(frames$event),
    n_episode_rows = count_frame_rows(frames$episode),
    n_daily_rows = count_frame_rows(frames$daily),
    n_anomalies = sum(vapply(frames, count_anomaly_rows, integer(1)), na.rm = TRUE),
    n_parse_warnings = sum(vapply(frames, count_parse_warning_rows, integer(1)), na.rm = TRUE)
  )
}

count_frame_rows <- function(x) {
  if (is.data.frame(x)) {
    return(nrow(x))
  }
  NA_integer_
}

count_anomaly_rows <- function(x) {
  if (!is.data.frame(x) || !"anomaly_any" %in% names(x)) {
    return(0L)
  }
  sum(x$anomaly_any %in% TRUE, na.rm = TRUE)
}

count_parse_warning_rows <- function(x) {
  if (!is.data.frame(x) || !"parse_warning" %in% names(x)) {
    return(0L)
  }
  warning <- as.character(x$parse_warning)
  sum(!is.na(warning) & nzchar(warning))
}

empty_qc_counts <- function() {
  list(
    n_event_rows = NA_integer_,
    n_episode_rows = NA_integer_,
    n_daily_rows = NA_integer_,
    n_anomalies = NA_integer_,
    n_parse_warnings = NA_integer_
  )
}

qc_error_result <- function(metadata, second_level_rda, message,
                            counts = empty_qc_counts(),
                            anomaly_qc = appusage_empty_anomaly_qc(
                              status = "error",
                              error_message = message
                            )) {
  list(
    qc_status = "error",
    second_level_status = if (is_present_string(second_level_rda) && file.exists(second_level_rda)) {
      "error"
    } else {
      "missing"
    },
    second_level_rda = ifelse(
      is_present_string(second_level_rda),
      normalizePath(second_level_rda, winslash = "/", mustWork = FALSE),
      NA_character_
    ),
    counts = counts,
    anomaly_qc = anomaly_qc,
    qc = list(
      pass_qc = FALSE,
      qc_error_message = message,
      n_recorded_days = NA_integer_,
      n_nonempty_days = NA_integer_,
      weekdays_covered = NA_character_,
      has_monday = NA,
      has_tuesday = NA,
      has_wednesday = NA,
      has_thursday = NA,
      has_friday = NA,
      has_saturday = NA,
      has_sunday = NA,
      pass_min_days = NA,
      pass_all_weekdays = NA,
      analysis_eligible_event = FALSE,
      analysis_eligible_episode = FALSE,
      analysis_eligible_daily = FALSE
    )
  )
}

update_qc_metadata <- function(metadata, metadata_file, qc_file, result,
                               started_at, finished_at) {
  if (is.null(metadata$processing) || !is.list(metadata$processing)) {
    metadata$processing <- list()
  }
  if (is.null(metadata$outputs) || !is.list(metadata$outputs)) {
    metadata$outputs <- list()
  }
  if (is.null(metadata$counts) || !is.list(metadata$counts)) {
    metadata$counts <- list()
  }

  metadata$updated_at <- format(finished_at, "%Y-%m-%dT%H:%M:%OS3%z")
  metadata$processing$second_level_status <- result$second_level_status
  metadata$processing$qc_status <- result$qc_status
  metadata$processing$qc_started_at <- format(started_at, "%Y-%m-%dT%H:%M:%OS3%z")
  metadata$processing$qc_finished_at <- format(finished_at, "%Y-%m-%dT%H:%M:%OS3%z")
  metadata$processing$qc_elapsed_sec <- as.numeric(difftime(finished_at, started_at, units = "secs"))

  metadata$outputs$metadata_json <- normalizePath(qc_file, winslash = "/", mustWork = FALSE)
  metadata$outputs$second_level_metadata_json <- normalizePath(qc_file, winslash = "/", mustWork = FALSE)
  metadata$outputs$second_level_rda <- result$second_level_rda

  metadata$counts$n_event_rows <- result$counts$n_event_rows
  metadata$counts$n_episode_rows <- result$counts$n_episode_rows
  metadata$counts$n_daily_rows <- result$counts$n_daily_rows
  metadata$counts$n_anomalies <- result$counts$n_anomalies
  metadata$counts$n_parse_warnings <- result$counts$n_parse_warnings
  metadata$counts$n_critical_anomalies <- result$anomaly_qc$n_critical_anomalies
  metadata$counts$n_warning_anomalies <- result$anomaly_qc$n_warning_anomalies

  metadata$qc <- c(
    list(
      qc_status = result$qc_status,
      qc_rule_version = "daily-qc-v1",
      qc_created_at = format(finished_at, "%Y-%m-%dT%H:%M:%OS3%z"),
      qc_function = "qc_appusage_day"
    ),
    result$qc
  )
  metadata$anomaly_qc <- result$anomaly_qc
  metadata$source_anomaly_qc <- result$anomaly_qc$source_anomaly_qc %||% list(
    status = "not_run", rule_version = "0.3.4-F"
  )
  metadata
}

build_qc_summary_from_metadata <- function(metadata_files, project_dir = NULL,
                                           strict = FALSE) {
  metadata_files <- as.character(metadata_files)
  metadata_files <- sort(metadata_files[!is.na(metadata_files) & nzchar(metadata_files)])
  project_dir <- project_dir %||% infer_project_dir_from_metadata_files(metadata_files)
  legacy_index <- legacy_proc3_metadata_index(project_dir)

  rows <- list()
  matched_legacy <- character()
  for (metadata_file in metadata_files) {
    key <- metadata_match_key(metadata_file)
    legacy_file <- if (is_present_string(key) && key %in% names(legacy_index)) {
      legacy_index[[key]]
    } else {
      NA_character_
    }
    if (is_present_string(legacy_file)) {
      matched_legacy <- c(matched_legacy, legacy_file)
    }
    rows[[length(rows) + 1L]] <- qc_summary_row_from_metadata(
      metadata_file,
      legacy_file = legacy_file,
      strict = strict
    )
  }

  unmatched_legacy <- setdiff(unname(legacy_index), matched_legacy)
  for (legacy_file in unmatched_legacy) {
    rows[[length(rows) + 1L]] <- qc_summary_row_from_legacy_metadata(
      legacy_file,
      strict = strict
    )
  }

  bind_qc_summary_rows(rows)
}

qc_summary_row_from_metadata <- function(metadata_file, legacy_file = NA_character_,
                                         strict = FALSE) {
  current <- read_qc_summary_metadata(metadata_file,
    metadata_label = "proc-2",
    strict = strict
  )
  if (!is.null(current$error)) {
    return(qc_summary_problem_row(
      metadata_file = metadata_file,
      metadata_label = "proc-2",
      error = current$error
    ))
  }

  current_row <- qc_summary_row_from_loaded_metadata(current$metadata, metadata_file)
  legacy_row <- NULL
  legacy_error_message <- NA_character_
  if (is_present_string(legacy_file)) {
    legacy <- read_qc_summary_metadata(legacy_file,
      metadata_label = "legacy proc-3",
      strict = strict
    )
    if (is.null(legacy$error)) {
      legacy_row <- qc_summary_row_from_loaded_metadata(legacy$metadata, legacy_file)
    } else {
      legacy_error_message <- paste(
        "Legacy proc-3 JSON could not be read:",
        conditionMessage(legacy$error)
      )
    }
  }

  merge_proc2_legacy_qc_summary(
    proc2_row = current_row,
    legacy_row = legacy_row,
    legacy_file = legacy_file,
    legacy_error_message = legacy_error_message
  )
}

qc_summary_row_from_legacy_metadata <- function(legacy_file, strict = FALSE) {
  legacy <- read_qc_summary_metadata(legacy_file,
    metadata_label = "legacy proc-3",
    strict = strict
  )
  if (!is.null(legacy$error)) {
    return(qc_summary_problem_row(
      metadata_file = legacy_file,
      metadata_label = "legacy proc-3",
      error = legacy$error,
      legacy_file = legacy_file
    ))
  }

  row <- qc_summary_row_from_loaded_metadata(legacy$metadata, legacy_file)
  row$second_level_metadata_file <- NA_character_
  row$qc_metadata_source <- if (summary_row_has_usable_qc(row)) {
    "legacy_proc-3"
  } else {
    "none"
  }
  row$legacy_proc3_json <- normalizePath(legacy_file, winslash = "/", mustWork = FALSE)
  row$legacy_qc_fallback_used <- summary_row_has_usable_qc(row)
  row$legacy_qc_conflict <- FALSE
  row$legacy_qc_conflict_fields <- NA_character_
  row$legacy_qc_error_message <- NA_character_
  row
}

qc_summary_row_from_loaded_metadata <- function(metadata, metadata_file) {
  qc_status <- qc_metadata_value(metadata, c("processing", "qc_status"))
  second_level_status <- qc_metadata_value(metadata, c("processing", "second_level_status"))
  row <- data.frame(
    participant_id = qc_metadata_value(metadata, c("participant_id")),
    participant_id_source = qc_metadata_value(metadata, c("participant_id_source")),
    wenjuanxing_sequence_id = qc_metadata_value(metadata, c("identity", "wenjuanxing_sequence_id"), default = NA_integer_),
    source_record_key = qc_metadata_value(metadata, c("identity", "source_record_key")),
    source_fingerprint = qc_metadata_value(metadata, c("source", "source_fingerprint")),
    source_cache_key = qc_metadata_value(metadata, c("identity", "source_cache_key")),
    detected_type = qc_metadata_value(metadata, c("export", "detected_type")),
    effective_timezone = qc_metadata_value(
      metadata,
      c("processing", "effective_timezone"),
      default = qc_metadata_value(
        metadata, c("export", "timezone"),
        default = appusage_default_timezone()
      )
    ),
    daily_self_check_status = qc_metadata_value(
      metadata, c("daily_aggregation_self_check", "status")
    ),
    daily_self_check_missing_source_keys = qc_metadata_value(
      metadata,
      c("daily_aggregation_self_check", "n_missing_or_unmatched_source_keys"),
      default = NA_integer_
    ),
    daily_self_check_missing_daily_duration = qc_metadata_value(
      metadata,
      c("daily_aggregation_self_check", "n_missing_daily_duration"),
      default = NA_integer_
    ),
    daily_self_check_numeric_mismatch = qc_metadata_value(
      metadata,
      c("daily_aggregation_self_check", "n_nonmissing_numeric_mismatch"),
      default = NA_integer_
    ),
    daily_self_check_episode_count_mismatch = qc_metadata_value(
      metadata,
      c("daily_aggregation_self_check", "n_episode_count_mismatch"),
      default = NA_integer_
    ),
    daily_self_check_duplicate_keys = qc_metadata_value(
      metadata,
      c("daily_aggregation_self_check", "n_duplicate_daily_keys"),
      default = NA_integer_
    ),
    daily_self_check_order_violation = qc_metadata_value(
      metadata,
      c("daily_aggregation_self_check", "order_violation"),
      default = NA
    ),
    daily_self_check_conservation_diff_ms = qc_metadata_value(
      metadata,
      c("daily_aggregation_self_check", "duration_conservation_difference_ms"),
      default = NA_real_
    ),
    filename_export_type = qc_metadata_value(metadata, c("export", "native_export_type_from_filename")),
    export_type_match = qc_metadata_value(metadata, c("export", "export_type_match"), default = NA),
    first_level_status = qc_metadata_value(metadata, c("processing", "first_level_status")),
    second_level_status = second_level_status,
    qc_status = qc_status,
    app_category_status = qc_metadata_value(metadata, c("processing", "app_category_status")),
    status = second_level_status,
    pass_qc = qc_metadata_value(metadata, c("qc", "pass_qc"), default = NA),
    analysis_eligible_event = qc_metadata_value(metadata, c("qc", "analysis_eligible_event"), default = NA),
    analysis_eligible_episode = qc_metadata_value(metadata, c("qc", "analysis_eligible_episode"), default = NA),
    analysis_eligible_daily = qc_metadata_value(metadata, c("qc", "analysis_eligible_daily"), default = NA),
    n_recorded_days = qc_metadata_value(metadata, c("qc", "n_recorded_days"), default = NA_integer_),
    n_nonempty_days = qc_metadata_value(metadata, c("qc", "n_nonempty_days"), default = NA_integer_),
    weekdays_covered = qc_metadata_value(metadata, c("qc", "weekdays_covered")),
    n_event_rows = qc_metadata_value(metadata, c("counts", "n_event_rows"), default = NA_integer_),
    n_episode_rows = qc_metadata_value(metadata, c("counts", "n_episode_rows"), default = NA_integer_),
    n_daily_rows = qc_metadata_value(metadata, c("counts", "n_daily_rows"), default = NA_integer_),
    n_anomalies = qc_metadata_value(
      metadata,
      c("anomaly_qc", "n_anomalies_total"),
      default = qc_metadata_value(metadata, c("counts", "n_anomalies"), default = NA_integer_)
    ),
    n_critical_anomalies = qc_metadata_value(
      metadata,
      c("anomaly_qc", "n_critical_anomalies"),
      default = qc_metadata_value(metadata, c("counts", "n_critical_anomalies"), default = NA_integer_)
    ),
    n_warning_anomalies = qc_metadata_value(
      metadata,
      c("anomaly_qc", "n_warning_anomalies"),
      default = qc_metadata_value(metadata, c("counts", "n_warning_anomalies"), default = NA_integer_)
    ),
    has_critical_anomaly = qc_metadata_value(
      metadata,
      c("anomaly_qc", "has_critical_anomaly"),
      default = NA
    ),
    has_warning_anomaly = qc_metadata_value(
      metadata,
      c("anomaly_qc", "has_warning_anomaly"),
      default = NA
    ),
    n_episode_anomalies = qc_metadata_value(
      metadata,
      c("anomaly_qc", "n_episode_anomalies"),
      default = NA_integer_
    ),
    n_event_anomalies = qc_metadata_value(
      metadata,
      c("anomaly_qc", "n_event_anomalies"),
      default = NA_integer_
    ),
    n_daily_anomalies = qc_metadata_value(
      metadata,
      c("anomaly_qc", "n_daily_anomalies"),
      default = NA_integer_
    ),
    n_export_span_anomalies = qc_metadata_value(
      metadata,
      c("anomaly_qc", "n_export_span_anomalies"),
      default = NA_integer_
    ),
    n_meta_duration_disagreements = qc_metadata_value(
      metadata,
      c("anomaly_qc", "n_meta_duration_disagreements"),
      default = NA_integer_
    ),
    max_abs_meta_duration_diff_ms = qc_metadata_value(
      metadata,
      c("anomaly_qc", "max_abs_meta_duration_diff_ms"),
      default = NA_real_
    ),
    max_daily_total_ms_observed = qc_metadata_value(
      metadata,
      c("anomaly_qc", "max_daily_total_ms_observed"),
      default = NA_real_
    ),
    max_observed_export_lookback_days = qc_metadata_value(
      metadata,
      c("anomaly_qc", "max_observed_export_lookback_days"),
      default = NA_real_
    ),
    second_level_total_elapsed_sec = qc_metadata_value(
      metadata,
      c("second_level", "profiling", "total_elapsed_sec"),
      default = NA_real_
    ),
    second_level_load_elapsed_sec = qc_metadata_value(
      metadata,
      c("second_level", "profiling", "load_elapsed_sec"),
      default = NA_real_
    ),
    second_level_convert_elapsed_sec = qc_metadata_value(
      metadata,
      c("second_level", "profiling", "convert_elapsed_sec"),
      default = NA_real_
    ),
    second_level_save_elapsed_sec = qc_metadata_value(
      metadata,
      c("second_level", "profiling", "save_elapsed_sec"),
      default = NA_real_
    ),
    second_level_inline_qc_elapsed_sec = qc_metadata_value(
      metadata,
      c("second_level", "profiling", "inline_qc_elapsed_sec"),
      default = NA_real_
    ),
    second_level_worker_pid = qc_metadata_value(
      metadata,
      c("second_level", "profiling", "worker_pid"),
      default = NA_integer_
    ),
    first_level_rda_size_bytes = qc_metadata_value(
      metadata,
      c("second_level", "profiling", "first_level_rda_size_bytes"),
      default = NA_real_
    ),
    second_level_rda_size_bytes = qc_metadata_value(
      metadata,
      c("second_level", "profiling", "second_level_rda_size_bytes"),
      default = NA_real_
    ),
    n_parse_warnings = qc_metadata_value(metadata, c("counts", "n_parse_warnings"), default = NA_integer_),
    n_category_matched_apps = qc_metadata_value(
      metadata,
      c("category_dictionary", "n_category_matched_apps"),
      default = NA_integer_
    ),
    category_match_rate = qc_metadata_value(
      metadata,
      c("category_dictionary", "category_match_rate"),
      default = NA_real_
    ),
    n_category_matched_rows = qc_metadata_value(
      metadata,
      c("category_dictionary", "n_category_matched_rows"),
      default = NA_integer_
    ),
    n_category_unmatched_rows = qc_metadata_value(
      metadata,
      c("category_dictionary", "n_category_unmatched_rows"),
      default = NA_integer_
    ),
    n_category_conflict_rows = qc_metadata_value(
      metadata,
      c("category_dictionary", "n_category_conflict_rows"),
      default = NA_integer_
    ),
    category_row_match_rate = qc_metadata_value(
      metadata,
      c("category_dictionary", "category_row_match_rate"),
      default = NA_real_
    ),
    n_category_app_uuid_rows = qc_metadata_value(
      metadata,
      c("category_dictionary", "n_category_app_uuid_rows"),
      default = NA_integer_
    ),
    n_category_app_name_repaired_rows = qc_metadata_value(
      metadata,
      c("category_dictionary", "n_category_app_name_repaired_rows"),
      default = NA_integer_
    ),
    n_category_app_name_rows = qc_metadata_value(
      metadata,
      c("category_dictionary", "n_category_app_name_rows"),
      default = NA_integer_
    ),
    error_message = qc_metadata_value(metadata, c("qc", "qc_error_message")),
    metadata_json = normalizePath(metadata_file, winslash = "/", mustWork = FALSE),
    first_level_rda = qc_metadata_value(metadata, c("outputs", "first_level_rda")),
    second_level_rda = qc_metadata_value(metadata, c("outputs", "second_level_rda")),
    first_level_data_file = qc_metadata_value(metadata, c("outputs", "first_level_rda")),
    second_level_data_file = qc_metadata_value(metadata, c("outputs", "second_level_rda")),
    second_level_metadata_file = normalizePath(metadata_file, winslash = "/", mustWork = FALSE),
    stringsAsFactors = FALSE
  )
  source_values <- appusage_source_qc_summary_from_metadata(metadata)
  for (name in names(source_values)) row[[name]] <- source_values[[name]]
  row
}

read_qc_summary_metadata <- function(metadata_file, metadata_label, strict) {
  tryCatch(
    list(
      metadata = jsonlite::read_json(metadata_file, simplifyVector = TRUE),
      error = NULL
    ),
    error = function(e) {
      if (isTRUE(strict)) {
        cli::cli_abort("{metadata_label} JSON could not be read: {conditionMessage(e)}")
      }
      list(metadata = NULL, error = e)
    }
  )
}

qc_summary_problem_row <- function(metadata_file, metadata_label, error,
                                   legacy_file = NA_character_) {
  metadata <- minimal_qc_metadata(metadata_file)
  row <- qc_summary_row_from_loaded_metadata(metadata, metadata_file)
  message <- paste(metadata_label, "JSON could not be read:", conditionMessage(error))
  row$status <- "error"
  row$qc_status <- "error"
  row$pass_qc <- FALSE
  row$error_message <- message
  row$qc_metadata_source <- "none"
  row$legacy_proc3_json <- if (is_present_string(legacy_file)) {
    normalizePath(legacy_file, winslash = "/", mustWork = FALSE)
  } else {
    NA_character_
  }
  row$legacy_qc_fallback_used <- FALSE
  row$legacy_qc_conflict <- FALSE
  row$legacy_qc_conflict_fields <- NA_character_
  row$legacy_qc_error_message <- if (is_present_string(legacy_file)) {
    message
  } else {
    NA_character_
  }
  row
}

merge_proc2_legacy_qc_summary <- function(proc2_row, legacy_row, legacy_file,
                                          legacy_error_message) {
  proc2_usable <- summary_row_has_usable_qc(proc2_row)
  legacy_usable <- !is.null(legacy_row) && summary_row_has_usable_qc(legacy_row)
  fallback_used <- !proc2_usable && legacy_usable
  conflicts <- if (proc2_usable && legacy_usable) {
    qc_summary_conflict_fields(proc2_row, legacy_row)
  } else {
    character()
  }

  out <- proc2_row
  if (fallback_used) {
    out <- copy_summary_fields(out, legacy_row, qc_summary_fallback_fields())
    out <- copy_missing_summary_fields(out, legacy_row, qc_summary_count_fields())
    source <- "proc-2_with_legacy_fallback"
  } else if (proc2_usable) {
    source <- "proc-2"
  } else {
    source <- "none"
  }

  out$qc_metadata_source <- source
  out$legacy_proc3_json <- if (is_present_string(legacy_file)) {
    normalizePath(legacy_file, winslash = "/", mustWork = FALSE)
  } else {
    NA_character_
  }
  out$legacy_qc_fallback_used <- fallback_used
  out$legacy_qc_conflict <- length(conflicts) > 0
  out$legacy_qc_conflict_fields <- if (length(conflicts) > 0) {
    paste(conflicts, collapse = ";")
  } else {
    NA_character_
  }
  out$legacy_qc_error_message <- legacy_error_message
  out
}

qc_summary_fallback_fields <- function() {
  c(
    "qc_status", "pass_qc", "analysis_eligible_event",
    "analysis_eligible_episode", "analysis_eligible_daily",
    "n_recorded_days", "n_nonempty_days", "weekdays_covered",
    "error_message"
  )
}

qc_summary_count_fields <- function() {
  c(
    "n_event_rows", "n_episode_rows", "n_daily_rows",
    "n_anomalies", "n_critical_anomalies", "n_warning_anomalies",
    "has_critical_anomaly", "has_warning_anomaly", "n_episode_anomalies",
    "n_event_anomalies", "n_daily_anomalies", "n_export_span_anomalies",
    "n_meta_duration_disagreements", "max_abs_meta_duration_diff_ms",
    "max_daily_total_ms_observed", "max_observed_export_lookback_days",
    "n_parse_warnings"
  )
}

qc_summary_conflict_fields <- function(proc2_row, legacy_row) {
  fields <- c(
    "qc_status", "pass_qc", "analysis_eligible_event",
    "analysis_eligible_episode", "analysis_eligible_daily",
    "n_recorded_days", "n_nonempty_days", "weekdays_covered"
  )
  fields <- intersect(fields, intersect(names(proc2_row), names(legacy_row)))
  fields[!vapply(fields, function(field) {
    qc_summary_values_equal(proc2_row[[field]][[1]], legacy_row[[field]][[1]])
  }, logical(1))]
}

qc_summary_values_equal <- function(x, y) {
  if ((length(x) == 0 || is.na(x)) && (length(y) == 0 || is.na(y))) {
    return(TRUE)
  }
  identical(as.character(x), as.character(y))
}

copy_summary_fields <- function(target, source, fields) {
  for (field in intersect(fields, intersect(names(target), names(source)))) {
    target[[field]] <- source[[field]]
  }
  target
}

copy_missing_summary_fields <- function(target, source, fields) {
  for (field in intersect(fields, intersect(names(target), names(source)))) {
    value <- target[[field]][[1]]
    if (length(value) == 0 || is.na(value)) {
      target[[field]] <- source[[field]]
    }
  }
  target
}

summary_row_has_usable_qc <- function(row) {
  status <- row$qc_status[[1]]
  is_present_string(status) && !status %in% c("not_run", "pending")
}

legacy_proc3_metadata_index <- function(project_dir) {
  if (!is_present_string(project_dir)) {
    return(stats::setNames(character(), character()))
  }
  proclevel_3 <- file.path(project_dir, "proclevel-3")
  if (!dir.exists(proclevel_3)) {
    return(stats::setNames(character(), character()))
  }
  files <- sort(list.files(
    proclevel_3,
    pattern = "_proc-3[.]json$",
    full.names = TRUE
  ))
  keys <- vapply(files, metadata_match_key, character(1))
  keep <- vapply(keys, is_present_string, logical(1)) & !duplicated(keys)
  stats::setNames(files[keep], keys[keep])
}

metadata_match_key <- function(metadata_file) {
  entities <- parse_appusage_filename(metadata_file)
  if (!is_present_string(entities$sub) || !is_present_string(entities$type)) {
    return(NA_character_)
  }
  paste(entities$sub, entities$type, sep = "\r")
}

infer_project_dir_from_metadata_files <- function(metadata_files) {
  metadata_files <- as.character(metadata_files)
  metadata_files <- metadata_files[!is.na(metadata_files) & nzchar(metadata_files)]
  if (length(metadata_files) == 0) {
    return(NA_character_)
  }
  parent <- dirname(normalizePath(metadata_files[[1]], winslash = "/", mustWork = FALSE))
  if (basename(parent) %in% c("proclevel-1", "proclevel-2", "proclevel-3")) {
    return(dirname(parent))
  }
  parent
}

bind_qc_summary_rows <- function(rows) {
  if (length(rows) == 0) {
    return(tibble::tibble())
  }
  all_names <- unique(unlist(lapply(rows, names), use.names = FALSE))
  rows <- lapply(rows, function(row) {
    missing <- setdiff(all_names, names(row))
    for (field in missing) {
      row[[field]] <- NA
    }
    row[, all_names, drop = FALSE]
  })
  tibble::as_tibble(do.call(rbind, rows))
}

qc_metadata_participant_id <- function(metadata, metadata_file) {
  value <- qc_metadata_value(metadata, c("participant_id"))
  if (is_present_string(value)) {
    return(value)
  }
  value <- qc_metadata_value(metadata, c("identity", "participant_id"))
  if (is_present_string(value)) {
    return(value)
  }
  parse_appusage_filename(metadata_file)$sub %||% "unknown"
}

qc_metadata_export_type <- function(metadata, metadata_file) {
  value <- qc_metadata_value(metadata, c("export", "detected_type"))
  if (is_present_string(value)) {
    return(value)
  }
  value <- qc_metadata_value(metadata, c("export", "content_detected_export_type"))
  if (is_present_string(value)) {
    return(value)
  }
  parse_appusage_filename(metadata_file)$type %||% "unknown"
}

qc_metadata_value <- function(x, path, default = NA_character_) {
  value <- x
  for (key in path) {
    if (!is.list(value) || is.null(value[[key]])) {
      return(default)
    }
    value <- value[[key]]
  }
  if (is.null(value) || length(value) == 0 || (is.list(value) && !is.atomic(value))) {
    return(default)
  }
  value <- value[[1]]
  if (is.null(value) || length(value) == 0) {
    return(default)
  }
  value
}

is_present_string <- function(x) {
  is.character(x) && length(x) == 1 && !is.na(x) && nzchar(x)
}
