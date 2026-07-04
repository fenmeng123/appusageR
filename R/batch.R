#' Batch preprocess APP Usage exports
#'
#' Preprocesses many APP Usage `.txt` exports without returning parsed data into
#' memory. When `output_dir` is supplied, the function creates a BIDS-like
#' project folder and creates `proclevel-1` only because first-level
#' preprocessing is the only step executed by this function. Later processing
#' functions create their own processing-level folders when they run.
#'
#' @param x Character vector of file paths, raw text records, or line vectors.
#' @param ids Optional participant IDs parallel to `x`.
#' @param self_report Optional self-report data frame. Reserved for future
#'   Wenjuanxing sequence-to-study-ID matching; currently errors if supplied.
#' @param participant_id_col Reserved study participant ID column name.
#' @param wenjuanxing_sequence_col Reserved Wenjuanxing sequence column name.
#' @param type Export type or `"auto"`.
#' @param input One of `"file"`, `"text"`, or `"lines"`.
#' @param output_dir Optional parent directory for the project output folder.
#' @param project_name Optional project name. Defaults to the next available
#'   `StudyN` name under `output_dir`.
#' @param project_id Optional short project ID. Defaults to a generated
#'   four-character hexadecimal ID.
#' @param tz Time zone passed to parsers.
#' @param encoding Source encoding.
#' @param strict If `TRUE`, stop on the first error after recording diagnostics.
#' @param overwrite Whether to overwrite existing cache files.
#' @param progress If `TRUE`, print simple progress messages every
#'   `progress_every` files.
#' @param progress_every Progress interval.
#' @param parallel Whether to use parallel workers. Defaults to `FALSE`.
#' @param n_cores Number of workers when `parallel = TRUE`; must not exceed the
#'   maximum available cores reported by the machine.
#'
#' @return Invisibly returns a tibble summary. It does not return parsed data.
#' @export
read_appusage_batch <- function(x, ids = NULL, self_report = NULL,
                                participant_id_col = NULL,
                                wenjuanxing_sequence_col = NULL,
                                type = "auto", input = "file",
                                output_dir = NULL, project_name = NULL,
                                project_id = NULL,
                                tz = "Asia/Shanghai", encoding = "auto",
                                strict = FALSE, overwrite = FALSE,
                                progress = TRUE, progress_every = 100,
                                parallel = FALSE, n_cores = 1) {
  input <- match.arg(input, c("file", "text", "lines"))
  if (!identical(type, "auto") && !type %in% c("line", "meta", "day", "app")) {
    cli::cli_abort("`type` must be 'auto', 'line', 'meta', 'day', or 'app'.")
  }
  n_cores <- validate_parallel_settings(parallel = parallel, n_cores = n_cores)
  id_plan <- resolve_participant_ids(
    x = x,
    ids = ids,
    input = input,
    self_report = self_report,
    participant_id_col = participant_id_col,
    wenjuanxing_sequence_col = wenjuanxing_sequence_col
  )
  output_project <- prepare_batch_output_project(
    output_dir = output_dir,
    project_name = project_name,
    project_id = project_id,
    overwrite = overwrite,
    n_inputs = length(x),
    input = input,
    tz = tz
  )

  rows <- process_batch_rows(
    x = x,
    id_plan = id_plan,
    type = type,
    input = input,
    output_dir = output_project$proclevel_1,
    tz = tz,
    encoding = encoding,
    overwrite = overwrite,
    progress = progress,
    progress_every = progress_every,
    parallel = parallel,
    n_cores = n_cores
  )
  if (strict) {
    failed <- which(vapply(rows, function(z) !identical(z$status, "success"), logical(1)))
    if (length(failed) > 0) {
      i <- failed[[1]]
      cli::cli_abort("Batch preprocessing failed at record {i}: {rows[[i]]$error_message}")
    }
  }

  summary <- tibble::as_tibble(do.call(rbind, rows))
  if (!is.null(output_project$project_root)) {
    summary$project_root <- output_project$project_root
    summary$proclevel_1_dir <- output_project$proclevel_1
    summary_file <- file.path(output_project$project_root, "analytic_summary_table_proclevel-1.csv")
    utils::write.csv(summary, summary_file, row.names = FALSE, na = "")
    write_dataset_description_json(
      output_project,
      summary = summary,
      proclevel = 1,
      summary_file = summary_file,
      status = "success"
    )
  }
  invisible(summary)
}

#' Batch write second-level APP Usage caches
#'
#' Takes a first-level batch summary returned by `read_appusage_batch()`, writes
#' second-level `proc-2` RDA files for successful first-level records, and writes
#' `analytic_summary_table_proclevel-2.csv` at the project root when the project
#' structure is available.
#'
#' @param batch_summary Summary returned by `read_appusage_batch()`.
#' @param output_dir Optional second-level output directory. Defaults to the
#'   sibling `proclevel-2` directory when first-level files are under
#'   `proclevel-1`.
#' @param overwrite Whether to overwrite existing second-level files.
#' @param resume Whether to skip already completed valid `proc-2` RDA/JSON
#'   pairs when `overwrite = FALSE`.
#' @param progress Whether to print progress messages.
#' @param parallel Whether to use Windows-safe PSOCK workers. Defaults to
#'   `FALSE`.
#' @param n_cores Number of requested workers when `parallel = TRUE`. Effective
#'   workers are capped at 12 and cannot exceed available logical cores.
#' @param ... Additional arguments passed to `write_second_level_appusage()`.
#'
#' @return Invisibly returns a tibble summary for second-level writing.
#' @export
write_second_level_batch <- function(batch_summary, output_dir = NULL,
                                     overwrite = FALSE, resume = FALSE,
                                     progress = TRUE, parallel = FALSE,
                                     n_cores = 1, ...) {
  if (!all(c("status", "data_file") %in% names(batch_summary))) {
    cli::cli_abort("`batch_summary` must come from `read_appusage_batch()`.")
  }
  n_workers <- resolve_appusage_parallel_workers(
    parallel = parallel,
    n_cores = n_cores,
    max_workers = 12L,
    stage = "second-level"
  )
  if (is.null(output_dir)) {
    project_root <- infer_project_root_from_summary(batch_summary)
    if (!is.na(project_root)) {
      output_dir <- file.path(project_root, "proclevel-2")
    }
  }
  eligible <- batch_summary$status == "success" &
    !is.na(batch_summary$data_file) &
    file.exists(batch_summary$data_file)
  if (!is.null(output_dir) && any(eligible, na.rm = TRUE)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  rows <- process_second_level_batch_rows(
    batch_summary = batch_summary,
    output_dir = output_dir,
    overwrite = overwrite,
    resume = resume,
    progress = progress,
    parallel = parallel,
    n_workers = n_workers,
    second_level_args = list(...)
  )
  project_root <- infer_project_root_from_summary(batch_summary)
  proc2_metadata <- if (!is.null(output_dir) && dir.exists(output_dir)) {
    sort(list.files(output_dir, pattern = "_proc-2[.]json$", full.names = TRUE))
  } else {
    character()
  }
  summary <- combine_second_level_batch_summary(proc2_metadata, rows, batch_summary)
  if (!is.na(project_root)) {
    summary_file <- file.path(project_root, "analytic_summary_table_proclevel-2.csv")
    utils::write.csv(summary, summary_file, row.names = FALSE, na = "")
    project <- project_info_from_root(project_root)
    write_dataset_description_json(
      project,
      summary = summary,
      proclevel = 2,
      summary_file = summary_file,
      status = "success"
    )
  }
  invisible(summary)
}

combine_second_level_batch_summary <- function(proc2_metadata, rows, batch_summary) {
  row_summary <- tibble::as_tibble(do.call(rbind, rows))
  metadata_summary <- if (length(proc2_metadata) > 0) {
    build_qc_summary_from_metadata(proc2_metadata)
  } else {
    tibble::tibble()
  }
  skipped_summary <- second_level_skipped_summary_rows(row_summary, batch_summary)

  if (nrow(metadata_summary) == 0 && nrow(skipped_summary) == 0) {
    return(row_summary)
  }

  summary <- bind_appusage_summary_rows(metadata_summary, skipped_summary)
  summary <- merge_second_level_row_diagnostics(summary, row_summary)
  if ("participant_id" %in% names(summary) && "participant_id" %in% names(batch_summary)) {
    participant_order <- match(
      as.character(summary$participant_id),
      as.character(batch_summary$participant_id)
    )
    summary <- summary[order(participant_order, seq_len(nrow(summary)), na.last = TRUE), , drop = FALSE]
  }
  tibble::as_tibble(summary)
}

merge_second_level_row_diagnostics <- function(summary, row_summary) {
  if (nrow(summary) == 0 || nrow(row_summary) == 0) {
    return(summary)
  }
  if (!"error_message" %in% names(summary)) {
    summary$error_message <- NA_character_
  }
  error_rows <- row_summary[row_summary$status == "error", , drop = FALSE]
  if (nrow(error_rows) == 0) {
    return(summary)
  }
  for (i in seq_len(nrow(error_rows))) {
    candidates <- rep(TRUE, nrow(summary))
    for (col in c("participant_id", "detected_type")) {
      if (col %in% names(summary) && col %in% names(error_rows)) {
        candidates <- candidates &
          as.character(summary[[col]]) == as.character(error_rows[[col]][[i]])
      }
    }
    matched <- which(candidates)
    if (length(matched) == 0) {
      next
    }
    j <- matched[[1]]
    if (!is_present_string(summary$error_message[[j]]) &&
      "error_message" %in% names(error_rows)) {
      summary$error_message[[j]] <- error_rows$error_message[[i]]
    }
    for (col in c("second_level_metadata_file", "second_level_data_file")) {
      if (col %in% names(summary) && col %in% names(error_rows) &&
        !is_present_string(summary[[col]][[j]]) &&
        is_present_string(error_rows[[col]][[i]])) {
        summary[[col]][[j]] <- error_rows[[col]][[i]]
      }
    }
  }
  summary
}

second_level_skipped_summary_rows <- function(row_summary, batch_summary) {
  skipped <- row_summary[row_summary$status == "skipped", , drop = FALSE]
  if ("skip_reason" %in% names(skipped)) {
    skipped <- skipped[!skipped$skip_reason %in% "existing_proc2_cache", , drop = FALSE]
  }
  if (nrow(skipped) == 0) {
    return(tibble::tibble())
  }

  rows <- lapply(seq_len(nrow(skipped)), function(i) {
    source_index <- match(skipped$index[[i]], batch_summary$index)
    if (is.na(source_index)) {
      source_index <- i
    }
    metadata_file <- batch_summary$metadata_file[[source_index]]
    metadata <- if (is_present_string(metadata_file) && file.exists(metadata_file)) {
      read_first_level_metadata(metadata_file)$metadata
    } else {
      minimal_qc_metadata(metadata_file %||% NA_character_)
    }
    first_level_status <- qc_metadata_value(
      metadata,
      c("processing", "first_level_status"),
      default = batch_summary$status[[source_index]]
    )
    skip_reason <- if ("skip_reason" %in% names(skipped)) {
      skipped$skip_reason[[i]]
    } else {
      "not_run_or_missing_first_level_rda"
    }
    first_level_error_message <- batch_summary$error_message[[source_index]]
    data.frame(
      participant_id = batch_summary$participant_id[[source_index]],
      participant_id_source = batch_summary$participant_id_source[[source_index]],
      wenjuanxing_sequence_id = batch_summary$wenjuanxing_sequence_id[[source_index]],
      detected_type = batch_summary$detected_type[[source_index]],
      filename_export_type = batch_summary$filename_export_type[[source_index]],
      export_type_match = batch_summary$export_type_match[[source_index]],
      first_level_status = first_level_status,
      second_level_status = "skipped",
      qc_status = "not_run",
      app_category_status = "not_run",
      status = "skipped",
      skip_reason = skip_reason,
      pass_qc = NA,
      analysis_eligible_event = NA,
      analysis_eligible_episode = NA,
      analysis_eligible_daily = NA,
      n_recorded_days = NA_integer_,
      n_nonempty_days = NA_integer_,
      weekdays_covered = NA_character_,
      n_event_rows = NA_integer_,
      n_episode_rows = NA_integer_,
      n_daily_rows = NA_integer_,
      n_anomalies = NA_integer_,
      n_critical_anomalies = NA_integer_,
      n_warning_anomalies = NA_integer_,
      has_critical_anomaly = NA,
      has_warning_anomaly = NA,
      n_episode_anomalies = NA_integer_,
      n_event_anomalies = NA_integer_,
      n_daily_anomalies = NA_integer_,
      n_export_span_anomalies = NA_integer_,
      n_meta_duration_disagreements = NA_integer_,
      max_abs_meta_duration_diff_ms = NA_real_,
      max_daily_total_ms_observed = NA_real_,
      max_observed_export_lookback_days = NA_real_,
      n_parse_warnings = batch_summary$n_parse_warnings[[source_index]],
      n_category_matched_apps = NA_integer_,
      category_match_rate = NA_real_,
      n_category_matched_rows = NA_integer_,
      n_category_unmatched_rows = NA_integer_,
      n_category_conflict_rows = NA_integer_,
      category_row_match_rate = NA_real_,
      n_category_app_uuid_rows = NA_integer_,
      n_category_app_name_repaired_rows = NA_integer_,
      n_category_app_name_rows = NA_integer_,
      first_level_error_message = first_level_error_message,
      error_message = NA_character_,
      metadata_json = NA_character_,
      first_level_rda = NA_character_,
      second_level_rda = NA_character_,
      first_level_data_file = NA_character_,
      second_level_data_file = NA_character_,
      second_level_metadata_file = NA_character_,
      stringsAsFactors = FALSE
    )
  })
  tibble::as_tibble(do.call(rbind, rows))
}

bind_appusage_summary_rows <- function(...) {
  frames <- Filter(function(x) !is.null(x) && nrow(x) > 0, list(...))
  if (length(frames) == 0) {
    return(tibble::tibble())
  }
  columns <- unique(unlist(lapply(frames, names), use.names = FALSE))
  frames <- lapply(frames, function(x) {
    missing <- setdiff(columns, names(x))
    for (col in missing) {
      x[[col]] <- NA
    }
    x[, columns, drop = FALSE]
  })
  tibble::as_tibble(do.call(rbind, frames))
}

preprocess_one_appusage <- function(x, id_info, type, input, output_dir,
                                    tz, encoding, overwrite, index) {
  warnings <- character()
  started_at <- Sys.time()
  source_file <- source_file_label(x, input)
  participant_id <- id_info$participant_id[[1]]
  participant_id_source <- id_info$participant_id_source[[1]]
  detected_type <- NA_character_
  metadata_file <- NA_character_
  data_file <- NA_character_

  result <- tryCatch(
    withCallingHandlers(
      {
        detected_type <- first_level_detect_type(
          x = x,
          input = input,
          type = type,
          encoding = encoding,
          id_info = id_info
        )
        if (identical(detected_type, "unknown")) {
          stop(batch_unsupported_error(detected_type))
        }

        parsed_data <- switch(detected_type,
          line = parse_line(
            x,
            input = input, participant_id = participant_id,
            source_file = source_file, tz = tz, encoding = encoding,
            strict = TRUE
          ),
          meta = parse_meta(
            x,
            input = input, participant_id = participant_id,
            source_file = source_file, tz = tz, encoding = encoding,
            strict = TRUE
          ),
          day = parse_day(
            x,
            input = input, participant_id = participant_id,
            source_file = source_file, tz = tz, encoding = encoding,
            strict = TRUE
          ),
          app = parse_app(
            x,
            input = input, participant_id = participant_id,
            source_file = source_file, tz = tz, encoding = encoding,
            strict = TRUE
          )
        )

        first_level_data <- as_first_level_data(parsed_data, detected_type)
        if (first_level_is_empty(first_level_data)) {
          stop(first_level_empty_raw_data_error(
            detected_type,
            parser_diagnostics(first_level_data)
          ))
        }
        info <- build_metadata(
          participant_id = participant_id,
          participant_id_source = participant_id_source,
          id_info = id_info,
          source_file = source_file,
          export_type = detected_type,
          export_type_match = filename_export_type_match(id_info, detected_type),
          input = input,
          encoding = encoding,
          tz = tz,
          started_at = started_at,
          finished_at = Sys.time(),
          status = "success",
          data = first_level_data,
          warnings = warnings,
          error = NULL,
          metadata_file = NA_character_,
          data_file = NA_character_
        )

        if (!is.null(output_dir)) {
          data_file <- file.path(
            output_dir,
            build_appusage_filename(
              participant_id = participant_id,
              export_type = detected_type,
              proc = 1,
              extension = "rda"
            )
          )
          metadata_file <- file.path(
            output_dir,
            build_appusage_filename(
              participant_id = participant_id,
              export_type = detected_type,
              proc = 1,
              extension = "json"
            )
          )
          if ((file.exists(data_file) || file.exists(metadata_file)) && !overwrite) {
            stop(batch_cache_exists_error(paste(c(data_file, metadata_file), collapse = "; ")))
          }
          data <- first_level_data
          save(data, file = data_file)
          info$outputs$metadata_json <- normalizePath(metadata_file, winslash = "/", mustWork = FALSE)
          info$outputs$first_level_rda <- normalizePath(data_file, winslash = "/", mustWork = FALSE)
          write_metadata_json(info, metadata_file)
        }

        list(
          status = "success",
          n_rows = metadata_n_rows(info),
          n_parse_warnings = info$counts$n_parse_warnings,
          error = NULL,
          traceback = NA_character_
        )
      },
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      list(
        status = "error",
        n_rows = NA_integer_,
        n_parse_warnings = NA_integer_,
        error = e,
        traceback = paste(vapply(sys.calls(), deparse_one_call, character(1)), collapse = "\n")
      )
    }
  )

  finished_at <- Sys.time()
  error <- result$error
  if (!is.null(error) && !is.null(output_dir) && !is.na(detected_type)) {
    metadata_file <- file.path(
      output_dir,
      build_appusage_filename(
        participant_id = participant_id,
        export_type = ifelse(is.na(detected_type), "unknown", detected_type),
        proc = 1,
        extension = "json"
      )
    )
    error_info <- build_metadata(
      participant_id = participant_id,
      participant_id_source = participant_id_source,
      id_info = id_info,
      source_file = source_file,
      export_type = detected_type,
      export_type_match = filename_export_type_match(id_info, detected_type),
      input = input,
      encoding = encoding,
      tz = tz,
      started_at = started_at,
      finished_at = finished_at,
      status = "error",
      data = NULL,
      warnings = warnings,
      error = error,
      metadata_file = metadata_file,
      data_file = NA_character_
    )
    write_metadata_json(error_info, metadata_file)
  }
  data.frame(
    index = index,
    participant_id = participant_id,
    participant_id_source = participant_id_source,
    wenjuanxing_sequence_id = id_info$wenjuanxing_sequence_id[[1]],
    filename_parse_status = id_info$filename_parse_status[[1]],
    filename_parse_warning = id_info$filename_parse_warning[[1]],
    native_export_file_name = id_info$native_export_file_name[[1]],
    filename_export_type = id_info$native_export_type_from_filename[[1]],
    native_export_created_at = id_info$native_export_created_at[[1]],
    export_type_match = filename_export_type_match(id_info, detected_type),
    source_file = source_file,
    detected_type = detected_type,
    status = result$status,
    metadata_file = metadata_file,
    data_file = data_file,
    n_rows = result$n_rows,
    n_parse_warnings = result$n_parse_warnings,
    warning_messages = paste(unique(warnings), collapse = "\n"),
    error_message = if (is.null(error)) NA_character_ else conditionMessage(error),
    error_class = if (is.null(error)) NA_character_ else paste(class(error), collapse = ","),
    error_call = if (is.null(error) || is.null(conditionCall(error))) {
      NA_character_
    } else {
      deparse_one_call(conditionCall(error))
    },
    traceback = result$traceback,
    started_at = format(started_at, "%Y-%m-%d %H:%M:%OS3 %z"),
    finished_at = format(finished_at, "%Y-%m-%d %H:%M:%OS3 %z"),
    elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
    stringsAsFactors = FALSE
  )
}

batch_unsupported_error <- function(detected_type) {
  structure(
    list(message = paste0(
      "Detected type '", detected_type,
      "' is not supported by batch preprocessing until the corresponding parser is implemented."
    )),
    class = c("appusage_unsupported_type", "error", "condition")
  )
}

batch_cache_exists_error <- function(output_file) {
  structure(
    list(message = paste0("Output cache already exists: ", output_file)),
    class = c("appusage_cache_exists", "error", "condition")
  )
}

default_batch_ids <- function(x, input) {
  sprintf("record-%06d", seq_along(x))
}

filename_export_type_match <- function(id_info, detected_type) {
  filename_type <- id_info$native_export_type_from_filename[[1]]
  if (is.na(filename_type) || is.na(detected_type)) {
    return(NA)
  }
  identical(filename_type, detected_type)
}

sanitize_cache_name <- function(x, index) {
  x <- as.character(x)
  if (is.na(x) || x == "") {
    x <- paste0("record_", index)
  }
  x <- gsub("[^A-Za-z0-9_-]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  if (x == "") {
    x <- paste0("record_", index)
  }
  x
}

deparse_one_call <- function(x) {
  paste(deparse(x, width.cutoff = 500), collapse = "")
}

parsed_n_rows <- function(parsed_data) {
  if (is.data.frame(parsed_data)) {
    nrow(parsed_data)
  } else if (is.list(parsed_data) && all(c("summary", "events") %in% names(parsed_data))) {
    nrow(parsed_data$summary) + nrow(parsed_data$events)
  } else {
    NA_integer_
  }
}

as_first_level_data <- function(parsed_data, detected_type) {
  diagnostics <- parser_diagnostics(parsed_data)
  out <- switch(detected_type,
    line = list(line = strip_individual_columns(parsed_data)),
    day = list(day = strip_individual_columns(parsed_data)),
    app = list(app = strip_individual_columns(parsed_data)),
    meta = list(
      meta_summary = strip_individual_columns(parsed_data$summary),
      meta_events = strip_individual_columns(parsed_data$events)
    ),
    parsed_data
  )
  attach_parser_diagnostics(out, diagnostics)
}

first_level_empty_raw_data_error <- function(detected_type, diagnostics = NULL) {
  structure(
    list(message = paste0(
      "APP Usage ", detected_type,
      " export format was recognized, but the parsed raw data are empty."
    ), parser_diagnostics = diagnostics),
    class = c("appusage_empty_raw_data", "error", "condition")
  )
}

first_level_detect_type <- function(x, input, type, encoding, id_info) {
  if (!identical(type, "auto")) {
    return(type)
  }
  filename_type <- id_info$native_export_type_from_filename[[1]]
  if (!is.na(filename_type) && identical(filename_type, "unknown")) {
    return("unknown")
  }
  if (!is.na(filename_type) && filename_type %in% c("line", "meta", "day", "app")) {
    return(filename_type)
  }
  detect_appusage_type(x, input = input, encoding = encoding)
}

first_level_is_empty <- function(first_level_data) {
  if (!is.list(first_level_data) || length(first_level_data) == 0) {
    return(TRUE)
  }
  row_counts <- vapply(first_level_data, function(x) {
    if (is.data.frame(x)) nrow(x) else NA_integer_
  }, integer(1))
  all(is.na(row_counts) | row_counts == 0)
}

strip_individual_columns <- function(x) {
  drop <- intersect(c("participant_id", "source_file", "source_path"), names(x))
  x[, setdiff(names(x), drop), drop = FALSE]
}

build_metadata <- function(participant_id, participant_id_source, id_info,
                           source_file, export_type, export_type_match, input,
                           encoding, tz, started_at, finished_at, status,
                           data, warnings, error, metadata_file, data_file) {
  source_meta <- source_metadata(source_file, input)
  parser_diag <- first_level_parser_diagnostics(data, error)
  list(
    schema_version = "0.2.0",
    package_version = as.character(utils::packageVersion("appusageR")),
    parser_version = as.character(utils::packageVersion("appusageR")),
    created_at = format(finished_at, "%Y-%m-%dT%H:%M:%OS3%z"),
    updated_at = format(finished_at, "%Y-%m-%dT%H:%M:%OS3%z"),
    participant_id = participant_id,
    participant_id_source = participant_id_source,
    identity = list(
      participant_id = participant_id,
      participant_id_source = participant_id_source,
      wenjuanxing_sequence_id = id_info$wenjuanxing_sequence_id[[1]]
    ),
    source = source_meta,
    export = list(
      detected_type = export_type,
      content_detected_export_type = export_type,
      native_export_type_from_filename = id_info$native_export_type_from_filename[[1]],
      export_type_match = export_type_match,
      type_resolution_rule = first_level_type_resolution_rule(id_info, export_type),
      filename_content_relation = first_level_filename_content_relation(id_info, export_type),
      native_export_created_at = id_info$native_export_created_at[[1]],
      input = input,
      encoding = encoding,
      timezone = tz
    ),
    processing = list(
      first_level_status = status,
      first_level_failure_reason = first_level_failure_reason(error),
      second_level_status = "pending",
      qc_status = "pending",
      started_at = format(started_at, "%Y-%m-%dT%H:%M:%OS3%z"),
      finished_at = format(finished_at, "%Y-%m-%dT%H:%M:%OS3%z"),
      elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs"))
    ),
    outputs = list(
      metadata_json = ifelse(is.na(metadata_file), NA_character_, normalizePath(metadata_file, winslash = "/", mustWork = FALSE)),
      first_level_rda = ifelse(is.na(data_file), NA_character_, normalizePath(data_file, winslash = "/", mustWork = FALSE)),
      second_level_rda = NA_character_
    ),
    parser_diagnostics = parser_diag %||% list(),
    counts = data_counts(data, warnings),
    qc = list(
      pass_qc = NA,
      n_recorded_days = NA,
      n_nonempty_days = NA,
      weekdays_covered = NA,
      flags = list()
    ),
    anomalies = list(),
    errors = error_metadata(error),
    warning_messages = unique(warnings)
  )
}

first_level_parser_diagnostics <- function(data, error) {
  diagnostics <- parser_diagnostics(data)
  if (!is.null(diagnostics)) {
    return(diagnostics)
  }
  condition_parser_diagnostics(error)
}

first_level_type_resolution_rule <- function(id_info, export_type) {
  filename_type <- id_info$native_export_type_from_filename[[1]]
  if (!is.na(filename_type) && filename_type %in% c("line", "meta", "day", "app")) {
    return("filename_priority")
  }
  if (!is.na(export_type) && export_type %in% c("line", "meta", "day", "app")) {
    return("content_fallback")
  }
  "unknown"
}

first_level_filename_content_relation <- function(id_info, export_type) {
  filename_type <- id_info$native_export_type_from_filename[[1]]
  if (is.na(filename_type) && is.na(export_type)) {
    return("unknown")
  }
  if (is.na(filename_type)) {
    return("content_only")
  }
  if (is.na(export_type)) {
    return("filename_only")
  }
  if (identical(filename_type, export_type)) {
    return("match_or_filename_priority")
  }
  "conflict"
}

first_level_failure_reason <- function(error) {
  if (is.null(error)) {
    return(NA_character_)
  }
  if (inherits(error, "appusage_empty_raw_data")) {
    return("empty_raw_data")
  }
  if (inherits(error, "appusage_unsupported_type")) {
    return("unknown_or_unsupported_type")
  }
  "parse_error"
}

source_metadata <- function(source_file, input) {
  if (!identical(input, "file") || is.na(source_file) || !file.exists(source_file)) {
    return(list(
      file_name = NA_character_,
      file_path = source_file,
      file_size = NA_real_,
      mtime = NA_character_,
      filename = list()
    ))
  }
  info <- file.info(source_file)
  list(
    file_name = basename(source_file),
    file_path = normalizePath(source_file, winslash = "/", mustWork = FALSE),
    file_size = unname(info$size),
    mtime = format(info$mtime, "%Y-%m-%dT%H:%M:%OS3%z"),
    filename = parse_wenjuanxing_upload_filename(source_file)
  )
}

data_counts <- function(data, warnings) {
  counts <- list(n_parse_warnings = 0L)
  if (is.null(data)) {
    counts$n_rows <- NA_integer_
    return(counts)
  }
  for (nm in names(data)) {
    value <- data[[nm]]
    if (is.data.frame(value)) {
      counts[[paste0("n_", nm, "_rows")]] <- nrow(value)
      if ("parse_warning" %in% names(value)) {
        counts$n_parse_warnings <- counts$n_parse_warnings + sum(!is.na(value$parse_warning))
      }
    }
  }
  counts$n_rows <- sum(unlist(counts[grepl("^n_.*_rows$", names(counts))]), na.rm = TRUE)
  counts$n_warning_messages <- length(unique(warnings))
  counts
}

metadata_n_rows <- function(info) {
  if (!is.null(info$counts$n_rows)) info$counts$n_rows else NA_integer_
}

error_metadata <- function(error) {
  if (is.null(error)) {
    return(list())
  }
  list(list(
    message = conditionMessage(error),
    class = paste(class(error), collapse = ","),
    call = if (is.null(conditionCall(error))) NA_character_ else deparse_one_call(conditionCall(error))
  ))
}

write_metadata_json <- function(info, metadata_file) {
  jsonlite::write_json(
    info,
    path = metadata_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
}

parsed_n_warnings <- function(parsed_data) {
  if (is.data.frame(parsed_data) && "parse_warning" %in% names(parsed_data)) {
    return(sum(!is.na(parsed_data$parse_warning)))
  }
  if (is.list(parsed_data) && all(c("summary", "events") %in% names(parsed_data))) {
    return(
      sum(!is.na(parsed_data$summary$parse_warning)) +
        sum(!is.na(parsed_data$events$parse_warning))
    )
  }
  NA_integer_
}

validate_parallel_settings <- function(parallel, n_cores) {
  if (!is.logical(parallel) || length(parallel) != 1 || is.na(parallel)) {
    cli::cli_abort("`parallel` must be TRUE or FALSE.")
  }
  n_cores <- suppressWarnings(as.integer(n_cores))
  if (length(n_cores) != 1 || is.na(n_cores) || n_cores < 1) {
    cli::cli_abort("`n_cores` must be a positive integer.")
  }
  max_cores <- parallel::detectCores(logical = TRUE)
  if (is.na(max_cores) || max_cores < 1) {
    max_cores <- 1L
  }
  if (n_cores > max_cores) {
    cli::cli_abort("`n_cores` ({n_cores}) cannot exceed available cores ({max_cores}).")
  }
  if (!parallel && n_cores != 1) {
    cli::cli_inform("`n_cores` is ignored when `parallel = FALSE`.")
  }
  n_cores
}

resolve_appusage_parallel_workers <- function(parallel, n_cores,
                                              max_workers = Inf,
                                              available_cores = parallel::detectCores(logical = TRUE),
                                              stage = "parallel") {
  if (!is.logical(parallel) || length(parallel) != 1 || is.na(parallel)) {
    cli::cli_abort("`parallel` must be TRUE or FALSE.")
  }
  n_cores <- suppressWarnings(as.integer(n_cores))
  if (length(n_cores) != 1 || is.na(n_cores) || n_cores < 1) {
    cli::cli_abort("`n_cores` must be a positive integer.")
  }
  available_cores <- suppressWarnings(as.integer(available_cores))
  if (length(available_cores) != 1 || is.na(available_cores) || available_cores < 1) {
    available_cores <- 1L
  }
  if (n_cores > available_cores) {
    cli::cli_abort("`n_cores` ({n_cores}) cannot exceed available cores ({available_cores}).")
  }
  if (!isTRUE(parallel)) {
    if (n_cores != 1) {
      cli::cli_inform("`n_cores` is ignored when `parallel = FALSE`.")
    }
    return(1L)
  }
  workers <- min(n_cores, available_cores, as.integer(max_workers))
  if (n_cores > workers) {
    cli::cli_inform("{stage} workers capped at {workers}.")
  }
  workers
}

process_second_level_batch_rows <- function(batch_summary, output_dir,
                                            overwrite, resume, progress,
                                            parallel, n_workers,
                                            second_level_args) {
  n <- nrow(batch_summary)
  if (!isTRUE(parallel) || n <= 1 || n_workers == 1) {
    rows <- vector("list", n)
    for (i in seq_len(n)) {
      if (isTRUE(progress) && (i == 1 || i == n)) {
        message(sprintf("Writing second-level APP Usage file %d/%d", i, n))
      }
      rows[[i]] <- write_second_level_one(
        batch_summary = batch_summary,
        index = i,
        output_dir = output_dir,
        overwrite = overwrite,
        resume = resume,
        second_level_args = second_level_args
      )
    }
    return(rows)
  }

  if (isTRUE(progress)) {
    message(sprintf(
      "Writing %d second-level APP Usage files with %d parallel workers",
      n, n_workers
    ))
  }
  cluster <- parallel::makeCluster(n_workers)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  package_root <- appusage_package_root_for_workers()
  parallel::clusterExport(
    cluster,
    varlist = c(
      "batch_summary", "output_dir", "overwrite", "resume",
      "second_level_args", "package_root"
    ),
    envir = environment()
  )
  parallel::clusterEvalQ(cluster, {
    if (requireNamespace("pkgload", quietly = TRUE) &&
      file.exists(file.path(package_root, "DESCRIPTION"))) {
      pkgload::load_all(package_root, quiet = TRUE)
    } else if (!requireNamespace("appusageR", quietly = TRUE)) {
        stop("Package appusageR is not available on the parallel worker.")
    }
    NULL
  })
  processing_order <- second_level_processing_order(batch_summary)
  ordered_rows <- parallel::parLapplyLB(cluster, processing_order, function(i) {
    worker <- get("write_second_level_one", envir = asNamespace("appusageR"))
    worker(
      batch_summary = batch_summary,
      index = i,
      output_dir = output_dir,
      overwrite = overwrite,
      resume = resume,
      second_level_args = second_level_args
    )
  })
  rows <- vector("list", n)
  rows[processing_order] <- ordered_rows
  if (isTRUE(progress)) {
    for (i in seq_along(rows)) {
      message(sprintf("Writing second-level APP Usage file %d/%d complete", i, n))
    }
  }
  rows
}

second_level_processing_order <- function(batch_summary) {
  n <- nrow(batch_summary)
  if (n == 0) {
    return(integer())
  }
  status <- if ("status" %in% names(batch_summary)) {
    as.character(batch_summary$status)
  } else {
    rep(NA_character_, n)
  }
  data_file <- if ("data_file" %in% names(batch_summary)) {
    as.character(batch_summary$data_file)
  } else {
    rep(NA_character_, n)
  }
  file_size <- rep(0, n)
  existing <- !is.na(data_file) & nzchar(data_file) & file.exists(data_file)
  if (any(existing)) {
    file_size[existing] <- as.numeric(file.info(data_file[existing])$size)
    file_size[is.na(file_size)] <- 0
  }
  n_rows <- if ("n_rows" %in% names(batch_summary)) {
    suppressWarnings(as.numeric(batch_summary$n_rows))
  } else {
    rep(0, n)
  }
  n_rows[is.na(n_rows)] <- 0
  detected_type <- if ("detected_type" %in% names(batch_summary)) {
    as.character(batch_summary$detected_type)
  } else {
    rep(NA_character_, n)
  }
  type_weight <- c(line = 4, meta = 3, day = 2, app = 1)
  weight <- unname(type_weight[detected_type])
  weight[is.na(weight)] <- 0
  elapsed <- if ("second_level_total_elapsed_sec" %in% names(batch_summary)) {
    suppressWarnings(as.numeric(batch_summary$second_level_total_elapsed_sec))
  } else if ("elapsed_sec" %in% names(batch_summary)) {
    suppressWarnings(as.numeric(batch_summary$elapsed_sec))
  } else {
    rep(0, n)
  }
  elapsed[is.na(elapsed)] <- 0
  eligible <- status == "success" & existing
  score <- file_size + n_rows * 100 + weight * 1e6 + elapsed * 1e5
  order(!eligible, -score, seq_len(n), na.last = TRUE)
}

appusage_package_root_for_workers <- function(start = getwd()) {
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    desc <- file.path(current, "DESCRIPTION")
    if (file.exists(desc)) {
      lines <- readLines(desc, warn = FALSE)
      if (any(grepl("^Package:\\s*appusageR\\s*$", lines))) {
        return(current)
      }
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      return(normalizePath(start, winslash = "/", mustWork = FALSE))
    }
    current <- parent
  }
}

process_batch_rows <- function(x, id_plan, type, input, output_dir, tz,
                               encoding, overwrite, progress, progress_every,
                               parallel, n_cores) {
  if (!isTRUE(parallel) || length(x) <= 1 || n_cores == 1) {
    rows <- vector("list", length(x))
    for (i in seq_along(x)) {
      if (isTRUE(progress) && (i == 1 || i %% progress_every == 0 || i == length(x))) {
        message(sprintf("Preprocessing APP Usage file %d/%d", i, length(x)))
      }
      rows[[i]] <- preprocess_one_appusage(
        x = x[[i]],
        id_info = id_plan[i, , drop = FALSE],
        type = type,
        input = input,
        output_dir = output_dir,
        tz = tz,
        encoding = encoding,
        overwrite = overwrite,
        index = i
      )
    }
    return(rows)
  }

  if (isTRUE(progress)) {
    message(sprintf(
      "Preprocessing %d APP Usage files with %d parallel workers",
      length(x), n_cores
    ))
  }
  cluster <- parallel::makeCluster(n_cores)
  on.exit(parallel::stopCluster(cluster), add = TRUE)
  package_root <- appusage_package_root_for_workers()
  parallel::clusterExport(
    cluster,
    varlist = c(
      "x", "id_plan", "type", "input", "output_dir", "tz", "encoding",
      "overwrite", "package_root"
    ),
    envir = environment()
  )
  parallel::clusterEvalQ(cluster, {
    if (!requireNamespace("appusageR", quietly = TRUE)) {
      if (requireNamespace("pkgload", quietly = TRUE) &&
        file.exists(file.path(package_root, "DESCRIPTION"))) {
        pkgload::load_all(package_root, quiet = TRUE)
      } else {
        stop("Package appusageR is not available on the parallel worker.")
      }
    }
    NULL
  })
  parallel::parLapplyLB(cluster, seq_along(x), function(i) {
    worker_preprocess <- get("preprocess_one_appusage", envir = asNamespace("appusageR"))
    worker_preprocess(
      x = x[[i]],
      id_info = id_plan[i, , drop = FALSE],
      type = type,
      input = input,
      output_dir = output_dir,
      tz = tz,
      encoding = encoding,
      overwrite = overwrite,
      index = i
    )
  })
}

prepare_batch_output_project <- function(output_dir, project_name, project_id,
                                         overwrite, n_inputs, input, tz) {
  if (is.null(output_dir)) {
    return(list(
      project_root = NULL,
      proclevel_1 = NULL
    ))
  }

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  project_name <- project_name %||% next_study_project_name(output_dir)
  project_id <- project_id %||% generate_project_id()
  folder_name <- paste0(sanitize_entity_value(project_name), "_", sanitize_entity_value(project_id))
  project_root <- file.path(output_dir, folder_name)

  if (dir.exists(project_root) && !isTRUE(overwrite)) {
    cli::cli_abort("Project output folder already exists: {.path {project_root}}")
  }
  dir.create(project_root, recursive = TRUE, showWarnings = FALSE)

  paths <- list(
    project_root = normalizePath(project_root, winslash = "/", mustWork = FALSE),
    proclevel_1 = normalizePath(file.path(project_root, "proclevel-1"), winslash = "/", mustWork = FALSE),
    project_name = project_name,
    project_id = project_id,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    n_inputs = n_inputs,
    input = input,
    timezone = tz
  )
  dir.create(paths$proclevel_1, recursive = TRUE, showWarnings = FALSE)
  write_dataset_description_json(paths, summary = NULL, proclevel = 0, summary_file = NA_character_, status = "created")
  paths
}

next_study_project_name <- function(output_dir) {
  existing <- list.dirs(output_dir, full.names = FALSE, recursive = FALSE)
  i <- 1L
  repeat {
    candidate <- paste0("Study", i)
    if (!any(grepl(paste0("^", candidate, "(?:_|$)"), existing))) {
      return(candidate)
    }
    i <- i + 1L
  }
}

generate_project_id <- function() {
  paste0(sample(c(0:9, letters[1:6]), size = 4, replace = TRUE), collapse = "")
}

write_dataset_description_json <- function(project, summary, proclevel,
                                           summary_file, status) {
  if (is.null(project$project_root)) {
    return(invisible(NULL))
  }
  description_file <- file.path(project$project_root, "dataset_descriptions.json")
  existing <- if (file.exists(description_file)) {
    tryCatch(
      jsonlite::read_json(description_file, simplifyVector = TRUE),
      error = function(e) list()
    )
  } else {
    list()
  }
  counts <- if (is.null(summary)) {
    list()
  } else {
    list(
      n_records = nrow(summary),
      n_success = if ("status" %in% names(summary)) sum(summary$status == "success", na.rm = TRUE) else NA_integer_,
      n_error = if ("status" %in% names(summary)) sum(summary$status == "error", na.rm = TRUE) else NA_integer_,
      n_skipped = if ("status" %in% names(summary)) sum(summary$status == "skipped", na.rm = TRUE) else NA_integer_,
      n_parse_warnings = if ("n_parse_warnings" %in% names(summary)) sum(summary$n_parse_warnings, na.rm = TRUE) else NA_integer_
    )
  }
  description <- list(
    schema_version = "0.3.0",
    dataset_type = "appusageR_preprocessed_dataset",
    project_name = coalesce_missing(project$project_name, existing$project_name, basename(project$project_root)),
    project_id = coalesce_missing(project$project_id, existing$project_id, NA_character_),
    dataset_name = basename(project$project_root),
    package_name = "appusageR",
    package_version = as.character(utils::packageVersion("appusageR")),
    created_at = coalesce_missing(project$created_at, existing$created_at, NA_character_),
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    timezone = coalesce_missing(project$timezone, existing$timezone, NA_character_),
    input = list(
      input_mode = coalesce_missing(project$input, existing$input$input_mode, NA_character_),
      n_inputs = coalesce_missing(project$n_inputs, existing$input$n_inputs, NA_integer_)
    ),
    directories = existing_processing_directories(project$project_root),
    appusage_files = build_appusage_file_profile(summary, existing$appusage_files),
    latest_proclevel = proclevel,
    latest_status = status,
    latest_summary_file = ifelse(is.na(summary_file), NA_character_, normalizePath(summary_file, winslash = "/", mustWork = FALSE)),
    counts = counts,
    qc = summarize_metadata_status(summary, "qc_status"),
    app_categories = summarize_metadata_status(summary, "app_category_status"),
    reserved_extensions = list(
      self_report = "reserved for Wenjuanxing/self-report matching",
      app_category_dictionary = "reserved for package_name category enrichment",
      ready_to_use_outputs = "reserved for final analysis datasets"
    )
  )
  jsonlite::write_json(description,
    path = description_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  invisible(description_file)
}

summarize_metadata_status <- function(summary, column) {
  if (is.null(summary) || !column %in% names(summary)) {
    return(list(status = "not_run", n_success = NA_integer_, n_error = NA_integer_))
  }
  values <- as.character(summary[[column]])
  values <- values[!is.na(values) & nzchar(values)]
  if (length(values) == 0) {
    return(list(status = "not_run", n_success = 0L, n_error = 0L))
  }
  table_values <- table(values)
  list(
    status = if (any(values == "error")) {
      "error"
    } else if (any(values == "success")) {
      "success"
    } else if (all(values == "not_run")) {
      "not_run"
    } else {
      paste(names(table_values), table_values, sep = ":", collapse = "; ")
    },
    n_success = sum(values == "success"),
    n_error = sum(values == "error"),
    n_not_run = sum(values == "not_run")
  )
}

coalesce_missing <- function(...) {
  values <- list(...)
  for (value in values) {
    if (!is.null(value) && length(value) > 0 && !all(is.na(value))) {
      return(value)
    }
  }
  NULL
}

existing_processing_directories <- function(project_root) {
  candidates <- c("proclevel-1", "proclevel-2", "proclevel-3")
  existing <- candidates[dir.exists(file.path(project_root, candidates))]
  out <- list()
  for (nm in existing) {
    key <- gsub("-", "_", nm, fixed = TRUE)
    out[[key]] <- normalizePath(file.path(project_root, nm), winslash = "/", mustWork = FALSE)
  }
  out
}

build_appusage_file_profile <- function(summary, existing = NULL) {
  if (is.null(summary) ||
    !all(c("detected_type", "native_export_created_at") %in% names(summary))) {
    return(existing %||% empty_appusage_file_profile())
  }
  recognized <- summary$detected_type %in% c("meta", "line", "app", "day")
  types <- summary$detected_type[recognized]
  dates <- as.Date(substr(summary$native_export_created_at[recognized], 1, 10))
  dates <- dates[!is.na(dates)]
  list(
    n_recognized_appusage_files = sum(recognized, na.rm = TRUE),
    n_meta = sum(types == "meta", na.rm = TRUE),
    n_line = sum(types == "line", na.rm = TRUE),
    n_app = sum(types == "app", na.rm = TRUE),
    n_day = sum(types == "day", na.rm = TRUE),
    native_export_date_min = if (length(dates) == 0) NA_character_ else format(min(dates), "%Y-%m-%d"),
    native_export_date_max = if (length(dates) == 0) NA_character_ else format(max(dates), "%Y-%m-%d")
  )
}

empty_appusage_file_profile <- function() {
  list(
    n_recognized_appusage_files = 0L,
    n_meta = 0L,
    n_line = 0L,
    n_app = 0L,
    n_day = 0L,
    native_export_date_min = NA_character_,
    native_export_date_max = NA_character_
  )
}

write_second_level_one <- function(batch_summary, index, output_dir, overwrite,
                                   resume = FALSE,
                                   second_level_args = list()) {
  first_file <- batch_summary$data_file[[index]]
  status <- batch_summary$status[[index]]
  started_at <- Sys.time()
  if (!identical(status, "success") || is.na(first_file) || !file.exists(first_file)) {
    finished_at <- Sys.time()
    skip_reason <- if (!identical(status, "success")) {
      "upstream_first_level_error"
    } else if (is.na(first_file)) {
      "missing_first_level_rda"
    } else {
      "first_level_rda_not_found"
    }
    return(data.frame(
      index = batch_summary$index[[index]],
      participant_id = batch_summary$participant_id[[index]],
      detected_type = batch_summary$detected_type[[index]],
      status = "skipped",
      skip_reason = skip_reason,
      first_level_data_file = first_file,
      second_level_data_file = NA_character_,
      second_level_metadata_file = NA_character_,
      error_message = NA_character_,
      started_at = format(started_at, "%Y-%m-%d %H:%M:%OS3 %z"),
      finished_at = format(finished_at, "%Y-%m-%d %H:%M:%OS3 %z"),
      elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
      stringsAsFactors = FALSE
    ))
  }

  cache <- second_level_existing_cache_status(
    first_level_rda = first_file,
    output_dir = output_dir,
    batch_summary = batch_summary,
    index = index
  )
  if (isTRUE(resume) && !isTRUE(overwrite) && identical(cache$status, "complete")) {
    finished_at <- Sys.time()
    return(data.frame(
      index = batch_summary$index[[index]],
      participant_id = batch_summary$participant_id[[index]],
      detected_type = batch_summary$detected_type[[index]],
      status = "skipped",
      skip_reason = "existing_proc2_cache",
      first_level_data_file = first_file,
      second_level_data_file = cache$rda_file,
      second_level_metadata_file = cache$json_file,
      error_message = NA_character_,
      started_at = format(started_at, "%Y-%m-%d %H:%M:%OS3 %z"),
      finished_at = format(finished_at, "%Y-%m-%d %H:%M:%OS3 %z"),
      elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
      stringsAsFactors = FALSE
    ))
  }
  effective_overwrite <- isTRUE(overwrite) ||
    (isTRUE(resume) && !isTRUE(overwrite) && identical(cache$status, "incomplete"))
  result <- tryCatch(
    list(
      output = do.call(
        write_second_level_appusage,
        c(
          list(
            first_level_rda = first_file,
            output_dir = output_dir,
            overwrite = effective_overwrite
          ),
          second_level_args
        )
      ),
      error = NULL
    ),
    error = function(e) list(output = NA_character_, error = e)
  )
  finished_at <- Sys.time()
  metadata_file <- if (is.null(result$error) && is_present_string(result$output)) {
    second_level_metadata_path(result$output)
  } else {
    write_second_level_status_metadata(
      batch_summary = batch_summary,
      index = index,
      output_dir = output_dir,
      status = "error",
      error = result$error,
      started_at = started_at,
      finished_at = finished_at
    )
  }
  data.frame(
    index = batch_summary$index[[index]],
    participant_id = batch_summary$participant_id[[index]],
    detected_type = batch_summary$detected_type[[index]],
    status = if (is.null(result$error)) "success" else "error",
    skip_reason = NA_character_,
    first_level_data_file = first_file,
    second_level_data_file = result$output,
    second_level_metadata_file = metadata_file,
    error_message = if (is.null(result$error)) NA_character_ else conditionMessage(result$error),
    started_at = format(started_at, "%Y-%m-%d %H:%M:%OS3 %z"),
    finished_at = format(finished_at, "%Y-%m-%d %H:%M:%OS3 %z"),
    elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
    stringsAsFactors = FALSE
  )
}

second_level_expected_paths <- function(first_level_rda, output_dir = NULL) {
  entities <- parse_appusage_filename(first_level_rda)
  participant_id <- entities$sub %||% "record-000001"
  export_type <- entities$type %||% "unknown"
  output_dir <- output_dir %||% default_second_level_output_dir(first_level_rda)
  rda_file <- file.path(
    output_dir,
    build_appusage_filename(
      participant_id = participant_id,
      export_type = export_type,
      proc = 2,
      extension = "rda"
    )
  )
  list(
    rda_file = normalizePath(rda_file, winslash = "/", mustWork = FALSE),
    json_file = normalizePath(second_level_metadata_path(rda_file), winslash = "/", mustWork = FALSE)
  )
}

second_level_existing_cache_status <- function(first_level_rda, output_dir = NULL,
                                               batch_summary = NULL,
                                               index = NULL) {
  paths <- second_level_expected_paths(first_level_rda, output_dir)
  rda_exists <- file.exists(paths$rda_file)
  json_exists <- file.exists(paths$json_file)
  if (!rda_exists && !json_exists) {
    return(c(paths, list(status = "missing", reason = "missing_pair")))
  }
  if (!rda_exists || !json_exists) {
    reason <- if (rda_exists) "rda_only_partial_cache" else "json_only_partial_cache"
    return(c(paths, list(status = "incomplete", reason = reason)))
  }
  metadata <- tryCatch(
    jsonlite::read_json(paths$json_file, simplifyVector = TRUE),
    error = function(e) NULL
  )
  if (is.null(metadata)) {
    return(c(paths, list(status = "incomplete", reason = "malformed_proc2_json")))
  }
  processing_status <- appusage_nested_value(metadata, c("processing", "second_level_status"))
  if (!identical(as.character(processing_status), "success")) {
    return(c(paths, list(status = "incomplete", reason = "non_success_proc2_json")))
  }
  expected_first <- normalizePath(first_level_rda, winslash = "/", mustWork = FALSE)
  recorded_first <- appusage_nested_value(metadata, c("outputs", "first_level_rda"))
  recorded_second <- appusage_nested_value(metadata, c("outputs", "second_level_rda"))
  if (is_present_string(recorded_first) &&
    !identical(normalizePath(recorded_first, winslash = "/", mustWork = FALSE), expected_first)) {
    return(c(paths, list(status = "incomplete", reason = "first_level_rda_mismatch")))
  }
  if (is_present_string(recorded_second) &&
    !identical(normalizePath(recorded_second, winslash = "/", mustWork = FALSE), paths$rda_file)) {
    return(c(paths, list(status = "incomplete", reason = "second_level_rda_mismatch")))
  }
  if (!is.null(batch_summary) && !is.null(index)) {
    recorded_id <- appusage_nested_value(metadata, c("identity", "participant_id"))
    recorded_type <- appusage_nested_value(metadata, c("export", "detected_type"))
    expected_id <- batch_summary$participant_id[[index]]
    expected_type <- batch_summary$detected_type[[index]]
    if (is_present_string(recorded_id) && is_present_string(expected_id) &&
      !identical(as.character(recorded_id), as.character(expected_id))) {
      return(c(paths, list(status = "incomplete", reason = "participant_id_mismatch")))
    }
    if (is_present_string(recorded_type) && is_present_string(expected_type) &&
      !identical(as.character(recorded_type), as.character(expected_type))) {
      return(c(paths, list(status = "incomplete", reason = "export_type_mismatch")))
    }
  }
  c(paths, list(status = "complete", reason = "valid_proc2_pair"))
}

appusage_nested_value <- function(x, path, default = NA_character_) {
  value <- x
  for (nm in path) {
    if (is.null(value) || !is.list(value) || is.null(value[[nm]])) {
      return(default)
    }
    value <- value[[nm]]
  }
  if (is.null(value) || length(value) == 0) {
    return(default)
  }
  value[[1]]
}

infer_project_root_from_summary <- function(batch_summary) {
  if ("project_root" %in% names(batch_summary)) {
    root <- unique(stats::na.omit(batch_summary$project_root))
    if (length(root) > 0) {
      return(root[[1]])
    }
  }
  files <- stats::na.omit(batch_summary$data_file)
  if (length(files) == 0) {
    return(NA_character_)
  }
  parent <- dirname(files[[1]])
  if (basename(parent) %in% c("proclevel-1", "proclevel-2", "proclevel-3")) {
    return(dirname(parent))
  }
  NA_character_
}

project_info_from_root <- function(project_root) {
  parts <- strsplit(basename(project_root), "_", fixed = TRUE)[[1]]
  list(
    project_root = normalizePath(project_root, winslash = "/", mustWork = FALSE),
    proclevel_1 = normalizePath(file.path(project_root, "proclevel-1"), winslash = "/", mustWork = FALSE),
    proclevel_2 = normalizePath(file.path(project_root, "proclevel-2"), winslash = "/", mustWork = FALSE),
    project_name = parts[[1]] %||% basename(project_root),
    project_id = if (length(parts) >= 2) parts[[2]] else NA_character_,
    created_at = NA_character_,
    n_inputs = NA_integer_,
    input = NA_character_,
    timezone = NA_character_
  )
}
