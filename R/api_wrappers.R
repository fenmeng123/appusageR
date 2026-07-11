#' Read APP Usage text into normalized lines
#'
#' Module 1 convenience wrapper for raw APP Usage text input. It keeps only the
#' normalized line vector plus lightweight source metadata, so the result can be
#' passed directly to `run_first_level_appusage()`.
#'
#' @param x File path, single text string, or character vector of lines.
#' @param input Input mode: `"file"`, `"text"`, or `"lines"`.
#' @param encoding Source encoding or `"auto"`.
#'
#' @return A lightweight `appusage_text` list.
#' @export
read_appusage_text <- function(x, input = c("file", "text", "lines"),
                               encoding = "auto") {
  input <- match.arg(input)
  warnings <- character()
  lines <- withCallingHandlers(
    read_appusage_lines(x, input = input, encoding = encoding),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  source_path <- if (identical(input, "file")) {
    normalizePath(x, winslash = "/", mustWork = FALSE)
  } else {
    NA_character_
  }
  out <- list(
    lines = lines,
    input = input,
    source_file = if (identical(input, "file")) basename(x) else NA_character_,
    source_path = source_path,
    encoding = encoding,
    requested_encoding = encoding,
    n_lines = length(lines),
    warnings = unique(warnings),
    read_warning = if (length(warnings) == 0) {
      NA_character_
    } else {
      paste(unique(warnings), collapse = "\n")
    }
  )
  class(out) <- c("appusage_text", "list")
  out
}

#' Run first-level APP Usage preprocessing for one source
#'
#' Module 2 wrapper around content detection and the faithful raw parsers. When
#' cache writing is requested, it writes the same BIDS-like `proc-1` RDA/JSON
#' pair used by batch preprocessing.
#'
#' @param x File path, text, lines, or an `appusage_text` object from
#'   `read_appusage_text()`.
#' @param input Input mode when `x` is not an `appusage_text` object.
#' @param type APP Usage export type, or `"auto"` for content detection.
#' @param participant_id Optional participant ID for metadata and cache naming.
#' @param tz Time zone passed to parsers.
#' @param encoding Source encoding or `"auto"`.
#' @param strict Whether parser errors should be strict.
#' @param output_dir Optional directory for first-level cache files.
#' @param write_cache Whether to write first-level cache files.
#' @param overwrite Whether existing cache files may be overwritten.
#'
#' @return An `appusage_first_level` list with parsed data and metadata.
#' @export
run_first_level_appusage <- function(x, input = c("file", "text", "lines"),
                                     type = c("auto", "line", "meta", "day", "app"),
                                     participant_id = NULL,
                                     tz = "Asia/Shanghai",
                                     encoding = "auto",
                                     strict = FALSE,
                                     output_dir = NULL,
                                     write_cache = !is.null(output_dir),
                                     overwrite = FALSE) {
  input <- match.arg(input)
  type <- match.arg(type)
  normalized <- normalize_first_level_input(x, input = input, encoding = encoding)
  parse_x <- normalized$x
  parse_input <- normalized$input
  source_file <- normalized$source_file
  metadata_input <- normalized$metadata_input
  id_info <- first_level_wrapper_id_info(
    source_file = source_file,
    input = metadata_input,
    participant_id = participant_id
  )
  participant_id <- id_info$participant_id[[1]]
  warnings <- character()
  started_at <- Sys.time()
  detected_type <- NA_character_
  preflight <- NULL

  result <- tryCatch(
    withCallingHandlers(
      {
        preflight <- appusage_source_preflight(
          x = parse_x,
          input = parse_input,
          encoding = encoding,
          filename_type = id_info$native_export_type_from_filename[[1]]
        )
        if (!identical(preflight$status, "ok")) {
          detected_type <- if (length(preflight$detected_components) == 1L) {
            preflight$detected_components[[1]]
          } else if (length(preflight$detected_components) > 1L) {
            "mixed"
          } else {
            "unknown"
          }
          stop(appusage_source_preflight_error(preflight))
        }
        parse_x <- preflight$lines
        parse_input <- "lines"
        detected_type <- first_level_detect_type(
          x = parse_x,
          input = parse_input,
          type = type,
          encoding = encoding,
          id_info = id_info,
          preflight = preflight
        )
        if (identical(detected_type, "unknown")) {
          cli::cli_abort("APP Usage export type could not be detected.")
        }
        if (isTRUE(preflight$filename_content_disagreement)) {
          warnings <- c(warnings, paste0(
            "Filename export type '", preflight$filename_type,
            "' disagrees with content-selected component '", detected_type,
            "'; content selection was used."
          ))
        }
        parsed <- parse_first_level_by_type(
          x = parse_x,
          input = parse_input,
          type = detected_type,
          participant_id = participant_id,
          source_file = source_file,
          tz = tz,
          encoding = encoding,
          strict = strict
        )
        if (identical(detected_type, "line")) {
          parsed_diagnostics <- parser_diagnostics(parsed)
          structural_quality <- parsed_diagnostics$format_specific$structural_quality %||% list()
          if (isTRUE(structural_quality$critical)) {
            stop(appusage_line_structural_quality_error(parsed_diagnostics))
          }
        }
        first_level_data <- as_first_level_data(parsed, detected_type)
        if (first_level_is_empty(first_level_data)) {
          stop(first_level_empty_raw_data_error(
            detected_type,
            parser_diagnostics(first_level_data)
          ))
        }
        list(
          status = "success",
          type = detected_type,
          parsed = parsed,
          data = first_level_data,
          error = NULL
        )
      },
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      if (isTRUE(strict)) {
        stop(e)
      }
      list(
        status = "error",
        type = if (!is.na(detected_type)) {
          detected_type
        } else if (identical(type, "auto")) {
          NA_character_
        } else {
          type
        },
        parsed = NULL,
        data = NULL,
        error = e
      )
    }
  )

  finished_at <- Sys.time()
  metadata <- build_metadata(
    participant_id = participant_id,
    participant_id_source = id_info$participant_id_source[[1]],
    id_info = id_info,
    source_file = source_file,
    export_type = result$type,
    export_type_match = filename_export_type_match(id_info, result$type),
    input = metadata_input,
    encoding = encoding,
    tz = tz,
    started_at = started_at,
    finished_at = finished_at,
    status = result$status,
    data = result$data,
    warnings = warnings,
    error = result$error,
    metadata_file = NA_character_,
    data_file = NA_character_,
    preflight = preflight
  )

  metadata_file <- NA_character_
  data_file <- NA_character_
  if (isTRUE(write_cache)) {
    if (is.null(output_dir)) {
      cli::cli_abort("`output_dir` is required when `write_cache = TRUE`.")
    }
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    output_type <- result$type
    if (!is_present_string(output_type)) {
      output_type <- "unknown"
    }
    data_file <- file.path(
      output_dir,
      build_appusage_filename(
        participant_id = participant_id,
        export_type = output_type,
        proc = 1,
        extension = "rda"
      )
    )
    metadata_file <- file.path(
      output_dir,
      build_appusage_filename(
        participant_id = participant_id,
        export_type = output_type,
        proc = 1,
        extension = "json"
      )
    )
    if ((file.exists(data_file) || file.exists(metadata_file)) && !isTRUE(overwrite)) {
      stop(batch_cache_exists_error(paste(c(data_file, metadata_file), collapse = "; ")))
    }
    metadata$outputs$metadata_json <- normalizePath(metadata_file, winslash = "/", mustWork = FALSE)
    if (identical(result$status, "success")) {
      data <- result$data
      save(data, file = data_file)
      metadata$outputs$first_level_rda <- normalizePath(data_file, winslash = "/", mustWork = FALSE)
    } else {
      data_file <- NA_character_
      metadata$outputs$first_level_rda <- NA_character_
    }
    write_metadata_json(metadata, metadata_file)
  }

  out <- list(
    status = result$status,
    type = result$type,
    data = result$data,
    parsed = result$parsed,
    metadata = metadata,
    data_file = ifelse(is.na(data_file), NA_character_, normalizePath(data_file, winslash = "/", mustWork = FALSE)),
    metadata_file = ifelse(is.na(metadata_file), NA_character_, normalizePath(metadata_file, winslash = "/", mustWork = FALSE)),
    error_message = if (is.null(result$error)) NA_character_ else conditionMessage(result$error)
  )
  class(out) <- c("appusage_first_level", "list")
  out
}

#' Run second-level APP Usage preprocessing for one source
#'
#' Module 3 wrapper around [make_second_level_appusage()] and
#' [write_second_level_appusage()]. Meta episode reconstruction is explicit and
#' enabled by default in second-level meta processing.
#'
#' @param x First-level wrapper result, first-level RDA path, parser output, or
#'   first-level data list.
#' @param export_type Optional export type override for in-memory data.
#' @param output_dir Optional directory for `proc-2` cache files.
#' @param write_cache Whether to write `proc-2` cache files.
#' @param overwrite Whether existing cache files may be overwritten.
#' @param include_collection_app Whether to keep `com.w.appusage` rows.
#' @param max_episode_ms Episode anomaly threshold.
#' @param max_daily_app_ms Daily-app anomaly threshold.
#' @param reconstruct_meta Whether to explicitly reconstruct meta Table 2 events
#'   into episode records. Defaults to `TRUE` for second-level meta output.
#' @param meta_pairing Pairing strategy for meta event reconstruction.
#' @param meta_start_event_types Event types that start meta episodes.
#' @param meta_end_event_types Event types that end meta episodes.
#' @param merge_meta_episodes Whether to merge adjacent complete reconstructed
#'   meta episodes for the same app when their time gap is small.
#' @param meta_episode_merge_gap_ms Maximum non-negative gap, in milliseconds,
#'   allowed when merging adjacent reconstructed meta episodes.
#' @param meta_daily_source Source for meta daily rows.
#'
#' @return An `appusage_second_level` list.
#' @export
run_second_level_appusage <- function(x, export_type = NULL,
                                      output_dir = NULL,
                                      write_cache = !is.null(output_dir),
                                      overwrite = FALSE,
                                      include_collection_app = TRUE,
                                      max_episode_ms = 24 * 60 * 60 * 1000,
                                      max_daily_app_ms = 24 * 60 * 60 * 1000,
                                      reconstruct_meta = TRUE,
                                      meta_pairing = c("package", "package_class"),
                                      meta_start_event_types = 1,
                                      meta_end_event_types = c(2, 23),
                                      merge_meta_episodes = TRUE,
                                      meta_episode_merge_gap_ms = 30 * 1000,
                                      meta_daily_source = c("summary", "episodes", "both")) {
  meta_pairing <- match.arg(meta_pairing)
  meta_daily_source <- match.arg(meta_daily_source)
  first_level_rda <- NA_character_
  first_data <- x
  if (inherits(x, "appusage_first_level")) {
    first_level_rda <- x$data_file
    first_data <- x$data
    export_type <- export_type %||% x$type
  } else if (is.character(x) && length(x) == 1 && file.exists(x)) {
    first_level_rda <- x
    first_data <- load_appusage_data_object(x)
    export_type <- export_type %||% parse_appusage_filename(x)$type
  }

  data_file <- NA_character_
  metadata_file <- NA_character_
  if (isTRUE(write_cache)) {
    if (!is_present_string(first_level_rda) || !file.exists(first_level_rda)) {
      cli::cli_abort("A first-level RDA path is required when `write_cache = TRUE`.")
    }
    data_file <- write_second_level_appusage(
      first_level_rda = first_level_rda,
      output_dir = output_dir,
      overwrite = overwrite,
      include_collection_app = include_collection_app,
      max_episode_ms = max_episode_ms,
      max_daily_app_ms = max_daily_app_ms,
      reconstruct_meta = reconstruct_meta,
      meta_pairing = meta_pairing,
      meta_start_event_types = meta_start_event_types,
      meta_end_event_types = meta_end_event_types,
      merge_meta_episodes = merge_meta_episodes,
      meta_episode_merge_gap_ms = meta_episode_merge_gap_ms,
      meta_daily_source = meta_daily_source
    )
    second_data <- load_appusage_data_object(data_file)
    metadata_file <- second_level_metadata_path(data_file)
  } else {
    second_data <- make_second_level_appusage(
      first_data,
      export_type = export_type,
      include_collection_app = include_collection_app,
      max_episode_ms = max_episode_ms,
      max_daily_app_ms = max_daily_app_ms,
      reconstruct_meta = reconstruct_meta,
      meta_pairing = meta_pairing,
      meta_start_event_types = meta_start_event_types,
      meta_end_event_types = meta_end_event_types,
      merge_meta_episodes = merge_meta_episodes,
      meta_episode_merge_gap_ms = meta_episode_merge_gap_ms,
      meta_daily_source = meta_daily_source
    )
  }

  out <- list(
    status = "success",
    data = second_data,
    data_file = ifelse(is.na(data_file), NA_character_, normalizePath(data_file, winslash = "/", mustWork = FALSE)),
    metadata_file = ifelse(is.na(metadata_file), NA_character_, normalizePath(metadata_file, winslash = "/", mustWork = FALSE))
  )
  class(out) <- c("appusage_second_level", "list")
  out
}

#' Run APP Usage QC
#'
#' Module 4 wrapper for in-memory daily QC or project-level QC metadata updates.
#' In project mode this calls [write_qc_metadata_batch()] and updates `proc-2`
#' JSON metadata without creating `proclevel-3`.
#'
#' @param x A daily data frame, second-level data list, or second-level wrapper
#'   result.
#' @param project_dir Optional project directory for metadata-writing QC.
#' @param participant_col Participant column for in-memory daily QC.
#' @param strict,overwrite,progress Passed to [write_qc_metadata_batch()] in
#'   project mode.
#' @param require_all_weekdays,min_nonempty_days,use_all_apps_row,include_collection_app,drop_likely_total_all_rows,all_row_tolerance
#'   Existing QC rule parameters.
#' @param max_episode_ms,max_daily_app_ms,max_daily_total_ms Numeric anomaly
#'   thresholds in milliseconds for project-mode QC metadata.
#' @param max_export_lookback_days Maximum expected lookback in days from native
#'   export timestamp to observed record dates.
#' @param meta_diff_abs_ms,meta_diff_ratio Thresholds for meta summary-vs-episode
#'   duration disagreement checks.
#'
#' @return A QC tibble or project summary tibble.
#' @export
run_qc_appusage <- function(x = NULL, project_dir = NULL,
                            participant_col = "participant_id",
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
  if (!is.null(project_dir)) {
    return(write_qc_metadata_batch(
      project_dir = project_dir,
      strict = strict,
      overwrite = overwrite,
      progress = progress,
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
    ))
  }
  if (inherits(x, "appusage_second_level")) {
    x <- x$data
  }
  daily <- if (is.list(x) && !is.data.frame(x) && !is.null(x$daily)) {
    x$daily
  } else {
    x
  }
  if (!is.data.frame(daily)) {
    cli::cli_abort("`x` must be a daily data frame, second-level data list, or `project_dir`.")
  }
  if (!participant_col %in% names(daily)) {
    daily[[participant_col]] <- "record-000001"
  }
  qc_appusage_day(
    daily,
    participant_col = participant_col,
    require_all_weekdays = require_all_weekdays,
    min_nonempty_days = min_nonempty_days,
    use_all_apps_row = use_all_apps_row,
    include_collection_app = include_collection_app,
    drop_likely_total_all_rows = drop_likely_total_all_rows,
    all_row_tolerance = all_row_tolerance
  )
}

#' Extract uncoded apps after category enrichment
#'
#' Module 5 helper for dictionary maintenance. It reports rows where either
#' category column is missing and does not infer or assign categories.
#'
#' @param x Data frame or second-level data list after [add_app_categories()].
#'
#' @return A tibble of de-duplicated uncoded app rows.
#' @export
extract_uncoded_apps <- function(x) {
  frames <- normalize_uncoded_app_frames(x)
  if (length(frames) == 0) {
    return(empty_uncoded_apps_tibble())
  }
  data <- tibble::as_tibble(do.call(rbind, frames))
  if (!all(c("app_name", "package_name") %in% names(data))) {
    return(empty_uncoded_apps_tibble())
  }
  if (!"Level_1_Category" %in% names(data)) {
    data$Level_1_Category <- NA_character_
  }
  if (!"Level_2_Category" %in% names(data)) {
    data$Level_2_Category <- NA_character_
  }
  if (!"source_export_type" %in% names(data)) {
    data$source_export_type <- NA_character_
  }
  missing_category <- is.na(data$Level_1_Category) | is.na(data$Level_2_Category) |
    !nzchar(as.character(data$Level_1_Category)) |
    !nzchar(as.character(data$Level_2_Category))
  data <- data[missing_category, , drop = FALSE]
  if (nrow(data) == 0) {
    return(empty_uncoded_apps_tibble())
  }
  key <- paste(data$package_name, data$app_name, data$source_export_type, sep = "\r")
  groups <- split(seq_len(nrow(data)), key)
  rows <- lapply(groups, function(idx) {
    duration <- if ("duration_ms" %in% names(data)) {
      sum(data$duration_ms[idx], na.rm = TRUE)
    } else {
      NA_real_
    }
    data.frame(
      package_name = data$package_name[idx[[1]]],
      app_name = data$app_name[idx[[1]]],
      source_export_type = data$source_export_type[idx[[1]]],
      n_rows = length(idx),
      total_duration_ms = duration,
      stringsAsFactors = FALSE
    )
  })
  tibble::as_tibble(do.call(rbind, rows))
}

#' Run the standard APP Usage preprocessing workflow
#'
#' Module 6 high-level wrapper around existing batch preprocessing, second-level
#' transformation, routine QC, and optional category enrichment.
#'
#' @param x Input vector passed to [read_appusage_batch()].
#' @param output_dir Parent output directory for the BIDS-like project folder.
#' @param ids Optional participant IDs aligned with `x`.
#' @param type,input,tz,encoding,strict Existing first-level batch parameters.
#' @param run_second_level Whether to run second-level preprocessing.
#' @param run_qc Whether to update QC metadata.
#' @param dictionary Optional category dictionary data frame or path.
#' @param run_category Whether to run category enrichment.
#' @param progress,parallel,n_cores Existing batch workflow controls.
#' @param overwrite Whether cache outputs may be overwritten.
#' @param project_name,project_id Optional project folder identifiers.
#' @param reconstruct_meta Whether to explicitly reconstruct meta Table 2 events
#'   during second-level preprocessing. Defaults to `TRUE`.
#' @param meta_pairing Pairing strategy for meta event reconstruction.
#' @param meta_start_event_types Event types that start meta episodes.
#' @param meta_end_event_types Event types that end meta episodes.
#' @param merge_meta_episodes Whether to merge adjacent complete reconstructed
#'   meta episodes for the same app when their time gap is small.
#' @param meta_episode_merge_gap_ms Maximum non-negative gap, in milliseconds,
#'   allowed when merging adjacent reconstructed meta episodes.
#' @param meta_daily_source Source for meta daily rows.
#'
#' @return Invisibly returns an `appusage_workflow_result` list of compact
#'   summaries and paths.
#' @export
run_appusage_workflow <- function(x, output_dir, ids = NULL,
                                  type = "auto", input = "file",
                                  tz = "Asia/Shanghai",
                                  encoding = "auto",
                                  strict = FALSE,
                                  run_second_level = TRUE,
                                  run_qc = TRUE,
                                  dictionary = NULL,
                                  run_category = !is.null(dictionary),
                                  progress = TRUE,
                                  parallel = FALSE,
                                  n_cores = 1,
                                  overwrite = FALSE,
                                  project_name = NULL,
                                  project_id = NULL,
                                  reconstruct_meta = TRUE,
                                  meta_pairing = c("package", "package_class"),
                                  meta_start_event_types = 1,
                                  meta_end_event_types = c(2, 23),
                                  merge_meta_episodes = TRUE,
                                  meta_episode_merge_gap_ms = 30 * 1000,
                                  meta_daily_source = c("summary", "episodes", "both")) {
  meta_pairing <- match.arg(meta_pairing)
  meta_daily_source <- match.arg(meta_daily_source)
  first <- read_appusage_batch(
    x,
    ids = ids,
    type = type,
    input = input,
    output_dir = output_dir,
    project_name = project_name,
    project_id = project_id,
    tz = tz,
    encoding = encoding,
    strict = strict,
    overwrite = overwrite,
    progress = progress,
    parallel = parallel,
    n_cores = n_cores
  )
  project_dir <- unique(stats::na.omit(first$project_root))[[1]]
  second <- NULL
  qc <- NULL
  categories <- NULL
  latest <- first
  if (isTRUE(run_second_level)) {
    second <- write_second_level_batch(first,
      overwrite = overwrite,
      progress = progress,
      reconstruct_meta = reconstruct_meta,
      meta_pairing = meta_pairing,
      meta_start_event_types = meta_start_event_types,
      meta_end_event_types = meta_end_event_types,
      merge_meta_episodes = merge_meta_episodes,
      meta_episode_merge_gap_ms = meta_episode_merge_gap_ms,
      meta_daily_source = meta_daily_source
    )
    latest <- second
  }
  if (isTRUE(run_qc)) {
    qc <- write_qc_metadata_batch(project_dir,
      strict = strict,
      progress = progress
    )
    latest <- qc
  }
  if (isTRUE(run_category)) {
    if (is.null(dictionary)) {
      cli::cli_abort("`dictionary` is required when `run_category = TRUE`.")
    }
    dictionary <- if (is.character(dictionary) && length(dictionary) == 1) {
      read_app_category_dictionary(dictionary)
    } else {
      dictionary
    }
    categories <- write_app_categories_batch(project_dir,
      dictionary = dictionary,
      progress = progress
    )
    latest <- categories
  }

  out <- list(
    project_dir = project_dir,
    first_level = first,
    second_level = second,
    qc = qc,
    categories = categories,
    latest = latest
  )
  class(out) <- c("appusage_workflow_result", "list")
  invisible(out)
}

normalize_first_level_input <- function(x, input, encoding) {
  if (inherits(x, "appusage_text")) {
    return(list(
      x = x$lines,
      input = "lines",
      source_file = x$source_path,
      metadata_input = x$input
    ))
  }
  source_file <- if (identical(input, "file")) {
    normalizePath(x, winslash = "/", mustWork = FALSE)
  } else {
    source_file_label(x, input)
  }
  list(
    x = x,
    input = input,
    source_file = source_file,
    metadata_input = input
  )
}

first_level_wrapper_id_info <- function(source_file, input, participant_id) {
  filename <- if (identical(input, "file") && is_present_string(source_file)) {
    parse_wenjuanxing_upload_filename(source_file)
  } else {
    list()
  }
  resolved_id <- participant_id %||% {
    seq_id <- filename$wenjuanxing_sequence_id %||% NA_integer_
    if (!is.na(seq_id)) as.character(seq_id) else "record-000001"
  }
  data.frame(
    participant_id = resolved_id,
    participant_id_source = if (!is.null(participant_id)) {
      "manual"
    } else if (!is.na(filename$wenjuanxing_sequence_id %||% NA_integer_)) {
      "filename"
    } else {
      "generated"
    },
    wenjuanxing_sequence_id = filename$wenjuanxing_sequence_id %||% NA_integer_,
    filename_parse_status = filename$filename_parse_status %||% NA_character_,
    filename_parse_warning = filename$filename_parse_warning %||% NA_character_,
    native_export_file_name = filename$native_export_file_name %||% NA_character_,
    native_export_type_from_filename =
      filename$native_export_type_from_filename %||% NA_character_,
    native_export_created_at = filename$native_export_created_at %||% NA_character_,
    stringsAsFactors = FALSE
  )
}

parse_first_level_by_type <- function(x, input, type, participant_id,
                                      source_file, tz, encoding, strict) {
  switch(type,
    line = parse_line(
      x,
      input = input,
      participant_id = participant_id,
      source_file = source_file,
      tz = tz,
      encoding = encoding,
      strict = strict
    ),
    meta = parse_meta(
      x,
      input = input,
      participant_id = participant_id,
      source_file = source_file,
      tz = tz,
      encoding = encoding,
      strict = strict
    ),
    day = parse_day(
      x,
      input = input,
      participant_id = participant_id,
      source_file = source_file,
      tz = tz,
      encoding = encoding,
      strict = strict
    ),
    app = parse_app(
      x,
      input = input,
      participant_id = participant_id,
      source_file = source_file,
      tz = tz,
      encoding = encoding,
      strict = strict
    )
  )
}

normalize_uncoded_app_frames <- function(x) {
  if (is.data.frame(x)) {
    return(list(x))
  }
  if (!is.list(x)) {
    cli::cli_abort("`x` must be a data frame or second-level data list.")
  }
  grains <- intersect(c("event", "episode", "daily"), names(x))
  frames <- lapply(grains, function(grain) {
    frame <- x[[grain]]
    if (!is.data.frame(frame) || nrow(frame) == 0) {
      return(NULL)
    }
    if (!"source_export_type" %in% names(frame)) {
      frame$source_export_type <- grain
    }
    frame
  })
  Filter(Negate(is.null), frames)
}

empty_uncoded_apps_tibble <- function() {
  tibble::tibble(
    package_name = character(),
    app_name = character(),
    source_export_type = character(),
    n_rows = integer(),
    total_duration_ms = numeric()
  )
}
