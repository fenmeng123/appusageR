#' Scan APP Usage project folders
#'
#' Finds project folders named like
#' `ProjectName-StudyID_ProjectID-ProjectID` and pairs candidate Wenjuanxing
#' Excel files by `ProjectID`. This is a structure-only scan and does not read
#' `.txt` contents.
#'
#' @param root Root directory containing project folders and candidate Excel
#'   files.
#' @param excel_pattern Case-insensitive pattern for candidate Wenjuanxing Excel
#'   files.
#' @param recursive Whether to scan folders and Excel files recursively.
#'
#' @return A tibble with one row per discovered project folder.
#' @export
scan_appusage_project_root <- function(root, excel_pattern = "WJXraw",
                                       recursive = FALSE) {
  if (length(root) != 1 || is.na(root) || !dir.exists(root)) {
    cli::cli_abort("`root` must be an existing directory.")
  }
  recursive <- isTRUE(recursive)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  dirs <- c(root, list.dirs(root,
    full.names = TRUE,
    recursive = recursive
  ))
  dirs <- unique(dirs[appusage_is_project_folder_name(basename(dirs))])

  excel_files <- list.files(root,
    pattern = "[.](xlsx|xls)$",
    full.names = TRUE,
    recursive = recursive,
    ignore.case = TRUE
  )
  if (is_present_string(excel_pattern)) {
    excel_files <- excel_files[grepl(excel_pattern, basename(excel_files),
      ignore.case = TRUE
    )]
  }

  rows <- lapply(dirs, function(project_dir) {
    meta <- appusage_parse_project_folder(project_dir)
    project_files <- list.files(project_dir,
      full.names = TRUE,
      recursive = FALSE,
      all.files = FALSE,
      no.. = TRUE
    )
    info <- if (length(project_files) > 0) file.info(project_files) else data.frame()
    is_file <- if (length(project_files) > 0) {
      !is.na(info$isdir) & !info$isdir
    } else {
      logical()
    }
    project_files <- project_files[is_file]
    info <- if (length(project_files) > 0) file.info(project_files) else data.frame()
    ext <- tolower(tools::file_ext(project_files))
    is_txt <- ext == "txt"
    excel_matches <- excel_files[grepl(
      appusage_project_id_excel_pattern(meta$project_id),
      basename(excel_files),
      ignore.case = TRUE
    )]
    data.frame(
      project_name = meta$project_name,
      study_id = meta$study_id,
      project_id = meta$project_id,
      project_label = meta$project_label,
      project_dir = normalizePath(project_dir, winslash = "/", mustWork = FALSE),
      excel_count = length(excel_matches),
      excel_path = if (length(excel_matches) > 0) {
        normalizePath(excel_matches[[1]], winslash = "/", mustWork = FALSE)
      } else {
        NA_character_
      },
      excel_paths = paste(normalizePath(excel_matches,
        winslash = "/",
        mustWork = FALSE
      ), collapse = ";"),
      n_files = length(project_files),
      n_txt_files = sum(is_txt, na.rm = TRUE),
      n_zero_byte_txt = if (length(project_files) > 0) {
        sum(is_txt & info$size == 0, na.rm = TRUE)
      } else {
        0L
      },
      n_non_txt_files = sum(!is_txt, na.rm = TRUE),
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0) {
    return(appusage_empty_project_scan())
  }
  tibble::as_tibble(do.call(rbind, rows))
}

#' Build an APP Usage project manifest
#'
#' Lists direct files in a project folder and records file metadata and
#' filename-derived APP Usage/Wenjuanxing fields without parsing `.txt`
#' contents.
#'
#' @param project_dir Project directory containing APP Usage export files.
#' @param self_report_file Optional paired Wenjuanxing/self-report file path.
#'
#' @return A tibble manifest with one row per direct file.
#' @export
build_appusage_project_manifest <- function(project_dir,
                                            self_report_file = NULL) {
  if (length(project_dir) != 1 || is.na(project_dir) || !dir.exists(project_dir)) {
    cli::cli_abort("`project_dir` must be an existing directory.")
  }
  if (!is.null(self_report_file) &&
    (length(self_report_file) != 1 || is.na(self_report_file) ||
      !file.exists(self_report_file))) {
    cli::cli_abort("`self_report_file` must be an existing file or `NULL`.")
  }

  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  project <- appusage_parse_project_folder(project_dir)
  files <- list.files(project_dir,
    full.names = TRUE,
    recursive = FALSE,
    all.files = FALSE,
    no.. = TRUE
  )
  files <- files[file.info(files)$isdir %in% FALSE]
  if (length(files) == 0) {
    return(appusage_empty_project_manifest(project, project_dir, self_report_file))
  }

  info <- file.info(files)
  ext <- tolower(tools::file_ext(files))
  is_txt <- ext == "txt"
  filename_meta <- lapply(files, parse_wenjuanxing_upload_filename)
  n_txt <- sum(is_txt, na.rm = TRUE)
  n_zero <- sum(is_txt & info$size == 0, na.rm = TRUE)
  n_non_txt <- sum(!is_txt, na.rm = TRUE)

  tibble::tibble(
    project_name = project$project_name,
    study_id = project$study_id,
    project_id = project$project_id,
    project_label = project$project_label,
    project_dir = project_dir,
    self_report_file = appusage_normalize_optional_path(self_report_file),
    index = seq_along(files),
    source_file = normalizePath(files, winslash = "/", mustWork = FALSE),
    source_basename = basename(files),
    extension = ext,
    is_txt = is_txt,
    is_zero_byte = info$size == 0,
    is_zero_byte_txt = is_txt & info$size == 0,
    file_size = as.numeric(info$size),
    modified_at = format(info$mtime, "%Y-%m-%dT%H:%M:%OS3%z"),
    wenjuanxing_sequence_id = vapply(filename_meta, function(x) {
      x$wenjuanxing_sequence_id %||% NA_integer_
    }, integer(1)),
    candidate_participant_id = vapply(filename_meta, function(x) {
      id <- x$wenjuanxing_sequence_id %||% NA_integer_
      if (is.na(id)) NA_character_ else as.character(id)
    }, character(1)),
    filename_parse_status = vapply(filename_meta, function(x) {
      x$filename_parse_status %||% NA_character_
    }, character(1)),
    filename_parse_warning = vapply(filename_meta, function(x) {
      x$filename_parse_warning %||% NA_character_
    }, character(1)),
    native_export_file_name = vapply(filename_meta, function(x) {
      x$native_export_file_name %||% NA_character_
    }, character(1)),
    uploaded_file_name = vapply(filename_meta, function(x) {
      x$uploaded_file_name %||% NA_character_
    }, character(1)),
    filename_export_type = vapply(filename_meta, function(x) {
      x$native_export_type_from_filename %||% NA_character_
    }, character(1)),
    native_export_created_at = vapply(filename_meta, function(x) {
      x$native_export_created_at %||% NA_character_
    }, character(1)),
    project_n_files = length(files),
    project_n_txt_files = n_txt,
    project_n_zero_byte_txt = n_zero,
    project_n_non_txt_files = n_non_txt
  )
}

#' Run a project-level APP Usage workflow
#'
#' Builds a project manifest, optionally performs a dry run, or delegates to the
#' existing first-level, second-level, and QC batch functions with serial
#' processing by default. Starting in 0.3.0, this wrapper can also discover a
#' project folder and paired Wenjuanxing Excel file from a raw-data root and
#' write a matched self-report Excel file.
#'
#' @param project_dir Optional direct project directory. If supplied, this
#'   preserves the 0.2.10 direct-folder behavior.
#' @param output_root Parent output directory.
#' @param raw_data_root Optional root containing multiple project folders and
#'   Wenjuanxing Excel files.
#' @param project_id Optional ProjectID. Required when `raw_data_root` is used.
#' @param project_name Optional project/study label for validation and output
#'   naming.
#' @param self_report_file Optional paired Wenjuanxing/self-report file path.
#' @param sequence_col Wenjuanxing sequence column. Defaults to
#'   `"\\u5e8f\\u53f7"`.
#' @param upload_col Project-specific upload/file column for self-report
#'   matching.
#' @param submit_time_col Optional Wenjuanxing submit-time column for duplicate
#'   candidate tie-breaking.
#' @param max_files Maximum number of `.txt` files to preprocess, selected by
#'   lexicographic basename order. Intended for bounded smoke tests.
#' @param self_report_n_max Maximum number of self-report rows to read.
#' @param resume Whether to reuse compatible existing outputs.
#' @param export_type_priority Export-type priority for duplicate upload
#'   candidates.
#' @param dry_run If `TRUE`, write only diagnostics/project manifest and
#'   workflow configuration output.
#' @param type,tz,encoding Existing parser controls passed to
#'   [read_appusage_batch()].
#' @param strict Whether to stop after writing diagnostic reports for failures.
#' @param overwrite Whether to overwrite the selected output study folder.
#' @param progress Whether delegated batch functions should report progress.
#' @param parallel,n_cores Parallel controls. Defaults are serial.
#' @param run_second_level Whether to run second-level processing.
#' @param run_qc Whether to run QC metadata after second-level processing.
#' @param diagnostic_verbosity Console diagnostic behavior for failures:
#'   `"summary"` prints source context and report paths, `"full"` prints the
#'   full issue report, and `"none"` suppresses diagnostic console output.
#' @param ... Additional arguments passed to [write_second_level_batch()].
#'
#' @return An `appusage_project_workflow` list with compact summaries and paths.
#' @export
run_appusage_project_workflow <- function(project_dir = NULL, output_root,
                                          raw_data_root = NULL,
                                          project_id = NULL,
                                          project_name = NULL,
                                          self_report_file = NULL,
                                          sequence_col = "\u5e8f\u53f7",
                                          upload_col = NULL,
                                          submit_time_col = NULL,
                                          max_files = Inf,
                                          self_report_n_max = Inf,
                                          resume = TRUE,
                                          export_type_priority = c("line", "meta", "day", "app"),
                                          dry_run = FALSE,
                                          type = "auto",
                                          tz = "Asia/Shanghai",
                                          encoding = "auto",
                                          strict = FALSE,
                                          overwrite = FALSE,
                                          progress = TRUE,
                                          parallel = FALSE,
                                          n_cores = 1,
                                          run_second_level = TRUE,
                                          run_qc = TRUE,
                                          diagnostic_verbosity = c("summary", "full", "none"),
                                          ...) {
  diagnostic_verbosity <- match.arg(diagnostic_verbosity)
  second_level_options <- list(...)
  resolved <- appusage_resolve_project_workflow_inputs(
    project_dir = project_dir,
    raw_data_root = raw_data_root,
    project_id = project_id,
    project_name = project_name,
    self_report_file = self_report_file
  )
  project_dir <- resolved$project_dir
  self_report_file <- resolved$self_report_file
  project_name <- resolved$project_name
  project_id <- resolved$project_id

  project <- appusage_project_output_info(
    project_dir = project_dir,
    output_root = output_root,
    project_name = project_name,
    project_id = project_id,
    output_style = resolved$output_style
  )
  if (isTRUE(overwrite) && dir.exists(project$project_root)) {
    appusage_project_progress(progress, "overwrite=TRUE: removing existing output study folder")
    unlink(project$project_root, recursive = TRUE, force = TRUE)
  }

  manifest <- build_appusage_project_manifest(project_dir, self_report_file)
  manifest <- appusage_limit_manifest_files(manifest, max_files = max_files)
  n_appusage <- sum(manifest$is_txt %in% TRUE, na.rm = TRUE)
  n_survey <- appusage_count_self_report_rows(self_report_file, self_report_n_max)
  console_start_time <- Sys.time()
  appusage_console_project_start(
    progress = progress,
    project = project,
    n_appusage = n_appusage,
    n_survey = n_survey,
    start_time = console_start_time
  )
  config <- appusage_build_workflow_configuration(
    raw_data_root = raw_data_root,
    resolved_project_dir = project_dir,
    resolved_self_report_file = self_report_file,
    project = project,
    output_root = output_root,
    sequence_col = sequence_col,
    upload_col = upload_col,
    submit_time_col = submit_time_col,
    max_files = max_files,
    self_report_n_max = self_report_n_max,
    export_type_priority = export_type_priority,
    resume = resume,
    overwrite = overwrite,
    first_level_options = list(
      type = type,
      input = "file",
      tz = tz,
      encoding = encoding,
      strict = strict,
      progress = progress,
      parallel = parallel,
      n_cores = n_cores
    ),
    second_level_options = second_level_options,
    qc_options = list(run_qc = run_qc),
    category_options = list()
  )
  if (isTRUE(resume) && !isTRUE(overwrite)) {
    appusage_rebuild_first_level_summary_if_needed(
      project_dir = project$project_root,
      manifest = manifest,
      strict = FALSE
    )
  }
  resume_state <- appusage_prepare_workflow_resume(project, config, resume, overwrite)

  if (isTRUE(dry_run)) {
    diagnostics <- appusage_prepare_diagnostics(project$project_root)
    manifest_file <- appusage_write_project_manifest(manifest, diagnostics$diagnostics_dir)
    configuration_file <- appusage_write_workflow_configuration(config)
    flow <- appusage_console_sample_size_flow(
      first = NULL,
      second = NULL,
      qc = NULL,
      matched = NULL,
      n_appusage = n_appusage,
      n_survey = n_survey
    )
    appusage_console_project_end(
      progress = progress,
      project = project,
      n_appusage = n_appusage,
      n_survey = n_survey,
      start_time = console_start_time,
      end_time = Sys.time(),
      flow = flow
    )
    out <- list(
      project_dir = project$project_root,
      manifest = manifest,
      manifest_file = manifest_file,
      configuration_file = configuration_file,
      diagnostics_dir = diagnostics$diagnostics_dir,
      first_level = NULL,
      second_level = NULL,
      qc = NULL,
      matched_self_report = NULL,
      matched_self_report_file = NA_character_,
      match_diagnostics = NULL,
      sample_size_flow = flow,
      dry_run = TRUE
    )
    class(out) <- c("appusage_project_workflow", "list")
    return(out)
  }

  txt_files <- manifest$source_file[manifest$is_txt %in% TRUE]
  if (length(txt_files) == 0) {
    cli::cli_abort("No `.txt` files were found in `project_dir`.")
  }

  first <- NULL
  second <- NULL
  qc <- NULL
  resumed <- FALSE
  if (isTRUE(resume_state$use_existing_second_level)) {
    resumed <- TRUE
    diagnostics <- appusage_prepare_diagnostics(project$project_root)
    manifest_file <- appusage_write_project_manifest(manifest, diagnostics$diagnostics_dir)
    first <- appusage_read_summary_csv(file.path(project$project_root, "analytic_summary_table_proclevel-1.csv"))
    second <- appusage_read_summary_csv(file.path(project$project_root, "analytic_summary_table_proclevel-2.csv"))
    qc <- if (isTRUE(run_qc)) second else NULL
  } else {
    if (isTRUE(resume_state$use_existing_first_level)) {
      resumed <- TRUE
      first <- appusage_read_summary_csv(
        file.path(project$project_root, "analytic_summary_table_proclevel-1.csv")
      )
      if (nrow(first) == 0) {
        cli::cli_abort("Existing first-level summary is empty. Use `overwrite = TRUE` to recreate the selected output study folder.")
      }
      config$output_study_dir <- project$project_root
      diagnostics <- appusage_prepare_diagnostics(project$project_root)
      manifest_file <- appusage_write_project_manifest(manifest, diagnostics$diagnostics_dir)
      appusage_console_emit_stage_summary(
        progress = progress,
        summary = first,
        stage_label = "first-level",
        total = n_appusage,
        project_root = project$project_root,
        status_col = "status"
      )
      appusage_abort_if_strict_failures(first, strict, "first_level")
    } else {
      first <- read_appusage_batch(
        txt_files,
        output_dir = output_root,
        project_name = project$output_project_name,
        project_id = project$output_project_id,
        type = type,
        input = "file",
        tz = tz,
        encoding = encoding,
        strict = FALSE,
        overwrite = overwrite,
        progress = FALSE,
        parallel = parallel,
        n_cores = n_cores,
        resume = resume
      )
      project$project_root <- unique(stats::na.omit(first$project_root))[[1]]
      config$output_study_dir <- project$project_root
      diagnostics <- appusage_prepare_diagnostics(project$project_root)
      manifest_file <- appusage_write_project_manifest(manifest, diagnostics$diagnostics_dir)
      first <- appusage_attach_diagnostics(
        summary = first,
        manifest = manifest,
        stage = "first_level",
        project = project,
        diagnostics = diagnostics
        ,
        diagnostic_verbosity = diagnostic_verbosity
      )
      utils::write.csv(first,
        file.path(project$project_root, "analytic_summary_table_proclevel-1.csv"),
        row.names = FALSE,
        na = ""
      )
      appusage_console_emit_stage_summary(
        progress = progress,
        summary = first,
        stage_label = "first-level",
        total = n_appusage,
        project_root = project$project_root,
        status_col = "status"
      )
      appusage_abort_if_strict_failures(first, strict, "first_level")
    }

    if (isTRUE(run_second_level)) {
      second <- write_second_level_batch(first,
        overwrite = overwrite,
        resume = resume,
        progress = FALSE,
        parallel = parallel,
        n_cores = n_cores,
        ...
      )
      second <- appusage_attach_diagnostics(
        summary = second,
        manifest = manifest,
        stage = "second_level",
        project = project,
        diagnostics = diagnostics,
        source_summary = first,
        diagnostic_verbosity = diagnostic_verbosity
      )
      utils::write.csv(second,
        file.path(project$project_root, "analytic_summary_table_proclevel-2.csv"),
        row.names = FALSE,
        na = ""
      )
      appusage_console_emit_stage_summary(
        progress = progress,
        summary = second,
        stage_label = "second-level",
        total = n_appusage,
        project_root = project$project_root,
        status_col = "status"
      )
      appusage_abort_if_strict_failures(second, strict, "second_level")
    }

    if (isTRUE(run_qc) && !is.null(second)) {
      qc <- if (appusage_second_summary_has_inline_qc(second)) {
        second
      } else {
        write_qc_metadata_batch(project$project_root,
          strict = FALSE,
          progress = FALSE,
          overwrite = TRUE
        )
      }
      qc <- appusage_attach_diagnostics(
        summary = qc,
        manifest = manifest,
        stage = "qc",
        project = project,
        diagnostics = diagnostics,
        source_summary = first,
        diagnostic_verbosity = diagnostic_verbosity
      )
      qc <- appusage_merge_qc_with_second_level_skips(qc, second)
      utils::write.csv(qc,
        file.path(project$project_root, "analytic_summary_table_proclevel-2.csv"),
        row.names = FALSE,
        na = ""
      )
      appusage_console_emit_stage_summary(
        progress = progress,
        summary = qc,
        stage_label = "QC-daily-qc-v1",
        total = n_appusage,
        project_root = project$project_root,
        status_col = "qc_status"
      )
      appusage_abort_if_strict_failures(qc, strict, "qc")
    }
  }

  configuration_file <- appusage_write_workflow_configuration(config)
  match_result <- appusage_maybe_match_self_report(
    self_report_file = self_report_file,
    manifest = manifest,
    project = project,
    first = first,
    second = second,
    sequence_col = sequence_col,
    upload_col = upload_col,
    submit_time_col = submit_time_col,
    self_report_n_max = self_report_n_max,
    export_type_priority = export_type_priority
  )
  if (is_present_string(match_result$matched_self_report_file)) {
    appusage_console_matching_stage(
      progress = progress,
      matched = match_result$matched_self_report,
      n_survey = n_survey
    )
  }
  flow <- appusage_console_sample_size_flow(
    first = first,
    second = second,
    qc = qc,
    matched = match_result$matched_self_report,
    n_appusage = n_appusage,
    n_survey = n_survey
  )
  appusage_console_project_end(
    progress = progress,
    project = project,
    n_appusage = n_appusage,
    n_survey = n_survey,
    start_time = console_start_time,
    end_time = Sys.time(),
    flow = flow
  )

  out <- list(
    project_dir = project$project_root,
    manifest = manifest,
    manifest_file = manifest_file,
    configuration_file = configuration_file,
    diagnostics_dir = diagnostics$diagnostics_dir,
    first_level = first,
    second_level = second,
    qc = qc,
    matched_self_report = match_result$matched_self_report,
    matched_self_report_file = match_result$matched_self_report_file,
    match_diagnostics = match_result$diagnostics,
    sample_size_flow = flow,
    resumed = resumed,
    dry_run = FALSE
  )
  class(out) <- c("appusage_project_workflow", "list")
  out
}

#' Diagnose an APP Usage processing error
#'
#' @param error Condition object or message.
#' @param source_file Optional source file path.
#' @param stage Processing stage.
#' @param context Optional named list or one-row data frame of source context.
#'
#' @return A structured `appusage_error_context` list.
#' @export
diagnose_appusage_error <- function(error, source_file = NULL, stage = NULL,
                                    context = NULL) {
  context <- appusage_context_list(context)
  source_file <- source_file %||% appusage_context_value(context, "source_file")
  file_info <- appusage_file_context(source_file)
  filename <- if (is_present_string(source_file)) {
    parse_wenjuanxing_upload_filename(source_file)
  } else {
    list()
  }
  is_condition <- inherits(error, "condition")
  message <- if (is_condition) conditionMessage(error) else as.character(error)
  context_error_class <- appusage_context_value(context, "error_class")
  call <- if (is_condition && !is.null(conditionCall(error))) {
    deparse_one_call(conditionCall(error))
  } else {
    appusage_context_value(context, "error_call")
  }
  function_name <- appusage_context_value(context, "function_name")
  code_function <- appusage_function_from_call(call) %||% function_name
  code_location <- appusage_function_location(code_function)

  out <- list(
    project_name = appusage_context_value(context, "project_name"),
    project_id = appusage_context_value(context, "project_id"),
    batch_index = appusage_context_value(context, "index"),
    stage = stage %||% appusage_context_value(context, "stage"),
    module = appusage_context_value(context, "module", "appusageR"),
    function_name = function_name,
    code_function = code_function,
    code_location = code_location,
    source_file = file_info$source_file,
    source_basename = file_info$source_basename,
    file_size = file_info$file_size,
    extension = file_info$extension,
    wenjuanxing_sequence_id = appusage_context_value(
      context,
      "wenjuanxing_sequence_id",
      filename$wenjuanxing_sequence_id %||% NA_integer_
    ),
    native_export_type = appusage_context_value(
      context,
      "filename_export_type",
      filename$native_export_type_from_filename %||% NA_character_
    ),
    native_export_created_at = appusage_context_value(
      context,
      "native_export_created_at",
      filename$native_export_created_at %||% NA_character_
    ),
    content_detected_type = appusage_context_value(context, "detected_type"),
    condition_class = if (is_present_string(context_error_class)) {
      context_error_class
    } else if (is_condition) {
      paste(class(error), collapse = ",")
    } else {
      appusage_context_value(context, "error_class", "character")
    },
    condition_message = message,
    condition_call = call,
    traceback = appusage_context_value(context, "traceback"),
    parser_diagnostics = appusage_context_value(context, "parser_diagnostics"),
    raw_line_number = appusage_context_value(context, "raw_line_number"),
    raw_line_window = appusage_context_value(
      context,
      "raw_line_window",
      appusage_source_excerpt(source_file)
    )
  )
  class(out) <- c("appusage_error_context", "list")
  out
}

#' Format an APP Usage issue report
#'
#' Formats data/source context before code traceback so failed source records
#' can be located before reading the R stack.
#'
#' @param error_context A context returned by `diagnose_appusage_error()`.
#'
#' @return A markdown/plain-text issue report string.
#' @export
format_appusage_issue_report <- function(error_context) {
  x <- appusage_context_list(error_context)
  data_fields <- c(
    "project_name", "project_id", "batch_index", "stage",
    "source_file", "source_basename", "file_size", "extension",
    "wenjuanxing_sequence_id", "native_export_type",
    "native_export_created_at", "content_detected_type",
    "raw_line_number"
  )
  code_fields <- c(
    "module", "function_name", "code_function", "code_location",
    "condition_class", "condition_message",
    "condition_call"
  )
  data_lines <- appusage_format_issue_fields(x, data_fields)
  code_lines <- appusage_format_issue_fields(x, code_fields)
  traceback <- appusage_context_value(x, "traceback")
  if (!is_present_string(traceback)) {
    traceback <- "No traceback was captured."
  }
  paste(
    "# appusageR diagnostic report",
    "",
    "## Source data context",
    paste(data_lines, collapse = "\n"),
    "",
    "## Error context",
    paste(code_lines, collapse = "\n"),
    "",
    "## Raw source excerpt",
    appusage_context_value(x, "raw_line_window", "No raw source excerpt captured."),
    "",
    "## Traceback",
    "```r",
    traceback,
    "```",
    sep = "\n"
  )
}

#' Preflight Wenjuanxing/self-report matching columns
#'
#' Checks that requested self-report columns exist and summarizes missing and
#' duplicate sequence/upload values. This does not perform final APP
#' Usage/self-report matching.
#'
#' @param self_report Data frame or Excel file path.
#' @param sequence_col Wenjuanxing sequence column name.
#' @param upload_col Wenjuanxing upload/file column name.
#' @param participant_id_col Optional study participant ID column name.
#' @param sheet Excel sheet passed to [readxl::read_excel()] when `self_report`
#'   is a path.
#' @param ... Additional arguments passed to [readxl::read_excel()].
#'
#' @return A one-row tibble with preflight diagnostics.
#' @export
preflight_self_report_matching <- function(self_report, sequence_col,
                                           upload_col,
                                           participant_id_col = NULL,
                                           sheet = 1, ...) {
  data <- appusage_read_self_report_preflight(self_report,
    sheet = sheet,
    ...
  )
  requested <- c(sequence_col, upload_col, participant_id_col)
  requested <- requested[!is.na(requested) & nzchar(requested)]
  missing_cols <- setdiff(requested, names(data))
  status <- if (length(missing_cols) > 0) "error" else "success"

  sequence <- if (sequence_col %in% names(data)) data[[sequence_col]] else NULL
  upload <- if (upload_col %in% names(data)) data[[upload_col]] else NULL
  participant <- if (!is.null(participant_id_col) &&
    participant_id_col %in% names(data)) {
    data[[participant_id_col]]
  } else {
    NULL
  }

  tibble::tibble(
    status = status,
    n_rows = nrow(data),
    sequence_col = sequence_col,
    upload_col = upload_col,
    participant_id_col = participant_id_col %||% NA_character_,
    missing_required_columns = paste(missing_cols, collapse = ";"),
    n_missing_sequence = appusage_count_missing(sequence),
    n_duplicate_sequence = appusage_count_duplicates(sequence),
    n_missing_upload = appusage_count_missing(upload),
    n_duplicate_upload = appusage_count_duplicates(upload),
    n_missing_participant_id = appusage_count_missing(participant),
    n_duplicate_participant_id = appusage_count_duplicates(participant)
  )
}

appusage_is_project_folder_name <- function(x) {
  grepl("_ProjectID-[^_]+$", x)
}

appusage_parse_project_folder <- function(path) {
  base <- basename(path)
  if (!appusage_is_project_folder_name(base)) {
    return(list(
      project_name = base,
      study_id = NA_character_,
      project_id = NA_character_,
      project_label = base,
      parse_status = "failed"
    ))
  }
  project_id <- sub("^.*_ProjectID-([^_]+)$", "\\1", base)
  label <- sub("_ProjectID-[^_]+$", "", base)
  if (grepl("^ProjectName-", label)) {
    project_name <- sub("^ProjectName-", "", label)
    study_id <- project_name
  } else {
    label_match <- regexec("^(.*)-([^-]+)$", label)
    label_parts <- regmatches(label, label_match)[[1]]
    if (length(label_parts) >= 3) {
      project_name <- label_parts[[3]]
      study_id <- label_parts[[3]]
    } else {
      project_name <- label
      study_id <- NA_character_
    }
  }
  list(
    project_name = project_name,
    study_id = study_id,
    project_id = project_id,
    project_label = label,
    parse_status = "success"
  )
}

appusage_empty_project_scan <- function() {
  tibble::tibble(
    project_name = character(),
    study_id = character(),
    project_id = character(),
    project_label = character(),
    project_dir = character(),
    excel_count = integer(),
    excel_path = character(),
    excel_paths = character(),
    n_files = integer(),
    n_txt_files = integer(),
    n_zero_byte_txt = integer(),
    n_non_txt_files = integer()
  )
}

appusage_empty_project_manifest <- function(project, project_dir,
                                            self_report_file) {
  tibble::tibble(
    project_name = character(),
    study_id = character(),
    project_id = character(),
    project_label = character(),
    project_dir = character(),
    self_report_file = character(),
    index = integer(),
    source_file = character(),
    source_basename = character(),
    extension = character(),
    is_txt = logical(),
    is_zero_byte = logical(),
    is_zero_byte_txt = logical(),
    file_size = numeric(),
    modified_at = character(),
    wenjuanxing_sequence_id = integer(),
    candidate_participant_id = character(),
    filename_parse_status = character(),
    filename_parse_warning = character(),
    native_export_file_name = character(),
    uploaded_file_name = character(),
    filename_export_type = character(),
    native_export_created_at = character(),
    project_n_files = integer(),
    project_n_txt_files = integer(),
    project_n_zero_byte_txt = integer(),
    project_n_non_txt_files = integer()
  )
}

appusage_normalize_optional_path <- function(path) {
  if (is.null(path) || length(path) == 0 || is.na(path)) {
    return(NA_character_)
  }
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

appusage_project_output_info <- function(project_dir, output_root,
                                         project_name, project_id,
                                         output_style = c("project", "study")) {
  output_style <- match.arg(output_style)
  if (length(output_root) != 1 || is.na(output_root)) {
    cli::cli_abort("`output_root` must be a single directory path.")
  }
  source_project <- appusage_parse_project_folder(project_dir)
  output_project_name <- project_name %||% source_project$project_name
  output_project_id <- project_id %||% source_project$project_id
  if (!is_present_string(output_project_name)) {
    output_project_name <- basename(project_dir)
  }
  if (!is_present_string(output_project_id)) {
    output_project_id <- generate_project_id()
  }
  name_prefix <- if (identical(output_style, "study")) "Study-" else "ProjectName-"
  output_project_name <- if (grepl("^(ProjectName|Study)-", output_project_name)) {
    output_project_name
  } else {
    paste0(name_prefix, output_project_name)
  }
  output_project_id <- if (grepl("^ProjectID-", output_project_id)) {
    output_project_id
  } else {
    paste0("ProjectID-", output_project_id)
  }
  project_root <- file.path(
    output_root,
    paste0(
      sanitize_entity_value(output_project_name),
      "_",
      sanitize_entity_value(output_project_id)
    )
  )
  list(
    project_name = appusage_plain_project_name(output_project_name),
    study_id = appusage_plain_project_name(output_project_name),
    project_id = appusage_plain_project_id(output_project_id),
    output_project_name = output_project_name,
    output_project_id = output_project_id,
    project_root = normalizePath(project_root, winslash = "/", mustWork = FALSE)
  )
}

appusage_plain_project_name <- function(x) {
  x <- as.character(x)
  x <- sub("^ProjectName-", "", x)
  sub("^Study-", "", x)
}

appusage_plain_project_id <- function(x) {
  sub("^ProjectID-", "", as.character(x))
}

appusage_resolve_project_workflow_inputs <- function(project_dir, raw_data_root,
                                                     project_id, project_name,
                                                     self_report_file) {
  root_mode <- is.null(project_dir) && is_present_string(raw_data_root)
  if (is.null(project_dir) && !root_mode) {
    cli::cli_abort("Provide either `project_dir` or `raw_data_root` with `project_id`.")
  }
  if (root_mode) {
    if (!is_present_string(project_id)) {
      cli::cli_abort("`project_id` is required when `raw_data_root` is used.")
    }
    requested_project_id <- appusage_plain_project_id(project_id)
    resolved <- appusage_resolve_project_by_id(raw_data_root, requested_project_id)
    if (is_present_string(project_name) &&
      !identical(as.character(resolved$project_name), as.character(project_name))) {
      cli::cli_abort("`project_name` ({.val {project_name}}) does not match discovered project folder ({.val {resolved$project_name}}).")
    }
    if (is.null(self_report_file)) {
      self_report_file <- resolved$excel_path
    }
    return(list(
      project_dir = resolved$project_dir,
      raw_data_root = normalizePath(raw_data_root, winslash = "/", mustWork = FALSE),
      self_report_file = self_report_file,
      project_id = requested_project_id,
      project_name = project_name %||% resolved$project_name,
      output_style = "study",
      root_mode = TRUE
    ))
  }

  if (!dir.exists(project_dir)) {
    cli::cli_abort("`project_dir` must be an existing directory.")
  }
  parsed <- appusage_parse_project_folder(project_dir)
  if (is_present_string(raw_data_root) &&
    is_present_string(project_id) &&
    is_present_string(parsed$project_id) &&
    !identical(appusage_plain_project_id(project_id), parsed$project_id)) {
    cli::cli_abort("`project_id` ({.val {project_id}}) does not match `project_dir` ({.val {parsed$project_id}}).")
  }
  list(
    project_dir = normalizePath(project_dir, winslash = "/", mustWork = FALSE),
    raw_data_root = appusage_normalize_optional_path(raw_data_root),
    self_report_file = self_report_file,
    project_id = appusage_plain_project_id(project_id %||% parsed$project_id),
    project_name = project_name %||% parsed$project_name,
    output_style = "project",
    root_mode = FALSE
  )
}

appusage_resolve_project_by_id <- function(raw_data_root, project_id,
                                           excel_pattern = "WJXraw") {
  if (length(raw_data_root) != 1 || is.na(raw_data_root) ||
    !dir.exists(raw_data_root)) {
    cli::cli_abort("`raw_data_root` must be an existing directory.")
  }
  requested_project_id <- appusage_plain_project_id(project_id)
  raw_data_root <- normalizePath(raw_data_root, winslash = "/", mustWork = FALSE)
  children <- list.files(raw_data_root,
    full.names = TRUE,
    recursive = FALSE,
    all.files = FALSE,
    no.. = TRUE
  )
  info <- if (length(children) > 0) file.info(children) else data.frame()
  dirs <- children[!is.na(info$isdir) & info$isdir]
  dir_meta <- lapply(dirs, appusage_parse_project_folder)
  project_hits <- dirs[vapply(dir_meta, function(x) {
    identical(x$project_id, requested_project_id)
  }, logical(1))]
  if (length(project_hits) != 1) {
    cli::cli_abort("Expected exactly one project folder for ProjectID {.val {requested_project_id}}; found {length(project_hits)}.")
  }
  project_meta <- appusage_parse_project_folder(project_hits[[1]])

  files <- children[!is.na(info$isdir) & !info$isdir]
  excel_files <- files[grepl("[.](xlsx|xls)$", basename(files), ignore.case = TRUE)]
  excel_files <- excel_files[!startsWith(basename(excel_files), "~$")]
  if (is_present_string(excel_pattern)) {
    excel_files <- excel_files[grepl(excel_pattern, basename(excel_files), ignore.case = TRUE)]
  }
  excel_files <- appusage_filter_sequence_self_report_excels(excel_files)
  excel_files <- excel_files[grepl(
    appusage_project_id_excel_pattern(requested_project_id),
    basename(excel_files),
    ignore.case = TRUE
  )]
  if (length(excel_files) != 1) {
    cli::cli_abort("Expected exactly one Wenjuanxing Excel file for ProjectID {.val {requested_project_id}}; found {length(excel_files)}.")
  }
  list(
    project_dir = normalizePath(project_hits[[1]], winslash = "/", mustWork = FALSE),
    project_name = project_meta$project_name,
    study_id = project_meta$study_id,
    project_id = project_meta$project_id,
    excel_path = normalizePath(excel_files[[1]], winslash = "/", mustWork = FALSE)
  )
}

appusage_filter_sequence_self_report_excels <- function(excel_files) {
  if (length(excel_files) <= 1) {
    return(excel_files)
  }
  base <- basename(excel_files)
  sequence_hits <- grepl("\u6309\u5e8f\u53f7", base) &
    !grepl("\u6309\u6587\u672c", base)
  if (sum(sequence_hits) > 0) {
    return(excel_files[sequence_hits])
  }
  excel_files
}

appusage_limit_manifest_files <- function(manifest, max_files = Inf) {
  if (!is.finite(max_files)) {
    return(manifest)
  }
  max_files <- as.integer(max_files)
  if (is.na(max_files) || max_files < 0) {
    cli::cli_abort("`max_files` must be non-negative or `Inf`.")
  }
  txt <- which(manifest$is_txt %in% TRUE)
  txt <- txt[order(manifest$source_basename[txt])]
  keep_txt <- utils::head(txt, max_files)
  keep <- sort(c(keep_txt, which(!manifest$is_txt %in% TRUE)))
  manifest[keep, , drop = FALSE]
}

appusage_build_workflow_configuration <- function(raw_data_root,
                                                  resolved_project_dir,
                                                  resolved_self_report_file,
                                                  project,
                                                  output_root,
                                                  sequence_col,
                                                  upload_col,
                                                  submit_time_col,
                                                  max_files,
                                                  self_report_n_max,
                                                  export_type_priority,
                                                  resume,
                                                  overwrite,
                                                  first_level_options,
                                                  second_level_options,
                                                  qc_options,
                                                  category_options) {
  now <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  list(
    package_version = as.character(utils::packageVersion("appusageR")),
    output_schema_version = "0.3.0",
    created_at = now,
    latest_run_at = now,
    raw_data_root = appusage_normalize_optional_path(raw_data_root),
    resolved_project_dir = normalizePath(resolved_project_dir, winslash = "/", mustWork = FALSE),
    resolved_self_report_file = appusage_normalize_optional_path(resolved_self_report_file),
    project_id = project$project_id,
    project_name = project$project_name,
    output_root = normalizePath(output_root, winslash = "/", mustWork = FALSE),
    output_study_dir = project$project_root,
    sequence_col = sequence_col,
    upload_col = upload_col %||% NA_character_,
    submit_time_col = submit_time_col %||% NA_character_,
    max_files = max_files,
    self_report_n_max = self_report_n_max,
    export_type_priority = export_type_priority,
    resume = isTRUE(resume),
    overwrite = isTRUE(overwrite),
    first_level_options = first_level_options,
    second_level_options = second_level_options,
    qc_options = qc_options,
    category_options = category_options
  )
}

appusage_prepare_workflow_resume <- function(project, config, resume, overwrite) {
  config_file <- file.path(project$project_root, "workflow_configuration.rds")
  first_summary_file <- file.path(project$project_root, "analytic_summary_table_proclevel-1.csv")
  second_summary_file <- file.path(project$project_root, "analytic_summary_table_proclevel-2.csv")
  if (isTRUE(overwrite) || !dir.exists(project$project_root)) {
    return(list(
      existing_config = NULL,
      use_existing_first_level = FALSE,
      use_existing_second_level = FALSE
    ))
  }
  if (!file.exists(config_file)) {
    return(list(
      existing_config = NULL,
      use_existing_first_level = isTRUE(resume) &&
        appusage_first_level_summary_complete(first_summary_file),
      use_existing_second_level = FALSE
    ))
  }
  existing <- readRDS(config_file)
  incompatible <- appusage_workflow_config_differences(existing, config)
  if (length(incompatible) > 0) {
    cli::cli_abort("Existing workflow configuration is incompatible for field(s): {paste(incompatible, collapse = ', ')}. Use `overwrite = TRUE` to recreate the selected output study folder.")
  }
  use_existing <- isTRUE(resume) &&
    length(list.files(file.path(project$project_root, "proclevel-2"),
      pattern = "_proc-2[.]rda$",
      full.names = TRUE
    )) > 0 &&
    file.exists(second_summary_file)
  use_existing_first <- isTRUE(resume) &&
    !isTRUE(use_existing) &&
    appusage_first_level_summary_complete(first_summary_file)
  if (dir.exists(project$project_root) && !isTRUE(resume) && !isTRUE(use_existing)) {
    cli::cli_abort("Project output folder already exists: {.path {project$project_root}}")
  }
  list(
    existing_config = existing,
    use_existing_first_level = use_existing_first,
    use_existing_second_level = use_existing
  )
}

appusage_workflow_config_differences <- function(existing, current) {
  fields <- c(
    "raw_data_root", "resolved_project_dir", "resolved_self_report_file",
    "project_id", "project_name", "output_root", "sequence_col",
    "upload_col", "submit_time_col", "max_files", "self_report_n_max"
  )
  fields[vapply(fields, function(field) {
    old <- existing[[field]]
    new <- current[[field]]
    !identical(as.character(old), as.character(new))
  }, logical(1))]
}

appusage_write_workflow_configuration <- function(config) {
  dir.create(config$output_study_dir, recursive = TRUE, showWarnings = FALSE)
  config$latest_run_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  config_file <- file.path(config$output_study_dir, "workflow_configuration.rds")
  saveRDS(config, config_file)
  normalizePath(config_file, winslash = "/", mustWork = FALSE)
}

appusage_read_summary_csv <- function(path) {
  if (!file.exists(path)) {
    return(tibble::tibble())
  }
  tibble::as_tibble(utils::read.csv(path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  ))
}

appusage_prepare_diagnostics <- function(project_root) {
  diagnostics_dir <- file.path(project_root, "diagnostics")
  error_dir <- file.path(diagnostics_dir, "error_reports")
  dir.create(error_dir, recursive = TRUE, showWarnings = FALSE)
  list(
    diagnostics_dir = normalizePath(diagnostics_dir, winslash = "/", mustWork = FALSE),
    error_dir = normalizePath(error_dir, winslash = "/", mustWork = FALSE)
  )
}

appusage_write_project_manifest <- function(manifest, diagnostics_dir) {
  dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
  manifest_file <- file.path(diagnostics_dir, "project_manifest.csv")
  utils::write.csv(manifest, manifest_file, row.names = FALSE, na = "")
  normalizePath(manifest_file, winslash = "/", mustWork = FALSE)
}

appusage_attach_diagnostics <- function(summary, manifest, stage, project,
                                        diagnostics,
                                        source_summary = NULL,
                                        diagnostic_verbosity = "summary") {
  if (is.null(summary) || nrow(summary) == 0) {
    return(summary)
  }
  summary$diagnostic_report <- NA_character_
  summary$diagnostic_json <- NA_character_
  failures <- appusage_failure_rows(summary, stage)
  for (i in failures) {
    context <- appusage_summary_context(
      summary = summary,
      index = i,
      manifest = manifest,
      stage = stage,
      project = project,
      source_summary = source_summary
    )
    error <- simpleError(context$error_message %||% paste(stage, "failed"))
    diag <- diagnose_appusage_error(error,
      source_file = context$source_file,
      stage = stage,
      context = context
    )
    paths <- appusage_write_error_report(diag, diagnostics$error_dir)
    appusage_emit_diagnostic(diag, paths, diagnostic_verbosity)
    summary$diagnostic_report[[i]] <- paths$md
    summary$diagnostic_json[[i]] <- paths$json
  }
  summary
}

appusage_project_id_excel_pattern <- function(project_id) {
  if (!is_present_string(project_id)) {
    return("a^")
  }
  id <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", project_id)
  paste0("ProjectID[-_]", id, "([_.-]|$)")
}

appusage_merge_qc_with_second_level_skips <- function(qc, second) {
  if (is.null(second) || nrow(second) == 0) {
    return(qc)
  }
  status_skipped <- if ("status" %in% names(second)) {
    second$status %in% "skipped"
  } else {
    rep(FALSE, nrow(second))
  }
  second_status_skipped <- if ("second_level_status" %in% names(second)) {
    second$second_level_status %in% "skipped"
  } else {
    rep(FALSE, nrow(second))
  }
  skipped <- second[status_skipped | second_status_skipped, , drop = FALSE]
  if (nrow(skipped) == 0) {
    return(qc)
  }
  if (is.null(qc) || nrow(qc) == 0) {
    return(skipped)
  }
  key_cols <- intersect(c("participant_id", "detected_type"), names(qc))
  if (all(key_cols %in% names(skipped))) {
    existing <- do.call(paste, c(qc[key_cols], sep = "\r"))
    skipped_key <- do.call(paste, c(skipped[key_cols], sep = "\r"))
    skipped <- skipped[!skipped_key %in% existing, , drop = FALSE]
  }
  if (nrow(skipped) == 0) {
    return(qc)
  }
  out <- bind_appusage_summary_rows(qc, skipped)
  if ("participant_id" %in% names(out) && "participant_id" %in% names(second)) {
    order_index <- match(as.character(out$participant_id), as.character(second$participant_id))
    out <- out[order(order_index, seq_len(nrow(out)), na.last = TRUE), , drop = FALSE]
  }
  tibble::as_tibble(out)
}

appusage_project_progress <- function(progress, text) {
  if (isTRUE(progress)) invisible(text)
  invisible(NULL)
}

appusage_console_timestamp <- function(time = Sys.time()) {
  format(as.POSIXct(time), "%Y-%m-%d %H:%M:%S")
}

appusage_console_hms <- function(seconds) {
  seconds <- max(0, as.integer(round(seconds)))
  h <- seconds %/% 3600
  m <- (seconds %% 3600) %/% 60
  s <- seconds %% 60
  sprintf("%02d:%02d:%02d", h, m, s)
}

appusage_console_emit <- function(progress, lines) {
  if (isTRUE(progress)) {
    message(paste(lines, collapse = "\n"))
  }
  invisible(lines)
}

appusage_console_project_start <- function(progress, project,
                                           n_appusage, n_survey,
                                           start_time = Sys.time()) {
  appusage_console_emit(progress, c(
    "============================================================",
    sprintf(
      "| Project ID: %s | Project Name: %s | N_appusage: %s | N_survey: %s |",
      project$project_id %||% NA_character_,
      project$project_name %||% NA_character_,
      n_appusage,
      n_survey
    ),
    sprintf(
      "| Project Preprocessing Start Time: %s |",
      appusage_console_timestamp(start_time)
    ),
    "============================================================"
  ))
}

appusage_console_project_end <- function(progress, project,
                                         n_appusage, n_survey,
                                         start_time, end_time,
                                         flow) {
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  lines <- c(
    "============================================================",
    sprintf(
      "| Project ID: %s | Project Name: %s | N_appusage: %s | N_survey: %s |",
      project$project_id %||% NA_character_,
      project$project_name %||% NA_character_,
      n_appusage,
      n_survey
    ),
    sprintf(
      "| Project Preprocessing End Time: %s |",
      appusage_console_timestamp(end_time)
    ),
    sprintf(
      "| Project Preprocessing Elapsed Time: %s |",
      appusage_console_hms(elapsed)
    ),
    "|-------------Sample Size Flow-------------|",
    appusage_console_percent_row("first-level", flow$first_level),
    appusage_console_percent_row("second-level", flow$second_level),
    appusage_console_percent_row("QC-daily-qc-v1", flow$qc_daily_qc_v1),
    appusage_console_percent_row("self-report matching", flow$self_report_matching),
    "============================================================"
  )
  appusage_console_emit(progress, lines)
}

appusage_console_file_stage <- function(progress, index, total, stage_label,
                                        file_label, status = c("success", "failed", "skipped"),
                                        diagnostic = NA_character_,
                                        time = Sys.time()) {
  status <- match.arg(status)
  status_text <- switch(status,
    success = "Done!",
    failed = "Failed!",
    skipped = "Skipped!"
  )
  if (identical(status, "failed") && is_present_string(diagnostic)) {
    status_text <- paste0(status_text, " diagnostic: ", diagnostic)
  }
  line <- sprintf(
    "[%s] %s/%s | %s | %s | %s",
    appusage_console_timestamp(time),
    index,
    total,
    stage_label,
    file_label,
    status_text
  )
  appusage_console_emit(progress, line)
}

appusage_console_matching_stage <- function(progress, matched, n_survey,
                                           time = Sys.time()) {
  status <- if (is.null(matched) || nrow(matched) == 0) {
    character()
  } else {
    as.character(matched$moSens_match_status)
  }
  n_matched <- sum(status == "matched", na.rm = TRUE)
  n_unmatched <- max(0L, as.integer(n_survey) - n_matched)
  line <- sprintf(
    "[%s] self-report matching | N_survey: %s | Matched: %s | Unmatched: %s | Done!",
    appusage_console_timestamp(time),
    n_survey,
    n_matched,
    n_unmatched
  )
  appusage_console_emit(progress, line)
}

appusage_console_emit_stage_summary <- function(progress, summary,
                                                stage_label, total,
                                                project_root,
                                                status_col = "status") {
  if (!isTRUE(progress) || is.null(summary) || nrow(summary) == 0) {
    return(invisible(NULL))
  }
  for (i in seq_len(nrow(summary))) {
    row <- summary[i, , drop = FALSE]
    status <- appusage_console_row_status(row, status_col)
    diagnostic <- appusage_console_diagnostic_path(row, project_root)
    appusage_console_file_stage(
      progress = progress,
      index = appusage_console_row_index(row, i),
      total = total,
      stage_label = stage_label,
      file_label = appusage_console_file_label(row),
      status = status,
      diagnostic = diagnostic
    )
  }
  invisible(NULL)
}

appusage_console_row_status <- function(row, status_col) {
  value <- if (status_col %in% names(row)) {
    as.character(row[[status_col]][[1]])
  } else if ("status" %in% names(row)) {
    as.character(row$status[[1]])
  } else {
    NA_character_
  }
  if (identical(value, "success")) {
    "success"
  } else if (identical(value, "error")) {
    "failed"
  } else {
    "skipped"
  }
}

appusage_console_diagnostic_path <- function(row, project_root) {
  path <- NA_character_
  for (col in c("diagnostic_report", "diagnostic_json")) {
    if (col %in% names(row) && is_present_string(row[[col]][[1]])) {
      path <- row[[col]][[1]]
      break
    }
  }
  if (!is_present_string(path)) {
    return(NA_character_)
  }
  appusage_relative_path(path, project_root)
}

appusage_console_row_index <- function(row, fallback) {
  if ("index" %in% names(row) && !is.na(row$index[[1]])) {
    as.integer(row$index[[1]])
  } else {
    as.integer(fallback)
  }
}

appusage_console_file_label <- function(row) {
  participant <- if ("participant_id" %in% names(row)) {
    as.character(row$participant_id[[1]])
  } else {
    NA_character_
  }
  export_type <- NA_character_
  for (col in c("detected_type", "filename_export_type", "native_export_type")) {
    if (col %in% names(row) && is_present_string(row[[col]][[1]])) {
      export_type <- as.character(row[[col]][[1]])
      break
    }
  }
  if (is_present_string(participant) && is_present_string(export_type)) {
    return(paste0(
      "sub-", sanitize_entity_value(participant),
      "_type-", sanitize_entity_value(export_type)
    ))
  }
  if ("source_basename" %in% names(row) && is_present_string(row$source_basename[[1]])) {
    return(sanitize_entity_value(tools::file_path_sans_ext(row$source_basename[[1]])))
  }
  if ("source_file" %in% names(row) && is_present_string(row$source_file[[1]])) {
    return(sanitize_entity_value(tools::file_path_sans_ext(basename(row$source_file[[1]]))))
  }
  "sub-unknown_type-unknown"
}

appusage_console_sample_size_flow <- function(first, second, qc, matched,
                                              n_appusage, n_survey) {
  list(
    first_level = appusage_console_processing_flow(first, n_appusage, "status"),
    second_level = appusage_console_processing_flow(second, n_appusage, "status"),
    qc_daily_qc_v1 = appusage_console_processing_flow(qc, n_appusage, "qc_status"),
    self_report_matching = appusage_console_matching_flow(matched, n_survey)
  )
}

appusage_console_processing_flow <- function(summary, total, status_col = "status") {
  total <- as.integer(total)
  if (is.null(summary) || nrow(summary) == 0 || total == 0) {
    counts <- c(Passed = 0L, Failed = 0L, Skipped = total)
    return(appusage_console_flow_record(counts, total))
  }
  values <- if (status_col %in% names(summary)) {
    as.character(summary[[status_col]])
  } else if ("status" %in% names(summary)) {
    as.character(summary$status)
  } else {
    rep(NA_character_, nrow(summary))
  }
  passed <- sum(values == "success", na.rm = TRUE)
  failed <- sum(values == "error", na.rm = TRUE)
  skipped <- max(0L, total - passed - failed)
  appusage_console_flow_record(c(Passed = passed, Failed = failed, Skipped = skipped), total)
}

appusage_console_matching_flow <- function(matched, total) {
  total <- as.integer(total)
  if (is.null(matched) || nrow(matched) == 0 || total == 0) {
    counts <- c(Matched = 0L, Unmatched = total)
    return(appusage_console_flow_record(counts, total))
  }
  status <- as.character(matched$moSens_match_status)
  matched_n <- sum(status == "matched", na.rm = TRUE)
  unmatched <- max(0L, total - matched_n)
  appusage_console_flow_record(c(Matched = matched_n, Unmatched = unmatched), total)
}

appusage_console_flow_record <- function(counts, total) {
  labels <- names(counts)
  counts <- as.integer(counts)
  names(counts) <- labels
  percents <- appusage_console_percent_values(counts, total)
  names(percents) <- labels
  list(
    counts = counts,
    total = as.integer(total),
    percents = percents
  )
}

appusage_console_percent_values <- function(counts, total, digits = 1) {
  counts <- as.numeric(counts)
  total <- as.numeric(total)
  if (length(counts) == 0) {
    return(numeric())
  }
  if (is.na(total) || total <= 0) {
    return(rep(0, length(counts)))
  }
  percents <- round(counts / total * 100, digits)
  if (length(percents) > 1) {
    percents[[length(percents)]] <- round(100 - sum(percents[-length(percents)]), digits)
  } else {
    percents[[1]] <- 100
  }
  percents
}

appusage_console_percent_row <- function(label, flow_record) {
  counts <- flow_record$counts
  percents <- flow_record$percents
  pieces <- vapply(seq_along(counts), function(i) {
    sprintf("%s: %s (%.1f%%)", names(counts)[[i]], counts[[i]], percents[[i]])
  }, character(1))
  sprintf(
    "| %s | %s | TOTAL: %s |",
    label,
    paste(pieces, collapse = " | "),
    flow_record$total
  )
}

appusage_count_self_report_rows <- function(self_report_file, n_max = Inf) {
  if (!is_present_string(self_report_file) || !file.exists(self_report_file)) {
    return(0L)
  }
  data <- tryCatch(
    appusage_read_self_report_rows(self_report_file, n_max),
    error = function(e) NULL
  )
  if (is.null(data)) {
    return(0L)
  }
  nrow(data)
}

appusage_failure_rows <- function(summary, stage) {
  if (identical(stage, "qc") && "qc_status" %in% names(summary)) {
    return(which(summary$qc_status == "error"))
  }
  if ("status" %in% names(summary)) {
    return(which(summary$status %in% c("error", "skipped") &
      !is.na(summary$error_message) &
      nzchar(summary$error_message)))
  }
  integer()
}

appusage_summary_context <- function(summary, index, manifest, stage, project,
                                     source_summary = NULL) {
  row <- as.list(summary[index, , drop = FALSE])
  row <- lapply(row, function(x) x[[1]])
  manifest_row <- appusage_manifest_match(row, manifest, source_summary)
  context <- c(
    list(
      project_name = project$project_name,
      project_id = project$project_id,
      stage = stage,
      module = "appusageR",
      function_name = appusage_stage_function(stage)
    ),
    as.list(manifest_row),
    row
  )
  context$source_file <- context$source_file %||% manifest_row$source_file
  context
}

appusage_manifest_match <- function(row, manifest, source_summary = NULL) {
  empty <- as.list(manifest[NA_integer_, , drop = FALSE])
  empty <- lapply(empty, function(x) NA)
  source_file <- row$source_file %||% NA_character_
  if (!is_present_string(source_file) && !is.null(source_summary) &&
    "index" %in% names(row) && "index" %in% names(source_summary)) {
    matched <- match(row$index, source_summary$index)
    if (!is.na(matched) && "source_file" %in% names(source_summary)) {
      source_file <- source_summary$source_file[[matched]]
    }
  }
  if (is_present_string(source_file)) {
    matched <- match(normalizePath(source_file,
      winslash = "/",
      mustWork = FALSE
    ), manifest$source_file)
    if (!is.na(matched)) {
      return(as.list(manifest[matched, , drop = FALSE]))
    }
  }
  if ("index" %in% names(row) && "index" %in% names(manifest)) {
    matched <- match(row$index, manifest$index)
    if (!is.na(matched)) {
      return(as.list(manifest[matched, , drop = FALSE]))
    }
  }
  if ("participant_id" %in% names(row) &&
    "candidate_participant_id" %in% names(manifest)) {
    matched <- match(as.character(row$participant_id), as.character(manifest$candidate_participant_id))
    if (!is.na(matched)) {
      return(as.list(manifest[matched, , drop = FALSE]))
    }
  }
  if ("wenjuanxing_sequence_id" %in% names(row) &&
    "wenjuanxing_sequence_id" %in% names(manifest)) {
    matched <- match(as.integer(row$wenjuanxing_sequence_id), as.integer(manifest$wenjuanxing_sequence_id))
    if (!is.na(matched)) {
      return(as.list(manifest[matched, , drop = FALSE]))
    }
  }
  empty
}

appusage_stage_function <- function(stage) {
  switch(stage,
    first_level = "read_appusage_batch",
    second_level = "write_second_level_batch",
    qc = "write_qc_metadata_batch",
    self_report_preflight = "preflight_self_report_matching",
    stage
  )
}

appusage_function_from_call <- function(call) {
  if (!is_present_string(call)) {
    return(NULL)
  }
  x <- trimws(strsplit(call, "\n", fixed = TRUE)[[1]][[1]])
  if (!grepl("^[[:alnum:]_.]+\\s*\\(", x)) {
    return(NULL)
  }
  sub("^([[:alnum:]_.]+)\\s*\\(.*$", "\\1", x)
}

appusage_function_location <- function(function_name) {
  if (!is_present_string(function_name)) {
    return(NA_character_)
  }
  srcref_location <- appusage_srcref_location(function_name)
  if (is_present_string(srcref_location)) {
    return(srcref_location)
  }
  source_location <- appusage_source_definition_location(function_name)
  if (is_present_string(source_location)) {
    return(source_location)
  }
  NA_character_
}

appusage_srcref_location <- function(function_name) {
  ns <- tryCatch(asNamespace("appusageR"), error = function(e) NULL)
  if (is.null(ns) || !exists(function_name, envir = ns, inherits = FALSE)) {
    return(NA_character_)
  }
  fn <- get(function_name, envir = ns, inherits = FALSE)
  ref <- attr(fn, "srcref", exact = TRUE)
  if (is.null(ref)) {
    return(NA_character_)
  }
  srcfile <- attr(ref, "srcfile", exact = TRUE)
  filename <- srcfile$filename %||% srcfile$wd %||% NA_character_
  if (!is_present_string(filename)) {
    return(NA_character_)
  }
  line <- as.integer(ref[[1]])
  paste0(normalizePath(filename, winslash = "/", mustWork = FALSE), ":", line)
}

appusage_source_definition_location <- function(function_name) {
  source_dirs <- unique(stats::na.omit(c(
    getOption("appusageR.source_dir"),
    Sys.getenv("APPUSAGER_SOURCE_DIR", unset = NA_character_),
    file.path(getwd(), "R"),
    file.path(dirname(getwd()), "sourcecode", "R")
  )))
  source_dirs <- source_dirs[dir.exists(source_dirs)]
  if (length(source_dirs) == 0) {
    return(NA_character_)
  }
  escaped <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", function_name)
  pattern <- paste0("^", escaped, "\\s*<-\\s*function\\b")
  for (source_dir in source_dirs) {
    files <- list.files(source_dir, pattern = "\\.[rR]$", full.names = TRUE)
    for (file in files) {
      lines <- readLines(file, warn = FALSE, encoding = "UTF-8")
      hit <- grep(pattern, lines)
      if (length(hit) > 0) {
        return(paste0(normalizePath(file, winslash = "/", mustWork = FALSE), ":", hit[[1]]))
      }
    }
  }
  NA_character_
}

appusage_write_error_report <- function(error_context, error_dir) {
  dir.create(error_dir, recursive = TRUE, showWarnings = FALSE)
  base <- paste(
    "source",
    error_context$batch_index %||% "unknown",
    "stage",
    error_context$stage %||% "unknown",
    sep = "-"
  )
  if (is_present_string(error_context$source_basename)) {
    base <- paste(
      tools::file_path_sans_ext(error_context$source_basename),
      "stage",
      error_context$stage %||% "unknown",
      sep = "-"
    )
  }
  base <- sanitize_entity_value(base)
  json_file <- file.path(error_dir, paste0(base, ".json"))
  md_file <- file.path(error_dir, paste0(base, ".md"))
  report <- format_appusage_issue_report(error_context)
  jsonlite::write_json(error_context,
    path = json_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  writeLines(report, md_file, useBytes = TRUE)
  list(
    json = normalizePath(json_file, winslash = "/", mustWork = FALSE),
    md = normalizePath(md_file, winslash = "/", mustWork = FALSE)
  )
}

appusage_emit_diagnostic <- function(error_context, paths,
                                     diagnostic_verbosity = "summary") {
  diagnostic_verbosity <- match.arg(diagnostic_verbosity, c("summary", "full", "none"))
  if (identical(diagnostic_verbosity, "none")) {
    return(invisible(NULL))
  }
  if (identical(diagnostic_verbosity, "full")) {
    message(format_appusage_issue_report(error_context))
    message("Diagnostic report: ", paths$md)
    return(invisible(NULL))
  }
  lines <- c(
    "appusageR diagnostic",
    "Source data context:",
    paste0("- stage: ", error_context$stage %||% NA_character_),
    paste0("- source_file: ", error_context$source_file %||% NA_character_),
    paste0("- source_basename: ", error_context$source_basename %||% NA_character_),
    paste0("- file_size: ", error_context$file_size %||% NA_real_),
    paste0("- wenjuanxing_sequence_id: ", error_context$wenjuanxing_sequence_id %||% NA_integer_),
    paste0("- native_export_type: ", error_context$native_export_type %||% NA_character_),
    "Error context:",
    paste0("- code_function: ", error_context$code_function %||% NA_character_),
    paste0("- code_location: ", error_context$code_location %||% NA_character_),
    paste0("- condition_class: ", error_context$condition_class %||% NA_character_),
    paste0("- condition_message: ", error_context$condition_message %||% NA_character_),
    "Traceback/report:",
    paste0("- diagnostic_report: ", paths$md)
  )
  message(paste(lines, collapse = "\n"))
  invisible(NULL)
}

appusage_source_excerpt <- function(source_file, max_lines = 5) {
  if (!is_present_string(source_file) || !file.exists(source_file)) {
    return(NA_character_)
  }
  info <- file.info(source_file)
  if (is.na(info$size) || info$size == 0) {
    return("<empty file>")
  }
  n_bytes <- min(info$size, 65536)
  lines <- tryCatch(
    {
      bytes <- readBin(source_file, what = "raw", n = n_bytes)
      split_lines(decode_raw_text(bytes, encoding = "auto"))
    },
    error = function(e) character()
  )
  if (length(lines) == 0) {
    return(NA_character_)
  }
  lines <- utils::head(lines, max_lines)
  paste(paste0(seq_along(lines), ": ", lines), collapse = "\n")
}

appusage_abort_if_strict_failures <- function(summary, strict, stage) {
  if (!isTRUE(strict)) {
    return(invisible(NULL))
  }
  failures <- appusage_failure_rows(summary, stage)
  if (length(failures) == 0) {
    return(invisible(NULL))
  }
  i <- failures[[1]]
  report <- if ("diagnostic_report" %in% names(summary)) {
    summary$diagnostic_report[[i]]
  } else {
    NA_character_
  }
  cli::cli_abort(c(
    "Project workflow failed during {stage}.",
    "x" = "First diagnostic report: {report}"
  ))
}

appusage_context_list <- function(context) {
  if (is.null(context)) {
    return(list())
  }
  if (inherits(context, "appusage_error_context")) {
    return(unclass(context))
  }
  if (is.data.frame(context)) {
    context <- as.list(context[1, , drop = FALSE])
    return(lapply(context, function(x) x[[1]]))
  }
  if (is.list(context)) {
    return(context)
  }
  list(value = context)
}

appusage_context_value <- function(context, name, default = NA_character_) {
  if (!is.list(context) || is.null(context[[name]]) ||
    length(context[[name]]) == 0) {
    return(default)
  }
  value <- context[[name]]
  if (length(value) > 1) {
    value <- value[[1]]
  }
  if (is.null(value) || length(value) == 0) {
    return(default)
  }
  value
}

appusage_file_context <- function(source_file) {
  if (!is_present_string(source_file)) {
    return(list(
      source_file = NA_character_,
      source_basename = NA_character_,
      file_size = NA_real_,
      extension = NA_character_
    ))
  }
  info <- if (file.exists(source_file)) file.info(source_file) else NULL
  list(
    source_file = normalizePath(source_file, winslash = "/", mustWork = FALSE),
    source_basename = basename(source_file),
    file_size = if (is.null(info)) NA_real_ else as.numeric(info$size),
    extension = tolower(tools::file_ext(source_file))
  )
}

appusage_format_issue_fields <- function(x, fields) {
  vapply(fields, function(field) {
    value <- appusage_context_value(x, field)
    if (length(value) == 0 || all(is.na(value))) {
      value <- "NA"
    }
    paste0("- ", field, ": ", paste(value, collapse = ";"))
  }, character(1))
}

appusage_read_self_report_preflight <- function(self_report, sheet, ...) {
  if (is.data.frame(self_report)) {
    return(self_report)
  }
  if (is.character(self_report) && length(self_report) == 1 &&
    file.exists(self_report)) {
    return(as.data.frame(readxl::read_excel(self_report,
      sheet = sheet,
      ...
    )))
  }
  cli::cli_abort("`self_report` must be a data frame or an existing Excel file path.")
}

appusage_count_missing <- function(x) {
  if (is.null(x)) {
    return(NA_integer_)
  }
  x_chr <- as.character(x)
  sum(is.na(x_chr) | !nzchar(trimws(x_chr)))
}

appusage_count_duplicates <- function(x) {
  if (is.null(x)) {
    return(NA_integer_)
  }
  x_chr <- as.character(x)
  x_chr <- x_chr[!is.na(x_chr) & nzchar(trimws(x_chr))]
  sum(duplicated(x_chr))
}

appusage_maybe_match_self_report <- function(self_report_file, manifest,
                                             project, first, second,
                                             sequence_col, upload_col,
                                             submit_time_col,
                                             self_report_n_max,
                                             export_type_priority) {
  if (!is_present_string(self_report_file) || !is_present_string(upload_col)) {
    return(list(
      matched_self_report = NULL,
      matched_self_report_file = NA_character_,
      diagnostics = NULL
    ))
  }
  matched <- appusage_match_self_report_table(
    self_report = self_report_file,
    manifest = manifest,
    project_root = project$project_root,
    first = first,
    second = second,
    sequence_col = sequence_col,
    upload_col = upload_col,
    submit_time_col = submit_time_col,
    self_report_n_max = self_report_n_max,
    export_type_priority = export_type_priority,
    project_id = project$project_id,
    project_name = project$project_name
  )
  out_file <- appusage_matched_excel_path(
    self_report_file = self_report_file,
    project_root = project$project_root,
    project_id = project$project_id
  )
  appusage_write_xlsx(matched$matched_self_report, out_file)
  appusage_write_match_metadata(project$project_root, matched$diagnostics)
  appusage_refresh_match_summary(project$project_root, matched$file_matches)
  list(
    matched_self_report = matched$matched_self_report,
    matched_self_report_file = normalizePath(out_file, winslash = "/", mustWork = FALSE),
    diagnostics = matched$diagnostics
  )
}

appusage_match_self_report_table <- function(self_report, manifest,
                                             project_root, first, second,
                                             sequence_col = "\u5e8f\u53f7",
                                             upload_col,
                                             submit_time_col = NULL,
                                             self_report_n_max = Inf,
                                             export_type_priority = c("line", "meta", "day", "app"),
                                             project_id = NA_character_,
                                             project_name = NA_character_) {
  data <- appusage_read_self_report_rows(self_report, self_report_n_max)
  required <- c(sequence_col, upload_col)
  missing <- setdiff(required, names(data))
  if (length(missing) > 0) {
    cli::cli_abort("Self-report table is missing required column(s): {paste(missing, collapse = ', ')}.")
  }
  if (!is.null(submit_time_col) && !submit_time_col %in% names(data)) {
    cli::cli_abort("Self-report table is missing `submit_time_col`: {.val {submit_time_col}}.")
  }

  manifest <- appusage_manifest_with_proc2_paths(manifest, first, second, project_root)
  upload_candidates <- lapply(data[[upload_col]], extract_wenjuanxing_upload_filenames)
  sequence <- appusage_sequence_vector(data[[sequence_col]])
  submit_time <- if (!is.null(submit_time_col)) {
    appusage_datetime_vector(data[[submit_time_col]])
  } else {
    rep(as.POSIXct(NA), nrow(data))
  }
  duplicate_sequence <- duplicated(sequence) | duplicated(sequence, fromLast = TRUE)

  rows <- vector("list", nrow(data))
  file_matches <- appusage_init_file_match_summary(manifest)
  for (i in seq_len(nrow(data))) {
    row_match <- appusage_match_one_self_report_row(
      row_index = i,
      sequence_id = sequence[[i]],
      upload_candidates = upload_candidates[[i]],
      submit_time = submit_time[[i]],
      manifest = manifest,
      export_type_priority = export_type_priority,
      duplicate_sequence = duplicate_sequence[[i]],
      project_root = project_root
    )
    rows[[i]] <- row_match$row
    if (!is.na(row_match$manifest_index)) {
      file_matches$self_report_row[[row_match$manifest_index]] <- i
      file_matches$self_report_sequence_id[[row_match$manifest_index]] <- sequence[[i]]
      file_matches$self_report_match_status[[row_match$manifest_index]] <- row_match$row$moSens_match_status
    }
  }
  additions <- tibble::as_tibble(do.call(rbind, rows))
  matched <- cbind(data, additions, stringsAsFactors = FALSE)
  matched$moSens_project_id <- project_id
  matched$moSens_project_name <- project_name
  diagnostics <- appusage_match_diagnostics(matched, file_matches,
    project_id = project_id,
    project_name = project_name
  )
  list(
    matched_self_report = tibble::as_tibble(matched),
    file_matches = file_matches,
    diagnostics = diagnostics
  )
}

appusage_read_self_report_rows <- function(self_report, n_max = Inf) {
  if (is.data.frame(self_report)) {
    data <- self_report
  } else if (is.character(self_report) && length(self_report) == 1 && file.exists(self_report)) {
    if (is.finite(n_max)) {
      data <- as.data.frame(readxl::read_excel(self_report, n_max = as.integer(n_max)))
    } else {
      data <- as.data.frame(readxl::read_excel(self_report))
    }
  } else {
    cli::cli_abort("`self_report` must be a data frame or existing Excel path.")
  }
  data
}

appusage_sequence_vector <- function(x) {
  text <- trimws(as.character(x))
  text[!nzchar(text)] <- NA_character_
  suppressWarnings(as.integer(text))
}

appusage_datetime_vector <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = "Asia/Shanghai"))
  }
  if (inherits(x, "Date")) {
    return(as.POSIXct(x, tz = "Asia/Shanghai"))
  }
  text <- trimws(as.character(x))
  text[!nzchar(text) | is.na(text)] <- NA_character_
  out <- rep(as.POSIXct(NA), length(text))
  for (i in seq_along(text)) {
    if (is.na(text[[i]])) next
    parsed <- as.POSIXct(NA)
    formats <- c(
      "%Y-%m-%dT%H:%M:%OS%z",
      "%Y-%m-%d %H:%M:%OS%z",
      "%Y-%m-%d %H:%M:%OS",
      "%Y/%m/%d %H:%M:%OS"
    )
    for (fmt in formats) {
      parsed <- suppressWarnings(as.POSIXct(text[[i]],
        format = fmt,
        tz = "Asia/Shanghai"
      ))
      if (!is.na(parsed)) break
    }
    if (is.na(parsed)) {
      parsed <- suppressWarnings(as.POSIXct(text[[i]], tz = "Asia/Shanghai"))
    }
    out[[i]] <- parsed
  }
  out
}

extract_wenjuanxing_upload_filenames <- function(x) {
  if (length(x) == 0 || is.na(x)) {
    return(character())
  }
  text <- paste(as.character(x), collapse = " ")
  text <- gsub("&amp;", "&", text, fixed = TRUE)
  decoded <- tryCatch(utils::URLdecode(text), error = function(e) text)
  candidates <- c(
    appusage_extract_upload_query_values(decoded, c("filename", "attname")),
    appusage_extract_txt_like_names(decoded)
  )
  candidates <- basename(gsub("\\\\", "/", candidates))
  candidates <- trimws(candidates)
  candidates <- candidates[nzchar(candidates)]
  candidates <- appusage_upload_candidate_variants(candidates)
  unique(candidates)
}

appusage_upload_candidate_variants <- function(candidates) {
  candidates <- unique(candidates)
  native <- vapply(candidates, function(x) {
    appusage_native_filename_postfix(x) %||% NA_character_
  }, character(1))
  out <- unique(c(candidates, stats::na.omit(native)))
  redundant_question_prefix <- vapply(out, function(x) {
    grepl("^[0-9]+_AppUsage_", x, ignore.case = TRUE) &&
      any(grepl(paste0("^[0-9]+_", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", x), "$"), out, ignore.case = TRUE))
  }, logical(1))
  out[!redundant_question_prefix]
}

appusage_extract_upload_query_values <- function(text, keys) {
  out <- character()
  for (key in keys) {
    pattern <- paste0("(?i)(?:[?&;]|^)", key, "=([^&;\\s]+[.][Tt][Xx][Tt])")
    matches <- gregexpr(pattern, text, perl = TRUE)[[1]]
    if (identical(matches[[1]], -1L)) {
      next
    }
    values <- regmatches(text, list(matches))[[1]]
    values <- sub(paste0("(?i)^.*", key, "="), "", values, perl = TRUE)
    values <- sub("[&;\\s].*$", "", values, perl = TRUE)
    out <- c(out, values)
  }
  out
}

appusage_extract_txt_like_names <- function(text) {
  pattern <- "(?i)(?:[[:alnum:].()+ -]+_)?AppUsage_(?:line|meta|day|app)_[0-9]{4}_[0-9]{1,2}_[0-9]{1,2}_[0-9]{1,2}_[0-9]{1,2}_[0-9]{1,2}[.]txt|[0-9]+_[0-9]+_(?:run_ver[0-9]+|log|content_[0-9]+(?:[+][(][0-9]+[)])?)[.]txt"
  matches <- gregexpr(pattern, text, perl = TRUE)[[1]]
  if (identical(matches[[1]], -1L)) {
    return(character())
  }
  regmatches(text, list(matches))[[1]]
}

appusage_normalized_upload_name <- function(x) {
  x <- basename(gsub("\\\\", "/", as.character(x)))
  x <- tryCatch(utils::URLdecode(x), error = function(e) x)
  tolower(trimws(x))
}

appusage_manifest_with_proc2_paths <- function(manifest, first, second, project_root) {
  manifest$second_level_rda <- NA_character_
  manifest$second_level_rda_relative <- NA_character_
  if (is.null(first) || is.null(second) || nrow(first) == 0 || nrow(second) == 0) {
    return(manifest)
  }
  first_idx <- if ("source_file" %in% names(first)) {
    match(as.character(manifest$source_file), as.character(first$source_file))
  } else {
    rep(NA_integer_, nrow(manifest))
  }
  has_first <- !is.na(first_idx)
  if (!any(has_first)) {
    return(manifest)
  }
  first_data <- rep(NA_character_, nrow(manifest))
  first_data[has_first] <- as.character(first$data_file[first_idx[has_first]])

  second_idx <- rep(NA_integer_, nrow(manifest))
  for (candidate_col in c("first_level_data_file", "first_level_rda")) {
    if (candidate_col %in% names(second)) {
      missing <- is.na(second_idx) & has_first
      second_idx[missing] <- match(first_data[missing], as.character(second[[candidate_col]]))
    }
  }
  missing <- is.na(second_idx) & has_first
  if (any(missing) &&
    all(c("participant_id", "detected_type") %in% names(second)) &&
    all(c("participant_id", "detected_type") %in% names(first))) {
    first_key <- paste(first$participant_id, first$detected_type, sep = "\r")
    second_key <- paste(second$participant_id, second$detected_type, sep = "\r")
    second_idx[missing] <- match(first_key[first_idx[missing]], second_key)
  }
  has_second <- !is.na(second_idx)
  if (!any(has_second)) {
    return(manifest)
  }
  rda <- vapply(second_idx[has_second], function(i) {
    appusage_summary_proc2_path(second, i)
  }, character(1))
  valid <- !is.na(rda) & nzchar(rda) & file.exists(rda)
  target <- which(has_second)[valid]
  if (length(target) > 0) {
    normalized <- normalizePath(rda[valid], winslash = "/", mustWork = FALSE)
    manifest$second_level_rda[target] <- normalized
    manifest$second_level_rda_relative[target] <- vapply(
      normalized,
      appusage_relative_path,
      character(1),
      root = project_root
    )
  }
  manifest
}

appusage_summary_proc2_path <- function(summary, index) {
  for (col in c("second_level_rda", "second_level_data_file", "data_file")) {
    if (col %in% names(summary) && is_present_string(summary[[col]][[index]])) {
      return(summary[[col]][[index]])
    }
  }
  NA_character_
}

appusage_relative_path <- function(path, root) {
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  root <- normalizePath(root, winslash = "/", mustWork = FALSE)
  prefix <- paste0(root, "/")
  if (startsWith(path, prefix)) {
    substring(path, nchar(prefix) + 1L)
  } else {
    path
  }
}

appusage_init_file_match_summary <- function(manifest) {
  tibble::tibble(
    source_file = manifest$source_file,
    source_basename = manifest$source_basename,
    wenjuanxing_sequence_id = manifest$wenjuanxing_sequence_id,
    filename_export_type = manifest$filename_export_type,
    second_level_rda = manifest$second_level_rda %||% rep(NA_character_, nrow(manifest)),
    self_report_row = rep(NA_integer_, nrow(manifest)),
    self_report_sequence_id = rep(NA_integer_, nrow(manifest)),
    self_report_match_status = ifelse(manifest$is_txt %in% TRUE,
      "unmatched_appusage_upload",
      "not_appusage_txt"
    )
  )
}

appusage_match_one_self_report_row <- function(row_index, sequence_id,
                                               upload_candidates,
                                               submit_time,
                                               manifest,
                                               export_type_priority,
                                               duplicate_sequence,
                                               project_root) {
  warning <- character()
  if (isTRUE(duplicate_sequence)) {
    warning <- c(warning, "duplicate_sequence_in_self_report")
  }
  if (is.na(sequence_id)) {
    return(list(
      manifest_index = NA_integer_,
      row = appusage_match_output_row(
        status = "unmatched_sequence",
        sequence_id = sequence_id,
        warning = c(warning, "missing_sequence")
      )
    ))
  }
  if (length(upload_candidates) == 0) {
    return(list(
      manifest_index = NA_integer_,
      row = appusage_match_output_row(
        status = "unmatched_no_upload_filename",
        sequence_id = sequence_id,
        warning = c(warning, "no_txt_filename_extracted")
      )
    ))
  }
  norm_candidates <- appusage_normalized_upload_name(upload_candidates)
  source_names <- appusage_normalized_upload_name(manifest$source_basename)
  uploaded_names <- appusage_normalized_upload_name(manifest$uploaded_file_name)
  native_names <- appusage_normalized_upload_name(manifest$native_export_file_name)
  seq_match <- manifest$wenjuanxing_sequence_id == sequence_id
  file_match <- source_names %in% norm_candidates |
    uploaded_names %in% norm_candidates |
    native_names %in% norm_candidates
  candidates <- which(seq_match & file_match & manifest$is_txt %in% TRUE)
  if (length(candidates) == 0) {
    status <- if (any(seq_match, na.rm = TRUE)) {
      "unmatched_filename"
    } else if (any(file_match, na.rm = TRUE)) {
      "unmatched_sequence"
    } else {
      "unmatched"
    }
    return(list(
      manifest_index = NA_integer_,
      row = appusage_match_output_row(
        status = status,
        sequence_id = sequence_id,
        warning = c(warning, "no_dual_key_match")
      )
    ))
  }
  resolved <- appusage_resolve_duplicate_manifest_candidates(
    manifest = manifest,
    candidates = candidates,
    submit_time = submit_time,
    export_type_priority = export_type_priority
  )
  chosen <- resolved$index
  warning <- c(warning, resolved$warning)
  if (!is_present_string(manifest$second_level_rda[[chosen]])) {
    return(list(
      manifest_index = chosen,
      row = appusage_match_output_row(
        status = "unmatched_no_proc2",
        sequence_id = sequence_id,
        source_file = manifest$source_file[[chosen]],
        export_type = manifest$filename_export_type[[chosen]],
        export_timestamp = manifest$native_export_created_at[[chosen]],
        warning = c(warning, "matched_upload_has_no_second_level_rda")
      )
    ))
  }
  list(
    manifest_index = chosen,
    row = appusage_match_output_row(
      status = "matched",
      sequence_id = sequence_id,
      data_dir = manifest$second_level_rda_relative[[chosen]],
      source_file = manifest$source_file[[chosen]],
      export_type = manifest$filename_export_type[[chosen]],
      export_timestamp = manifest$native_export_created_at[[chosen]],
      warning = warning
    )
  )
}

appusage_match_output_row <- function(status, sequence_id,
                                      data_dir = NA_character_,
                                      source_file = NA_character_,
                                      export_type = NA_character_,
                                      export_timestamp = NA_character_,
                                      warning = character()) {
  data.frame(
    moSens_match_status = status,
    moSens_data_dir = data_dir,
    moSens_project_id = NA_character_,
    moSens_project_name = NA_character_,
    moSens_wenjuanxing_sequence_id = sequence_id,
    moSens_appusage_source_file = source_file,
    moSens_appusage_export_type = export_type,
    moSens_appusage_export_timestamp = export_timestamp,
    moSens_match_warning = paste(unique(warning[nzchar(warning)]), collapse = ";"),
    stringsAsFactors = FALSE
  )
}

appusage_resolve_duplicate_manifest_candidates <- function(manifest, candidates,
                                                           submit_time,
                                                           export_type_priority) {
  warning <- character()
  if (length(candidates) == 1) {
    return(list(index = candidates[[1]], warning = warning))
  }
  types <- as.character(manifest$filename_export_type[candidates])
  priority <- match(types, export_type_priority)
  priority[is.na(priority)] <- length(export_type_priority) + 1L
  min_priority <- min(priority)
  candidates <- candidates[priority == min_priority]
  warning <- c(warning, "multiple_candidates_resolved_by_export_type_priority")
  if (length(candidates) == 1) {
    return(list(index = candidates[[1]], warning = warning))
  }
  export_times <- appusage_datetime_vector(manifest$native_export_created_at[candidates])
  if (!is.na(submit_time) && any(!is.na(export_times))) {
    diff <- abs(as.numeric(difftime(submit_time, export_times, units = "secs")))
    candidates <- candidates[diff == min(diff, na.rm = TRUE)]
    warning <- c(warning, "multiple_candidates_resolved_by_nearest_submit_time")
  } else {
    warning <- c(warning, "submit_time_missing_or_unparseable_priority_only")
  }
  candidates <- candidates[order(manifest$source_basename[candidates])]
  list(index = candidates[[1]], warning = warning)
}

appusage_match_diagnostics <- function(matched, file_matches,
                                       project_id, project_name) {
  status <- as.character(matched$moSens_match_status)
  list(
    status = "success",
    project_id = project_id,
    project_name = project_name,
    n_self_report_rows = nrow(matched),
    n_matched_self_report_rows = sum(status == "matched", na.rm = TRUE),
    n_unmatched_self_report_rows = sum(status != "matched", na.rm = TRUE),
    match_status_counts = as.list(table(status)),
    n_appusage_uploads = nrow(file_matches),
    n_unmatched_appusage_uploads = sum(file_matches$self_report_match_status == "unmatched_appusage_upload", na.rm = TRUE)
  )
}

appusage_matched_excel_path <- function(self_report_file, project_root,
                                        project_id) {
  stem <- tools::file_path_sans_ext(basename(self_report_file))
  key <- sub(paste0("^ProjectID[-_]", appusage_plain_project_id(project_id), "[-_]?"), "", stem)
  key <- if (grepl("^WJXraw", key, ignore.case = TRUE)) key else paste0("WJXraw-", key)
  key <- appusage_sanitize_excel_key(key)
  file.path(
    project_root,
    paste0("ProjectID-", appusage_plain_project_id(project_id), "_", key, "_Stat-matched.xlsx")
  )
}

appusage_sanitize_excel_key <- function(x) {
  x <- gsub("[<>:\"/\\\\|?*]+", "-", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

appusage_write_xlsx <- function(data, path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    cli::cli_abort("Package `openxlsx` is required to write matched Excel output.")
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  openxlsx::write.xlsx(as.data.frame(data), file = path, overwrite = TRUE)
  invisible(path)
}

appusage_write_match_metadata <- function(project_root, diagnostics) {
  if (is.null(diagnostics)) {
    return(invisible(NULL))
  }
  description_file <- file.path(project_root, "dataset_descriptions.json")
  description <- if (file.exists(description_file)) {
    tryCatch(
      jsonlite::read_json(description_file, simplifyVector = TRUE),
      error = function(e) list()
    )
  } else {
    list()
  }
  description$self_report_matching <- diagnostics
  jsonlite::write_json(description,
    path = description_file,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
  invisible(description_file)
}

appusage_refresh_match_summary <- function(project_root, file_matches) {
  if (is.null(file_matches) || nrow(file_matches) == 0) {
    return(invisible(NULL))
  }
  summary_file <- file.path(project_root, "analytic_summary_table_proclevel-2.csv")
  if (!file.exists(summary_file)) {
    return(invisible(NULL))
  }
  summary <- appusage_read_summary_csv(summary_file)
  if (nrow(summary) == 0) {
    return(invisible(NULL))
  }
  summary$self_report_match_status <- NA_character_
  summary$self_report_sequence_id <- NA_integer_
  matched <- rep(NA_integer_, nrow(summary))
  if ("second_level_rda" %in% names(summary)) {
    matched <- match(as.character(summary$second_level_rda), as.character(file_matches$second_level_rda))
  }
  missing <- is.na(matched)
  if (any(missing) && all(c("participant_id", "detected_type") %in% names(summary))) {
    summary_key <- paste(summary$participant_id, summary$detected_type, sep = "\r")
    file_key <- paste(file_matches$wenjuanxing_sequence_id, file_matches$filename_export_type, sep = "\r")
    matched[missing] <- match(summary_key[missing], file_key)
  }
  has_match <- !is.na(matched)
  if (any(has_match)) {
    summary$self_report_match_status[has_match] <- file_matches$self_report_match_status[matched[has_match]]
    summary$self_report_sequence_id[has_match] <- file_matches$self_report_sequence_id[matched[has_match]]
  }
  utils::write.csv(summary, summary_file, row.names = FALSE, na = "")
  invisible(summary_file)
}

appusage_second_summary_has_inline_qc <- function(second) {
  if (is.null(second) || nrow(second) == 0 || !"qc_status" %in% names(second)) {
    return(FALSE)
  }
  status <- if ("status" %in% names(second)) {
    as.character(second$status)
  } else if ("second_level_status" %in% names(second)) {
    as.character(second$second_level_status)
  } else {
    rep(NA_character_, nrow(second))
  }
  successful <- status == "success"
  if (!any(successful, na.rm = TRUE)) {
    return(FALSE)
  }
  qc_status <- as.character(second$qc_status[successful])
  all(!is.na(qc_status) & nzchar(qc_status) & !qc_status %in% c("not_run", "pending"))
}
