# First-level cache recovery and memory-safe batch helpers

appusage_memory_allocation_error_pattern <- function() {
  paste(
    c(
      "cannot allocate",
      "could not allocate",
      "memory exhausted",
      "memory allocation",
      "std::bad_alloc",
      "vector memory exhausted",
      "protect.*stack overflow",
      "out of memory",
      "r_allocstringbuffer",
      "failed to realloc",
      "realloc working memory stack",
      "cannot reserve memory",
      "not enough memory",
      "insufficient memory",
      "cannot grow vector",
      "bad allocation"
    ),
    collapse = "|"
  )
}

appusage_is_memory_allocation_text <- function(...) {
  text <- paste(..., collapse = " ")
  if (!nzchar(text)) {
    return(FALSE)
  }
  grepl(appusage_memory_allocation_error_pattern(), tolower(text))
}

appusage_is_memory_risk_control_flow <- function(error_message,
                                                 memory_risk_signal = FALSE) {
  isTRUE(memory_risk_signal) &&
    grepl(
      "missing value where true/false needed",
      tolower(error_message %||% ""),
      fixed = TRUE
    )
}

appusage_first_level_memory_risk_signal <- function(x, input, parallel, n_cores) {
  size <- appusage_source_size_info(x, input = input)
  reasons <- character()
  if (isTRUE(parallel) && n_cores >= 4L) {
    reasons <- c(reasons, "four_or_more_concurrent_workers")
  }
  if (isTRUE(parallel) && n_cores > 1L &&
    !is.na(size$max_source_size_bytes) &&
    size$max_source_size_bytes >= 64 * 1024^2) {
    reasons <- c(reasons, "large_source_file")
  }
  if (isTRUE(parallel) && n_cores > 1L &&
    !is.na(size$total_source_size_bytes) &&
    size$total_source_size_bytes >= 512 * 1024^2) {
    reasons <- c(reasons, "large_batch_bytes")
  }
  list(
    active = length(reasons) > 0L,
    reason = if (length(reasons) == 0L) {
      NA_character_
    } else {
      paste(unique(reasons), collapse = ";")
    }
  )
}

#' Rebuild first-level analytic summary from existing cache metadata
#'
#' Reconstructs `analytic_summary_table_proclevel-1.csv` from existing
#' `proclevel-1` JSON/RDA cache files. This is intended for interrupted large
#' project runs where per-file caches were written but the final summary table
#' was not.
#'
#' @param project_dir Output study/project directory containing `proclevel-1`.
#' @param manifest Optional project manifest produced by
#'   `build_appusage_project_manifest()`. When supplied, raw text files not
#'   represented in cache metadata are reported as `not_processed`.
#' @param write Logical; write the rebuilt summary CSV and dataset description.
#' @param strict Logical; abort on malformed metadata when `TRUE`. When `FALSE`,
#'   malformed metadata is represented as an incomplete/problem row.
#'
#' @return A tibble with one row per recovered cache record plus optional
#'   `not_processed` manifest rows.
#' @export
rebuild_first_level_summary_from_cache <- function(project_dir,
                                                   manifest = NULL,
                                                   write = TRUE,
                                                   strict = FALSE) {
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  proc1_dir <- file.path(project_dir, "proclevel-1")

  if (!dir.exists(proc1_dir)) {
    cli::cli_abort("No proclevel-1 directory found at {.path {proc1_dir}}.")
  }

  json_files <- list.files(
    proc1_dir,
    pattern = "_proc-1[.]json$",
    full.names = TRUE
  )
  rda_files <- list.files(
    proc1_dir,
    pattern = "_proc-1[.]rda$",
    full.names = TRUE
  )

  manifest <- appusage_prepare_manifest_for_rebuild(manifest)
  json_rows <- lapply(json_files, function(path) {
    appusage_proc1_row_from_json(path, proc1_dir, manifest, strict = strict)
  })

  json_rows <- Filter(Negate(is.null), json_rows)
  known_rdas <- unique(stats::na.omit(vapply(
    json_rows,
    function(row) {
      value <- row[["data_file"]]
      if (length(value) == 0L || is.na(value) || !nzchar(value)) {
        return(NA_character_)
      }
      normalizePath(value, winslash = "/", mustWork = FALSE)
    },
    character(1)
  )))

  rda_only <- setdiff(
    normalizePath(rda_files, winslash = "/", mustWork = FALSE),
    known_rdas
  )
  rda_only_rows <- lapply(rda_only, function(path) {
    appusage_proc1_incomplete_rda_row(path, proc1_dir, manifest)
  })

  cache_rows <- c(json_rows, rda_only_rows)
  not_processed_rows <- appusage_proc1_not_processed_rows(manifest, cache_rows)

  summary <- do.call(
    bind_appusage_summary_rows,
    c(cache_rows, not_processed_rows)
  )

  if (nrow(summary) > 0L && "index" %in% names(summary)) {
    summary <- summary[order(summary$index, na.last = TRUE), , drop = FALSE]
  }

  if (isTRUE(write)) {
    summary_file <- file.path(project_dir, "analytic_summary_table_proclevel-1.csv")
    dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(summary, summary_file, row.names = FALSE, na = "")

    project_info <- project_info_from_root(project_dir)
    write_dataset_description_json(
      project_info,
      summary,
      proclevel = 1,
      summary_file = summary_file,
      status = "reconstructed"
    )
  }

  tibble::as_tibble(summary)
}

appusage_rebuild_first_level_summary_if_needed <- function(project_dir,
                                                           manifest = NULL,
                                                           strict = FALSE) {
  if (!is_present_string(project_dir)) {
    return(NULL)
  }

  summary_file <- file.path(project_dir, "analytic_summary_table_proclevel-1.csv")
  proc1_dir <- file.path(project_dir, "proclevel-1")

  if (file.exists(summary_file) || !dir.exists(proc1_dir)) {
    return(NULL)
  }

  json_files <- list.files(proc1_dir, pattern = "_proc-1[.]json$", full.names = TRUE)
  rda_files <- list.files(proc1_dir, pattern = "_proc-1[.]rda$", full.names = TRUE)
  if (length(json_files) == 0L && length(rda_files) == 0L) {
    return(NULL)
  }

  rebuild_first_level_summary_from_cache(
    project_dir = project_dir,
    manifest = manifest,
    write = TRUE,
    strict = strict
  )
}

appusage_prepare_manifest_for_rebuild <- function(manifest) {
  if (is.null(manifest)) {
    return(NULL)
  }
  manifest <- as.data.frame(manifest, stringsAsFactors = FALSE)
  if (!"source_file" %in% names(manifest) && "path" %in% names(manifest)) {
    manifest$source_file <- manifest$path
  }
  if (!"source_file" %in% names(manifest)) {
    return(NULL)
  }
  manifest$source_file_norm <- normalizePath(
    manifest$source_file,
    winslash = "/",
    mustWork = FALSE
  )
  if (!"index" %in% names(manifest)) {
    manifest$index <- seq_len(nrow(manifest))
  }
  manifest
}

appusage_proc1_row_from_json <- function(json_path,
                                         proc1_dir,
                                         manifest = NULL,
                                         strict = FALSE) {
  json_norm <- normalizePath(json_path, winslash = "/", mustWork = FALSE)
  metadata <- tryCatch(
    jsonlite::read_json(json_norm, simplifyVector = FALSE),
    error = function(e) e
  )

  if (inherits(metadata, "error")) {
    if (isTRUE(strict)) {
      stop(metadata)
    }
    return(appusage_proc1_problem_row(
      metadata_file = json_norm,
      data_file = NA_character_,
      status = "incomplete",
      cache_rebuild_status = "malformed_json",
      cache_rebuild_issue = conditionMessage(metadata),
      manifest_row = NULL
    ))
  }

  source_file <- appusage_first_nonmissing(
    appusage_nested_value(metadata, c("source", "source_file")),
    appusage_nested_value(metadata, c("source", "file_path")),
    appusage_nested_value(metadata, c("source_file"))
  )
  manifest_row <- appusage_match_manifest_row(source_file, manifest)

  detected_type <- appusage_first_nonmissing(
    appusage_nested_value(metadata, c("export", "detected_type")),
    appusage_nested_value(metadata, c("content_detected_export_type")),
    appusage_nested_value(metadata, c("detected_type"))
  )

  status <- appusage_first_nonmissing(
    appusage_nested_value(metadata, c("processing", "first_level_status")),
    appusage_nested_value(metadata, c("first_level_status")),
    appusage_nested_value(metadata, c("status"))
  )
  if (!is_present_string(status)) {
    status <- "unknown"
  }

  data_file <- appusage_first_nonmissing(
    appusage_nested_value(metadata, c("outputs", "first_level_rda")),
    appusage_nested_value(metadata, c("data_file"))
  )
  if (!is_present_string(data_file)) {
    data_file <- sub("[.]json$", ".rda", json_norm)
  }
  data_file <- normalizePath(data_file, winslash = "/", mustWork = FALSE)

  has_rda <- file.exists(data_file)
  cache_status <- "complete"
  cache_issue <- NA_character_
  rebuilt_status <- status

  if (identical(status, "success") && !has_rda) {
    rebuilt_status <- "incomplete"
    cache_status <- "success_json_missing_rda"
    cache_issue <- "First-level JSON records success but paired RDA is missing."
  } else if (!identical(status, "success")) {
    cache_status <- "recorded_error"
  }

  row <- appusage_proc1_problem_row(
    metadata_file = json_norm,
    data_file = if (has_rda) data_file else NA_character_,
    status = rebuilt_status,
    cache_rebuild_status = cache_status,
    cache_rebuild_issue = cache_issue,
    manifest_row = manifest_row
  )

  row$source_file <- appusage_first_nonmissing(source_file, row$source_file)
  row$participant_id <- appusage_first_nonmissing(
    appusage_nested_value(metadata, c("participant_id")),
    appusage_nested_value(metadata, c("identity", "participant_id")),
    row$participant_id
  )
  row$participant_id_source <- appusage_first_nonmissing(
    appusage_nested_value(metadata, c("participant_id_source")),
    appusage_nested_value(metadata, c("identity", "participant_id_source")),
    row$participant_id_source
  )
  source_identity <- appusage_metadata_source_identity(metadata)
  row$source_record_key <- appusage_first_nonmissing(
    source_identity$source_record_key,
    row$source_record_key
  )
  row$source_fingerprint <- appusage_first_nonmissing(
    source_identity$source_fingerprint,
    row$source_fingerprint
  )
  row$source_cache_key <- appusage_first_nonmissing(
    source_identity$source_cache_key,
    row$source_cache_key
  )
  row$detected_type <- appusage_first_nonmissing(detected_type, row$detected_type)
  preflight <- metadata$source$preflight %||% list()
  row$selected_component <- appusage_first_nonmissing(
    preflight$selected_component,
    row$selected_component
  )
  row$detected_components <- paste(
    preflight$detected_components %||% character(),
    collapse = ";"
  )
  row$mixed_content <- isTRUE(preflight$mixed_content %||% FALSE)
  row$component_selection_rule <- appusage_first_nonmissing(
    preflight$selection_rule,
    row$component_selection_rule
  )
  row$filename_content_disagreement <- isTRUE(
    preflight$filename_content_disagreement %||% FALSE
  )
  quality <- metadata$structural_quality %||%
    appusage_nested_value(metadata, c("parser_diagnostics", "format_specific", "structural_quality"), default = list())
  quality_fields <- appusage_structural_quality_summary_fields(quality)
  for (name in names(quality_fields)) row[[name]] <- quality_fields[[name]]
  row$content_detected_type <- appusage_first_nonmissing(
    appusage_nested_value(metadata, c("content_detection", "detected_type")),
    row$content_detected_type
  )
  row$n_rows <- suppressWarnings(as.integer(appusage_first_nonmissing(
    appusage_nested_value(metadata, c("counts", "n_rows")),
    appusage_nested_value(metadata, c("parser_diagnostics", "parse_quality", "n_rows_out")),
    appusage_nested_value(metadata, c("n_rows")),
    row$n_rows
  )))
  row$n_parse_warnings <- suppressWarnings(as.integer(appusage_first_nonmissing(
    appusage_nested_value(metadata, c("parse_diagnostics", "n_warnings")),
    appusage_nested_value(metadata, c("n_parse_warnings")),
    row$n_parse_warnings
  )))
  row$error_class <- appusage_first_nonmissing(
    appusage_nested_value(metadata, c("error", "class")),
    appusage_nested_value(metadata, c("errors", "class")),
    appusage_nested_value(metadata, c("processing", "error_class")),
    row$error_class
  )
  row$error_message <- appusage_first_nonmissing(
    appusage_nested_value(metadata, c("error", "message")),
    appusage_nested_value(metadata, c("errors", "message")),
    appusage_nested_value(metadata, c("processing", "error_message")),
    row$error_message
  )
  row$failure_family <- appusage_classify_failure_family(
    row$error_class,
    row$error_message,
    status = row$status
  )
  row <- appusage_attach_provenance_summary(
    row,
    appusage_metadata_provenance(metadata)
  )
  row$summary_source <- "reconstructed_proc1_cache"
  row
}

appusage_proc1_incomplete_rda_row <- function(rda_path,
                                              proc1_dir,
                                              manifest = NULL) {
  rda_norm <- normalizePath(rda_path, winslash = "/", mustWork = FALSE)
  appusage_proc1_problem_row(
    metadata_file = NA_character_,
    data_file = rda_norm,
    status = "incomplete",
    cache_rebuild_status = "rda_missing_json",
    cache_rebuild_issue = "First-level RDA exists without paired JSON metadata.",
    manifest_row = appusage_match_manifest_row(NA_character_, manifest)
  )
}

appusage_proc1_not_processed_rows <- function(manifest, cache_rows) {
  if (is.null(manifest) || nrow(manifest) == 0L) {
    return(list())
  }

  cached_sources <- unique(stats::na.omit(vapply(
    cache_rows,
    function(row) {
      value <- row[["source_file"]]
      if (length(value) == 0L || is.na(value) || !nzchar(value)) {
        return(NA_character_)
      }
      normalizePath(value, winslash = "/", mustWork = FALSE)
    },
    character(1)
  )))

  is_txt <- if ("is_txt" %in% names(manifest)) {
    isTRUE_vector(manifest$is_txt)
  } else if ("extension" %in% names(manifest)) {
    tolower(manifest$extension) == "txt"
  } else {
    grepl("[.]txt$", manifest$source_file, ignore.case = TRUE)
  }

  not_processed <- manifest[is_txt & !(manifest$source_file_norm %in% cached_sources), ,
    drop = FALSE
  ]
  if (nrow(not_processed) == 0L) {
    return(list())
  }

  lapply(seq_len(nrow(not_processed)), function(i) {
    appusage_proc1_problem_row(
      metadata_file = NA_character_,
      data_file = NA_character_,
      status = "not_processed",
      cache_rebuild_status = "not_processed",
      cache_rebuild_issue = "Raw txt file has no first-level cache metadata.",
      manifest_row = not_processed[i, , drop = FALSE]
    )
  })
}

appusage_proc1_problem_row <- function(metadata_file,
                                       data_file,
                                       status,
                                       cache_rebuild_status,
                                       cache_rebuild_issue,
                                       manifest_row = NULL) {
  manifest_value <- function(name, default = NA) {
    if (is.null(manifest_row) || !name %in% names(manifest_row)) {
      return(default)
    }
    value <- manifest_row[[name]][1]
    if (length(value) == 0L) {
      default
    } else {
      value
    }
  }

  source_file <- manifest_value("source_file", NA_character_)
  parsed <- if (is_present_string(source_file)) {
    parse_appusage_filename(basename(source_file))
  } else {
    NULL
  }

  data.frame(
    index = suppressWarnings(as.integer(manifest_value("index", NA_integer_))),
    source_file = as.character(source_file),
    source_record_key = as.character(manifest_value("source_record_key", NA_character_)),
    source_fingerprint = as.character(manifest_value("source_fingerprint", NA_character_)),
    source_cache_key = as.character(manifest_value("source_cache_key", NA_character_)),
    participant_id = as.character(manifest_value("participant_id", NA_character_)),
    participant_id_source = as.character(manifest_value(
      "participant_id_source",
      NA_character_
    )),
    wenjuanxing_sequence_id = suppressWarnings(as.integer(manifest_value(
      "wenjuanxing_sequence_id",
      NA_integer_
    ))),
    filename_export_type = as.character(appusage_first_nonmissing(
      manifest_value("filename_export_type", NA_character_),
      if (!is.null(parsed)) parsed$type else NA_character_
    )),
    native_export_timestamp = as.character(manifest_value(
      "native_export_timestamp",
      NA_character_
    )),
    content_detected_type = NA_character_,
    detected_type = as.character(manifest_value("filename_export_type", NA_character_)),
    detected_components = NA_character_,
    selected_component = NA_character_,
    mixed_content = NA,
    component_selection_rule = NA_character_,
    filename_content_disagreement = NA,
    structural_quality_status = NA_character_,
    structural_quality_critical = NA,
    structural_quality_warning = NA,
    structural_valid_interval_ratio = NA_real_,
    structural_candidate_rows = NA_integer_,
    structural_parsed_rows = NA_integer_,
    structural_missing_timestamp_count = NA_integer_,
    structural_missing_duration_count = NA_integer_,
    structural_header_contamination_count = NA_integer_,
    structural_exact_duplicate_count = NA_integer_,
    structural_exact_duplicate_ratio = NA_real_,
    structural_malformed_identity_count = NA_integer_,
    structural_date_mismatch_count = NA_integer_,
    structural_critical_reasons = NA_character_,
    structural_warning_reasons = NA_character_,
    status = status,
    data_file = as.character(data_file),
    metadata_file = as.character(metadata_file),
    n_rows = NA_integer_,
    n_parse_warnings = NA_integer_,
    error_class = NA_character_,
    error_message = NA_character_,
    traceback = NA_character_,
    failure_family = appusage_classify_failure_family(
      NA_character_,
      NA_character_,
      status = status
    ),
    worker_pid = NA_integer_,
    retry_attempt = 0L,
    retry_worker_count = NA_integer_,
    original_error_class = NA_character_,
    original_error_message = NA_character_,
    original_failure_family = NA_character_,
    cache_rebuild_status = cache_rebuild_status,
    cache_rebuild_issue = cache_rebuild_issue,
    summary_source = "reconstructed_proc1_cache",
    stringsAsFactors = FALSE
  )
}

appusage_match_manifest_row <- function(source_file, manifest) {
  if (is.null(manifest) || nrow(manifest) == 0L || !is_present_string(source_file)) {
    return(NULL)
  }
  source_norm <- normalizePath(source_file, winslash = "/", mustWork = FALSE)
  idx <- which(manifest$source_file_norm == source_norm)
  if (length(idx) == 0L) {
    idx <- which(basename(manifest$source_file_norm) == basename(source_norm))
  }
  if (length(idx) == 0L) {
    return(NULL)
  }
  manifest[idx[1], , drop = FALSE]
}

isTRUE_vector <- function(x) {
  !is.na(x) & x
}

appusage_first_nonmissing <- function(...) {
  values <- list(...)
  for (value in values) {
    if (length(value) == 0L) {
      next
    }
    if (length(value) > 1L) {
      value <- value[[1]]
    }
    if (is.null(value) || is.na(value)) {
      next
    }
    if (is.character(value) && !nzchar(value)) {
      next
    }
    return(value)
  }
  NA
}

appusage_classify_failure_family <- function(error_class = NA_character_,
                                             error_message = NA_character_,
                                             status = "error",
                                             memory_risk_signal = FALSE) {
  if (!identical(status, "error") && !identical(status, "incomplete")) {
    return(NA_character_)
  }

  text <- paste(
    paste(error_class, collapse = " "),
    paste(error_message, collapse = " ")
  )
  text <- tolower(text)

  source_families <- c(
    appusage_zero_byte_source = "source_zero_byte",
    appusage_binary_source = "source_binary",
    appusage_unknown_content = "source_unknown_content",
    appusage_header_only_source = "source_header_only",
    appusage_mixed_content_source = "source_mixed_content"
  )
  source_hit <- names(source_families)[vapply(
    names(source_families),
    function(x) grepl(tolower(x), text, fixed = TRUE),
    logical(1)
  )]
  if (length(source_hit) > 0L) {
    return(unname(source_families[[source_hit[[1]]]]))
  }
  if (grepl("appusage_line_structural_quality", text, fixed = TRUE)) {
    return("structural_quality")
  }

  if (appusage_is_memory_allocation_text(text)) {
    return("memory_allocation")
  }
  if (grepl("missing value where true/false needed", text, fixed = TRUE)) {
    if (appusage_is_memory_risk_control_flow(text, memory_risk_signal)) {
      return("memory_allocation")
    }
    return("parser_control_flow")
  }
  if (grepl("unsupported|unknown.*export|unlock", text)) {
    return("unsupported_export_type")
  }
  if (grepl("empty|zero-byte|zero byte|no app usage lines", text)) {
    return("empty_or_missing_content")
  }
  if (grepl("parse|malformed|invalid|header|column", text)) {
    return("malformed_or_parse_error")
  }
  if (identical(status, "incomplete")) {
    return("incomplete_cache")
  }

  "parser_or_unknown"
}

appusage_annotate_first_level_row <- function(row,
                                              retry_attempt = 0L,
                                              retry_worker_count = NA_integer_,
                                              original_row = NULL) {
  row <- as.data.frame(row, stringsAsFactors = FALSE)

  if (!"failure_family" %in% names(row)) {
    row$failure_family <- NA_character_
  }
  if (!"worker_pid" %in% names(row)) {
    row$worker_pid <- NA_integer_
  }
  if (!"retry_attempt" %in% names(row)) {
    row$retry_attempt <- 0L
  }
  if (!"retry_worker_count" %in% names(row)) {
    row$retry_worker_count <- NA_integer_
  }
  if (!"original_error_class" %in% names(row)) {
    row$original_error_class <- NA_character_
  }
  if (!"original_error_message" %in% names(row)) {
    row$original_error_message <- NA_character_
  }
  if (!"original_failure_family" %in% names(row)) {
    row$original_failure_family <- NA_character_
  }
  for (name in c(
    "original_error_call", "original_traceback", "original_worker_task_index",
    "original_raw_line_number", "original_raw_line_window"
  )) {
    if (!name %in% names(row)) row[[name]] <- NA
  }

  if (nrow(row) == 0L) {
    return(row)
  }

  row$failure_family <- appusage_classify_failure_family(
    row$error_class[1],
    row$error_message[1],
    status = row$status[1],
    memory_risk_signal = isTRUE(appusage_get_col_value(row, "memory_risk_signal", FALSE))
  )
  if (is.na(row$worker_pid[1])) {
    row$worker_pid <- Sys.getpid()
  }
  row$retry_attempt <- as.integer(retry_attempt)
  row$retry_worker_count <- as.integer(retry_worker_count)

  if (!is.null(original_row)) {
    original_row <- as.data.frame(original_row, stringsAsFactors = FALSE)
    row$original_error_class <- appusage_get_col_value(
      original_row,
      "error_class",
      NA_character_
    )
    row$original_error_message <- appusage_get_col_value(
      original_row,
      "error_message",
      NA_character_
    )
    row$original_failure_family <- appusage_get_col_value(
      original_row,
      "failure_family",
      appusage_classify_failure_family(
        appusage_get_col_value(original_row, "error_class", NA_character_),
        appusage_get_col_value(original_row, "error_message", NA_character_),
        status = appusage_get_col_value(original_row, "status", "error")
      )
    )
    row$original_error_call <- appusage_get_col_value(original_row, "error_call", NA_character_)
    row$original_traceback <- appusage_get_col_value(original_row, "traceback", NA_character_)
    row$original_worker_task_index <- appusage_get_col_value(
      original_row,
      "worker_task_index",
      appusage_get_col_value(original_row, "index", NA_integer_)
    )
    row$original_raw_line_number <- appusage_get_col_value(original_row, "raw_line_number", NA_integer_)
    row$original_raw_line_window <- appusage_get_col_value(original_row, "raw_line_window", NA_character_)
  }

  row
}

appusage_get_col_value <- function(row, name, default = NA) {
  if (!name %in% names(row) || nrow(row) == 0L) {
    return(default)
  }
  value <- row[[name]][1]
  if (length(value) == 0L) {
    default
  } else {
    value
  }
}

appusage_retry_memory_row <- function(row,
                                      retry_fun,
                                      retry_worker_count = 1L,
                                      enabled = TRUE) {
  row <- as.data.frame(row, stringsAsFactors = FALSE)
  if (!isTRUE(enabled) || nrow(row) == 0L) {
    return(row)
  }
  family <- appusage_get_col_value(row, "failure_family", NA_character_)
  status <- appusage_get_col_value(row, "status", NA_character_)
  if (!identical(status, "error") || !appusage_recoverable_runtime_family(family)) {
    return(row)
  }

  retry <- retry_fun()
  retry <- appusage_annotate_first_level_row(
    retry,
    retry_attempt = 1L,
    retry_worker_count = retry_worker_count,
    original_row = row
  )
  retry
}

appusage_recoverable_runtime_family <- function(family) {
  identical(family, "memory_allocation")
}

appusage_first_level_worker_decision <- function(parallel,
                                                 n_cores,
                                                 x,
                                                 input = "file",
                                                 max_workers = 12L,
                                                 worker_cap_override = FALSE,
                                                 available_cores = parallel::detectCores(logical = TRUE),
                                                 memory_info = NULL,
                                                 size_info = NULL) {
  requested <- suppressWarnings(as.integer(n_cores))
  if (is.na(requested) || requested < 1L) {
    requested <- 1L
  }
  available_cores <- suppressWarnings(as.integer(available_cores))
  if (is.na(available_cores) || available_cores < 1L) {
    available_cores <- 1L
  }
  source_file_count <- length(x)
  max_workers <- suppressWarnings(as.integer(max_workers))
  if (is.na(max_workers) || max_workers < 1L) {
    max_workers <- 12L
  }
  memory_info <- memory_info %||% appusage_detect_memory_info()
  size_info <- size_info %||% appusage_source_size_info(x, input = input)

  if (!isTRUE(parallel)) {
    return(appusage_worker_decision_record(
      requested_workers = requested,
      detected_logical_cores = available_cores,
      selected_workers = 1L,
      max_workers = max_workers,
      worker_cap_override = worker_cap_override,
      source_file_count = source_file_count,
      size_info = size_info,
      memory_info = memory_info,
      cap_reason = "serial_parallel_false"
    ))
  }

  if (source_file_count < 1L) {
    return(appusage_worker_decision_record(
      requested_workers = requested,
      detected_logical_cores = available_cores,
      selected_workers = 0L,
      max_workers = max_workers,
      worker_cap_override = worker_cap_override,
      source_file_count = source_file_count,
      size_info = size_info,
      memory_info = memory_info,
      cap_reason = "no_inputs"
    ))
  }

  base <- min(requested, available_cores, source_file_count)
  cap_reason <- "requested_available_or_file_count"
  selected <- base

  if (isTRUE(worker_cap_override)) {
    cap_reason <- "explicit_worker_cap_override"
  } else {
    selected <- min(selected, max_workers)
    if (selected < base) {
      cap_reason <- "ordinary_max_workers_cap"
    }

    risk <- appusage_first_level_memory_risk(
      source_file_count = source_file_count,
      size_info = size_info,
      memory_info = memory_info
    )
    selected <- min(selected, risk$worker_cap)
    cap_reason <- risk$cap_reason
  }

  selected <- max(1L, as.integer(selected))
  appusage_worker_decision_record(
    requested_workers = requested,
    detected_logical_cores = available_cores,
    selected_workers = selected,
    max_workers = max_workers,
    worker_cap_override = worker_cap_override,
    source_file_count = source_file_count,
    size_info = size_info,
    memory_info = memory_info,
    cap_reason = cap_reason
  )
}

appusage_resolve_first_level_workers <- function(parallel,
                                                 n_cores,
                                                 x,
                                                 input = "file",
                                                 max_workers = 12L,
                                                 worker_cap_override = FALSE,
                                                 available_cores = parallel::detectCores(logical = TRUE),
                                                 memory_info = NULL,
                                                 size_info = NULL) {
  decision <- appusage_first_level_worker_decision(
    parallel = parallel,
    n_cores = n_cores,
    x = x,
    input = input,
    max_workers = max_workers,
    worker_cap_override = worker_cap_override,
    available_cores = available_cores,
    memory_info = memory_info,
    size_info = size_info
  )
  decision$selected_workers
}

appusage_source_size_info <- function(x, input = "file") {
  sizes <- numeric()
  if (identical(input, "file") && length(x) > 0L) {
    sizes <- suppressWarnings(file.info(x)$size)
    sizes <- sizes[!is.na(sizes)]
  }
  total_size <- if (length(sizes) > 0L) sum(sizes) else 0
  max_size <- if (length(sizes) > 0L) max(sizes) else 0
  average_size <- if (length(sizes) > 0L) mean(sizes) else 0
  list(
    total_source_size_bytes = as.numeric(total_size),
    max_source_size_bytes = as.numeric(max_size),
    average_source_size_bytes = as.numeric(average_size),
    n_sized_files = as.integer(length(sizes))
  )
}

appusage_detect_memory_info <- function() {
  out <- list(
    detected_total_memory_bytes = NA_real_,
    detected_free_memory_bytes = NA_real_,
    memory_detected = FALSE,
    memory_source = "unavailable"
  )
  if (!identical(.Platform$OS.type, "windows")) {
    return(out)
  }
  command <- paste(
    "try {",
    "$os = Get-CimInstance Win32_OperatingSystem;",
    "Write-Output $os.TotalVisibleMemorySize;",
    "Write-Output $os.FreePhysicalMemory",
    "} catch { exit 1 }"
  )
  values <- tryCatch(
    suppressWarnings(as.numeric(system2(
      "powershell",
      c("-NoProfile", "-Command", command),
      stdout = TRUE,
      stderr = FALSE
    ))),
    error = function(e) numeric()
  )
  values <- values[!is.na(values)]
  if (length(values) >= 2L) {
    out$detected_total_memory_bytes <- values[[1]] * 1024
    out$detected_free_memory_bytes <- values[[2]] * 1024
    out$memory_detected <- TRUE
    out$memory_source <- "Win32_OperatingSystem"
  }
  out
}

appusage_first_level_memory_risk <- function(source_file_count,
                                             size_info,
                                             memory_info) {
  gb <- 1024^3
  total_size <- size_info$total_source_size_bytes %||% 0
  max_size <- size_info$max_source_size_bytes %||% 0
  average_size <- size_info$average_source_size_bytes %||% 0
  memory_detected <- isTRUE(memory_info$memory_detected)
  total_memory <- memory_info$detected_total_memory_bytes %||% NA_real_
  free_memory <- memory_info$detected_free_memory_bytes %||% NA_real_

  if (max_size > 200 * 1024^2) {
    return(list(worker_cap = 4L, cap_reason = "large_file_risk"))
  }
  if (max_size > 75 * 1024^2 || average_size > 15 * 1024^2) {
    return(list(worker_cap = 6L, cap_reason = "moderate_large_file_risk"))
  }
  if (!memory_detected) {
    if (source_file_count >= 5000L || total_size > 2 * gb) {
      return(list(worker_cap = 6L, cap_reason = "conservative_large_project_memory_unknown"))
    }
    if (source_file_count >= 1000L || total_size > gb) {
      return(list(worker_cap = 8L, cap_reason = "conservative_medium_project_memory_unknown"))
    }
    return(list(worker_cap = Inf, cap_reason = "memory_unknown_low_project_risk"))
  }
  if (!is.na(free_memory) && free_memory < 8 * gb) {
    return(list(worker_cap = 4L, cap_reason = "low_free_memory"))
  }
  if (!is.na(free_memory) && free_memory < 16 * gb) {
    return(list(worker_cap = 8L, cap_reason = "moderate_free_memory"))
  }
  if (!is.na(total_memory) && total_memory >= 32 * gb &&
    !is.na(free_memory) && free_memory >= 16 * gb) {
    return(list(worker_cap = Inf, cap_reason = "adaptive_low_memory_risk"))
  }
  if (source_file_count >= 5000L || total_size > 2 * gb) {
    return(list(worker_cap = 8L, cap_reason = "adaptive_large_project_general_cap"))
  }
  list(worker_cap = Inf, cap_reason = "adaptive_general_low_risk")
}

appusage_worker_decision_record <- function(requested_workers,
                                            detected_logical_cores,
                                            selected_workers,
                                            max_workers,
                                            worker_cap_override,
                                            source_file_count,
                                            size_info,
                                            memory_info,
                                            cap_reason) {
  list(
    requested_workers = as.integer(requested_workers),
    detected_logical_cores = as.integer(detected_logical_cores),
    selected_workers = as.integer(selected_workers),
    max_workers = as.integer(max_workers),
    worker_cap_override = isTRUE(worker_cap_override),
    source_file_count = as.integer(source_file_count),
    total_source_size_bytes = as.numeric(size_info$total_source_size_bytes %||% NA_real_),
    max_source_size_bytes = as.numeric(size_info$max_source_size_bytes %||% NA_real_),
    average_source_size_bytes = as.numeric(size_info$average_source_size_bytes %||% NA_real_),
    n_sized_files = as.integer(size_info$n_sized_files %||% NA_integer_),
    detected_total_memory_bytes = as.numeric(memory_info$detected_total_memory_bytes %||% NA_real_),
    detected_free_memory_bytes = as.numeric(memory_info$detected_free_memory_bytes %||% NA_real_),
    memory_detected = isTRUE(memory_info$memory_detected),
    memory_source = memory_info$memory_source %||% NA_character_,
    cap_reason = cap_reason
  )
}

appusage_read_first_level_checkpoint <- function(path) {
  if (!is_present_string(path) || !file.exists(path)) {
    return(NULL)
  }
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )
}

appusage_read_first_level_resume_seed <- function(project_root,
                                                  checkpoint_file = NULL,
                                                  resume = FALSE,
                                                  overwrite = FALSE) {
  if (!isTRUE(resume) || isTRUE(overwrite) || !is_present_string(project_root)) {
    return(NULL)
  }

  final_summary <- appusage_read_first_level_checkpoint(file.path(
    project_root,
    "analytic_summary_table_proclevel-1.csv"
  ))
  checkpoint <- appusage_read_first_level_checkpoint(checkpoint_file)

  seeds <- Filter(Negate(is.null), list(final_summary, checkpoint))
  if (length(seeds) == 0L) {
    return(NULL)
  }

  seed <- do.call(bind_appusage_summary_rows, seeds)
  if (!"index" %in% names(seed) || nrow(seed) == 0L) {
    return(seed)
  }
  seed$index <- suppressWarnings(as.integer(seed$index))
  seed <- seed[!is.na(seed$index), , drop = FALSE]
  seed <- seed[!duplicated(seed$index, fromLast = TRUE), , drop = FALSE]
  seed[order(seed$index), , drop = FALSE]
}

appusage_write_first_level_checkpoint <- function(rows, path) {
  if (!is_present_string(path)) {
    return(invisible(NULL))
  }
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(invisible(NULL))
  }
  summary <- do.call(bind_appusage_summary_rows, rows)
  if ("index" %in% names(summary)) {
    summary <- summary[order(summary$index, na.last = TRUE), , drop = FALSE]
  }
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(summary, path, row.names = FALSE, na = "")
  appusage_refresh_workflow_checkpoint_safely(
    project_root = dirname(path),
    stage = "first_level",
    checkpoint_path = path,
    row_count = nrow(summary)
  )
  invisible(path)
}

appusage_restore_checkpoint_rows <- function(existing_rows, n) {
  rows <- vector("list", n)
  if (is.null(existing_rows) || nrow(existing_rows) == 0L || !"index" %in% names(existing_rows)) {
    return(rows)
  }
  existing_rows$index <- suppressWarnings(as.integer(existing_rows$index))
  existing_rows <- existing_rows[!is.na(existing_rows$index), , drop = FALSE]
  existing_rows <- existing_rows[existing_rows$index >= 1L & existing_rows$index <= n, ,
    drop = FALSE
  ]
  if (nrow(existing_rows) == 0L) {
    return(rows)
  }
  existing_rows <- existing_rows[!duplicated(existing_rows$index, fromLast = TRUE), ,
    drop = FALSE
  ]
  for (i in seq_len(nrow(existing_rows))) {
    row <- existing_rows[i, , drop = FALSE]
    if (appusage_first_level_seed_row_complete(row)) {
      rows[[existing_rows$index[i]]] <- row
    }
  }
  rows
}

appusage_index_seed_rows <- function(existing_rows, n) {
  rows <- vector("list", n)
  if (is.null(existing_rows) || nrow(existing_rows) == 0L || !"index" %in% names(existing_rows)) {
    return(rows)
  }
  existing_rows$index <- suppressWarnings(as.integer(existing_rows$index))
  existing_rows <- existing_rows[!is.na(existing_rows$index), , drop = FALSE]
  existing_rows <- existing_rows[existing_rows$index >= 1L & existing_rows$index <= n, ,
    drop = FALSE
  ]
  existing_rows <- existing_rows[!duplicated(existing_rows$index, fromLast = TRUE), ,
    drop = FALSE
  ]
  for (i in seq_len(nrow(existing_rows))) {
    rows[[existing_rows$index[i]]] <- existing_rows[i, , drop = FALSE]
  }
  rows
}

appusage_first_level_seed_row_complete <- function(row) {
  status <- appusage_get_col_value(row, "status", NA_character_)
  family <- appusage_get_col_value(row, "failure_family", NA_character_)

  if (identical(status, "success")) {
    data_file <- appusage_get_col_value(row, "data_file", NA_character_)
    metadata_file <- appusage_get_col_value(row, "metadata_file", NA_character_)
    return(is_present_string(data_file) && file.exists(data_file) &&
      is_present_string(metadata_file) && file.exists(metadata_file))
  }

  if (identical(status, "error")) {
    return(!identical(family, "memory_allocation"))
  }

  FALSE
}

appusage_first_level_seed_requires_overwrite <- function(row) {
  if (is.null(row) || nrow(row) == 0L) {
    return(FALSE)
  }
  status <- appusage_get_col_value(row, "status", NA_character_)
  if (identical(status, "not_processed")) {
    return(FALSE)
  }
  TRUE
}

appusage_first_level_seed_is_memory_retry <- function(row) {
  if (is.null(row) || nrow(row) == 0L) {
    return(FALSE)
  }
  identical(appusage_get_col_value(row, "status", NA_character_), "error") &&
    identical(appusage_get_col_value(row, "failure_family", NA_character_), "memory_allocation")
}

appusage_first_level_summary_complete <- function(summary) {
  if (is.character(summary) && length(summary) == 1L) {
    summary <- appusage_read_first_level_checkpoint(summary)
  }
  if (is.null(summary) || nrow(summary) == 0L || !"status" %in% names(summary)) {
    return(FALSE)
  }
  if (any(summary$status %in% c("not_processed", "incomplete"), na.rm = TRUE)) {
    return(FALSE)
  }
  if ("failure_family" %in% names(summary) &&
    any(summary$failure_family == "memory_allocation", na.rm = TRUE)) {
    return(FALSE)
  }
  success <- summary$status == "success"
  if (any(success, na.rm = TRUE)) {
    if (!all(c("data_file", "metadata_file") %in% names(summary))) {
      return(FALSE)
    }
    data_ok <- vapply(summary$data_file[success], function(path) {
      is_present_string(path) && file.exists(path)
    }, logical(1))
    meta_ok <- vapply(summary$metadata_file[success], function(path) {
      is_present_string(path) && file.exists(path)
    }, logical(1))
    if (!all(data_ok & meta_ok)) {
      return(FALSE)
    }
  }
  TRUE
}

appusage_first_level_summary_counts <- function(summary) {
  if (is.null(summary) || nrow(summary) == 0L || !"status" %in% names(summary)) {
    return(list(
      n_success_complete = 0L,
      n_recorded_errors = 0L,
      n_incomplete_cache = 0L,
      n_not_processed = 0L
    ))
  }
  list(
    n_success_complete = sum(summary$status == "success", na.rm = TRUE),
    n_recorded_errors = sum(summary$status == "error", na.rm = TRUE),
    n_incomplete_cache = sum(summary$status == "incomplete", na.rm = TRUE),
    n_not_processed = sum(summary$status == "not_processed", na.rm = TRUE)
  )
}
