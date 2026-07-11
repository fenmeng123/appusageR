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
#' @param n_cores Requested workers when `parallel = TRUE`; effective workers
#'   are capped by available cores and first-level memory-risk heuristics.
#' @param resume Whether to reuse durable first-level checkpoint rows when
#'   resuming an interrupted first-level batch.
#' @param checkpoint_every Write a durable checkpoint summary after this many
#'   processed files. Defaults to `progress_every`.
#' @param max_workers Maximum ordinary first-level parallel workers.
#' @param worker_cap_override Whether to bypass the ordinary first-level worker
#'   cap while still respecting available cores and file count.
#' @param retry_memory_allocation Whether to retry memory-allocation failures
#'   with a reduced worker count.
#' @param memory_retry_workers Worker count recorded for memory retries.
#' @param provenance Optional implementation provenance supplied by a project
#'   workflow. Standalone calls compute one provenance record for the batch.
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
                                parallel = FALSE, n_cores = 1,
                                resume = FALSE, checkpoint_every = NULL,
                                 max_workers = 12, worker_cap_override = FALSE,
                                 retry_memory_allocation = TRUE,
                                 memory_retry_workers = 1,
                                 provenance = NULL) {
  input <- match.arg(input, c("file", "text", "lines"))
  tz <- appusage_resolve_timezone(tz)
  provenance <- appusage_resolve_run_provenance(provenance, tz = tz)
  if (!identical(type, "auto") && !type %in% c("line", "meta", "day", "app")) {
    cli::cli_abort("`type` must be 'auto', 'line', 'meta', 'day', or 'app'.")
  }
  worker_decision <- appusage_first_level_worker_decision(
    parallel = parallel,
    n_cores = n_cores,
    x = x,
    input = input,
    max_workers = max_workers,
    worker_cap_override = worker_cap_override
  )
  n_cores <- worker_decision$selected_workers
  if (isTRUE(progress) && isTRUE(parallel)) {
    message(sprintf(
      "First-level worker decision: requested=%d selected=%d reason=%s override=%s",
      worker_decision$requested_workers,
      worker_decision$selected_workers,
      worker_decision$cap_reason,
      worker_decision$worker_cap_override
    ))
  }
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
    resume = resume,
    n_inputs = length(x),
    input = input,
    tz = tz
  )

  if (is.null(checkpoint_every)) {
    checkpoint_every <- progress_every
  }
  checkpoint_file <- NULL
  existing_checkpoint <- NULL
  if (!is.null(output_project$project_root)) {
    checkpoint_file <- file.path(
      output_project$project_root,
      "analytic_summary_table_proclevel-1.checkpoint.csv"
    )
    existing_checkpoint <- appusage_read_first_level_resume_seed(
      project_root = output_project$project_root,
      checkpoint_file = checkpoint_file,
      resume = resume,
      overwrite = overwrite
    )
  }

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
    n_cores = n_cores,
    checkpoint_every = checkpoint_every,
    checkpoint_file = checkpoint_file,
    existing_rows = existing_checkpoint,
    retry_memory_allocation = retry_memory_allocation,
    memory_retry_workers = memory_retry_workers,
    provenance = provenance
  )
  if (strict) {
    failed <- which(vapply(rows, function(z) !identical(z$status, "success"), logical(1)))
    if (length(failed) > 0) {
      i <- failed[[1]]
      cli::cli_abort("Batch preprocessing failed at record {i}: {rows[[i]]$error_message}")
    }
  }

  summary <- tibble::as_tibble(do.call(bind_appusage_summary_rows, rows))
  summary$first_level_requested_workers <- worker_decision$requested_workers
  summary$first_level_selected_workers <- worker_decision$selected_workers
  summary$first_level_worker_cap_reason <- worker_decision$cap_reason
  summary$first_level_worker_cap_override <- worker_decision$worker_cap_override
  summary <- appusage_attach_provenance_summary(summary, provenance)
  attr(summary, "first_level_worker_decision") <- worker_decision
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
#' @param provenance Optional implementation provenance supplied by a project
#'   workflow. Standalone calls compute one provenance record for the batch.
#' @param ... Additional arguments passed to `write_second_level_appusage()`.
#'
#' @return Invisibly returns a tibble summary for second-level writing.
#' @export
write_second_level_batch <- function(batch_summary, output_dir = NULL,
                                     overwrite = FALSE, resume = FALSE,
                                     progress = TRUE, parallel = FALSE,
                                     n_cores = 1, provenance = NULL, ...) {
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
  provenance <- appusage_resolve_run_provenance(provenance)
  second_level_args <- list(...)
  second_level_args$provenance <- provenance
  rows <- process_second_level_batch_rows(
    batch_summary = batch_summary,
    output_dir = output_dir,
    overwrite = overwrite,
    resume = resume,
    progress = progress,
    parallel = parallel,
    n_workers = n_workers,
    second_level_args = second_level_args
  )
  project_root <- infer_project_root_from_summary(batch_summary)
  previous_summary_file <- if (!is.na(project_root)) {
    file.path(project_root, "analytic_summary_table_proclevel-2.csv")
  } else {
    NA_character_
  }
  previous_summary <- if (is_present_string(previous_summary_file) &&
    file.exists(previous_summary_file)) {
    tryCatch(
      utils::read.csv(previous_summary_file, stringsAsFactors = FALSE),
      error = function(e) tibble::tibble()
    )
  } else {
    tibble::tibble()
  }
  summary <- refresh_second_level_summary_from_metadata(
    batch_summary = batch_summary,
    output_dir = output_dir,
    rows = rows,
    previous_summary = previous_summary
  )
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

refresh_second_level_summary_from_metadata <- function(batch_summary,
                                                       output_dir,
                                                       rows = list(),
                                                       previous_summary = NULL) {
  proc2_metadata <- if (!is.null(output_dir) && dir.exists(output_dir)) {
    sort(list.files(output_dir, pattern = "_proc-2[.]json$", full.names = TRUE))
  } else {
    character()
  }
  summary <- combine_second_level_batch_summary(proc2_metadata, rows, batch_summary)
  if (!is.null(previous_summary) && nrow(previous_summary) > 0L) {
    summary <- restore_previous_matching_fields(summary, previous_summary)
  }
  reconcile_second_level_summary_cardinality(
    summary,
    row_summary = tibble::tibble(),
    batch_summary = batch_summary
  )
}

#' Rerun second-level processing for a filtered project subset
#'
#' Rebuilds only selected first-level records from an existing project folder,
#' then refreshes the project-level second-level summary from current `proc-2`
#' metadata while preserving non-rebuilt rows such as upstream first-level
#' failures and self-report matching annotations.
#'
#' @param project_dir Project folder containing `analytic_summary_table_proclevel-1.csv`.
#' @param filter Selection rule for records to rebuild. A named list applies
#'   exact column matches, `*_regex` entries apply regular expressions to the
#'   matching column name, and `where` may be a function returning a logical
#'   vector. A bare function is also accepted.
#' @param output_dir Optional `proclevel-2` output directory.
#' @param eligible_only Whether to rebuild only first-level rows with
#'   `status == "success"` and an existing first-level `data_file`.
#' @param overwrite Whether selected second-level RDA/JSON pairs may be replaced.
#' @param resume Whether complete selected pairs should be skipped.
#' @param progress Whether to print progress messages.
#' @param parallel,n_cores Parallel controls passed to [write_second_level_batch()].
#' @param update_workflow_configuration Whether to append the subset rerun record
#'   to `workflow_configuration.rds`.
#' @param ... Additional arguments passed to [write_second_level_batch()].
#'
#' @return Invisibly returns an `appusage_second_level_subset_rerun` list.
#' @export
rerun_second_level_project_subset <- function(project_dir,
                                              filter = list(detected_type = "meta"),
                                              output_dir = NULL,
                                              eligible_only = TRUE,
                                              overwrite = TRUE,
                                              resume = FALSE,
                                              progress = TRUE,
                                              parallel = FALSE,
                                              n_cores = 1,
                                              update_workflow_configuration = TRUE,
                                              ...) {
  if (length(project_dir) != 1 || is.na(project_dir) || !dir.exists(project_dir)) {
    cli::cli_abort("`project_dir` must be an existing project directory.")
  }
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = TRUE)
  first_summary_file <- file.path(project_dir, "analytic_summary_table_proclevel-1.csv")
  if (!file.exists(first_summary_file)) {
    cli::cli_abort("Project is missing {.path analytic_summary_table_proclevel-1.csv}.")
  }
  first <- utils::read.csv(first_summary_file, stringsAsFactors = FALSE)
  if (nrow(first) == 0) {
    cli::cli_abort("First-level summary is empty: {.path {first_summary_file}}.")
  }
  old_summary_file <- file.path(project_dir, "analytic_summary_table_proclevel-2.csv")
  old_second <- if (file.exists(old_summary_file)) {
    utils::read.csv(old_summary_file, stringsAsFactors = FALSE)
  } else {
    tibble::tibble()
  }

  selected <- filter_second_level_rerun_candidates(first, filter)
  if (isTRUE(eligible_only)) {
    selected <- eligible_second_level_rerun_candidates(selected)
  }
  second_level_args <- list(...)
  if (isTRUE(progress)) {
    message(sprintf(
      "Selected %d/%d first-level rows for second-level subset rerun%s",
      nrow(selected), nrow(first),
      if (isTRUE(eligible_only)) " after first-level success/data-file eligibility filtering" else ""
    ))
  }
  if (nrow(selected) == 0) {
    summary <- tibble::as_tibble(old_second)
    config_file <- if (isTRUE(update_workflow_configuration)) {
      appusage_record_second_level_subset_rerun(
        project_dir = project_dir,
        filter = filter,
        selected = selected,
        summary = summary,
        output_dir = output_dir,
        eligible_only = eligible_only,
        overwrite = overwrite,
        resume = resume,
        parallel = parallel,
        n_cores = n_cores,
        second_level_args = second_level_args
      )
    } else {
      NA_character_
    }
    out <- list(
      project_dir = project_dir,
      selected_first_level = selected,
      second_level = summary,
      summary = summary,
      configuration_file = config_file,
      n_selected = 0L
    )
    class(out) <- c("appusage_second_level_subset_rerun", "list")
    return(invisible(out))
  }

  subset_summary <- write_second_level_batch(selected,
    output_dir = output_dir,
    overwrite = overwrite,
    resume = resume,
    progress = progress,
    parallel = parallel,
    n_cores = n_cores,
    ...
  )
  summary <- merge_subset_second_level_summary(
    old_summary = old_second,
    new_summary = subset_summary,
    first_summary = first
  )
  summary_file <- file.path(project_dir, "analytic_summary_table_proclevel-2.csv")
  utils::write.csv(summary, summary_file, row.names = FALSE, na = "")
  write_dataset_description_json(
    project_info_from_root(project_dir),
    summary = summary,
    proclevel = 2,
    summary_file = summary_file,
    status = "success"
  )
  config_file <- if (isTRUE(update_workflow_configuration)) {
    appusage_record_second_level_subset_rerun(
      project_dir = project_dir,
      filter = filter,
      selected = selected,
      summary = summary,
      output_dir = output_dir,
      eligible_only = eligible_only,
      overwrite = overwrite,
      resume = resume,
      parallel = parallel,
      n_cores = n_cores,
      second_level_args = second_level_args
    )
  } else {
    NA_character_
  }
  out <- list(
    project_dir = project_dir,
    selected_first_level = selected,
    second_level = subset_summary,
    summary = summary,
    configuration_file = config_file,
    n_selected = nrow(selected)
  )
  class(out) <- c("appusage_second_level_subset_rerun", "list")
  invisible(out)
}

filter_second_level_rerun_candidates <- function(batch_summary, filter = NULL) {
  batch_summary <- tibble::as_tibble(batch_summary)
  n <- nrow(batch_summary)
  if (is.null(filter)) {
    return(batch_summary)
  }
  if (is.function(filter)) {
    keep <- filter(batch_summary)
    return(batch_summary[appusage_validate_filter_result(keep, n), , drop = FALSE])
  }
  if (!is.list(filter)) {
    cli::cli_abort("`filter` must be NULL, a function, or a named list.")
  }
  keep <- rep(TRUE, n)
  for (nm in names(filter)) {
    value <- filter[[nm]]
    if (identical(nm, "where")) {
      if (!is.function(value)) {
        cli::cli_abort("`filter$where` must be a function.")
      }
      keep <- keep & appusage_validate_filter_result(value(batch_summary), n)
      next
    }
    if (grepl("_regex$", nm)) {
      col <- sub("_regex$", "", nm)
      if (!col %in% names(batch_summary)) {
        cli::cli_abort("Regex filter column is missing from `batch_summary`: {.field {col}}.")
      }
      text <- as.character(batch_summary[[col]])
      text[is.na(text)] <- ""
      keep <- keep & grepl(paste(value, collapse = "|"), text)
      next
    }
    if (!nm %in% names(batch_summary)) {
      cli::cli_abort("Filter column is missing from `batch_summary`: {.field {nm}}.")
    }
    column <- as.character(batch_summary[[nm]])
    keep <- keep & column %in% as.character(value)
  }
  batch_summary[keep %in% TRUE, , drop = FALSE]
}

eligible_second_level_rerun_candidates <- function(batch_summary) {
  batch_summary <- tibble::as_tibble(batch_summary)
  if (nrow(batch_summary) == 0) {
    return(batch_summary)
  }
  for (col in c("status", "data_file")) {
    if (!col %in% names(batch_summary)) {
      cli::cli_abort("First-level eligibility filtering requires a {.field {col}} column.")
    }
  }
  data_file <- as.character(batch_summary$data_file)
  data_file[is.na(data_file)] <- ""
  keep <- identical_first_level_success(batch_summary$status) &
    nzchar(data_file) &
    file.exists(data_file)
  batch_summary[keep %in% TRUE, , drop = FALSE]
}

identical_first_level_success <- function(status) {
  tolower(as.character(status)) == "success"
}

appusage_validate_filter_result <- function(keep, n) {
  if (!is.logical(keep) || length(keep) != n) {
    cli::cli_abort("Custom second-level filter must return a logical vector with length {n}.")
  }
  keep[is.na(keep)] <- FALSE
  keep
}

merge_subset_second_level_summary <- function(old_summary, new_summary, first_summary) {
  old_summary <- tibble::as_tibble(old_summary)
  new_summary <- tibble::as_tibble(new_summary)
  if (nrow(old_summary) == 0) {
    return(order_second_level_summary(new_summary, first_summary))
  }
  if (nrow(new_summary) == 0) {
    return(order_second_level_summary(old_summary, first_summary))
  }
  old_key <- second_level_summary_key(old_summary)
  new_key <- second_level_summary_key(new_summary)
  keep_old <- is.na(old_key) | !old_key %in% stats::na.omit(new_key)
  merged <- bind_appusage_summary_rows(new_summary, old_summary[keep_old, , drop = FALSE])
  merged <- restore_previous_matching_fields(merged, old_summary)
  reconcile_second_level_summary_cardinality(
    merged,
    row_summary = tibble::tibble(),
    batch_summary = first_summary
  )
}

restore_previous_matching_fields <- function(summary, old_summary) {
  matching_cols <- grep("^self_report_", names(old_summary), value = TRUE)
  if (length(matching_cols) == 0 || nrow(summary) == 0 || nrow(old_summary) == 0) {
    return(summary)
  }
  summary_key <- second_level_summary_key(summary)
  old_key <- second_level_summary_key(old_summary)
  old_idx <- match(summary_key, old_key)
  for (col in matching_cols) {
    if (!col %in% names(summary)) {
      summary[[col]] <- typed_summary_na(old_summary[[col]], nrow(summary))
    }
    has_old <- !is.na(old_idx)
    old_values <- old_summary[[col]][old_idx[has_old]]
    should_restore <- appusage_missing_summary_value(summary[[col]][has_old]) &
      !appusage_missing_summary_value(old_values)
    target <- which(has_old)[should_restore]
    if (length(target) > 0) {
      summary[[col]][target] <- old_values[should_restore]
    }
  }
  summary
}

typed_summary_na <- function(template, n) {
  if (is.integer(template)) {
    return(rep(NA_integer_, n))
  }
  if (is.numeric(template)) {
    return(rep(NA_real_, n))
  }
  if (is.logical(template)) {
    return(rep(NA, n))
  }
  rep(NA_character_, n)
}

appusage_missing_summary_value <- function(x) {
  if (is.character(x)) {
    return(is.na(x) | !nzchar(x))
  }
  is.na(x)
}

order_second_level_summary <- function(summary, first_summary) {
  summary <- tibble::as_tibble(summary)
  if (nrow(summary) == 0) {
    return(summary)
  }
  first_key <- first_level_summary_key(first_summary)
  summary_key <- second_level_summary_key(summary)
  order_index <- match(summary_key, first_key)
  summary <- summary[order(order_index, seq_len(nrow(summary)), na.last = TRUE), , drop = FALSE]
  tibble::as_tibble(summary)
}

first_level_summary_key <- function(summary) {
  summary <- tibble::as_tibble(summary)
  out <- rep(NA_character_, nrow(summary))
  if ("source_record_key" %in% names(summary)) {
    value <- as.character(summary$source_record_key)
    value[is.na(value) | !nzchar(value)] <- NA_character_
    out <- value
  }
  if ("data_file" %in% names(summary)) {
    value <- normalized_summary_path(summary$data_file)
    fill <- (is.na(out) | !nzchar(out)) & !is.na(value) & nzchar(value)
    out[fill] <- value[fill]
  }
  fallback <- appusage_identity_summary_key(summary)
  missing <- is.na(out) | !nzchar(out)
  out[missing] <- fallback[missing]
  out
}

second_level_summary_key <- function(summary) {
  summary <- tibble::as_tibble(summary)
  out <- rep(NA_character_, nrow(summary))
  if ("source_record_key" %in% names(summary)) {
    value <- as.character(summary$source_record_key)
    value[is.na(value) | !nzchar(value)] <- NA_character_
    out <- value
  }
  for (col in c("first_level_data_file", "first_level_rda", "data_file")) {
    if (col %in% names(summary)) {
      value <- normalized_summary_path(summary[[col]])
      fill <- (is.na(out) | !nzchar(out)) & !is.na(value) & nzchar(value)
      out[fill] <- value[fill]
    }
  }
  fallback <- appusage_identity_summary_key(summary)
  missing <- is.na(out) | !nzchar(out)
  out[missing] <- fallback[missing]
  out
}

normalized_summary_path <- function(x) {
  x <- as.character(x)
  missing <- is.na(x) | !nzchar(x)
  out <- x
  out[!missing] <- vapply(out[!missing], function(path) {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }, character(1))
  out[missing] <- NA_character_
  out
}

appusage_identity_summary_key <- function(summary) {
  summary <- tibble::as_tibble(summary)
  get_col <- function(col) {
    if (col %in% names(summary)) {
      value <- as.character(summary[[col]])
      value[is.na(value)] <- ""
      value
    } else {
      rep("", nrow(summary))
    }
  }
  paste(
    "identity",
    get_col("participant_id"),
    get_col("detected_type"),
    get_col("filename_export_type"),
    get_col("wenjuanxing_sequence_id"),
    sep = "\r"
  )
}

appusage_record_second_level_subset_rerun <- function(project_dir, filter, selected,
                                                      summary, output_dir,
                                                      eligible_only,
                                                      overwrite, resume,
                                                      parallel, n_cores,
                                                      second_level_args) {
  config_file <- file.path(project_dir, "workflow_configuration.rds")
  config <- if (file.exists(config_file)) {
    readRDS(config_file)
  } else {
    list(
      package_version = as.character(utils::packageVersion("appusageR")),
      output_schema_version = "0.3.0",
      created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
      output_study_dir = project_dir
    )
  }
  event <- list(
    operation = "second_level_subset_rerun",
    run_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"),
    filter = appusage_serializable_filter(filter),
    n_selected = nrow(selected),
    selected_detected_type_counts = appusage_named_count_list(selected, "detected_type"),
    n_summary_rows = nrow(summary),
    output_dir = appusage_normalize_optional_path(output_dir %||% file.path(project_dir, "proclevel-2")),
    eligible_only = isTRUE(eligible_only),
    overwrite = isTRUE(overwrite),
    resume = isTRUE(resume),
    parallel = isTRUE(parallel),
    n_cores = n_cores,
    second_level_options = appusage_serializable_list(second_level_args)
  )
  history <- config$second_level_rerun_history
  if (is.null(history)) {
    history <- list()
  }
  config$second_level_rerun_history <- c(history, list(event))
  config$latest_second_level_rerun <- event
  config$latest_run_at <- event$run_at
  saveRDS(config, config_file)
  normalizePath(config_file, winslash = "/", mustWork = FALSE)
}

appusage_named_count_list <- function(data, column) {
  if (!column %in% names(data) || nrow(data) == 0) {
    return(list())
  }
  counts <- table(as.character(data[[column]]), useNA = "ifany")
  as.list(stats::setNames(as.integer(counts), names(counts)))
}

appusage_serializable_filter <- function(filter) {
  if (is.null(filter)) {
    return(list(type = "all"))
  }
  if (is.function(filter)) {
    return(list(type = "function", label = paste(deparse(filter), collapse = "\n")))
  }
  if (is.list(filter)) {
    return(list(type = "list", criteria = appusage_serializable_list(filter)))
  }
  list(type = class(filter)[[1]], value = as.character(filter))
}

appusage_serializable_list <- function(x) {
  if (length(x) == 0) {
    return(list())
  }
  out <- vector("list", length(x))
  names(out) <- names(x)
  for (i in seq_along(x)) {
    value <- x[[i]]
    out[[i]] <- if (is.function(value)) {
      list(type = "function", label = paste(deparse(value), collapse = "\n"))
    } else if (is.list(value) && !is.data.frame(value)) {
      appusage_serializable_list(value)
    } else if (is.atomic(value)) {
      as.character(value)
    } else {
      paste(utils::capture.output(utils::str(value, give.attr = FALSE)), collapse = "\n")
    }
  }
  out
}

combine_second_level_batch_summary <- function(proc2_metadata, rows, batch_summary) {
  row_summary <- if (length(rows) > 0L) {
    do.call(bind_appusage_summary_rows, rows)
  } else {
    tibble::tibble()
  }
  metadata_summary <- if (length(proc2_metadata) > 0) {
    build_qc_summary_from_metadata(proc2_metadata)
  } else {
    tibble::tibble()
  }
  skipped_summary <- second_level_skipped_summary_rows(row_summary, batch_summary)

  summary <- bind_appusage_summary_rows(metadata_summary, skipped_summary)
  summary <- merge_second_level_row_diagnostics(summary, row_summary)
  reconcile_second_level_summary_cardinality(summary, row_summary, batch_summary)
}

reconcile_second_level_summary_cardinality <- function(summary, row_summary,
                                                        batch_summary) {
  summary <- tibble::as_tibble(summary)
  row_summary <- tibble::as_tibble(row_summary)
  batch_summary <- tibble::as_tibble(batch_summary)
  if (nrow(batch_summary) == 0L) {
    return(summary[0, , drop = FALSE])
  }
  first_keys <- first_level_summary_key(batch_summary)
  summary_keys <- second_level_summary_key(summary)
  used <- rep(FALSE, nrow(summary))
  output <- vector("list", nrow(batch_summary))

  for (i in seq_len(nrow(batch_summary))) {
    candidates <- which(!used & !is.na(summary_keys) & summary_keys == first_keys[[i]])
    row_index <- if ("index" %in% names(row_summary) && "index" %in% names(batch_summary)) {
      match(batch_summary$index[[i]], row_summary$index)
    } else {
      NA_integer_
    }
    preferred_json <- if (!is.na(row_index)) {
      appusage_summary_cell(row_summary, "second_level_metadata_file", row_index)
    } else {
      NA_character_
    }
    chosen <- NA_integer_
    if (length(candidates) > 0L && is_present_string(preferred_json)) {
      candidate_paths <- rep(NA_character_, length(candidates))
      for (col in c("second_level_metadata_file", "metadata_json")) {
        if (col %in% names(summary)) {
          values <- normalized_summary_path(summary[[col]][candidates])
          fill <- is.na(candidate_paths) & !is.na(values)
          candidate_paths[fill] <- values[fill]
        }
      }
      matched <- which(candidate_paths == normalized_summary_path(preferred_json))
      if (length(matched) > 0L) {
        chosen <- candidates[[matched[[1]]]]
      }
    }
    if (is.na(chosen) && length(candidates) > 0L) {
      chosen <- candidates[[1]]
    }
    if (!is.na(chosen)) {
      used[[chosen]] <- TRUE
      record <- summary[chosen, , drop = FALSE]
    } else {
      record <- second_level_summary_fallback_row(
        batch_summary, i, row_summary, row_index
      )
    }
    duplicate_count <- max(
      sum(first_keys == first_keys[[i]], na.rm = TRUE),
      length(candidates)
    )
    source_key <- appusage_summary_cell(batch_summary, "source_record_key", i)
    record$source_key_duplicate_count <- as.integer(duplicate_count)
    record$source_key_diagnostic <- if (!is_present_string(source_key)) {
      "legacy_identity_fallback"
    } else if (duplicate_count > 1L) {
      "duplicate_source_key"
    } else if (is.na(chosen)) {
      "missing_summary_candidate"
    } else {
      "ok"
    }
    output[[i]] <- record
  }
  do.call(bind_appusage_summary_rows, output)
}

second_level_summary_fallback_row <- function(batch_summary, index,
                                              row_summary = tibble::tibble(),
                                              row_index = NA_integer_) {
  first_status <- as.character(batch_summary$status[[index]])
  second_status <- if (identical(first_status, "success")) "incomplete" else "skipped"
  row_status <- if (!is.na(row_index)) {
    as.character(appusage_summary_cell(row_summary, "status", row_index, second_status))
  } else {
    second_status
  }
  out <- data.frame(
    index = appusage_summary_cell(batch_summary, "index", index, index),
    participant_id = appusage_summary_cell(batch_summary, "participant_id", index),
    participant_id_source = appusage_summary_cell(batch_summary, "participant_id_source", index),
    wenjuanxing_sequence_id = appusage_summary_cell(batch_summary, "wenjuanxing_sequence_id", index, NA_integer_),
    source_record_key = appusage_summary_cell(batch_summary, "source_record_key", index),
    source_fingerprint = appusage_summary_cell(batch_summary, "source_fingerprint", index),
    source_cache_key = appusage_summary_cell(batch_summary, "source_cache_key", index),
    detected_type = appusage_summary_cell(batch_summary, "detected_type", index),
    filename_export_type = appusage_summary_cell(batch_summary, "filename_export_type", index),
    first_level_status = first_status,
    second_level_status = row_status,
    status = row_status,
    skip_reason = if (identical(first_status, "success")) {
      "missing_or_incomplete_proc2_metadata"
    } else {
      "upstream_first_level_error"
    },
    first_level_data_file = appusage_summary_cell(batch_summary, "data_file", index),
    second_level_data_file = if (!is.na(row_index)) appusage_summary_cell(row_summary, "second_level_data_file", row_index) else NA_character_,
    second_level_metadata_file = if (!is.na(row_index)) appusage_summary_cell(row_summary, "second_level_metadata_file", row_index) else NA_character_,
    error_message = if (!is.na(row_index)) appusage_summary_cell(row_summary, "error_message", row_index) else appusage_summary_cell(batch_summary, "error_message", index),
    stringsAsFactors = FALSE
  )
  for (col in grep("^self_report_", names(batch_summary), value = TRUE)) {
    out[[col]] <- batch_summary[[col]][[index]]
  }
  tibble::as_tibble(out)
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
  for (col in c("error_class", "worker_stage")) {
    if (!col %in% names(summary)) {
      summary[[col]] <- NA_character_
    }
  }
  for (col in c("worker_task_index", "worker_pid")) {
    if (!col %in% names(summary)) {
      summary[[col]] <- NA_integer_
    }
  }
  matched <- match(second_level_summary_key(error_rows), second_level_summary_key(summary))
  for (i in which(!is.na(matched))) {
    j <- matched[[i]]
    if (is.na(j)) {
      next
    }
    if (!is_present_string(summary$error_message[[j]]) &&
      "error_message" %in% names(error_rows)) {
      summary$error_message[[j]] <- error_rows$error_message[[i]]
    }
    for (col in c("second_level_metadata_file", "second_level_data_file", "error_class", "worker_stage")) {
      if (col %in% names(summary) && col %in% names(error_rows) &&
        !is_present_string(summary[[col]][[j]]) &&
        is_present_string(error_rows[[col]][[i]])) {
        summary[[col]][[j]] <- error_rows[[col]][[i]]
      }
    }
    for (col in c("worker_task_index", "worker_pid")) {
      if (col %in% names(summary) && col %in% names(error_rows) &&
        (is.na(summary[[col]][[j]]) || !nzchar(as.character(summary[[col]][[j]]))) &&
        !is.na(error_rows[[col]][[i]])) {
        summary[[col]][[j]] <- error_rows[[col]][[i]]
      }
    }
  }
  summary
}

second_level_skipped_summary_rows <- function(row_summary, batch_summary) {
  if (nrow(row_summary) == 0L || !"status" %in% names(row_summary)) {
    return(tibble::tibble())
  }
  skipped <- row_summary[row_summary$status == "skipped", , drop = FALSE]
  if ("skip_reason" %in% names(skipped)) {
    skipped <- skipped[!skipped$skip_reason %in% "existing_proc2_cache", , drop = FALSE]
  }
  if (nrow(skipped) == 0) {
    return(tibble::tibble())
  }

  source_indices <- match(skipped$index, batch_summary$index)
  rows <- lapply(seq_len(nrow(skipped)), function(i) {
    source_index <- source_indices[[i]]
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
      source_record_key = appusage_summary_cell(batch_summary, "source_record_key", source_index),
      source_fingerprint = appusage_summary_cell(batch_summary, "source_fingerprint", source_index),
      source_cache_key = appusage_summary_cell(batch_summary, "source_cache_key", source_index),
      detected_type = batch_summary$detected_type[[source_index]],
      filename_export_type = batch_summary$filename_export_type[[source_index]],
      export_type_match = batch_summary$export_type_match[[source_index]],
      first_level_status = first_level_status,
      second_level_status = "skipped",
      qc_status = "not_run",
      app_category_status = "not_run",
      status = "skipped",
      skip_reason = skip_reason,
      pair_state = "not_applicable",
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
                                    tz, encoding, overwrite, index,
                                    memory_risk_signal = FALSE,
                                    memory_risk_reason = NA_character_,
                                    provenance = NULL) {
  warnings <- character()
  started_at <- Sys.time()
  source_file <- source_file_label(x, input)
  participant_id <- id_info$participant_id[[1]]
  participant_id_source <- id_info$participant_id_source[[1]]
  provenance <- appusage_resolve_run_provenance(provenance, tz = tz)
  detected_type <- NA_character_
  metadata_file <- NA_character_
  data_file <- NA_character_
  preflight <- NULL
  structural_quality <- NULL
  source_identity <- appusage_source_identity(
    x = x,
    input = input,
    id_info = id_info,
    participant_id = participant_id,
    export_type = "unknown"
  )

  result <- tryCatch(
    withCallingHandlers(
      {
        preflight <- appusage_source_preflight(
          x = x,
          input = input,
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
          stop(batch_unsupported_error(detected_type))
        }
        if (isTRUE(preflight$filename_content_disagreement)) {
          warnings <- c(warnings, paste0(
            "Filename export type '", preflight$filename_type,
            "' disagrees with content-selected component '", detected_type,
            "'; content selection was used."
          ))
        }
        source_identity <- appusage_source_identity(
          x = x,
          input = input,
          id_info = id_info,
          participant_id = participant_id,
          export_type = detected_type,
          fingerprint = source_identity$source_fingerprint
        )

        parsed_data <- switch(detected_type,
          line = parse_line(
            parse_x,
            input = parse_input, participant_id = participant_id,
            source_file = source_file, tz = tz, encoding = encoding,
            strict = TRUE
          ),
          meta = parse_meta(
            parse_x,
            input = parse_input, participant_id = participant_id,
            source_file = source_file, tz = tz, encoding = encoding,
            strict = TRUE
          ),
          day = parse_day(
            parse_x,
            input = parse_input, participant_id = participant_id,
            source_file = source_file, tz = tz, encoding = encoding,
            strict = TRUE
          ),
          app = parse_app(
            parse_x,
            input = parse_input, participant_id = participant_id,
            source_file = source_file, tz = tz, encoding = encoding,
            strict = TRUE
          )
        )
        if (identical(detected_type, "line")) {
          parsed_diagnostics <- parser_diagnostics(parsed_data)
          structural_quality <- parsed_diagnostics$format_specific$structural_quality %||% list()
          if (isTRUE(structural_quality$critical)) {
            stop(appusage_line_structural_quality_error(parsed_diagnostics))
          }
        }

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
          data_file = NA_character_,
          preflight = preflight,
          memory_risk_signal = memory_risk_signal,
          source_identity = source_identity,
          provenance = provenance
        )

        if (!is.null(output_dir)) {
          data_file <- file.path(
            output_dir,
            build_appusage_filename(
              participant_id = participant_id,
              export_type = detected_type,
              proc = 1,
              extension = "rda",
              source_key = source_identity$source_cache_key
            )
          )
          metadata_file <- file.path(
            output_dir,
            build_appusage_filename(
              participant_id = participant_id,
              export_type = detected_type,
              proc = 1,
              extension = "json",
              source_key = source_identity$source_cache_key
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
  if (is.null(structural_quality) && !is.null(error)) {
    structural_quality <- error$structural_quality %||%
      condition_parser_diagnostics(error)$format_specific$structural_quality
  }
  source_identity <- appusage_source_identity(
    x = x,
    input = input,
    id_info = id_info,
    participant_id = participant_id,
    export_type = ifelse(is.na(detected_type), "unknown", detected_type),
    fingerprint = source_identity$source_fingerprint
  )
  if (!is.null(error) && !is.null(output_dir)) {
    metadata_file <- file.path(
      output_dir,
      build_appusage_filename(
        participant_id = participant_id,
        export_type = ifelse(is.na(detected_type), "unknown", detected_type),
        proc = 1,
        extension = "json",
        source_key = source_identity$source_cache_key
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
      data_file = NA_character_,
      preflight = preflight,
      memory_risk_signal = memory_risk_signal,
      source_identity = source_identity,
      provenance = provenance
    )
    write_metadata_json(error_info, metadata_file)
  }
  preflight_fields <- appusage_preflight_summary_fields(preflight)
  structural_fields <- appusage_structural_quality_summary_fields(structural_quality)
  row <- data.frame(
    index = index,
    participant_id = participant_id,
    participant_id_source = participant_id_source,
    wenjuanxing_sequence_id = id_info$wenjuanxing_sequence_id[[1]],
    filename_parse_status = id_info$filename_parse_status[[1]],
    filename_parse_warning = id_info$filename_parse_warning[[1]],
    native_export_file_name = id_info$native_export_file_name[[1]],
    filename_export_type = id_info$native_export_type_from_filename[[1]],
    native_export_type_raw = id_info$native_export_type_raw[[1]],
    native_export_created_at = id_info$native_export_created_at[[1]],
    export_type_match = filename_export_type_match(id_info, detected_type),
    source_file = source_file,
    source_record_key = source_identity$source_record_key,
    source_fingerprint = source_identity$source_fingerprint,
    source_cache_key = source_identity$source_cache_key,
    detected_type = detected_type,
    effective_timezone = appusage_resolve_timezone(tz),
    status = result$status,
    metadata_file = metadata_file,
    data_file = data_file,
    n_rows = result$n_rows,
    n_parse_warnings = result$n_parse_warnings,
    preflight_status = preflight_fields$preflight_status,
    detected_components = preflight_fields$detected_components,
    selected_component = preflight_fields$selected_component,
    mixed_content = preflight_fields$mixed_content,
    component_selection_rule = preflight_fields$component_selection_rule,
    filename_content_disagreement = preflight_fields$filename_content_disagreement,
    structural_boundary_count = preflight_fields$structural_boundary_count,
    structural_quality_status = structural_fields$structural_quality_status,
    structural_quality_critical = structural_fields$structural_quality_critical,
    structural_quality_warning = structural_fields$structural_quality_warning,
    structural_valid_interval_ratio = structural_fields$structural_valid_interval_ratio,
    structural_candidate_rows = structural_fields$structural_candidate_rows,
    structural_parsed_rows = structural_fields$structural_parsed_rows,
    structural_missing_timestamp_count = structural_fields$structural_missing_timestamp_count,
    structural_missing_duration_count = structural_fields$structural_missing_duration_count,
    structural_header_contamination_count = structural_fields$structural_header_contamination_count,
    structural_exact_duplicate_count = structural_fields$structural_exact_duplicate_count,
    structural_exact_duplicate_ratio = structural_fields$structural_exact_duplicate_ratio,
    structural_malformed_identity_count = structural_fields$structural_malformed_identity_count,
    structural_date_mismatch_count = structural_fields$structural_date_mismatch_count,
    structural_critical_reasons = structural_fields$structural_critical_reasons,
    structural_warning_reasons = structural_fields$structural_warning_reasons,
    preflight_has_record_rows = preflight_fields$preflight_has_record_rows,
    binary_signature = preflight_fields$binary_signature,
    nul_byte_ratio = preflight_fields$nul_byte_ratio,
    control_byte_ratio = preflight_fields$control_byte_ratio,
    encoding_attempted_candidates = preflight_fields$encoding_attempted_candidates,
    encoding_supported_candidates = preflight_fields$encoding_supported_candidates,
    encoding_selected = preflight_fields$encoding_selected,
    encoding_conversion_failures = preflight_fields$encoding_conversion_failures,
    memory_risk_signal = isTRUE(memory_risk_signal),
    memory_risk_reason = memory_risk_reason,
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
  appusage_attach_provenance_summary(row, provenance)
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

first_level_detect_type <- function(x, input, type, encoding, id_info,
                                    preflight = NULL) {
  native_type_raw <- if ("native_export_type_raw" %in% names(id_info)) {
    id_info$native_export_type_raw[[1]]
  } else {
    NA_character_
  }
  if (is_present_string(native_type_raw) &&
    identical(tolower(trimws(native_type_raw)), "unlock")) {
    return("unknown")
  }
  if (!is.null(preflight) && is_present_string(preflight$selected_component)) {
    return(preflight$selected_component)
  }
  components <- appusage_detect_components_from_lines(
    read_appusage_lines(x, input = input, encoding = encoding)
  )
  if (length(components) == 1L) components[[1]] else "unknown"
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
                           data, warnings, error, metadata_file, data_file,
                           preflight = NULL,
                           memory_risk_signal = FALSE,
                           source_identity = list(),
                           provenance = NULL) {
  source_meta <- source_metadata(source_file, input)
  compact_preflight <- appusage_compact_source_preflight(preflight)
  source_meta$preflight <- compact_preflight
  source_meta$source_fingerprint <- source_identity$source_fingerprint %||% NA_character_
  source_meta$source_cache_key <- source_identity$source_cache_key %||% NA_character_
  parser_diag <- first_level_parser_diagnostics(data, error)
  metadata <- list(
    schema_version = appusage_output_schema_version(),
    package_version = as.character(utils::packageVersion("appusageR")),
    parser_version = as.character(utils::packageVersion("appusageR")),
    created_at = format(finished_at, "%Y-%m-%dT%H:%M:%OS3%z"),
    updated_at = format(finished_at, "%Y-%m-%dT%H:%M:%OS3%z"),
    participant_id = participant_id,
    participant_id_source = participant_id_source,
    identity = list(
      participant_id = participant_id,
      participant_id_source = participant_id_source,
      wenjuanxing_sequence_id = id_info$wenjuanxing_sequence_id[[1]],
      source_record_key = source_identity$source_record_key %||% NA_character_,
      source_cache_key = source_identity$source_cache_key %||% NA_character_
    ),
    source = source_meta,
    export = list(
      detected_type = export_type,
      content_detected_export_type = export_type,
      native_export_type_from_filename = id_info$native_export_type_from_filename[[1]],
      native_export_type_raw = id_info$native_export_type_raw[[1]],
      export_type_match = export_type_match,
      type_resolution_rule = first_level_type_resolution_rule(id_info, export_type),
      filename_content_relation = first_level_filename_content_relation(id_info, export_type),
      native_export_created_at = id_info$native_export_created_at[[1]],
      input = input,
      encoding = encoding,
      encoding_diagnostics = compact_preflight$encoding %||% list(),
      detected_components = compact_preflight$detected_components %||% character(),
      selected_component = compact_preflight$selected_component %||% export_type,
      mixed_content = compact_preflight$mixed_content %||% FALSE,
      component_selection_rule = compact_preflight$selection_rule %||% NA_character_,
      filename_content_disagreement = compact_preflight$filename_content_disagreement %||% FALSE,
      boundary_diagnostics = compact_preflight$boundary_diagnostics %||% list(),
      timezone = tz
    ),
    processing = list(
      first_level_status = status,
      first_level_failure_reason = first_level_failure_reason(error),
      memory_risk_signal = isTRUE(memory_risk_signal),
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
    warning_messages = unique(warnings),
    implementation_provenance = appusage_resolve_run_provenance(provenance, tz = tz)
  )
  metadata$structural_quality <- parser_diag$format_specific$structural_quality %||% list(
    status = "not_applicable"
  )
  metadata
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
  if (inherits(error, "appusage_source_preflight_error")) {
    return(error$source_preflight$failure_family %||% "source_preflight_failure")
  }
  if (inherits(error, "appusage_line_structural_quality")) {
    return("structural_quality_critical")
  }
  error_message <- tryCatch(conditionMessage(error), error = function(e) "")
  if (appusage_is_memory_allocation_text(class(error), error_message)) {
    return("memory_allocation")
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

appusage_stop_cluster_safely <- function(cluster,
                                         stop_cluster = parallel::stopCluster) {
  tryCatch(
    {
      stop_cluster(cluster)
      invisible(NULL)
    },
    error = function(e) invisible(e)
  )
}

appusage_parallel_lapply_lb <- function(cluster, tasks, fun) {
  parallel::parLapplyLB(cluster, tasks, fun)
}

appusage_is_cluster_transport_error <- function(error) {
  if (inherits(error, c(
    "appusage_psock_transport_error",
    "simulated_cluster_transport_error"
  ))) {
    return(TRUE)
  }
  message <- tolower(conditionMessage(error))
  condition_call <- conditionCall(error)
  call_text <- if (is.null(condition_call)) {
    ""
  } else {
    tolower(paste(deparse(condition_call), collapse = " "))
  }
  patterns <- c(
    "error (reading|writing) from connection",
    "error in unserialize",
    "unserialize\\(",
    "invalid connection",
    "socket connection",
    "broken pipe",
    "connection reset",
    "connection.*closed",
    "node.*(died|dead|unavailable)",
    "node.*failed to (send|receive|connect)",
    "worker.*(died|dead|terminated|unavailable)",
    "workers?.*failed to connect",
    "failed to connect.*workers?",
    "cluster setup failed",
    "cannot open.*socket connection",
    "socketconnection"
  )
  if (any(vapply(patterns, grepl, logical(1), x = message, perl = TRUE))) {
    return(TRUE)
  }
  psock_call <- grepl(
    "socketconnection|makepsockcluster|newpsocknode|makecluster",
    call_text,
    perl = TRUE
  )
  psock_call && grepl("cannot open.*connection", message, perl = TRUE)
}

appusage_second_level_retry_worker_counts <- function(worker_count) {
  worker_count <- suppressWarnings(as.integer(worker_count))
  if (length(worker_count) != 1L || is.na(worker_count) || worker_count < 1L) {
    stop("`worker_count` must be a positive integer.")
  }
  unique(as.integer(c(
    worker_count,
    max(1L, floor(worker_count / 2L)),
    1L
  )))
}

appusage_start_second_level_cluster <- function(worker_count, export_env) {
  package_root <- get("package_root", envir = export_env, inherits = FALSE)
  cluster <- parallel::makeCluster(worker_count)
  initialized <- FALSE
  on.exit({
    if (!initialized) {
      tryCatch(
        appusage_stop_cluster_safely(cluster),
        error = function(e) invisible(e)
      )
    }
  }, add = TRUE)
  parallel::clusterExport(
    cluster,
    varlist = c(
      "batch_summary", "output_dir", "overwrite", "resume",
      "second_level_args"
    ),
    envir = export_env
  )
  parallel::clusterCall(cluster, function(package_root) {
    if (!requireNamespace("appusageR", quietly = TRUE)) {
      if (requireNamespace("pkgload", quietly = TRUE) &&
        file.exists(file.path(package_root, "DESCRIPTION"))) {
        pkgload::load_all(package_root, quiet = TRUE)
      } else {
        stop("Package appusageR is not available on the parallel worker.")
      }
    }
    NULL
  }, package_root)
  initialized <- TRUE
  cluster
}

appusage_execute_second_level_chunk <- function(
    cluster, task_indices, batch_summary, output_dir, overwrite, resume,
    second_level_args) {
  appusage_parallel_lapply_lb(cluster, task_indices, function(i) {
    worker <- get("write_second_level_one", envir = asNamespace("appusageR"))
    annotator <- get("appusage_annotate_worker_result", envir = asNamespace("appusageR"))
    row <- worker(
      batch_summary = batch_summary,
      index = i,
      output_dir = output_dir,
      overwrite = overwrite,
      resume = resume,
      second_level_args = second_level_args
    )
    annotator(
      row,
      stage = "second-level",
      task_index = i,
      worker_pid = Sys.getpid()
    )
  })
}

appusage_execute_second_level_serial <- function(
    task_indices, batch_summary, output_dir, overwrite, resume,
    second_level_args) {
  lapply(task_indices, function(i) {
    appusage_annotate_worker_result(
      write_second_level_one(
        batch_summary = batch_summary,
        index = i,
        output_dir = output_dir,
        overwrite = overwrite,
        resume = resume,
        second_level_args = second_level_args
      ),
      stage = "second-level",
      task_index = i,
      worker_pid = Sys.getpid()
    )
  })
}

appusage_write_second_level_cluster_diagnostic <- function(
    error, output_dir, current_task_indices, planned_task_indices,
    worker_count, initial_error = error, retry_attempt = 0L,
    pending_task_indices = current_task_indices,
    completed_via_cache_indices = integer()) {
  output_path <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
  project_path <- normalizePath(dirname(output_path), winslash = "/", mustWork = FALSE)
  diagnostics_dir <- file.path(project_path, "diagnostics")
  dir.create(diagnostics_dir, recursive = TRUE, showWarnings = FALSE)
  diagnostic_file <- tempfile(
    pattern = paste0(
      "second_level_cluster_failure_",
      format(Sys.time(), "%Y%m%dT%H%M%S"),
      "_"
    ),
    tmpdir = diagnostics_dir,
    fileext = ".json"
  )
  condition_call <- conditionCall(error)
  diagnostic <- list(
    stage = "second-level",
    failure_scope = "cluster",
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%OS3 %z"),
    condition_class = paste(class(error), collapse = ","),
    condition_message = conditionMessage(error),
    condition_call = if (is.null(condition_call)) {
      NA_character_
    } else {
      deparse_one_call(condition_call)
    },
    initial_condition_class = paste(class(initial_error), collapse = ","),
    initial_condition_message = conditionMessage(initial_error),
    initial_condition_call = if (is.null(conditionCall(initial_error))) {
      NA_character_
    } else {
      deparse_one_call(conditionCall(initial_error))
    },
    retry_attempt = as.integer(retry_attempt),
    current_task_indices = as.integer(current_task_indices),
    planned_task_indices = as.integer(planned_task_indices),
    pending_task_indices = as.integer(pending_task_indices),
    completed_via_cache_indices = as.integer(completed_via_cache_indices),
    current_task_scope = "submitted_batch",
    worker_count = as.integer(worker_count),
    output_path = output_path,
    project_path = project_path,
    package_version = as.character(utils::packageVersion("appusageR"))
  )
  write_metadata_json(diagnostic, diagnostic_file)
  normalizePath(diagnostic_file, winslash = "/", mustWork = FALSE)
}

appusage_write_second_level_cluster_diagnostic_safely <- function(...) {
  tryCatch(
    appusage_write_second_level_cluster_diagnostic(...),
    error = function(e) NA_character_
  )
}

appusage_signal_second_level_cluster_error <- function(
    error, output_dir, current_task_indices, planned_task_indices,
    worker_count) {
  tryCatch(
    appusage_write_second_level_cluster_diagnostic_safely(
      error = error,
      output_dir = output_dir,
      current_task_indices = current_task_indices,
      planned_task_indices = planned_task_indices,
      worker_count = worker_count
    ),
    error = function(e) NA_character_
  )
  stop(error)
}

appusage_rescan_second_level_chunk <- function(
    batch_summary, task_indices, output_dir, second_level_args) {
  completed_rows <- list()
  completed_indices <- integer()
  pending_indices <- integer()
  for (i in task_indices) {
    first_file <- batch_summary$data_file[[i]]
    upstream_success <- identical(batch_summary$status[[i]], "success") &&
      is_present_string(first_file) && file.exists(first_file)
    cache <- if (upstream_success) {
      second_level_existing_cache_status(
        first_level_rda = first_file,
        output_dir = output_dir,
        batch_summary = batch_summary,
        index = i
      )
    } else {
      list(status = "missing")
    }
    if (identical(cache$status, "complete")) {
      completed_indices <- c(completed_indices, i)
      completed_rows[[as.character(i)]] <- appusage_annotate_worker_result(
        write_second_level_one(
          batch_summary = batch_summary,
          index = i,
          output_dir = output_dir,
          overwrite = FALSE,
          resume = TRUE,
          second_level_args = second_level_args
        ),
        stage = "second-level",
        task_index = i,
        worker_pid = Sys.getpid()
      )
    } else {
      pending_indices <- c(pending_indices, i)
    }
  }
  list(
    completed_rows = completed_rows,
    completed_indices = as.integer(completed_indices),
    pending_indices = as.integer(pending_indices)
  )
}

appusage_rescan_second_level_chunk_safely <- function(
    batch_summary, task_indices, output_dir, second_level_args) {
  tryCatch(
    appusage_rescan_second_level_chunk(
      batch_summary,
      task_indices,
      output_dir,
      second_level_args
    ),
    error = function(e) list(
      completed_rows = list(),
      completed_indices = integer(),
      pending_indices = as.integer(task_indices)
    )
  )
}

appusage_merge_recovered_second_level_rows <- function(
    task_indices, cache_scan, attempted_indices = integer(),
    attempted_rows = list()) {
  attempted_by_index <- if (length(attempted_indices) > 0L) {
    stats::setNames(attempted_rows, as.character(attempted_indices))
  } else {
    list()
  }
  lapply(task_indices, function(i) {
    key <- as.character(i)
    if (!is.null(cache_scan$completed_rows[[key]])) {
      return(cache_scan$completed_rows[[key]])
    }
    attempted_by_index[[key]]
  })
}

appusage_recover_second_level_chunk <- function(
    first_error, task_indices, planned_task_indices, worker_count,
    batch_summary, output_dir, overwrite, second_level_args, export_env) {
  scan <- appusage_rescan_second_level_chunk_safely(
    batch_summary,
    task_indices,
    output_dir,
    second_level_args
  )
  tryCatch(
    appusage_write_second_level_cluster_diagnostic_safely(
      error = first_error,
      initial_error = first_error,
      output_dir = output_dir,
      current_task_indices = task_indices,
      planned_task_indices = planned_task_indices,
      pending_task_indices = scan$pending_indices,
      completed_via_cache_indices = scan$completed_indices,
      worker_count = worker_count,
      retry_attempt = 0L
    ),
    error = function(e) NA_character_
  )
  if (length(scan$pending_indices) == 0L) {
    return(list(
      rows = appusage_merge_recovered_second_level_rows(task_indices, scan),
      cluster = NULL,
      worker_count = worker_count
    ))
  }

  retry_counts <- appusage_second_level_retry_worker_counts(worker_count)
  for (attempt in seq_along(retry_counts)) {
    attempt_workers <- retry_counts[[attempt]]
    attempt_indices <- scan$pending_indices
    attempt_cluster <- NULL
    attempt_error <- NULL
    attempt_rows <- NULL
    if (attempt_workers == 1L) {
      attempt_rows <- tryCatch(
        appusage_execute_second_level_serial(
          attempt_indices,
          batch_summary,
          output_dir,
          overwrite = overwrite,
          resume = TRUE,
          second_level_args = second_level_args
        ),
        error = function(e) {
          attempt_error <<- e
          NULL
        }
      )
    } else {
      attempt_cluster <- tryCatch(
        appusage_start_second_level_cluster(attempt_workers, export_env),
        error = function(e) {
          attempt_error <<- e
          NULL
        }
      )
      if (is.null(attempt_error)) {
        attempt_rows <- tryCatch(
          appusage_execute_second_level_chunk(
            attempt_cluster,
            attempt_indices,
            batch_summary,
            output_dir,
            overwrite = overwrite,
            resume = TRUE,
            second_level_args = second_level_args
          ),
          error = function(e) {
            attempt_error <<- e
            NULL
          }
        )
      }
    }

    if (is.null(attempt_error)) {
      post_scan <- appusage_rescan_second_level_chunk_safely(
        batch_summary,
        task_indices,
        output_dir,
        second_level_args
      )
      return(list(
        rows = appusage_merge_recovered_second_level_rows(
          task_indices,
          post_scan,
          attempted_indices = attempt_indices,
          attempted_rows = attempt_rows
        ),
        cluster = attempt_cluster,
        worker_count = attempt_workers
      ))
    }

    if (!is.null(attempt_cluster)) {
      owned_cluster <- attempt_cluster
      attempt_cluster <- NULL
      tryCatch(
        appusage_stop_cluster_safely(owned_cluster),
        error = function(e) invisible(e)
      )
    }
    scan <- appusage_rescan_second_level_chunk_safely(
      batch_summary,
      task_indices,
      output_dir,
      second_level_args
    )
    tryCatch(
      appusage_write_second_level_cluster_diagnostic_safely(
        error = attempt_error,
        initial_error = first_error,
        output_dir = output_dir,
        current_task_indices = attempt_indices,
        planned_task_indices = planned_task_indices,
        pending_task_indices = scan$pending_indices,
        completed_via_cache_indices = scan$completed_indices,
        worker_count = attempt_workers,
        retry_attempt = attempt
      ),
      error = function(e) NA_character_
    )
    if (!appusage_is_cluster_transport_error(attempt_error)) {
      stop(first_error)
    }
    if (length(scan$pending_indices) == 0L) {
      return(list(
        rows = appusage_merge_recovered_second_level_rows(task_indices, scan),
        cluster = NULL,
        worker_count = attempt_workers
      ))
    }
  }
  stop(first_error)
}

appusage_second_level_chunk_size <- function(worker_count, multiplier = 6L) {
  worker_count <- suppressWarnings(as.integer(worker_count))
  multiplier <- suppressWarnings(as.integer(multiplier))
  if (length(worker_count) != 1L || is.na(worker_count) || worker_count < 1L) {
    stop("`worker_count` must be a positive integer.")
  }
  if (length(multiplier) != 1L || is.na(multiplier) || multiplier < 1L) {
    stop("`multiplier` must be a positive integer.")
  }
  as.integer(worker_count * multiplier)
}

appusage_second_level_checkpoint_base_path <- function(batch_summary,
                                                        output_dir) {
  project_root <- infer_project_root_from_summary(batch_summary)
  if (!is_present_string(project_root)) {
    project_root <- dirname(normalizePath(
      output_dir,
      winslash = "/",
      mustWork = FALSE
    ))
  }
  file.path(project_root, "analytic_summary_table_proclevel-2.checkpoint.csv")
}

appusage_second_level_checkpoint_summary <- function(rows, batch_summary) {
  completed_rows <- Filter(Negate(is.null), rows)
  if (length(completed_rows) == 0L) {
    return(tibble::tibble())
  }
  metadata_files <- unique(unlist(lapply(completed_rows, function(row) {
    if (!"second_level_metadata_file" %in% names(row)) {
      return(character())
    }
    path <- row$second_level_metadata_file[[1]]
    if (!is_present_string(path) || !file.exists(path)) {
      return(character())
    }
    as.character(path)
  }), use.names = FALSE))
  completed_indices <- vapply(completed_rows, function(row) {
    as.integer(row$index[[1]])
  }, integer(1))
  batch_indices <- if ("index" %in% names(batch_summary)) {
    match(completed_indices, as.integer(batch_summary$index))
  } else {
    completed_indices
  }
  completed_batch <- batch_summary[stats::na.omit(batch_indices), , drop = FALSE]
  combine_second_level_batch_summary(
    proc2_metadata = metadata_files,
    rows = completed_rows,
    batch_summary = completed_batch
  )
}

appusage_second_level_checkpoint_version_path <- function(base_path, run_id,
                                                          chunk_index) {
  stem <- tools::file_path_sans_ext(basename(base_path))
  file.path(
    dirname(base_path),
    sprintf(
      "%s.run-%s.chunk-%06d.csv",
      stem,
      run_id,
      as.integer(chunk_index)
    )
  )
}

appusage_second_level_checkpoint_run_id <- function() {
  nonce <- basename(tempfile(pattern = "nonce-"))
  run_id <- paste(
    format(Sys.time(), "%Y%m%dT%H%M%OS6"),
    Sys.getpid(),
    nonce,
    sep = "-"
  )
  gsub("[^A-Za-z0-9._-]", "-", run_id)
}

appusage_validate_second_level_checkpoint <- function(
    path, expected_names = NULL, expected_rows = NULL,
    read_csv = utils::read.csv) {
  checkpoint <- read_csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!is.data.frame(checkpoint)) {
    stop("Second-level checkpoint is not a data frame.")
  }
  if (!is.null(expected_names) && !identical(names(checkpoint), expected_names)) {
    stop("Second-level checkpoint columns failed validation.")
  }
  if (!is.null(expected_rows) && nrow(checkpoint) != expected_rows) {
    stop("Second-level checkpoint row count failed validation.")
  }
  checkpoint
}

appusage_write_second_level_checkpoint <- function(
    summary, base_path, run_id, chunk_index,
    write_csv = utils::write.csv,
    read_csv = utils::read.csv,
    promote_file = file.rename) {
  dir.create(dirname(base_path), recursive = TRUE, showWarnings = FALSE)
  final_path <- appusage_second_level_checkpoint_version_path(
    base_path,
    run_id,
    chunk_index
  )
  if (file.exists(final_path)) {
    stop("Second-level checkpoint version already exists: ", final_path)
  }
  temporary_path <- tempfile(
    pattern = paste0(".", basename(base_path), "."),
    tmpdir = dirname(base_path),
    fileext = ".tmp"
  )
  on.exit(unlink(temporary_path, force = TRUE), add = TRUE)
  write_csv(summary, temporary_path, row.names = FALSE, na = "")
  appusage_validate_second_level_checkpoint(
    temporary_path,
    expected_names = names(summary),
    expected_rows = nrow(summary),
    read_csv = read_csv
  )
  promoted <- isTRUE(promote_file(temporary_path, final_path))
  if (!promoted || !file.exists(final_path)) {
    stop("Could not promote the validated second-level checkpoint.")
  }
  tryCatch(
    appusage_validate_second_level_checkpoint(
      final_path,
      expected_names = names(summary),
      expected_rows = nrow(summary),
      read_csv = read_csv
    ),
    error = function(e) {
      unlink(final_path, force = TRUE)
      stop(e)
    }
  )
  appusage_prune_second_level_checkpoints(base_path, keep = 2L)
  appusage_refresh_workflow_checkpoint_safely(
    project_root = dirname(base_path),
    stage = "second_level",
    checkpoint_path = final_path,
    row_count = nrow(summary)
  )
  normalizePath(final_path, winslash = "/", mustWork = FALSE)
}

appusage_second_level_checkpoint_files <- function(base_path) {
  if (!dir.exists(dirname(base_path))) {
    return(character())
  }
  stem <- tools::file_path_sans_ext(basename(base_path))
  candidates <- list.files(dirname(base_path), full.names = TRUE)
  candidates[
    startsWith(basename(candidates), paste0(stem, ".run-")) &
      endsWith(basename(candidates), ".csv")
  ]
}

appusage_prune_second_level_checkpoints <- function(base_path, keep = 2L) {
  keep <- suppressWarnings(as.integer(keep))
  if (length(keep) != 1L || is.na(keep) || keep < 1L) {
    stop("`keep` must be a positive integer.")
  }
  candidates <- appusage_second_level_checkpoint_files(base_path)
  if (length(candidates) <= keep) {
    return(invisible(candidates))
  }
  info <- file.info(candidates)
  candidates <- candidates[order(info$mtime, basename(candidates), decreasing = TRUE)]
  valid <- vapply(candidates, function(path) {
    !is.null(tryCatch(
      appusage_validate_second_level_checkpoint(path),
      error = function(e) NULL
    ))
  }, logical(1))
  retained <- candidates[valid][seq_len(min(keep, sum(valid)))]
  obsolete <- setdiff(candidates, retained)
  if (length(obsolete) > 0L) {
    unlink(obsolete, force = TRUE)
  }
  invisible(retained)
}

appusage_read_latest_second_level_checkpoint <- function(base_path) {
  candidates <- appusage_second_level_checkpoint_files(base_path)
  if (file.exists(base_path)) {
    candidates <- c(candidates, base_path)
  }
  if (length(candidates) == 0L) {
    return(NULL)
  }
  info <- file.info(candidates)
  candidates <- candidates[order(info$mtime, basename(candidates), decreasing = TRUE)]
  for (path in candidates) {
    checkpoint <- tryCatch(
      appusage_validate_second_level_checkpoint(path),
      error = function(e) NULL
    )
    if (!is.null(checkpoint)) {
      return(list(
        path = normalizePath(path, winslash = "/", mustWork = FALSE),
        summary = checkpoint
      ))
    }
  }
  NULL
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

appusage_annotate_worker_result <- function(row, stage, task_index,
                                            worker_pid = Sys.getpid()) {
  if (!is.data.frame(row) || nrow(row) == 0L) {
    return(row)
  }

  row$worker_stage <- stage
  row$worker_task_index <- as.integer(task_index)
  row$worker_pid <- as.integer(worker_pid)
  row
}

appusage_progress_value <- function(row, columns, default = NA_character_) {
  if (!is.data.frame(row) || nrow(row) == 0L) {
    return(default)
  }

  for (column in columns) {
    if (!column %in% names(row)) {
      next
    }
    value <- row[[column]][[1]]
    if (!is.null(value) && length(value) > 0L && !is.na(value)) {
      return(as.character(value))
    }
  }

  default
}

appusage_emit_parallel_progress <- function(progress, stage, row, index, total) {
  if (!isTRUE(progress)) {
    return(invisible(NULL))
  }

  status <- appusage_progress_value(row, "status", "unknown")
  source <- appusage_progress_value(
    row,
    c("source_basename", "output_basename", "participant_id", "file_label"),
    NA_character_
  )
  worker_pid <- appusage_progress_value(row, "worker_pid", NA_character_)
  diagnostic <- appusage_progress_value(
    row,
    c("diagnostic_report", "second_level_metadata_file", "metadata_file"),
    NA_character_
  )
  error_message <- appusage_progress_value(
    row,
    c("condition_message", "error_message"),
    NA_character_
  )

  pieces <- c(
    sprintf("[parallel-progress] %s", stage),
    sprintf("task=%d/%d", as.integer(index), as.integer(total)),
    sprintf("status=%s", status)
  )
  if (!is.na(source)) {
    pieces <- c(pieces, sprintf("source=%s", source))
  }
  if (!is.na(worker_pid)) {
    pieces <- c(pieces, sprintf("worker_pid=%s", worker_pid))
  }
  if (!is.na(diagnostic)) {
    pieces <- c(pieces, sprintf("diagnostic=%s", diagnostic))
  }
  if (!is.na(error_message) && !identical(status, "success")) {
    pieces <- c(pieces, sprintf("error=%s", error_message))
  }

  message(paste(pieces, collapse = " | "))
  invisible(NULL)
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
                                            second_level_args,
                                            checkpoint_chunk_multiplier = 6L) {
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
  processing_order <- second_level_processing_order(batch_summary)
  rows <- vector("list", n)
  checkpoint_base_path <- appusage_second_level_checkpoint_base_path(
    batch_summary,
    output_dir
  )
  checkpoint_run_id <- appusage_second_level_checkpoint_run_id()
  package_root <- appusage_package_root_for_workers()
  cluster <- NULL
  active_worker_count <- n_workers
  export_env <- environment()
  stop_owned_cluster <- function() {
    if (is.null(cluster)) {
      return(invisible(NULL))
    }
    owned_cluster <- cluster
    cluster <<- NULL
    tryCatch(
      appusage_stop_cluster_safely(owned_cluster),
      error = function(e) invisible(e)
    )
    invisible(NULL)
  }
  on.exit(stop_owned_cluster(), add = TRUE)
  chunk_size <- appusage_second_level_chunk_size(
    n_workers,
    checkpoint_chunk_multiplier
  )
  chunks <- split(
    processing_order,
    ceiling(seq_along(processing_order) / chunk_size)
  )
  for (chunk_index in seq_along(chunks)) {
    chunk <- chunks[[chunk_index]]
    chunk_rows <- NULL
    if (active_worker_count == 1L) {
      chunk_rows <- appusage_execute_second_level_serial(
        chunk,
        batch_summary,
        output_dir,
        overwrite = overwrite,
        resume = resume,
        second_level_args = second_level_args
      )
    } else {
      if (is.null(cluster)) {
        setup_result <- tryCatch(
          list(
            cluster = appusage_start_second_level_cluster(
              active_worker_count,
              export_env
            ),
            error = NULL
          ),
          error = function(e) list(cluster = NULL, error = e)
        )
        if (is.null(setup_result$error)) {
          cluster <- setup_result$cluster
        } else if (appusage_is_cluster_transport_error(setup_result$error)) {
          recovery <- appusage_recover_second_level_chunk(
            first_error = setup_result$error,
            task_indices = chunk,
            planned_task_indices = processing_order,
            worker_count = active_worker_count,
            batch_summary = batch_summary,
            output_dir = output_dir,
            overwrite = overwrite,
            second_level_args = second_level_args,
            export_env = export_env
          )
          chunk_rows <- recovery$rows
          cluster <- recovery$cluster
          active_worker_count <- recovery$worker_count
        } else {
          appusage_signal_second_level_cluster_error(
            error = setup_result$error,
            output_dir = output_dir,
            current_task_indices = chunk,
            planned_task_indices = processing_order,
            worker_count = active_worker_count
          )
        }
      }
      if (is.null(chunk_rows)) {
        chunk_result <- tryCatch(
          list(
            rows = appusage_execute_second_level_chunk(
              cluster,
              chunk,
              batch_summary,
              output_dir,
              overwrite = overwrite,
              resume = resume,
              second_level_args = second_level_args
            ),
            error = NULL
          ),
          error = function(e) list(rows = NULL, error = e)
        )
        if (is.null(chunk_result$error)) {
          chunk_rows <- chunk_result$rows
        } else {
          first_error <- chunk_result$error
          stop_owned_cluster()
          if (!appusage_is_cluster_transport_error(first_error)) {
            appusage_signal_second_level_cluster_error(
              error = first_error,
              output_dir = output_dir,
              current_task_indices = chunk,
              planned_task_indices = processing_order,
              worker_count = active_worker_count
            )
          }
          recovery <- appusage_recover_second_level_chunk(
            first_error = first_error,
            task_indices = chunk,
            planned_task_indices = processing_order,
            worker_count = active_worker_count,
            batch_summary = batch_summary,
            output_dir = output_dir,
            overwrite = overwrite,
            second_level_args = second_level_args,
            export_env = export_env
          )
          chunk_rows <- recovery$rows
          cluster <- recovery$cluster
          active_worker_count <- recovery$worker_count
        }
      }
    }
    rows[chunk] <- chunk_rows
    if (isTRUE(progress)) {
      for (i in chunk) {
        appusage_emit_parallel_progress(
          progress = progress,
          stage = "second-level",
          row = rows[[i]],
          index = i,
          total = n
        )
      }
    }
    checkpoint_summary <- appusage_second_level_checkpoint_summary(
      rows,
      batch_summary
    )
    appusage_write_second_level_checkpoint(
      checkpoint_summary,
      checkpoint_base_path,
      run_id = checkpoint_run_id,
      chunk_index = chunk_index
    )
  }
  rows
}

second_level_processing_order <- function(batch_summary) {
  schedule <- second_level_processing_score(batch_summary)
  if (nrow(schedule) == 0) {
    return(integer())
  }
  order(!schedule$eligible, -schedule$scheduling_score, schedule$index, na.last = TRUE)
}

second_level_processing_score <- function(batch_summary) {
  n <- nrow(batch_summary)
  if (n == 0) {
    return(data.frame(
      index = integer(),
      eligible = logical(),
      scheduling_score = numeric(),
      file_size_bytes = numeric(),
      source_size_bytes = numeric(),
      row_signal = numeric(),
      type_weight = numeric(),
      elapsed_signal_sec = numeric(),
      stringsAsFactors = FALSE
    ))
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
  existing <- !is.na(data_file) & nzchar(data_file) & file.exists(data_file)
  file_size <- second_level_file_size_signal(batch_summary, data_file, existing)
  source_size <- second_level_numeric_first(
    batch_summary,
    c("source_file_size_bytes", "source_size_bytes", "file_size_bytes", "file_size"),
    default = 0
  )
  n_rows <- second_level_row_count_signal(batch_summary)
  detected_type <- if ("detected_type" %in% names(batch_summary)) {
    as.character(batch_summary$detected_type)
  } else {
    rep(NA_character_, n)
  }
  weight <- second_level_type_weight(detected_type)
  elapsed <- second_level_numeric_first(
    batch_summary,
    c("second_level_total_elapsed_sec", "second_level_elapsed_sec", "elapsed_sec"),
    default = 0
  )
  eligible <- status == "success" & existing
  score <- file_size + source_size * 0.25 + n_rows * 100 + weight * 1e6 + elapsed * 1e5
  data.frame(
    index = seq_len(n),
    eligible = eligible,
    scheduling_score = score,
    file_size_bytes = file_size,
    source_size_bytes = source_size,
    row_signal = n_rows,
    type_weight = weight,
    elapsed_signal_sec = elapsed,
    stringsAsFactors = FALSE
  )
}

second_level_file_size_signal <- function(batch_summary, data_file, existing) {
  size <- second_level_numeric_first(
    batch_summary,
    c(
      "first_level_rda_size_bytes",
      "first_level_data_size_bytes",
      "data_file_size_bytes",
      "rda_size_bytes"
    ),
    default = NA_real_
  )
  missing_size <- is.na(size) & existing
  if (any(missing_size)) {
    info <- file.info(data_file[missing_size])
    size[missing_size] <- as.numeric(info$size)
  }
  size[is.na(size)] <- 0
  size
}

second_level_row_count_signal <- function(batch_summary) {
  n <- nrow(batch_summary)
  rows <- second_level_numeric_first(batch_summary, "n_rows", default = NA_real_)
  missing_rows <- is.na(rows)
  grain_columns <- intersect(c("n_event_rows", "n_episode_rows", "n_daily_rows"), names(batch_summary))
  if (any(missing_rows) && length(grain_columns) > 0) {
    grain_rows <- rep(0, n)
    for (column in grain_columns) {
      value <- suppressWarnings(as.numeric(batch_summary[[column]]))
      value[is.na(value)] <- 0
      grain_rows <- grain_rows + value
    }
    rows[missing_rows] <- grain_rows[missing_rows]
  }
  rows[is.na(rows)] <- 0
  rows
}

second_level_type_weight <- function(detected_type) {
  type_weight <- c(line = 4, meta = 3, day = 2, app = 1)
  weight <- unname(type_weight[as.character(detected_type)])
  weight[is.na(weight)] <- 0
  weight
}

second_level_numeric_first <- function(batch_summary, columns, default = 0) {
  n <- nrow(batch_summary)
  out <- rep(NA_real_, n)
  for (column in columns) {
    if (!column %in% names(batch_summary)) {
      next
    }
    value <- suppressWarnings(as.numeric(batch_summary[[column]]))
    replace <- is.na(out) & !is.na(value)
    out[replace] <- value[replace]
  }
  out[is.na(out)] <- default
  out
}

appusage_package_root_for_workers <- function(start = getwd()) {
  option_root <- getOption("appusageR.package_root", NULL)
  if (length(option_root) == 1 && !is.na(option_root) && nzchar(option_root)) {
    option_root <- normalizePath(option_root, winslash = "/", mustWork = FALSE)
    if (appusage_is_package_root(option_root)) {
      return(option_root)
    }
  }
  loaded_root <- tryCatch(system.file(package = "appusageR"), error = function(e) "")
  if (length(loaded_root) == 1 && nzchar(loaded_root)) {
    loaded_root <- normalizePath(loaded_root, winslash = "/", mustWork = FALSE)
    if (appusage_is_package_root(loaded_root)) {
      return(loaded_root)
    }
  }
  current <- normalizePath(start, winslash = "/", mustWork = FALSE)
  repeat {
    if (appusage_is_package_root(current)) {
      return(current)
    }
    parent <- dirname(current)
    if (identical(parent, current)) {
      return(normalizePath(start, winslash = "/", mustWork = FALSE))
    }
    current <- parent
  }
}

appusage_is_package_root <- function(path) {
  desc <- file.path(path, "DESCRIPTION")
  if (!file.exists(desc)) {
    return(FALSE)
  }
  lines <- readLines(desc, warn = FALSE)
  any(grepl("^Package:\\s*appusageR\\s*$", lines))
}

process_batch_rows <- function(x, id_plan, type, input, output_dir, tz,
                               encoding, overwrite, progress, progress_every,
                               parallel, n_cores, checkpoint_every = NULL,
                               checkpoint_file = NULL, existing_rows = NULL,
                               retry_memory_allocation = TRUE,
                               memory_retry_workers = 1L,
                               provenance = NULL) {
  memory_risk <- appusage_first_level_memory_risk_signal(
    x = x,
    input = input,
    parallel = parallel,
    n_cores = n_cores
  )
  rows <- appusage_restore_checkpoint_rows(existing_rows, length(x))
  seed_rows <- appusage_index_seed_rows(existing_rows, length(x))
  pending <- which(vapply(rows, is.null, logical(1)))
  checkpoint_every <- suppressWarnings(as.integer(checkpoint_every %||% progress_every))
  if (is.na(checkpoint_every) || checkpoint_every < 1L) {
    checkpoint_every <- length(x)
  }

  overwrite_plan <- rep(isTRUE(overwrite), length(x))
  for (i in pending) {
    overwrite_plan[[i]] <- isTRUE(overwrite) ||
      appusage_first_level_seed_requires_overwrite(seed_rows[[i]])
  }

  retry_one <- function(row, i, worker_count) {
    row <- appusage_annotate_first_level_row(
      row,
      retry_attempt = 0L,
      retry_worker_count = worker_count
    )
    appusage_retry_memory_row(
      row,
      retry_fun = function() {
        preprocess_one_appusage(
          x = x[[i]],
          id_info = id_plan[i, , drop = FALSE],
          type = type,
          input = input,
          output_dir = output_dir,
          tz = tz,
          encoding = encoding,
          overwrite = TRUE,
          index = i,
          memory_risk_signal = FALSE,
          memory_risk_reason = "serial_retry",
          provenance = provenance
        )
      },
      retry_worker_count = memory_retry_workers,
      enabled = retry_memory_allocation
    )
  }

  memory_retry_pending <- pending[vapply(
    pending,
    function(i) appusage_first_level_seed_is_memory_retry(seed_rows[[i]]),
    logical(1)
  )]
  if (isTRUE(retry_memory_allocation) && length(memory_retry_pending) > 0L) {
    for (i in memory_retry_pending) {
      if (isTRUE(progress)) {
        message(sprintf(
          "Retrying memory-allocation APP Usage file %d/%d with %d worker",
          i, length(x), memory_retry_workers
        ))
      }
      row <- preprocess_one_appusage(
        x = x[[i]],
        id_info = id_plan[i, , drop = FALSE],
        type = type,
        input = input,
        output_dir = output_dir,
        tz = tz,
        encoding = encoding,
        overwrite = TRUE,
        index = i,
        memory_risk_signal = FALSE,
        memory_risk_reason = "serial_retry",
        provenance = provenance
      )
      rows[[i]] <- appusage_annotate_first_level_row(
        row,
        retry_attempt = 1L,
        retry_worker_count = memory_retry_workers,
        original_row = seed_rows[[i]]
      )
    }
    appusage_write_first_level_checkpoint(rows, checkpoint_file)
    pending <- which(vapply(rows, is.null, logical(1)))
  }

  if (!isTRUE(parallel) || length(x) <= 1 || n_cores == 1) {
    processed_since_checkpoint <- 0L
    for (i in pending) {
      if (isTRUE(progress) && (i == 1 || i %% progress_every == 0 || i == length(x))) {
        message(sprintf("Preprocessing APP Usage file %d/%d", i, length(x)))
      }
      row <- preprocess_one_appusage(
        x = x[[i]],
        id_info = id_plan[i, , drop = FALSE],
        type = type,
        input = input,
        output_dir = output_dir,
        tz = tz,
        encoding = encoding,
        overwrite = overwrite_plan[[i]],
        index = i,
        memory_risk_signal = FALSE,
        memory_risk_reason = "serial_execution",
        provenance = provenance
      )
      rows[[i]] <- retry_one(row, i, worker_count = 1L)
      processed_since_checkpoint <- processed_since_checkpoint + 1L
      if (processed_since_checkpoint >= checkpoint_every) {
        appusage_write_first_level_checkpoint(rows, checkpoint_file)
        processed_since_checkpoint <- 0L
      }
    }
    appusage_write_first_level_checkpoint(rows, checkpoint_file)
    return(rows)
  }

  if (length(pending) == 0L) {
    appusage_write_first_level_checkpoint(rows, checkpoint_file)
    return(rows)
  }

  if (isTRUE(progress)) {
    message(sprintf(
      "Preprocessing %d APP Usage files with %d parallel workers",
      length(pending), n_cores
    ))
  }
  cluster <- parallel::makeCluster(n_cores)
  on.exit(appusage_stop_cluster_safely(cluster), add = TRUE)
  package_root <- appusage_package_root_for_workers()
  parallel::clusterExport(
    cluster,
    varlist = "package_root",
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
  chunks <- split(
    pending,
    ceiling(seq_along(pending) / checkpoint_every)
  )
  for (chunk in chunks) {
    chunk_tasks <- appusage_make_first_level_worker_tasks(
      indices = chunk,
      x = x,
      id_plan = id_plan,
      type = type,
      input = input,
      output_dir = output_dir,
      tz = tz,
      encoding = encoding,
      overwrite_plan = overwrite_plan,
      memory_risk_signal = memory_risk$active,
      memory_risk_reason = memory_risk$reason,
      provenance = provenance
    )
    chunk_rows <- parallel::parLapplyLB(cluster, chunk_tasks, function(task) {
      worker_task <- get("appusage_process_first_level_worker_task", envir = asNamespace("appusageR"))
      worker_task(task)
    })
    for (j in seq_along(chunk)) {
      task_index <- chunk[[j]]
      rows[[task_index]] <- retry_one(
        chunk_rows[[j]],
        task_index,
        worker_count = n_cores
      )
      appusage_emit_parallel_progress(
        progress = progress,
        stage = "first-level",
        row = rows[[task_index]],
        index = task_index,
        total = length(x)
      )
    }
    appusage_write_first_level_checkpoint(rows, checkpoint_file)
  }
  rows
}

appusage_make_first_level_worker_tasks <- function(indices, x, id_plan, type,
                                                   input, output_dir, tz,
                                                   encoding, overwrite_plan,
                                                   memory_risk_signal = FALSE,
                                                    memory_risk_reason = NA_character_,
                                                    provenance = NULL) {
  lapply(indices, function(i) {
    appusage_make_first_level_worker_task(
      index = i,
      x = x[[i]],
      id_info = id_plan[i, , drop = FALSE],
      type = type,
      input = input,
      output_dir = output_dir,
      tz = tz,
      encoding = encoding,
      overwrite = overwrite_plan[[i]],
      memory_risk_signal = memory_risk_signal,
      memory_risk_reason = memory_risk_reason,
      provenance = provenance
    )
  })
}

appusage_make_first_level_worker_task <- function(index, x, id_info, type,
                                                  input, output_dir, tz,
                                                  encoding, overwrite,
                                                  memory_risk_signal = FALSE,
                                                   memory_risk_reason = NA_character_,
                                                   provenance = NULL) {
  list(
    index = index,
    x = x,
    id_info = id_info,
    type = type,
    input = input,
    output_dir = output_dir,
    tz = tz,
    encoding = encoding,
    overwrite = overwrite,
    memory_risk_signal = memory_risk_signal,
    memory_risk_reason = memory_risk_reason,
    provenance = provenance
  )
}

appusage_process_first_level_worker_task <- function(task) {
  row <- preprocess_one_appusage(
    x = task$x,
    id_info = task$id_info,
    type = task$type,
    input = task$input,
    output_dir = task$output_dir,
    tz = task$tz,
    encoding = task$encoding,
    overwrite = task$overwrite,
    index = task$index,
    memory_risk_signal = task$memory_risk_signal %||% FALSE,
    memory_risk_reason = task$memory_risk_reason %||% NA_character_,
    provenance = task$provenance %||% NULL
  )
  appusage_annotate_worker_result(
    row,
    stage = "first-level",
    task_index = task$index,
    worker_pid = Sys.getpid()
  )
}

prepare_batch_output_project <- function(output_dir, project_name, project_id,
                                         overwrite, resume = FALSE, n_inputs,
                                         input, tz) {
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

  if (dir.exists(project_root) && !isTRUE(overwrite) && !isTRUE(resume)) {
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
  source_record_key <- appusage_summary_cell(batch_summary, "source_record_key", index)
  source_fingerprint <- appusage_summary_cell(batch_summary, "source_fingerprint", index)
  source_cache_key <- appusage_summary_cell(batch_summary, "source_cache_key", index)
  effective_timezone <- appusage_resolve_timezone(
    second_level_args$tz %||%
      appusage_summary_cell(batch_summary, "effective_timezone", index, NULL)
  )
  if (!identical(status, "success") || is.na(first_file) || !file.exists(first_file)) {
    finished_at <- Sys.time()
    skip_reason <- if (!identical(status, "success")) {
      "upstream_first_level_error"
    } else if (is.na(first_file)) {
      "missing_first_level_rda"
    } else {
      "first_level_rda_not_found"
    }
    return(appusage_attach_daily_self_check_summary(data.frame(
      index = batch_summary$index[[index]],
      participant_id = batch_summary$participant_id[[index]],
      detected_type = batch_summary$detected_type[[index]],
      source_record_key = source_record_key,
      source_fingerprint = source_fingerprint,
      source_cache_key = source_cache_key,
      effective_timezone = effective_timezone,
      status = "skipped",
      skip_reason = skip_reason,
      first_level_data_file = first_file,
      second_level_data_file = NA_character_,
      second_level_metadata_file = NA_character_,
      error_class = NA_character_,
      error_message = NA_character_,
      started_at = format(started_at, "%Y-%m-%d %H:%M:%OS3 %z"),
      finished_at = format(finished_at, "%Y-%m-%d %H:%M:%OS3 %z"),
      elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
      stringsAsFactors = FALSE
    ), NA_character_))
  }

  cache <- second_level_existing_cache_status(
    first_level_rda = first_file,
    output_dir = output_dir,
    batch_summary = batch_summary,
    index = index
  )
  if (isTRUE(resume) && !isTRUE(overwrite) && identical(cache$status, "complete")) {
    finished_at <- Sys.time()
    return(appusage_attach_daily_self_check_summary(data.frame(
      index = batch_summary$index[[index]],
      participant_id = batch_summary$participant_id[[index]],
      detected_type = batch_summary$detected_type[[index]],
      source_record_key = source_record_key,
      source_fingerprint = source_fingerprint,
      source_cache_key = source_cache_key,
      effective_timezone = effective_timezone,
      status = "skipped",
      skip_reason = "existing_proc2_cache",
      pair_state = cache$pair_state,
      first_level_data_file = first_file,
      second_level_data_file = cache$rda_file,
      second_level_metadata_file = cache$json_file,
      error_class = NA_character_,
      error_message = NA_character_,
      started_at = format(started_at, "%Y-%m-%d %H:%M:%OS3 %z"),
      finished_at = format(finished_at, "%Y-%m-%d %H:%M:%OS3 %z"),
      elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
      stringsAsFactors = FALSE
    ), cache$json_file))
  }
  if (identical(cache$pair_state, "source_key_collision")) {
    finished_at <- Sys.time()
    return(appusage_attach_daily_self_check_summary(data.frame(
      index = batch_summary$index[[index]],
      participant_id = batch_summary$participant_id[[index]],
      detected_type = batch_summary$detected_type[[index]],
      source_record_key = source_record_key,
      source_fingerprint = source_fingerprint,
      source_cache_key = source_cache_key,
      effective_timezone = effective_timezone,
      status = "error",
      skip_reason = cache$reason,
      pair_state = cache$pair_state,
      first_level_data_file = first_file,
      second_level_data_file = cache$rda_file,
      second_level_metadata_file = cache$json_file,
      error_class = "appusage_source_cache_collision",
      error_message = paste("Second-level cache ownership validation failed:", cache$reason),
      started_at = format(started_at, "%Y-%m-%d %H:%M:%OS3 %z"),
      finished_at = format(finished_at, "%Y-%m-%d %H:%M:%OS3 %z"),
      elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
      stringsAsFactors = FALSE
    ), cache$json_file))
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
      finished_at = finished_at,
      provenance = second_level_args$provenance %||% NULL
    )
  }
  appusage_attach_daily_self_check_summary(data.frame(
    index = batch_summary$index[[index]],
    participant_id = batch_summary$participant_id[[index]],
    detected_type = batch_summary$detected_type[[index]],
    source_record_key = source_record_key,
    source_fingerprint = source_fingerprint,
    source_cache_key = source_cache_key,
    effective_timezone = effective_timezone,
    pair_state = cache$pair_state,
    status = if (is.null(result$error)) "success" else "error",
    skip_reason = NA_character_,
    first_level_data_file = first_file,
    second_level_data_file = result$output,
    second_level_metadata_file = metadata_file,
    error_class = if (is.null(result$error)) NA_character_ else paste(class(result$error), collapse = ","),
    error_message = if (is.null(result$error)) NA_character_ else conditionMessage(result$error),
    started_at = format(started_at, "%Y-%m-%d %H:%M:%OS3 %z"),
    finished_at = format(finished_at, "%Y-%m-%d %H:%M:%OS3 %z"),
    elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
    stringsAsFactors = FALSE
  ), metadata_file)
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
      extension = "rda",
      source_key = entities$src %||% NULL
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
  entities <- parse_appusage_filename(first_level_rda)
  expected_key <- if (!is.null(batch_summary) && !is.null(index)) {
    appusage_summary_cell(batch_summary, "source_record_key", index)
  } else {
    NA_character_
  }
  expected_fingerprint <- if (!is.null(batch_summary) && !is.null(index)) {
    appusage_summary_cell(batch_summary, "source_fingerprint", index)
  } else {
    NA_character_
  }
  first_metadata <- tryCatch(
    read_first_level_metadata_for_second(first_level_rda),
    error = function(e) NULL
  )
  first_identity <- if (is.null(first_metadata)) list() else appusage_metadata_source_identity(first_metadata)
  expected_key <- appusage_first_nonmissing(expected_key, first_identity$source_record_key)
  expected_fingerprint <- appusage_first_nonmissing(
    expected_fingerprint,
    first_identity$source_fingerprint
  )
  legacy_name <- !is_present_string(entities$src)
  legacy_count <- 1L
  if (legacy_name && !is.null(batch_summary) && !is.null(index)) {
    same_id <- as.character(batch_summary$participant_id) ==
      as.character(batch_summary$participant_id[[index]])
    same_type <- as.character(batch_summary$detected_type) ==
      as.character(batch_summary$detected_type[[index]])
    legacy_count <- sum(same_id & same_type, na.rm = TRUE)
  }
  key_count <- 1L
  if (!is.null(batch_summary) && !is.null(index) && is_present_string(expected_key) &&
    "source_record_key" %in% names(batch_summary)) {
    key_count <- sum(
      as.character(batch_summary$source_record_key) == as.character(expected_key),
      na.rm = TRUE
    )
  }
  result <- function(status, pair_state, reason, metadata = NULL) {
    c(paths, list(
      status = status,
      pair_state = pair_state,
      reason = reason,
      metadata = metadata,
      legacy_name = legacy_name,
      owned_by_task = !identical(pair_state, "source_key_collision")
    ))
  }
  rda_exists <- file.exists(paths$rda_file)
  json_exists <- file.exists(paths$json_file)
  if (key_count > 1L) {
    return(result("collision", "source_key_collision", "duplicate_source_record_key"))
  }
  if (!rda_exists && !json_exists) {
    return(result("missing", "missing_pair", "missing_pair"))
  }
  if (rda_exists && !json_exists) {
    if (legacy_name && legacy_count > 1L) {
      return(result("collision", "source_key_collision", "ambiguous_legacy_rda_only_cache"))
    }
    return(result("incomplete", "rda_only_incomplete", "rda_only_partial_cache"))
  }
  metadata <- tryCatch(
    jsonlite::read_json(paths$json_file, simplifyVector = TRUE),
    error = function(e) e
  )
  if (inherits(metadata, "error")) {
    if (legacy_name && legacy_count > 1L) {
      return(result("collision", "source_key_collision", "ambiguous_legacy_corrupt_json"))
    }
    return(result("incomplete", "corrupt_json", "malformed_proc2_json"))
  }
  processing_status <- appusage_nested_value(metadata, c("processing", "second_level_status"))
  recorded_identity <- appusage_metadata_source_identity(metadata)
  if (!identical(as.character(processing_status), "success")) {
    if (is_present_string(expected_key) &&
      is_present_string(recorded_identity$source_record_key) &&
      !identical(as.character(recorded_identity$source_record_key), as.character(expected_key))) {
      return(result("collision", "source_key_collision", "source_record_key_mismatch", metadata))
    }
    if (legacy_name && legacy_count > 1L) {
      return(result("collision", "source_key_collision", "ambiguous_legacy_error_json", metadata))
    }
    return(result("incomplete", "json_only_error", "non_success_proc2_json", metadata))
  }
  if (is_present_string(expected_key)) {
    if (!is_present_string(recorded_identity$source_record_key)) {
      return(result("incomplete", "stale_source_identity", "missing_source_record_key", metadata))
    }
    if (!identical(as.character(recorded_identity$source_record_key), as.character(expected_key))) {
      return(result("collision", "source_key_collision", "source_record_key_mismatch", metadata))
    }
  }
  if (is_present_string(expected_fingerprint)) {
    if (!is_present_string(recorded_identity$source_fingerprint)) {
      return(result("incomplete", "stale_source_identity", "missing_source_fingerprint", metadata))
    }
    if (!identical(
      as.character(recorded_identity$source_fingerprint),
      as.character(expected_fingerprint)
    )) {
      return(result("incomplete", "stale_source_identity", "source_fingerprint_mismatch", metadata))
    }
  }
  if (!rda_exists) {
    return(result("incomplete", "json_success_missing_rda", "success_json_missing_rda", metadata))
  }
  rda_size <- appusage_file_size_bytes(paths$rda_file)
  if (is.na(rda_size) || rda_size <= 0) {
    return(result("incomplete", "rda_only_incomplete", "empty_proc2_rda", metadata))
  }
  expected_first <- normalizePath(first_level_rda, winslash = "/", mustWork = FALSE)
  recorded_first <- appusage_nested_value(metadata, c("outputs", "first_level_rda"))
  recorded_second <- appusage_nested_value(metadata, c("outputs", "second_level_rda"))
  recorded_metadata <- appusage_nested_value(metadata, c("outputs", "metadata_json"))
  if (is_present_string(recorded_first) &&
    !identical(normalizePath(recorded_first, winslash = "/", mustWork = FALSE), expected_first)) {
    return(result("incomplete", "stale_source_identity", "first_level_rda_mismatch", metadata))
  }
  if (!is_present_string(recorded_second)) {
    return(result("incomplete", "stale_source_identity", "missing_second_level_rda_path", metadata))
  }
  if (!identical(normalizePath(recorded_second, winslash = "/", mustWork = FALSE), paths$rda_file)) {
    return(result("incomplete", "stale_source_identity", "second_level_rda_mismatch", metadata))
  }
  if (!is_present_string(recorded_metadata)) {
    return(result("incomplete", "stale_source_identity", "missing_metadata_json_path", metadata))
  }
  if (!identical(
    normalizePath(recorded_metadata, winslash = "/", mustWork = FALSE),
    paths$json_file
  )) {
    return(result("incomplete", "stale_source_identity", "metadata_json_mismatch", metadata))
  }
  if (!is.null(batch_summary) && !is.null(index)) {
    recorded_id <- appusage_nested_value(metadata, c("identity", "participant_id"))
    recorded_type <- appusage_nested_value(metadata, c("export", "detected_type"))
    expected_id <- batch_summary$participant_id[[index]]
    expected_type <- batch_summary$detected_type[[index]]
    if (is_present_string(recorded_id) && is_present_string(expected_id) &&
      !identical(as.character(recorded_id), as.character(expected_id))) {
      return(result("collision", "source_key_collision", "participant_id_mismatch", metadata))
    }
    if (is_present_string(recorded_type) && is_present_string(expected_type) &&
      !identical(as.character(recorded_type), as.character(expected_type))) {
      return(result("collision", "source_key_collision", "export_type_mismatch", metadata))
    }
  }
  if (legacy_name) {
    if (legacy_count > 1L) {
      return(result("collision", "source_key_collision", "ambiguous_legacy_proc2_pair", metadata))
    }
    return(result("complete", "legacy_unambiguous_pair", "legacy_unambiguous_pair", metadata))
  }
  result("complete", "complete_valid_pair", "valid_proc2_pair", metadata)
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
