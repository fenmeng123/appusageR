#' Create second-level APP Usage analysis data
#'
#' Converts first-level APP Usage parser output into a stable second-level list
#' with three analysis grains: `event`, `episode`, and `daily`. Unavailable
#' grains are returned as zero-row tibbles. Meta Table 2 is preserved as event
#' data. Meta event-to-episode reconstruction is explicit, diagnosable, and
#' enabled by default in second-level meta processing.
#'
#' @param data First-level APP Usage data. This may be a first-level RDA `data`
#'   list, a parser tibble, or the list returned by `parse_meta()`.
#' @param export_type Optional export type override: `"line"`, `"meta"`,
#'   `"day"`, or `"app"`.
#' @param include_collection_app Whether to keep `com.w.appusage` rows in
#'   second-level outputs.
#' @param max_episode_ms Episode duration above this value is flagged as an
#'   anomaly.
#' @param max_daily_app_ms Daily app duration above this value is flagged as an
#'   anomaly.
#' @param reconstruct_meta Whether to explicitly reconstruct meta Table 2 events
#'   into episode records. Defaults to `TRUE` for research-ready second-level
#'   meta output; `parse_meta()` itself remains a faithful raw parser.
#' @param meta_pairing Pairing strategy for meta event reconstruction.
#' @param meta_start_event_types Event types that start meta episodes.
#' @param meta_end_event_types Event types that end meta episodes.
#' @param merge_meta_episodes Whether to merge adjacent reconstructed meta
#'   episodes for the same app when the time gap is small.
#' @param meta_episode_merge_gap_ms Maximum gap, in milliseconds, allowed when
#'   merging adjacent reconstructed meta episodes.
#' @param meta_daily_source Source for meta daily rows. `"summary"` keeps Table
#'   1 summary-derived daily rows, `"episodes"` uses explicitly reconstructed
#'   Table 2 episodes, and `"both"` returns both with provenance.
#'
#' @return A list with `event`, `episode`, and `daily` tibbles.
#' @export
make_second_level_appusage <- function(data, export_type = NULL,
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
  if (!identical(meta_daily_source, "summary") && !isTRUE(reconstruct_meta)) {
    cli::cli_abort("`meta_daily_source = {.val {meta_daily_source}}` requires `reconstruct_meta = TRUE`.")
  }
  first <- normalize_first_level_appusage(data, export_type = export_type)

  event <- empty_second_event_tibble()
  episode <- empty_second_episode_tibble()
  daily <- empty_second_daily_tibble()
  meta_episode <- empty_second_episode_tibble()
  meta_summary_daily <- empty_second_daily_tibble()
  meta_episode_daily <- empty_second_daily_tibble()

  if (!is.null(first$meta_events)) {
    event <- second_level_events(first$meta_events)
  }
  if (!is.null(first$line)) {
    episode <- second_level_episodes(first$line, max_episode_ms = max_episode_ms)
    daily <- daily_from_episodes(episode, max_daily_app_ms = max_daily_app_ms)
  }
  if (isTRUE(reconstruct_meta) && !is.null(first$meta_events)) {
    meta_episode <- reconstruct_meta_episodes(
      first$meta_events,
      pairing = meta_pairing,
      start_event_types = meta_start_event_types,
      end_event_types = meta_end_event_types,
      max_episode_ms = max_episode_ms,
      merge_contiguous = merge_meta_episodes,
      merge_gap_ms = meta_episode_merge_gap_ms
    )
    episode <- conform_second_episode(rbind(episode, meta_episode))
  }
  if (!is.null(first$day)) {
    daily <- second_level_daily(first$day, max_daily_app_ms = max_daily_app_ms)
  }
  if (!is.null(first$app)) {
    daily <- second_level_daily(first$app, max_daily_app_ms = max_daily_app_ms)
  }
  if (!is.null(first$meta_summary)) {
    meta_summary_daily <- second_level_meta_summary(first$meta_summary, max_daily_app_ms = max_daily_app_ms)
  }
  if (isTRUE(reconstruct_meta) && !is.null(first$meta_events)) {
    meta_episode_daily <- aggregate_meta_episodes_daily(
      meta_episode,
      summary_daily = meta_summary_daily,
      max_daily_app_ms = max_daily_app_ms
    )
  }
  if (!is.null(first$meta_summary) || (isTRUE(reconstruct_meta) && !is.null(first$meta_events))) {
    meta_summary_daily <- compare_meta_daily_sources(meta_summary_daily, meta_episode_daily)
    meta_episode_daily <- compare_meta_daily_sources(meta_episode_daily, meta_summary_daily)
    daily <- switch(meta_daily_source,
      summary = meta_summary_daily,
      episodes = meta_episode_daily,
      both = conform_second_daily(rbind(meta_summary_daily, meta_episode_daily))
    )
  }

  if (!isTRUE(include_collection_app)) {
    event <- filter_collection_app(event)
    episode <- filter_collection_app(episode)
    daily <- filter_collection_app(daily)
  }

  out <- list(
    event = event,
    episode = episode,
    daily = daily
  )
  attr(out, "meta_reconstruction_diagnostics") <-
    attr(meta_episode, "meta_reconstruction_diagnostics", exact = TRUE)
  out
}

#' Write a second-level APP Usage cache
#'
#' Reads a first-level APP Usage RDA file containing exactly one object named
#' `data`, converts it with `make_second_level_appusage()`, and writes matching
#' `proc-2` RDA and JSON files. The RDA still contains exactly one object named
#' `data`; the JSON stores second-level transformation metadata.
#'
#' @param first_level_rda Path to a first-level RDA file.
#' @param output_dir Output directory. Defaults to the sibling `proclevel-2`
#'   directory when `first_level_rda` is under `proclevel-1`, otherwise the
#'   first-level RDA directory.
#' @param overwrite Whether to overwrite an existing output file.
#' @param include_collection_app Whether to keep `com.w.appusage` rows in
#'   second-level outputs.
#' @param max_episode_ms Episode duration above this value is flagged as an
#'   anomaly.
#' @param max_daily_app_ms Daily app duration above this value is flagged as an
#'   anomaly.
#' @param reconstruct_meta Whether to explicitly reconstruct meta Table 2 events
#'   into episode records. Defaults to `TRUE` for second-level meta output.
#' @param meta_pairing Pairing strategy for meta event reconstruction.
#' @param meta_start_event_types Event types that start meta episodes.
#' @param meta_end_event_types Event types that end meta episodes.
#' @param merge_meta_episodes Whether to merge adjacent reconstructed meta
#'   episodes for the same app when the time gap is small.
#' @param meta_episode_merge_gap_ms Maximum gap, in milliseconds, allowed when
#'   merging adjacent reconstructed meta episodes.
#' @param meta_daily_source Source for meta daily rows.
#' @param inline_qc Whether to compute routine daily QC metadata while the
#'   second-level object is still in memory.
#' @param require_all_weekdays Whether inline daily QC requires Monday through
#'   Sunday coverage.
#' @param min_nonempty_days Minimum number of non-empty recorded days required
#'   by inline daily QC.
#' @param use_all_apps_row,drop_likely_total_all_rows,all_row_tolerance Routine
#'   daily QC controls passed to [qc_appusage_day()].
#' @param max_daily_total_ms,max_export_lookback_days,meta_diff_abs_ms,meta_diff_ratio
#'   Anomaly/QC thresholds used for inline metadata.
#'
#' @return Invisibly returns the written second-level RDA path.
#' @export
write_second_level_appusage <- function(first_level_rda, output_dir = NULL,
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
                                        meta_daily_source = c("summary", "episodes", "both"),
                                        inline_qc = TRUE,
                                        require_all_weekdays = TRUE,
                                        min_nonempty_days = 7,
                                        use_all_apps_row = FALSE,
                                        drop_likely_total_all_rows = TRUE,
                                        all_row_tolerance = 0.10,
                                        max_daily_total_ms = 24 * 60 * 60 * 1000,
                                        max_export_lookback_days = 31,
                                        meta_diff_abs_ms = 60 * 1000,
                                        meta_diff_ratio = 0.20) {
  if (!file.exists(first_level_rda)) {
    cli::cli_abort("First-level RDA file does not exist: {.path {first_level_rda}}")
  }
  meta_pairing <- match.arg(meta_pairing)
  meta_daily_source <- match.arg(meta_daily_source)
  started_at <- Sys.time()
  load_started_at <- Sys.time()
  first <- load_appusage_data_object(first_level_rda)
  load_finished_at <- Sys.time()
  first_metadata <- read_first_level_metadata_for_second(first_level_rda)

  entities <- parse_appusage_filename(first_level_rda)
  participant_id <- entities$sub %||% "record-000001"
  export_type <- entities$type %||% infer_first_level_type(first) %||% "unknown"
  source_cache_key <- entities$src %||%
    appusage_metadata_source_identity(first_metadata)$source_cache_key
  convert_started_at <- Sys.time()
  second <- make_second_level_appusage(
    first,
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
  convert_finished_at <- Sys.time()

  output_dir <- output_dir %||% default_second_level_output_dir(first_level_rda)
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  output_file <- file.path(
    output_dir,
    build_appusage_filename(
      participant_id = participant_id,
      export_type = export_type,
      proc = 2,
      extension = "rda",
      source_key = source_cache_key
    )
  )
  metadata_file <- second_level_metadata_path(output_file)
  if ((file.exists(output_file) || file.exists(metadata_file)) && !isTRUE(overwrite)) {
    stop(batch_cache_exists_error(paste(c(output_file, metadata_file), collapse = "; ")))
  }
  appusage_cleanup_second_level_transaction_artifacts(output_file, metadata_file)
  transaction <- appusage_second_level_transaction_paths(output_file, metadata_file)
  on.exit(
    appusage_cleanup_paths(c(transaction$temp_rda, transaction$temp_json)),
    add = TRUE
  )
  data <- second
  save_started_at <- Sys.time()
  appusage_save_second_level_data(data, transaction$temp_rda)
  save_finished_at <- Sys.time()
  inline_qc_result <- NULL
  inline_qc_started_at <- NULL
  inline_qc_finished_at <- NULL
  if (isTRUE(inline_qc)) {
    inline_qc_started_at <- Sys.time()
    inline_qc_result <- tryCatch(
      run_qc_for_second_level_data(
        data = second,
        second_level_rda = output_file,
        participant_id = participant_id,
        require_all_weekdays = require_all_weekdays,
        min_nonempty_days = min_nonempty_days,
        use_all_apps_row = use_all_apps_row,
        include_collection_app = include_collection_app,
        drop_likely_total_all_rows = drop_likely_total_all_rows,
        all_row_tolerance = all_row_tolerance,
        metadata = first_metadata,
        max_episode_ms = max_episode_ms,
        max_daily_app_ms = max_daily_app_ms,
        max_daily_total_ms = max_daily_total_ms,
        max_export_lookback_days = max_export_lookback_days,
        meta_diff_abs_ms = meta_diff_abs_ms,
        meta_diff_ratio = meta_diff_ratio
      ),
      error = function(e) {
        counts <- tryCatch(second_level_qc_counts(second), error = function(e2) empty_qc_counts())
        anomaly_qc <- tryCatch(
          qc_appusage_anomalies(
            second,
            metadata = first_metadata,
            max_episode_ms = max_episode_ms,
            max_daily_app_ms = max_daily_app_ms,
            max_daily_total_ms = max_daily_total_ms,
            max_export_lookback_days = max_export_lookback_days,
            meta_diff_abs_ms = meta_diff_abs_ms,
            meta_diff_ratio = meta_diff_ratio
          ),
          error = function(e2) {
            appusage_empty_anomaly_qc(
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
          }
        )
        counts$n_anomalies <- anomaly_qc$n_anomalies_total
        qc_error_result(
          metadata = NULL,
          second_level_rda = output_file,
          message = conditionMessage(e),
          counts = counts,
          anomaly_qc = anomaly_qc
        )
      }
    )
    inline_qc_finished_at <- Sys.time()
  }
  finished_at <- Sys.time()
  profiling <- second_level_profile(
    first_level_rda = first_level_rda,
    second_level_rda = transaction$temp_rda,
    second_level_data = second,
    started_at = started_at,
    finished_at = finished_at,
    load_started_at = load_started_at,
    load_finished_at = load_finished_at,
    convert_started_at = convert_started_at,
    convert_finished_at = convert_finished_at,
    save_started_at = save_started_at,
    save_finished_at = save_finished_at,
    inline_qc_started_at = inline_qc_started_at,
    inline_qc_finished_at = inline_qc_finished_at
  )
  metadata <- build_second_level_success_metadata(
    first_metadata = first_metadata,
    first_level_rda = first_level_rda,
    second_level_rda = output_file,
    second_level_data = second,
    include_collection_app = include_collection_app,
    max_episode_ms = max_episode_ms,
    max_daily_app_ms = max_daily_app_ms,
    reconstruct_meta = reconstruct_meta,
    meta_pairing = meta_pairing,
    meta_start_event_types = meta_start_event_types,
    meta_end_event_types = meta_end_event_types,
    merge_meta_episodes = merge_meta_episodes,
    meta_episode_merge_gap_ms = meta_episode_merge_gap_ms,
    meta_daily_source = meta_daily_source,
    inline_qc_result = inline_qc_result,
    inline_qc_started_at = inline_qc_started_at,
    inline_qc_finished_at = inline_qc_finished_at,
    profiling = profiling,
    started_at = started_at,
    finished_at = finished_at
  )
  write_metadata_json(metadata, transaction$temp_json)
  appusage_validate_second_level_success_metadata(
    transaction$temp_json,
    output_file,
    metadata_file
  )
  appusage_publish_second_level_pair(
    transaction = transaction,
    output_file = output_file,
    metadata_file = metadata_file
  )
  invisible(normalizePath(output_file, winslash = "/", mustWork = FALSE))
}

second_level_profile <- function(first_level_rda, second_level_rda,
                                 second_level_data, started_at, finished_at,
                                 load_started_at, load_finished_at,
                                 convert_started_at, convert_finished_at,
                                 save_started_at, save_finished_at,
                                 inline_qc_started_at = NULL,
                                 inline_qc_finished_at = NULL) {
  counts <- if (is.null(second_level_data)) empty_qc_counts() else second_level_qc_counts(second_level_data)
  list(
    total_elapsed_sec = as.numeric(difftime(finished_at, started_at, units = "secs")),
    load_elapsed_sec = as.numeric(difftime(load_finished_at, load_started_at, units = "secs")),
    convert_elapsed_sec = as.numeric(difftime(convert_finished_at, convert_started_at, units = "secs")),
    save_elapsed_sec = as.numeric(difftime(save_finished_at, save_started_at, units = "secs")),
    inline_qc_elapsed_sec = if (!is.null(inline_qc_started_at) && !is.null(inline_qc_finished_at)) {
      as.numeric(difftime(inline_qc_finished_at, inline_qc_started_at, units = "secs"))
    } else {
      NA_real_
    },
    metadata_json_write_elapsed_sec = NA_real_,
    n_event_rows = counts$n_event_rows,
    n_episode_rows = counts$n_episode_rows,
    n_daily_rows = counts$n_daily_rows,
    first_level_rda_size_bytes = if (file.exists(first_level_rda)) file.info(first_level_rda)$size[[1]] else NA_real_,
    second_level_rda_size_bytes = if (file.exists(second_level_rda)) file.info(second_level_rda)$size[[1]] else NA_real_,
    worker_pid = Sys.getpid()
  )
}

default_second_level_output_dir <- function(first_level_rda) {
  parent <- dirname(first_level_rda)
  if (identical(basename(parent), "proclevel-1")) {
    return(file.path(dirname(parent), "proclevel-2"))
  }
  parent
}

second_level_metadata_path <- function(second_level_rda) {
  entities <- parse_appusage_filename(second_level_rda)
  file.path(
    dirname(second_level_rda),
    build_appusage_filename(
      participant_id = entities$sub %||% "unknown",
      export_type = entities$type %||% "unknown",
      proc = 2,
      extension = "json",
      source_key = entities$src %||% NULL
    )
  )
}

appusage_second_level_transaction_id <- function() {
  token <- paste(
    format(Sys.time(), "%Y%m%dT%H%M%OS6"),
    Sys.getpid(),
    basename(tempfile(pattern = "txn-")),
    sep = "-"
  )
  gsub("[^A-Za-z0-9._-]", "-", token)
}

appusage_second_level_transaction_paths <- function(output_file, metadata_file) {
  token <- appusage_second_level_transaction_id()
  list(
    temp_rda = file.path(
      dirname(output_file),
      paste0(".", basename(output_file), ".appusage-tmp-", token)
    ),
    temp_json = file.path(
      dirname(metadata_file),
      paste0(".", basename(metadata_file), ".appusage-tmp-", token)
    ),
    backup_rda = file.path(
      dirname(output_file),
      paste0(".", basename(output_file), ".appusage-backup-", token)
    ),
    backup_json = file.path(
      dirname(metadata_file),
      paste0(".", basename(metadata_file), ".appusage-backup-", token)
    )
  )
}

appusage_second_level_owned_artifacts <- function(output_file, metadata_file) {
  directory <- dirname(output_file)
  if (!dir.exists(directory)) {
    return(character())
  }
  candidates <- list.files(directory, full.names = TRUE, all.files = TRUE)
  prefixes <- c(
    paste0(".", basename(output_file), ".appusage-"),
    paste0(".", basename(metadata_file), ".appusage-")
  )
  candidates[vapply(
    basename(candidates),
    function(path) any(startsWith(path, prefixes)),
    logical(1)
  )]
}

appusage_cleanup_paths <- function(paths) {
  paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  if (length(paths) > 0L) {
    unlink(paths, force = TRUE)
  }
  invisible(NULL)
}

appusage_cleanup_second_level_transaction_artifacts <- function(
    output_file, metadata_file) {
  appusage_cleanup_paths(appusage_second_level_owned_artifacts(
    output_file,
    metadata_file
  ))
}

appusage_file_size_bytes <- function(path) {
  info <- suppressWarnings(file.info(path))
  if (nrow(info) == 0L || !"size" %in% names(info)) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(info$size[[1]]))
}

appusage_validate_nonempty_file <- function(path, label) {
  size <- appusage_file_size_bytes(path)
  if (!file.exists(path) || is.na(size) || size <= 0) {
    stop(label, " was not written as a non-empty file.")
  }
  invisible(path)
}

appusage_save_second_level_data <- function(data, path) {
  save(data, file = path)
  appusage_validate_nonempty_file(path, "Second-level RDA temporary artifact")
  invisible(path)
}

appusage_normalized_paths_equal <- function(x, y) {
  is_present_string(x) && is_present_string(y) && identical(
    normalizePath(x, winslash = "/", mustWork = FALSE),
    normalizePath(y, winslash = "/", mustWork = FALSE)
  )
}

appusage_validate_second_level_success_metadata <- function(
    path, output_file, metadata_file) {
  appusage_validate_nonempty_file(path, "Second-level JSON temporary artifact")
  metadata <- jsonlite::read_json(path, simplifyVector = TRUE)
  status <- appusage_nested_value(metadata, c("processing", "second_level_status"))
  recorded_rda <- appusage_nested_value(metadata, c("outputs", "second_level_rda"))
  recorded_json <- appusage_nested_value(metadata, c("outputs", "metadata_json"))
  entities <- parse_appusage_filename(output_file)
  recorded_identity <- appusage_metadata_source_identity(metadata)
  if (!identical(as.character(status), "success")) {
    stop("Second-level success metadata does not contain success status.")
  }
  if (!appusage_normalized_paths_equal(recorded_rda, output_file)) {
    stop("Second-level success metadata RDA path failed validation.")
  }
  if (!is_present_string(recorded_json)) {
    stop("Second-level success metadata JSON path is missing.")
  }
  if (!appusage_normalized_paths_equal(recorded_json, metadata_file)) {
    stop("Second-level success metadata JSON path failed validation.")
  }
  if (is_present_string(entities$src)) {
    if (!is_present_string(recorded_identity$source_record_key) ||
      !is_present_string(recorded_identity$source_fingerprint)) {
      stop("Second-level success metadata source identity is missing.")
    }
    if (!identical(
      sanitize_entity_value(recorded_identity$source_cache_key),
      as.character(entities$src)
    )) {
      stop("Second-level success metadata source cache key failed validation.")
    }
  }
  metadata
}

appusage_promote_file <- function(from, to) {
  isTRUE(file.rename(from, to))
}

appusage_publish_second_level_pair <- function(transaction, output_file,
                                               metadata_file) {
  old_json_backed <- FALSE
  old_rda_backed <- FALSE
  new_rda_published <- FALSE
  committed <- FALSE
  rollback <- function() {
    if (file.exists(metadata_file)) {
      unlink(metadata_file, force = TRUE)
    }
    if (new_rda_published && file.exists(output_file)) {
      unlink(output_file, force = TRUE)
    }
    rda_ready <- !old_rda_backed
    if (old_rda_backed && file.exists(transaction$backup_rda)) {
      rda_ready <- tryCatch(
        appusage_promote_file(transaction$backup_rda, output_file),
        error = function(e) FALSE
      )
    }
    if (old_json_backed && rda_ready && file.exists(output_file) &&
      file.exists(transaction$backup_json)) {
      tryCatch(
        appusage_promote_file(transaction$backup_json, metadata_file),
        error = function(e) FALSE
      )
    }
    invisible(NULL)
  }
  on.exit({
    if (!committed) {
      rollback()
    }
    appusage_cleanup_paths(c(
      transaction$temp_rda,
      transaction$temp_json,
      if (committed) transaction$backup_rda else character(),
      if (committed) transaction$backup_json else character()
    ))
  }, add = TRUE)

  if (file.exists(metadata_file)) {
    if (!appusage_promote_file(metadata_file, transaction$backup_json)) {
      stop("Could not back up the existing second-level success JSON marker.")
    }
    old_json_backed <- TRUE
  }
  if (file.exists(output_file)) {
    if (!appusage_promote_file(output_file, transaction$backup_rda)) {
      stop("Could not back up the existing second-level RDA.")
    }
    old_rda_backed <- TRUE
  }
  if (!appusage_promote_file(transaction$temp_rda, output_file)) {
    stop("Could not promote the second-level RDA.")
  }
  new_rda_published <- TRUE
  if (!appusage_promote_file(transaction$temp_json, metadata_file)) {
    stop("Could not promote the second-level success JSON marker.")
  }
  committed <- TRUE
  invisible(list(rda = output_file, json = metadata_file))
}

appusage_atomic_write_metadata_json <- function(metadata, metadata_file) {
  output_file <- sub("[.]json$", ".rda", metadata_file)
  appusage_cleanup_second_level_transaction_artifacts(output_file, metadata_file)
  transaction <- appusage_second_level_transaction_paths(output_file, metadata_file)
  old_backed <- FALSE
  committed <- FALSE
  write_metadata_json(metadata, transaction$temp_json)
  appusage_validate_nonempty_file(
    transaction$temp_json,
    "Second-level status JSON temporary artifact"
  )
  jsonlite::read_json(transaction$temp_json, simplifyVector = TRUE)
  on.exit({
    if (!committed && old_backed && file.exists(transaction$backup_json) &&
      !file.exists(metadata_file)) {
      tryCatch(
        appusage_promote_file(transaction$backup_json, metadata_file),
        error = function(e) FALSE
      )
    }
    appusage_cleanup_paths(c(
      transaction$temp_json,
      if (committed) transaction$backup_json else character()
    ))
  }, add = TRUE)
  if (file.exists(metadata_file)) {
    if (!appusage_promote_file(metadata_file, transaction$backup_json)) {
      stop("Could not back up the existing second-level status JSON.")
    }
    old_backed <- TRUE
  }
  if (!appusage_promote_file(transaction$temp_json, metadata_file)) {
    stop("Could not promote the second-level status JSON.")
  }
  committed <- TRUE
  normalizePath(metadata_file, winslash = "/", mustWork = FALSE)
}

appusage_valid_second_level_success_pair <- function(metadata_file) {
  if (!file.exists(metadata_file)) {
    return(FALSE)
  }
  output_file <- sub("[.]json$", ".rda", metadata_file)
  rda_size <- appusage_file_size_bytes(output_file)
  if (!file.exists(output_file) || is.na(rda_size) || rda_size <= 0) {
    return(FALSE)
  }
  tryCatch({
    appusage_validate_second_level_success_metadata(
      metadata_file,
      output_file,
      metadata_file
    )
    TRUE
  }, error = function(e) FALSE)
}

first_level_metadata_path <- function(first_level_rda) {
  sub("[.]rda$", ".json", first_level_rda, ignore.case = TRUE)
}

read_first_level_metadata_for_second <- function(first_level_rda) {
  metadata_file <- first_level_metadata_path(first_level_rda)
  if (file.exists(metadata_file)) {
    return(read_first_level_metadata(metadata_file)$metadata)
  }
  entities <- parse_appusage_filename(first_level_rda)
  participant_id <- entities$sub %||% "unknown"
  export_type <- entities$type %||% "unknown"
  list(
    schema_version = "0.2.0",
    package_version = as.character(utils::packageVersion("appusageR")),
    parser_version = as.character(utils::packageVersion("appusageR")),
    created_at = NA_character_,
    updated_at = NA_character_,
    participant_id = participant_id,
    participant_id_source = NA_character_,
    identity = list(
      participant_id = participant_id,
      participant_id_source = NA_character_,
      wenjuanxing_sequence_id = NA_integer_,
      source_record_key = NA_character_,
      source_cache_key = entities$src %||% NA_character_
    ),
    source = list(
      source_fingerprint = NA_character_,
      source_cache_key = entities$src %||% NA_character_
    ),
    export = list(
      detected_type = export_type,
      content_detected_export_type = export_type,
      native_export_type_from_filename = NA_character_,
      export_type_match = NA
    ),
    processing = list(first_level_status = "unknown"),
    outputs = list(
      metadata_json = NA_character_,
      first_level_rda = normalizePath(first_level_rda, winslash = "/", mustWork = FALSE)
    ),
    counts = list(n_parse_warnings = NA_integer_),
    errors = list()
  )
}

build_second_level_success_metadata <- function(first_metadata = NULL,
                                                first_level_rda,
                                                second_level_rda,
                                                second_level_data,
                                                include_collection_app,
                                                max_episode_ms,
                                                max_daily_app_ms,
                                                reconstruct_meta,
                                                meta_pairing,
                                                meta_start_event_types,
                                                meta_end_event_types,
                                                merge_meta_episodes,
                                                meta_episode_merge_gap_ms,
                                                meta_daily_source,
                                                inline_qc_result = NULL,
                                                inline_qc_started_at = NULL,
                                                inline_qc_finished_at = NULL,
                                                profiling = NULL,
                                                started_at,
                                                finished_at) {
  first_metadata <- first_metadata %||% read_first_level_metadata_for_second(first_level_rda)
  metadata_file <- second_level_metadata_path(second_level_rda)
  metadata <- build_second_level_metadata(
    first_metadata = first_metadata,
    first_level_rda = first_level_rda,
    first_level_metadata_file = first_level_metadata_path(first_level_rda),
    second_level_rda = second_level_rda,
    second_level_metadata_file = metadata_file,
    second_level_data = second_level_data,
    status = "success",
    error = NULL,
    include_collection_app = include_collection_app,
    max_episode_ms = max_episode_ms,
    max_daily_app_ms = max_daily_app_ms,
    reconstruct_meta = reconstruct_meta,
    meta_pairing = meta_pairing,
    meta_start_event_types = meta_start_event_types,
    meta_end_event_types = meta_end_event_types,
    merge_meta_episodes = merge_meta_episodes,
    meta_episode_merge_gap_ms = meta_episode_merge_gap_ms,
    meta_daily_source = meta_daily_source,
    inline_qc_result = inline_qc_result,
    inline_qc_started_at = inline_qc_started_at,
    inline_qc_finished_at = inline_qc_finished_at,
    profiling = profiling,
    started_at = started_at,
    finished_at = finished_at
  )
  metadata
}

write_second_level_success_metadata <- function(...) {
  arguments <- list(...)
  metadata <- do.call(build_second_level_success_metadata, arguments)
  metadata_file <- second_level_metadata_path(arguments$second_level_rda)
  appusage_atomic_write_metadata_json(metadata, metadata_file)
  invisible(metadata_file)
}

write_second_level_status_metadata <- function(batch_summary, index, output_dir,
                                               status, error = NULL,
                                               started_at = Sys.time(),
                                               finished_at = Sys.time()) {
  if (is.null(output_dir)) {
    return(NA_character_)
  }
  participant_id <- batch_summary$participant_id[[index]]
  export_type <- batch_summary$detected_type[[index]]
  first_level_rda <- batch_summary$data_file[[index]]
  first_metadata_file <- batch_summary$metadata_file[[index]]
  if (!is_present_string(export_type)) {
    export_type <- "unknown"
  }
  metadata_file <- file.path(
    output_dir,
    build_appusage_filename(
      participant_id = participant_id,
      export_type = export_type,
      proc = 2,
      extension = "json",
      source_key = parse_appusage_filename(first_level_rda)$src %||% NULL
    )
  )
  first_metadata <- if (is_present_string(first_metadata_file) && file.exists(first_metadata_file)) {
    read_first_level_metadata(first_metadata_file)$metadata
  } else if (is_present_string(first_level_rda)) {
    read_first_level_metadata_for_second(first_level_rda)
  } else {
    minimal_qc_metadata(metadata_file)
  }
  if (is.null(first_metadata$identity) || !is.list(first_metadata$identity)) {
    first_metadata$identity <- list()
  }
  if (is.null(first_metadata$source) || !is.list(first_metadata$source)) {
    first_metadata$source <- list()
  }
  first_metadata$identity$source_record_key <- appusage_summary_cell(
    batch_summary, "source_record_key", index,
    first_metadata$identity$source_record_key %||% NA_character_
  )
  first_metadata$identity$source_cache_key <- appusage_summary_cell(
    batch_summary, "source_cache_key", index,
    first_metadata$identity$source_cache_key %||% NA_character_
  )
  first_metadata$source$source_fingerprint <- appusage_summary_cell(
    batch_summary, "source_fingerprint", index,
    first_metadata$source$source_fingerprint %||% NA_character_
  )
  first_metadata$source$source_cache_key <- first_metadata$identity$source_cache_key
  metadata <- build_second_level_metadata(
    first_metadata = first_metadata,
    first_level_rda = first_level_rda,
    first_level_metadata_file = first_metadata_file,
    second_level_rda = NA_character_,
    second_level_metadata_file = metadata_file,
    second_level_data = NULL,
    status = status,
    error = error,
    include_collection_app = NA,
    max_episode_ms = NA_real_,
    max_daily_app_ms = NA_real_,
    reconstruct_meta = FALSE,
    meta_pairing = NA_character_,
    meta_start_event_types = NA_real_,
    meta_end_event_types = NA_real_,
    merge_meta_episodes = FALSE,
    meta_episode_merge_gap_ms = NA_real_,
    meta_daily_source = "summary",
    started_at = started_at,
    finished_at = finished_at
  )
  if (appusage_valid_second_level_success_pair(metadata_file)) {
    return(normalizePath(metadata_file, winslash = "/", mustWork = FALSE))
  }
  appusage_atomic_write_metadata_json(metadata, metadata_file)
}

build_second_level_metadata <- function(first_metadata, first_level_rda,
                                        first_level_metadata_file,
                                        second_level_rda,
                                        second_level_metadata_file,
                                        second_level_data, status, error,
                                        include_collection_app,
                                        max_episode_ms,
                                        max_daily_app_ms,
                                        reconstruct_meta,
                                        meta_pairing,
                                        meta_start_event_types,
                                        meta_end_event_types,
                                        merge_meta_episodes,
                                        meta_episode_merge_gap_ms,
                                        meta_daily_source,
                                        inline_qc_result = NULL,
                                        inline_qc_started_at = NULL,
                                        inline_qc_finished_at = NULL,
                                        profiling = NULL,
                                        started_at,
                                        finished_at) {
  metadata <- first_metadata
  if (is.null(metadata$processing) || !is.list(metadata$processing)) {
    metadata$processing <- list()
  }
  if (is.null(metadata$outputs) || !is.list(metadata$outputs)) {
    metadata$outputs <- list()
  }
  metadata$schema_version <- "0.2.0"
  metadata$package_version <- as.character(utils::packageVersion("appusageR"))
  metadata$parser_version <- metadata$parser_version %||%
    as.character(utils::packageVersion("appusageR"))
  metadata$updated_at <- format(finished_at, "%Y-%m-%dT%H:%M:%OS3%z")
  metadata$processing$second_level_status <- status
  metadata$processing$second_level_started_at <- format(started_at, "%Y-%m-%dT%H:%M:%OS3%z")
  metadata$processing$second_level_finished_at <- format(finished_at, "%Y-%m-%dT%H:%M:%OS3%z")
  metadata$processing$second_level_elapsed_sec <- as.numeric(
    difftime(finished_at, started_at, units = "secs")
  )
  metadata$processing$second_level_function <- "make_second_level_appusage"
  metadata$processing$qc_status <- "not_run"
  metadata$processing$app_category_status <- "not_run"
  metadata$second_level <- list(
    function_name = "make_second_level_appusage",
    parameters = list(
      include_collection_app = include_collection_app,
      max_episode_ms = max_episode_ms,
      max_daily_app_ms = max_daily_app_ms,
      reconstruct_meta = reconstruct_meta,
      meta_pairing = meta_pairing,
      meta_start_event_types = as.numeric(meta_start_event_types),
      meta_end_event_types = as.numeric(meta_end_event_types),
      merge_meta_episodes = isTRUE(merge_meta_episodes),
      meta_episode_merge_gap_ms = as.numeric(meta_episode_merge_gap_ms),
      meta_daily_source = meta_daily_source
    ),
    profiling = profiling %||% list()
  )
  metadata$processing$meta_reconstruction_status <- if (isTRUE(reconstruct_meta)) {
    "requested"
  } else {
    "not_requested"
  }
  metadata$meta_reconstruction <- summarize_meta_reconstruction(
    second_level_data,
    requested = reconstruct_meta,
    pairing = meta_pairing,
    start_event_types = meta_start_event_types,
    end_event_types = meta_end_event_types
  )
  metadata$meta_daily_comparison <- summarize_meta_daily_comparison(
    second_level_data,
    selected_source = meta_daily_source,
    reconstruction_used = isTRUE(reconstruct_meta) &&
      !is.null(second_level_data) &&
      is.data.frame(second_level_data$episode) &&
      any(second_level_data$episode$episode_source == "meta_events", na.rm = TRUE)
  )
  metadata$outputs$metadata_json <- normalizePath(
    second_level_metadata_file,
    winslash = "/",
    mustWork = FALSE
  )
  metadata$outputs$first_level_metadata_json <- ifelse(
    is_present_string(first_level_metadata_file),
    normalizePath(first_level_metadata_file, winslash = "/", mustWork = FALSE),
    NA_character_
  )
  metadata$outputs$first_level_rda <- ifelse(
    is_present_string(first_level_rda),
    normalizePath(first_level_rda, winslash = "/", mustWork = FALSE),
    NA_character_
  )
  metadata$outputs$second_level_metadata_json <- normalizePath(
    second_level_metadata_file,
    winslash = "/",
    mustWork = FALSE
  )
  metadata$outputs$second_level_rda <- ifelse(
    is_present_string(second_level_rda),
    normalizePath(second_level_rda, winslash = "/", mustWork = FALSE),
    NA_character_
  )
  metadata$first_level_counts <- metadata$counts
  metadata$counts <- if (is.null(second_level_data)) {
    empty_qc_counts()
  } else {
    second_level_qc_counts(second_level_data)
  }
  metadata$qc <- list(
    qc_status = "not_run",
    pass_qc = NA,
    n_recorded_days = NA,
    n_nonempty_days = NA,
    weekdays_covered = NA
  )
  metadata$category_dictionary <- list(app_category_status = "not_run")
  metadata$errors <- error_metadata(error)
  if (!is.null(inline_qc_result)) {
    metadata <- update_qc_metadata(
      metadata = metadata,
      metadata_file = second_level_metadata_file,
      qc_file = second_level_metadata_file,
      result = inline_qc_result,
      started_at = inline_qc_started_at %||% started_at,
      finished_at = inline_qc_finished_at %||% finished_at
    )
    metadata$qc$qc_function <- "qc_appusage_day_inline"
  }
  metadata
}

summarize_meta_reconstruction <- function(second_level_data, requested,
                                          pairing, start_event_types,
                                          end_event_types) {
  diagnostics <- if (is.list(second_level_data)) {
    attr(second_level_data, "meta_reconstruction_diagnostics", exact = TRUE)
  } else {
    NULL
  }
  episodes <- if (is.list(second_level_data) && is.data.frame(second_level_data$episode)) {
    second_level_data$episode
  } else {
    empty_second_episode_tibble()
  }
  diagnostics <- diagnostics %||%
    attr(episodes, "meta_reconstruction_diagnostics", exact = TRUE)
  meta <- if ("episode_source" %in% names(episodes)) {
    episodes[episodes$episode_source == "meta_events", , drop = FALSE]
  } else {
    empty_second_episode_tibble()
  }
  n_complete <- diagnostics$n_complete_episodes %||%
    sum(meta$reconstruction_status == "complete", na.rm = TRUE)
  n_invalid <- diagnostics$n_invalid_pairs %||%
    sum(meta$reconstruction_status == "invalid_pair", na.rm = TRUE)
  n_dropped <- diagnostics$n_dropped_unmatched_events %||% 0L
  n_dropped_starts <- diagnostics$n_dropped_unmatched_starts %||% 0L
  n_dropped_ends <- diagnostics$n_dropped_unmatched_ends %||% 0L
  dropped_prop <- diagnostics$dropped_unmatched_event_proportion %||% NA_real_
  list(
    requested = isTRUE(requested),
    status = if (isTRUE(requested)) "requested" else "not_requested",
    pairing_strategy = diagnostics$pairing_strategy %||% ifelse(is.na(pairing), NA_character_, pairing),
    start_event_types = diagnostics$start_event_types %||% as.numeric(start_event_types),
    end_event_types = diagnostics$end_event_types %||% as.numeric(end_event_types),
    n_event_rows = diagnostics$n_event_rows %||% NA_integer_,
    n_start_events = diagnostics$n_start_events %||% NA_integer_,
    n_end_events = diagnostics$n_end_events %||% NA_integer_,
    n_device_shutdown_events = diagnostics$n_device_shutdown_events %||% NA_integer_,
    n_device_startup_events = diagnostics$n_device_startup_events %||% NA_integer_,
    n_unknown_event_type_rows = diagnostics$n_unknown_event_type_rows %||% NA_integer_,
    unknown_event_types = diagnostics$unknown_event_types %||% numeric(),
    n_complete_episodes = n_complete,
    n_invalid_pairs = n_invalid,
    n_unmatched_starts = sum(meta$unmatched_start, na.rm = TRUE),
    n_unmatched_ends = sum(meta$unmatched_end, na.rm = TRUE),
    n_dropped_unmatched_events = n_dropped,
    n_dropped_unmatched_starts = n_dropped_starts,
    n_dropped_unmatched_ends = n_dropped_ends,
    dropped_unmatched_event_proportion = dropped_prop,
    n_duration_inferred_episodes = diagnostics$n_duration_inferred_episodes %||% 0L,
    merge_contiguous_episodes = diagnostics$merge_contiguous_episodes %||% FALSE,
    merge_gap_ms = diagnostics$merge_gap_ms %||% NA_real_,
    n_premerge_episode_rows = diagnostics$n_premerge_episode_rows %||% nrow(meta),
    n_episode_merge_groups = diagnostics$n_episode_merge_groups %||% 0L,
    n_episode_merge_edges = diagnostics$n_episode_merge_edges %||% 0L,
    n_episode_rows_reduced_by_merge = diagnostics$n_episode_rows_reduced_by_merge %||% 0L,
    total_merged_gap_ms = diagnostics$total_merged_gap_ms %||% 0,
    n_timeline_clipped_episodes = diagnostics$n_timeline_clipped_episodes %||% 0L,
    total_timeline_clipped_ms = diagnostics$total_timeline_clipped_ms %||% 0,
    n_timeline_clipped_to_nonpositive = diagnostics$n_timeline_clipped_to_nonpositive %||% 0L,
    n_device_boundary_episodes = diagnostics$n_device_boundary_episodes %||%
      sum(meta$device_boundary_involved, na.rm = TRUE),
    n_reconstruction_warnings = diagnostics$n_reconstruction_warnings %||%
      sum(!is.na(meta$reconstruction_warning) & meta$reconstruction_warning != "", na.rm = TRUE)
  )
}

summarize_meta_daily_comparison <- function(second_level_data, selected_source,
                                            reconstruction_used) {
  daily <- if (is.list(second_level_data) && is.data.frame(second_level_data$daily)) {
    conform_second_daily(second_level_data$daily)
  } else {
    empty_second_daily_tibble()
  }
  meta <- daily[daily$source_export_type == "meta", , drop = FALSE]
  source_counts <- table(meta$daily_source, useNA = "no")
  source_counts <- as.list(stats::setNames(as.integer(source_counts), names(source_counts)))
  comparison <- unique_meta_daily_comparisons(meta)
  matched <- comparison$duration_agreement_status %in% c("matched_exact", "matched_with_difference")
  diffs <- abs(comparison$duration_diff_ms[matched])
  episode_rows <- meta[meta$daily_source == "meta_episodes", , drop = FALSE]
  list(
    selected_source = selected_source,
    reconstruction_used_for_daily = isTRUE(reconstruction_used),
    n_meta_daily_rows_by_source = source_counts,
    n_matched_summary_episode_keys = sum(matched, na.rm = TRUE),
    n_summary_only_keys = sum(comparison$duration_agreement_status == "summary_only", na.rm = TRUE),
    n_episode_only_keys = sum(comparison$duration_agreement_status == "episode_only", na.rm = TRUE),
    total_abs_duration_diff_ms = if (length(diffs) == 0) 0 else sum(diffs, na.rm = TRUE),
    max_abs_duration_diff_ms = if (length(diffs) == 0) 0 else max(diffs, na.rm = TRUE),
    n_unmatched_starts = sum(episode_rows$unmatched_start_count, na.rm = TRUE),
    n_unmatched_ends = sum(episode_rows$unmatched_end_count, na.rm = TRUE),
    n_invalid_pairs = sum(episode_rows$invalid_pair_count, na.rm = TRUE),
    n_reconstruction_warnings = sum(episode_rows$reconstruction_warning_count, na.rm = TRUE)
  )
}

unique_meta_daily_comparisons <- function(meta) {
  if (nrow(meta) == 0) {
    return(meta)
  }
  key <- meta_daily_key(meta)
  keep <- !duplicated(paste(
    key,
    meta$duration_agreement_status,
    meta$duration_diff_ms,
    sep = "\r"
  ))
  meta[keep, , drop = FALSE]
}

normalize_first_level_appusage <- function(data, export_type = NULL) {
  if (is.data.frame(data)) {
    type <- export_type %||% infer_data_frame_export_type(data)
    out <- list()
    out[[type]] <- strip_individual_columns(data)
    return(out)
  }

  if (is.list(data) && all(c("summary", "events") %in% names(data))) {
    return(list(
      meta_summary = strip_individual_columns(data$summary),
      meta_events = strip_individual_columns(data$events)
    ))
  }

  if (!is.list(data)) {
    cli::cli_abort("`data` must be a parser tibble or first-level APP Usage list.")
  }

  out <- list()
  if (!is.null(data$line)) out$line <- strip_individual_columns(data$line)
  if (!is.null(data$day)) out$day <- strip_individual_columns(data$day)
  if (!is.null(data$app)) out$app <- strip_individual_columns(data$app)
  if (!is.null(data$meta_summary)) out$meta_summary <- strip_individual_columns(data$meta_summary)
  if (!is.null(data$meta_events)) out$meta_events <- strip_individual_columns(data$meta_events)

  if (length(out) == 0 && !is.null(export_type) && is.data.frame(data[[1]])) {
    out[[export_type]] <- strip_individual_columns(data[[1]])
  }
  out
}

infer_first_level_type <- function(data) {
  if (is.list(data)) {
    if (!is.null(data$line)) {
      return("line")
    }
    if (!is.null(data$day)) {
      return("day")
    }
    if (!is.null(data$app)) {
      return("app")
    }
    if (!is.null(data$meta_summary) || !is.null(data$meta_events)) {
      return("meta")
    }
  }
  if (is.data.frame(data)) {
    return(infer_data_frame_export_type(data))
  }
  NA_character_
}

infer_data_frame_export_type <- function(data) {
  if ("start_ts_ms" %in% names(data) && "end_ts_ms" %in% names(data)) {
    return("line")
  }
  if ("event_ts_ms" %in% names(data)) {
    return("meta_events")
  }
  if ("total_duration_ms" %in% names(data)) {
    return("meta_summary")
  }
  if ("duration_ms" %in% names(data)) {
    return(if ("export_type" %in% names(data)) unique(stats::na.omit(data$export_type))[[1]] else "day")
  }
  "unknown"
}

second_level_events <- function(x) {
  if (nrow(x) == 0) {
    return(empty_second_event_tibble())
  }
  event_date <- as.Date(x$event_datetime)
  event_date[is.na(event_date)] <- x$table_date[is.na(event_date)]
  out <- tibble::tibble(
    date = event_date,
    table_date = x$table_date,
    app_name = x$app_name,
    activity_type = classify_activity_type(x$app_name),
    package_name = x$package_name,
    class_name = x$class_name,
    event_datetime = x$event_datetime,
    event_ts_ms = x$event_ts_ms,
    event_type = x$event_type,
    event_type_label = x$event_type_label,
    configuration = x$configuration,
    source_export_type = "meta",
    parse_warning = x$parse_warning,
    is_collection_app = x$package_name == "com.w.appusage"
  )
  add_event_anomalies(out)[order(out$event_ts_ms, seq_len(nrow(out))), , drop = FALSE]
}

#' Reconstruct meta event episodes
#'
#' Explicitly converts APP Usage meta Table 2 event logs into episode-level
#' records. This function is not called by [parse_meta()]. It is called by
#' default in second-level meta workflows, with unmatched-event diagnostics
#' stored in metadata.
#'
#' @param events Meta event data frame, usually `parse_meta(...)$events` or a
#'   first-level `meta_events` table.
#' @param pairing Pair within `package_name` or within `package_name` plus
#'   `class_name`.
#' @param start_event_types Event types treated as starts.
#' @param end_event_types Event types treated as ends.
#' @param tz Time zone used when timestamps must be converted to datetimes.
#' @param max_episode_ms Episode duration above this value is flagged as an
#'   anomaly.
#' @param merge_contiguous Whether to merge adjacent complete reconstructed
#'   episodes for the same app when their time gap is small.
#' @param merge_gap_ms Maximum non-negative gap, in milliseconds, allowed when
#'   merging adjacent reconstructed meta episodes.
#' @param ... Reserved for future reconstruction options.
#'
#' @return A second-level episode tibble compatible with line-derived episodes.
#'   The tibble carries a `meta_reconstruction_diagnostics` attribute for JSON
#'   metadata writers.
#' @noRd
.reconstruct_meta_episodes_legacy <- function(events, pairing = c("package", "package_class"),
                                              start_event_types = 1,
                                              end_event_types = c(2, 23),
                                              tz = "Asia/Shanghai",
                                              max_episode_ms = 24 * 60 * 60 * 1000,
                                              ...) {
  pairing <- match.arg(pairing)
  events <- normalize_meta_events_for_reconstruction(events, tz = tz)
  if (nrow(events) == 0) {
    return(empty_second_episode_tibble())
  }

  events$.row_order <- seq_len(nrow(events))
  events <- events[order(events$event_ts_ms, events$.row_order, na.last = TRUE), , drop = FALSE]
  keys <- meta_reconstruction_key(events, pairing)
  groups <- split(seq_len(nrow(events)), keys)
  rows <- list()

  for (idx in groups) {
    open_start <- NULL
    open_boundary <- FALSE
    group <- events[idx, , drop = FALSE]
    for (i in seq_len(nrow(group))) {
      event <- group[i, , drop = FALSE]
      event_type <- event$event_type[[1]]
      is_start <- !is.na(event_type) && event_type %in% start_event_types
      is_end <- !is.na(event_type) && event_type %in% end_event_types
      is_boundary <- !is.na(event_type) && event_type %in% c(26, 27)

      if (is_boundary && !is.null(open_start)) {
        open_boundary <- TRUE
        next
      }

      if (is_start) {
        if (!is.null(open_start)) {
          rows[[length(rows) + 1L]] <- meta_episode_row(
            start_event = open_start,
            end_event = NULL,
            pairing = pairing,
            status = "unmatched_start",
            unmatched_start = TRUE,
            unmatched_end = FALSE,
            device_boundary_involved = open_boundary
          )
        }
        open_start <- event
        open_boundary <- FALSE
        next
      }

      if (is_end) {
        if (is.null(open_start)) {
          rows[[length(rows) + 1L]] <- meta_episode_row(
            start_event = NULL,
            end_event = event,
            pairing = pairing,
            status = "unmatched_end",
            unmatched_start = FALSE,
            unmatched_end = TRUE,
            device_boundary_involved = FALSE
          )
        } else {
          rows[[length(rows) + 1L]] <- meta_episode_row(
            start_event = open_start,
            end_event = event,
            pairing = pairing,
            status = "complete",
            unmatched_start = FALSE,
            unmatched_end = FALSE,
            device_boundary_involved = open_boundary
          )
          open_start <- NULL
          open_boundary <- FALSE
        }
      }
    }

    if (!is.null(open_start)) {
      rows[[length(rows) + 1L]] <- meta_episode_row(
        start_event = open_start,
        end_event = NULL,
        pairing = pairing,
        status = "unmatched_start",
        unmatched_start = TRUE,
        unmatched_end = FALSE,
        device_boundary_involved = open_boundary
      )
    }
  }

  if (length(rows) == 0) {
    return(empty_second_episode_tibble())
  }

  out <- tibble::as_tibble(do.call(rbind, rows))
  out <- add_duration_anomalies(out,
    duration_col = "duration_ms",
    max_duration_ms = max_episode_ms,
    start_col = "start_datetime",
    end_col = "end_datetime"
  )
  paired <- !out$unmatched_start & !out$unmatched_end
  invalid <- paired & (out$anomaly_missing_duration | out$anomaly_negative_duration)
  out$reconstruction_status[invalid] <- "invalid_pair"
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$unmatched_start,
    "unmatched_start"
  )
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$unmatched_end,
    "unmatched_end"
  )
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$anomaly_negative_duration,
    "negative_duration"
  )
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$anomaly_cross_date,
    "cross_date"
  )
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$anomaly_extreme_duration,
    "overlong_episode"
  )
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$device_boundary_involved,
    "device_boundary_involved"
  )
  out$anomaly_any <- out$anomaly_any | out$device_boundary_involved
  out$anomaly_reason <- append_reconstruction_warning(
    out$anomaly_reason,
    out$device_boundary_involved,
    "device_boundary_involved"
  )
  conform_second_episode(out[order(out$start_ts_ms, out$end_ts_ms, seq_len(nrow(out)), na.last = TRUE), , drop = FALSE])
}

#' Reconstruct meta event episodes
#'
#' Explicitly converts APP Usage meta Table 2 event logs into episode-level
#' records. This function is not called by [parse_meta()]. It is called by
#' default in second-level meta workflows, with unmatched-event diagnostics
#' stored in metadata.
#'
#' @param events Meta event data frame, usually `parse_meta(...)$events` or a
#'   first-level `meta_events` table.
#' @param pairing Pair within `package_name` or within `package_name` plus
#'   `class_name`.
#' @param start_event_types Event types treated as starts.
#' @param end_event_types Event types treated as ends.
#' @param tz Time zone used when timestamps must be converted to datetimes.
#' @param max_episode_ms Episode duration above this value is flagged as an
#'   anomaly.
#' @param merge_contiguous Whether to merge adjacent complete reconstructed
#'   episodes for the same app when their time gap is small.
#' @param merge_gap_ms Maximum non-negative gap, in milliseconds, allowed when
#'   merging adjacent reconstructed meta episodes.
#' @param ... Reserved for future reconstruction options.
#'
#' @return A second-level episode tibble compatible with line-derived episodes.
#'   The tibble carries a `meta_reconstruction_diagnostics` attribute for JSON
#'   metadata writers.
#' @export
reconstruct_meta_episodes <- function(events, pairing = c("package", "package_class"),
                                      start_event_types = 1,
                                      end_event_types = c(2, 23),
                                      tz = "Asia/Shanghai",
                                      max_episode_ms = 24 * 60 * 60 * 1000,
                                      merge_contiguous = TRUE,
                                      merge_gap_ms = 30 * 1000,
                                      ...) {
  pairing <- match.arg(pairing)
  events <- normalize_meta_events_for_reconstruction(events, tz = tz)
  diagnostics <- init_meta_reconstruction_diagnostics(
    events = events,
    pairing = pairing,
    start_event_types = start_event_types,
    end_event_types = end_event_types,
    merge_contiguous = merge_contiguous,
    merge_gap_ms = merge_gap_ms
  )
  if (nrow(events) == 0) {
    out <- empty_second_episode_tibble()
    return(attach_meta_reconstruction_diagnostics(
      out,
      finalize_meta_reconstruction_diagnostics(diagnostics, out)
    ))
  }

  events$.row_order <- seq_len(nrow(events))
  events$.reconstruction_key <- meta_reconstruction_key(events, pairing)
  events <- events[order(events$event_ts_ms, events$.row_order, na.last = TRUE), , drop = FALSE]
  rows <- list()

  add_row <- function(row) {
    rows[[length(rows) + 1L]] <<- row
  }

  drop_unmatched <- function(event, reason) {
    duration <- meta_event_value(event, "event_duration_ms", NA_real_)
    if (!is.na(duration) && duration >= 0) {
      add_row(meta_duration_inferred_episode_row(
        event = event,
        pairing = pairing,
        duration_ms = duration,
        reason = reason
      ))
      diagnostics$n_duration_inferred_episodes <<-
        diagnostics$n_duration_inferred_episodes + 1L
      return(invisible(NULL))
    }
    diagnostics$n_dropped_unmatched_events <<-
      diagnostics$n_dropped_unmatched_events + 1L
    if (identical(reason, "unmatched_start")) {
      diagnostics$n_dropped_unmatched_starts <<-
        diagnostics$n_dropped_unmatched_starts + 1L
    } else if (identical(reason, "unmatched_end")) {
      diagnostics$n_dropped_unmatched_ends <<-
        diagnostics$n_dropped_unmatched_ends + 1L
    }
    invisible(NULL)
  }

  scan_group <- function(group) {
    open_start <- NULL
    pending_end <- NULL

    close_pending <- function(device_boundary_involved = FALSE) {
      if (is.null(open_start) || is.null(pending_end)) {
        return(invisible(FALSE))
      }
      add_row(meta_episode_row(
        start_event = open_start,
        end_event = pending_end,
        pairing = pairing,
        status = "complete",
        unmatched_start = FALSE,
        unmatched_end = FALSE,
        device_boundary_involved = device_boundary_involved
      ))
      open_start <<- NULL
      pending_end <<- NULL
      invisible(TRUE)
    }

    close_with_event <- function(end_event, device_boundary_involved = FALSE) {
      if (is.null(open_start)) {
        drop_unmatched(end_event, "unmatched_end")
        return(invisible(FALSE))
      }
      add_row(meta_episode_row(
        start_event = open_start,
        end_event = end_event,
        pairing = pairing,
        status = "complete",
        unmatched_start = FALSE,
        unmatched_end = FALSE,
        device_boundary_involved = device_boundary_involved
      ))
      open_start <<- NULL
      pending_end <<- NULL
      invisible(TRUE)
    }

    for (i in seq_len(nrow(group))) {
      event <- group[i, , drop = FALSE]
      event_type <- event$event_type[[1]]
      is_start <- !is.na(event_type) && event_type %in% start_event_types
      is_end <- !is.na(event_type) && event_type %in% end_event_types
      is_pause <- !is.na(event_type) && event_type == 2
      is_stop <- !is.na(event_type) && event_type == 23
      is_shutdown <- !is.na(event_type) && event_type == 26
      is_startup <- !is.na(event_type) && event_type == 27

      if (is_shutdown) {
        if (!is.null(pending_end)) {
          close_pending()
        }
        if (!is.null(open_start)) {
          close_with_event(event, device_boundary_involved = TRUE)
        }
        next
      }

      if (is_startup) {
        next
      }

      if (!is.null(pending_end) && !is_stop) {
        close_pending()
      }

      if (is_start) {
        if (!is.null(open_start)) {
          drop_unmatched(open_start, "unmatched_start")
        }
        open_start <- event
        pending_end <- NULL
        next
      }

      if (is_pause) {
        if (is.null(open_start)) {
          drop_unmatched(event, "unmatched_end")
        } else {
          pending_end <- event
        }
        next
      }

      if (is_stop) {
        if (is.null(open_start)) {
          drop_unmatched(event, "unmatched_end")
        } else if (!is.null(pending_end)) {
          pending_ts <- meta_event_value(pending_end, "event_ts_ms", NA_real_)
          event_ts <- meta_event_value(event, "event_ts_ms", NA_real_)
          if (is.na(pending_ts) || (!is.na(event_ts) && event_ts >= pending_ts)) {
            pending_end <- event
          }
          close_pending()
        } else {
          close_with_event(event)
        }
        next
      }

      if (is_end) {
        close_with_event(event)
      }
    }

    if (!is.null(pending_end)) {
      close_pending()
    }
    if (!is.null(open_start)) {
      drop_unmatched(open_start, "unmatched_start")
    }
    invisible(NULL)
  }

  boundary_rows <- events[events$event_type %in% c(26, 27), , drop = FALSE]
  groups <- split(seq_len(nrow(events)), events$.reconstruction_key)
  for (idx in groups) {
    group <- events[idx, , drop = FALSE]
    if (nrow(boundary_rows) > 0) {
      group <- unique(rbind(group, boundary_rows))
      group <- group[order(group$event_ts_ms, group$.row_order, na.last = TRUE), , drop = FALSE]
    }
    scan_group(group)
  }

  if (length(rows) == 0) {
    out <- empty_second_episode_tibble()
    return(attach_meta_reconstruction_diagnostics(
      out,
      finalize_meta_reconstruction_diagnostics(diagnostics, out)
    ))
  }

  out <- tibble::as_tibble(do.call(rbind, rows))
  out <- add_duration_anomalies(out,
    duration_col = "duration_ms",
    max_duration_ms = max_episode_ms,
    start_col = "start_datetime",
    end_col = "end_datetime"
  )
  paired <- !out$unmatched_start & !out$unmatched_end
  invalid <- paired & (out$anomaly_missing_duration | out$anomaly_negative_duration)
  out$reconstruction_status[invalid] <- "invalid_pair"
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$unmatched_start,
    "unmatched_start"
  )
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$unmatched_end,
    "unmatched_end"
  )
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$anomaly_negative_duration,
    "negative_duration"
  )
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$anomaly_cross_date,
    "cross_date"
  )
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$anomaly_extreme_duration,
    "overlong_episode"
  )
  out$reconstruction_warning <- append_reconstruction_warning(
    out$reconstruction_warning,
    out$device_boundary_involved,
    "device_boundary_involved"
  )
  out$anomaly_any <- out$anomaly_any | out$device_boundary_involved
  out$anomaly_reason <- append_reconstruction_warning(
    out$anomaly_reason,
    out$device_boundary_involved,
    "device_boundary_involved"
  )
  out <- conform_second_episode(out[
    order(out$start_ts_ms, out$end_ts_ms, seq_len(nrow(out)), na.last = TRUE),
    ,
    drop = FALSE
  ])
  if (isTRUE(merge_contiguous)) {
    premerge_rows <- nrow(out)
    out <- merge_contiguous_meta_episodes(out, merge_gap_ms = merge_gap_ms)
    merge_diagnostics <- attr(out, "meta_episode_merge_diagnostics", exact = TRUE)
    diagnostics$n_premerge_episode_rows <- premerge_rows
    diagnostics$n_episode_merge_groups <- merge_diagnostics$n_episode_merge_groups
    diagnostics$n_episode_merge_edges <- merge_diagnostics$n_episode_merge_edges
    diagnostics$n_episode_rows_reduced_by_merge <- merge_diagnostics$n_episode_rows_reduced_by_merge
    diagnostics$total_merged_gap_ms <- merge_diagnostics$total_merged_gap_ms
  }
  clipped <- clip_overlapping_meta_timeline(out)
  out <- clipped$episodes
  diagnostics$n_timeline_clipped_episodes <- clipped$diagnostics$n_timeline_clipped_episodes
  diagnostics$total_timeline_clipped_ms <- clipped$diagnostics$total_timeline_clipped_ms
  diagnostics$n_timeline_clipped_to_nonpositive <- clipped$diagnostics$n_timeline_clipped_to_nonpositive
  attach_meta_reconstruction_diagnostics(
    out,
    finalize_meta_reconstruction_diagnostics(diagnostics, out)
  )
}

init_meta_reconstruction_diagnostics <- function(events, pairing,
                                                 start_event_types,
                                                 end_event_types,
                                                 merge_contiguous = FALSE,
                                                 merge_gap_ms = NA_real_) {
  event_type <- if ("event_type" %in% names(events)) events$event_type else numeric()
  known <- !is.na(event_type)
  unknown <- known & !(event_type %in% 0:31)
  list(
    n_event_rows = nrow(events),
    pairing_strategy = pairing,
    start_event_types = as.numeric(start_event_types),
    end_event_types = as.numeric(end_event_types),
    n_start_events = sum(event_type %in% start_event_types, na.rm = TRUE),
    n_end_events = sum(event_type %in% end_event_types, na.rm = TRUE),
    n_device_shutdown_events = sum(event_type == 26, na.rm = TRUE),
    n_device_startup_events = sum(event_type == 27, na.rm = TRUE),
    n_unknown_event_type_rows = sum(unknown, na.rm = TRUE),
    unknown_event_types = sort(unique(event_type[unknown])),
    n_dropped_unmatched_events = 0L,
    n_dropped_unmatched_starts = 0L,
    n_dropped_unmatched_ends = 0L,
    n_duration_inferred_episodes = 0L,
    merge_contiguous_episodes = isTRUE(merge_contiguous),
    merge_gap_ms = as.numeric(merge_gap_ms),
    n_premerge_episode_rows = NA_integer_,
    n_episode_merge_groups = 0L,
    n_episode_merge_edges = 0L,
    n_episode_rows_reduced_by_merge = 0L,
    total_merged_gap_ms = 0,
    n_timeline_clipped_episodes = 0L,
    total_timeline_clipped_ms = 0,
    n_timeline_clipped_to_nonpositive = 0L
  )
}

finalize_meta_reconstruction_diagnostics <- function(diagnostics, episodes) {
  diagnostics$n_premerge_episode_rows <- diagnostics$n_premerge_episode_rows %||% nrow(episodes)
  diagnostics$n_output_episode_rows <- nrow(episodes)
  diagnostics$n_complete_episodes <- sum(episodes$reconstruction_status == "complete", na.rm = TRUE)
  diagnostics$n_invalid_pairs <- sum(episodes$reconstruction_status == "invalid_pair", na.rm = TRUE)
  diagnostics$n_device_boundary_episodes <- sum(episodes$device_boundary_involved, na.rm = TRUE)
  diagnostics$n_reconstruction_warnings <- sum(
    !is.na(episodes$reconstruction_warning) & episodes$reconstruction_warning != "",
    na.rm = TRUE
  )
  diagnostics$n_timeline_clipped_episodes <- diagnostics$n_timeline_clipped_episodes %||% 0L
  diagnostics$total_timeline_clipped_ms <- diagnostics$total_timeline_clipped_ms %||% 0
  diagnostics$n_timeline_clipped_to_nonpositive <- diagnostics$n_timeline_clipped_to_nonpositive %||% 0L
  diagnostics$dropped_unmatched_event_proportion <- if (diagnostics$n_event_rows > 0) {
    diagnostics$n_dropped_unmatched_events / diagnostics$n_event_rows
  } else {
    NA_real_
  }
  diagnostics
}

clip_overlapping_meta_timeline <- function(episodes) {
  episodes <- conform_second_episode(episodes)
  diagnostics <- list(
    n_timeline_clipped_episodes = 0L,
    total_timeline_clipped_ms = 0,
    n_timeline_clipped_to_nonpositive = 0L
  )
  eligible <- which(
    episodes$episode_source == "meta_events" &
      episodes$reconstruction_status == "complete" &
      !is.na(episodes$start_ts_ms) &
      !is.na(episodes$end_ts_ms) &
      episodes$end_ts_ms > episodes$start_ts_ms
  )
  if (length(eligible) <= 1L) {
    return(list(episodes = episodes, diagnostics = diagnostics))
  }
  ordered <- eligible[order(
    episodes$start_ts_ms[eligible],
    episodes$end_ts_ms[eligible],
    eligible,
    na.last = TRUE
  )]
  previous <- ordered[[1]]
  for (current in ordered[-1]) {
    if (!identical(episodes$reconstruction_status[[previous]], "complete")) {
      previous <- current
      next
    }
    current_start <- episodes$start_ts_ms[[current]]
    previous_end <- episodes$end_ts_ms[[previous]]
    if (!is.na(current_start) && !is.na(previous_end) && current_start < previous_end) {
      original_end <- previous_end
      episodes <- clip_one_meta_episode_end(
        episodes = episodes,
        index = previous,
        new_end_ts_ms = current_start
      )
      clipped_ms <- original_end - current_start
      diagnostics$n_timeline_clipped_episodes <- diagnostics$n_timeline_clipped_episodes + 1L
      diagnostics$total_timeline_clipped_ms <- diagnostics$total_timeline_clipped_ms + clipped_ms
      if (is.na(episodes$duration_ms[[previous]]) || episodes$duration_ms[[previous]] <= 0) {
        episodes$reconstruction_status[[previous]] <- "invalid_pair"
        episodes$duration_ms[[previous]] <- NA_real_
        episodes$duration_min[[previous]] <- NA_real_
        episodes$reconstruction_warning <- append_reconstruction_warning(
          episodes$reconstruction_warning,
          seq_len(nrow(episodes)) == previous,
          "timeline_clipped_to_nonpositive"
        )
        episodes$anomaly_reason <- append_reconstruction_warning(
          episodes$anomaly_reason,
          seq_len(nrow(episodes)) == previous,
          "timeline_clipped_to_nonpositive"
        )
        diagnostics$n_timeline_clipped_to_nonpositive <- diagnostics$n_timeline_clipped_to_nonpositive + 1L
      }
    }
    previous <- current
  }
  episodes <- conform_second_episode(episodes[order(
    episodes$start_ts_ms,
    episodes$end_ts_ms,
    seq_len(nrow(episodes)),
    na.last = TRUE
  ), , drop = FALSE])
  list(episodes = episodes, diagnostics = diagnostics)
}

clip_one_meta_episode_end <- function(episodes, index, new_end_ts_ms) {
  new_duration <- new_end_ts_ms - episodes$start_ts_ms[[index]]
  episodes$end_ts_ms[[index]] <- new_end_ts_ms
  episodes$end_datetime[[index]] <- meta_episode_datetime_from_ms(
    new_end_ts_ms,
    episodes$end_datetime[[index]]
  )
  episodes$duration_ms[[index]] <- new_duration
  episodes$duration_min[[index]] <- new_duration / 60000
  episodes$duration_text[[index]] <- NA_character_
  flag <- seq_len(nrow(episodes)) == index
  episodes$reconstruction_warning <- append_reconstruction_warning(
    episodes$reconstruction_warning,
    flag,
    "timeline_clipped"
  )
  episodes$anomaly_reason <- append_reconstruction_warning(
    episodes$anomaly_reason,
    flag,
    "timeline_clipped"
  )
  episodes$anomaly_any[[index]] <- TRUE
  episodes
}

meta_episode_datetime_from_ms <- function(ms, reference_datetime) {
  tz <- attr(reference_datetime, "tzone")
  tz <- if (length(tz) > 0 && is_present_string(tz[[1]])) {
    tz[[1]]
  } else {
    "Asia/Shanghai"
  }
  ms_to_datetime(ms, tz = tz)
}

merge_contiguous_meta_episodes <- function(episodes, merge_gap_ms = 30 * 1000) {
  episodes <- conform_second_episode(episodes)
  if (nrow(episodes) <= 1) {
    return(attach_meta_episode_merge_diagnostics(
      episodes,
      empty_meta_episode_merge_diagnostics(nrow(episodes), merge_gap_ms)
    ))
  }
  episodes$.original_order <- seq_len(nrow(episodes))
  episodes <- episodes[order(episodes$start_ts_ms, episodes$end_ts_ms, episodes$.original_order, na.last = TRUE), , drop = FALSE]
  n <- nrow(episodes)
  prev <- seq_len(n) - 1L
  same_key <- c(FALSE, vapply(seq.int(2L, n), function(i) {
    meta_episode_mergeable_pair(episodes[prev[[i]], , drop = FALSE], episodes[i, , drop = FALSE], merge_gap_ms)
  }, logical(1)))
  group <- cumsum(!same_key)
  groups <- split(seq_len(n), group)
  rows <- lapply(groups, function(idx) {
    merge_meta_episode_group(episodes[idx, , drop = FALSE])
  })
  out <- tibble::as_tibble(do.call(rbind, rows))
  out$.original_order <- NULL
  out <- conform_second_episode(out[order(out$start_ts_ms, out$end_ts_ms, seq_len(nrow(out)), na.last = TRUE), , drop = FALSE])
  attach_meta_episode_merge_diagnostics(
    out,
    list(
      merge_gap_ms = as.numeric(merge_gap_ms),
      n_premerge_episode_rows = n,
      n_postmerge_episode_rows = nrow(out),
      n_episode_merge_groups = sum(lengths(groups) > 1L),
      n_episode_merge_edges = sum(same_key, na.rm = TRUE),
      n_episode_rows_reduced_by_merge = n - nrow(out),
      total_merged_gap_ms = sum(out$merged_gap_ms, na.rm = TRUE)
    )
  )
}

meta_episode_mergeable_pair <- function(previous, current, merge_gap_ms) {
  if (is.na(merge_gap_ms) || merge_gap_ms < 0) {
    return(FALSE)
  }
  if (!identical(previous$episode_source[[1]], "meta_events") ||
    !identical(current$episode_source[[1]], "meta_events")) {
    return(FALSE)
  }
  if (!identical(previous$reconstruction_status[[1]], "complete") ||
    !identical(current$reconstruction_status[[1]], "complete")) {
    return(FALSE)
  }
  if (isTRUE(previous$device_boundary_involved[[1]]) ||
    isTRUE(current$device_boundary_involved[[1]])) {
    return(FALSE)
  }
  required_previous <- c(
    previous$app_name[[1]],
    previous$package_name[[1]],
    previous$activity_type[[1]],
    as.character(previous$date[[1]])
  )
  required_current <- c(
    current$app_name[[1]],
    current$package_name[[1]],
    current$activity_type[[1]],
    as.character(current$date[[1]])
  )
  if (any(is.na(required_previous) | required_previous == "") ||
    any(is.na(required_current) | required_current == "")) {
    return(FALSE)
  }
  same_identity <- identical(previous$app_name[[1]], current$app_name[[1]]) &&
    identical(previous$package_name[[1]], current$package_name[[1]]) &&
    identical(previous$activity_type[[1]], current$activity_type[[1]]) &&
    identical(as.character(previous$date[[1]]), as.character(current$date[[1]]))
  if (!same_identity) {
    return(FALSE)
  }
  gap <- current$start_ts_ms[[1]] - previous$end_ts_ms[[1]]
  !is.na(gap) && gap >= 0 && gap <= merge_gap_ms
}

merge_meta_episode_group <- function(group) {
  group <- conform_second_episode(group)
  if (nrow(group) == 1) {
    if (is.na(group$source_episode_count[[1]])) {
      group$source_episode_count <- 1L
    }
    if (is.na(group$source_duration_ms[[1]])) {
      group$source_duration_ms <- group$duration_ms
    }
    if (is.na(group$merged_gap_ms[[1]])) {
      group$merged_gap_ms <- 0
    }
    return(group)
  }
  first <- group[1, , drop = FALSE]
  last <- group[nrow(group), , drop = FALSE]
  gaps <- group$start_ts_ms[-1] - group$end_ts_ms[-nrow(group)]
  duration_ms <- sum(group$duration_ms, na.rm = TRUE)
  first$end_ts_ms <- last$end_ts_ms
  first$end_datetime <- last$end_datetime
  first$duration_ms <- duration_ms
  first$duration_min <- duration_ms / 60000
  first$parse_warning <- compact_character_values(c(group$parse_warning, group$reconstruction_warning))
  first$is_collection_app <- any(group$is_collection_app, na.rm = TRUE)
  first$end_event_type <- last$end_event_type
  first$end_event_type_label <- last$end_event_type_label
  first$end_class_name <- last$end_class_name
  first$device_boundary_involved <- any(group$device_boundary_involved, na.rm = TRUE)
  first$anomaly_missing_duration <- any(group$anomaly_missing_duration, na.rm = TRUE)
  first$anomaly_negative_duration <- any(group$anomaly_negative_duration, na.rm = TRUE)
  first$anomaly_extreme_duration <- any(group$anomaly_extreme_duration, na.rm = TRUE)
  first$anomaly_cross_date <- any(group$anomaly_cross_date, na.rm = TRUE)
  first$anomaly_any <- any(group$anomaly_any, na.rm = TRUE)
  first$anomaly_reason <- compact_character_values(group$anomaly_reason)
  first$reconstruction_warning <- compact_character_values(group$reconstruction_warning)
  source_count <- sum(group$source_episode_count, na.rm = TRUE)
  first$source_episode_count <- if (source_count > 0) source_count else nrow(group)
  first$source_duration_ms <- duration_ms
  first$merged_gap_ms <- sum(pmax(gaps, 0), na.rm = TRUE)
  first
}

empty_meta_episode_merge_diagnostics <- function(n_rows, merge_gap_ms) {
  list(
    merge_gap_ms = as.numeric(merge_gap_ms),
    n_premerge_episode_rows = n_rows,
    n_postmerge_episode_rows = n_rows,
    n_episode_merge_groups = 0L,
    n_episode_merge_edges = 0L,
    n_episode_rows_reduced_by_merge = 0L,
    total_merged_gap_ms = 0
  )
}

attach_meta_episode_merge_diagnostics <- function(episodes, diagnostics) {
  attr(episodes, "meta_episode_merge_diagnostics") <- diagnostics
  episodes
}

attach_meta_reconstruction_diagnostics <- function(episodes, diagnostics) {
  attr(episodes, "meta_reconstruction_diagnostics") <- diagnostics
  episodes
}

normalize_meta_events_for_reconstruction <- function(events, tz) {
  if (is.null(events)) {
    return(empty_second_event_tibble())
  }
  if (!is.data.frame(events)) {
    cli::cli_abort("`events` must be a meta event data frame.")
  }
  events <- tibble::as_tibble(events)
  n <- nrow(events)
  event_ts_ms <- if ("event_ts_ms" %in% names(events)) {
    parse_t_timestamp(events$event_ts_ms)
  } else {
    rep(NA_real_, n)
  }
  event_datetime <- if ("event_datetime" %in% names(events)) {
    coerce_meta_event_datetime(events$event_datetime, tz = tz)
  } else {
    as.POSIXct(rep(NA_real_, n), origin = "1970-01-01", tz = tz)
  }
  missing_datetime <- is.na(event_datetime) & !is.na(event_ts_ms)
  event_datetime[missing_datetime] <- ms_to_datetime(event_ts_ms[missing_datetime], tz = tz)
  event_type <- if ("event_type" %in% names(events)) {
    suppressWarnings(as.numeric(as.character(events$event_type)))
  } else {
    rep(NA_real_, n)
  }
  event_type_label <- if ("event_type_label" %in% names(events)) {
    as.character(events$event_type_label)
  } else {
    label_event_type(event_type)
  }
  missing_label <- is.na(event_type_label) | event_type_label == ""
  event_type_label[missing_label] <- label_event_type(event_type[missing_label])
  table_date <- if ("table_date" %in% names(events)) {
    as.Date(events$table_date)
  } else {
    as.Date(event_datetime)
  }
  event_duration_ms <- if ("event_duration_ms" %in% names(events)) {
    parse_ms_value(events$event_duration_ms)
  } else if ("duration_ms" %in% names(events)) {
    parse_ms_value(events$duration_ms)
  } else {
    rep(NA_real_, n)
  }

  tibble::tibble(
    table_date = table_date,
    app_name = meta_column(events, "app_name", NA_character_),
    package_name = meta_column(events, "package_name", NA_character_),
    class_name = meta_column(events, "class_name", NA_character_),
    event_datetime = event_datetime,
    event_ts_ms = event_ts_ms,
    event_type = event_type,
    event_type_label = event_type_label,
    event_duration_ms = event_duration_ms,
    parse_warning = meta_column(events, "parse_warning", NA_character_)
  )
}

coerce_meta_event_datetime <- function(x, tz) {
  if (inherits(x, "POSIXt")) {
    return(as.POSIXct(x, tz = tz))
  }
  if (is.numeric(x)) {
    return(as.POSIXct(x / 1000, origin = "1970-01-01", tz = tz))
  }
  safe_as_datetime(x, tz = tz)
}

meta_column <- function(data, name, default) {
  if (name %in% names(data)) {
    return(data[[name]])
  }
  rep(default, nrow(data))
}

meta_reconstruction_key <- function(events, pairing) {
  package <- reconstruction_key_value(events$package_name)
  if (identical(pairing, "package_class")) {
    return(paste(package, reconstruction_key_value(events$class_name), sep = "\r"))
  }
  package
}

reconstruction_key_value <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "<NA>"
  x
}

meta_episode_row <- function(start_event, end_event, pairing, status,
                             unmatched_start, unmatched_end,
                             device_boundary_involved) {
  start_ts_ms <- meta_event_value(start_event, "event_ts_ms", NA_real_)
  end_ts_ms <- meta_event_value(end_event, "event_ts_ms", NA_real_)
  start_datetime <- meta_event_value(start_event, "event_datetime", as.POSIXct(NA_real_, origin = "1970-01-01"))
  end_datetime <- meta_event_value(end_event, "event_datetime", as.POSIXct(NA_real_, origin = "1970-01-01"))
  duration_ms <- if (!is.na(start_ts_ms) && !is.na(end_ts_ms)) {
    end_ts_ms - start_ts_ms
  } else {
    NA_real_
  }
  date <- as.Date(start_datetime)
  if (is.na(date)) {
    date <- as.Date(end_datetime)
  }

  app_name <- first_present_character(
    meta_event_value(start_event, "app_name", NA_character_),
    meta_event_value(end_event, "app_name", NA_character_)
  )
  package_name <- first_present_character(
    meta_event_value(start_event, "package_name", NA_character_),
    meta_event_value(end_event, "package_name", NA_character_)
  )
  parse_warning <- compact_character_values(c(
    meta_event_value(start_event, "parse_warning", NA_character_),
    meta_event_value(end_event, "parse_warning", NA_character_)
  ))

  data.frame(
    date = date,
    app_name = app_name,
    activity_type = classify_activity_type(app_name),
    package_name = package_name,
    start_ts_ms = start_ts_ms,
    end_ts_ms = end_ts_ms,
    start_datetime = start_datetime,
    end_datetime = end_datetime,
    duration_ms = duration_ms,
    duration_min = duration_ms / 60000,
    duration_text = NA_character_,
    source_episode_count = 1L,
    source_duration_ms = duration_ms,
    merged_gap_ms = 0,
    source_export_type = "meta",
    parse_warning = parse_warning,
    is_collection_app = identical(package_name, "com.w.appusage"),
    episode_source = "meta_events",
    pairing_strategy = pairing,
    start_event_type = meta_event_value(start_event, "event_type", NA_real_),
    end_event_type = meta_event_value(end_event, "event_type", NA_real_),
    start_event_type_label = meta_event_value(start_event, "event_type_label", NA_character_),
    end_event_type_label = meta_event_value(end_event, "event_type_label", NA_character_),
    start_class_name = meta_event_value(start_event, "class_name", NA_character_),
    end_class_name = meta_event_value(end_event, "class_name", NA_character_),
    reconstruction_status = status,
    reconstruction_warning = NA_character_,
    unmatched_start = unmatched_start,
    unmatched_end = unmatched_end,
    device_boundary_involved = device_boundary_involved,
    stringsAsFactors = FALSE
  )
}

meta_duration_inferred_episode_row <- function(event, pairing, duration_ms,
                                               reason) {
  start_ts_ms <- meta_event_value(event, "event_ts_ms", NA_real_)
  end_ts_ms <- if (!is.na(start_ts_ms)) start_ts_ms + duration_ms else NA_real_
  start_datetime <- meta_event_value(
    event,
    "event_datetime",
    as.POSIXct(NA_real_, origin = "1970-01-01")
  )
  start_tz <- attr(start_datetime, "tzone")
  start_tz <- if (length(start_tz) > 0 && is_present_string(start_tz[[1]])) {
    start_tz[[1]]
  } else {
    "Asia/Shanghai"
  }
  end_datetime <- if (!is.na(end_ts_ms)) {
    ms_to_datetime(end_ts_ms, tz = start_tz)
  } else {
    as.POSIXct(NA_real_, origin = "1970-01-01")
  }
  app_name <- meta_event_value(event, "app_name", NA_character_)
  package_name <- meta_event_value(event, "package_name", NA_character_)
  warning <- paste0("duration_inferred_from_", reason)

  data.frame(
    date = as.Date(start_datetime),
    app_name = app_name,
    activity_type = classify_activity_type(app_name),
    package_name = package_name,
    start_ts_ms = start_ts_ms,
    end_ts_ms = end_ts_ms,
    start_datetime = start_datetime,
    end_datetime = end_datetime,
    duration_ms = duration_ms,
    duration_min = duration_ms / 60000,
    duration_text = NA_character_,
    source_episode_count = 1L,
    source_duration_ms = duration_ms,
    merged_gap_ms = 0,
    source_export_type = "meta",
    parse_warning = meta_event_value(event, "parse_warning", NA_character_),
    is_collection_app = identical(package_name, "com.w.appusage"),
    episode_source = "meta_events",
    pairing_strategy = pairing,
    start_event_type = meta_event_value(event, "event_type", NA_real_),
    end_event_type = meta_event_value(event, "event_type", NA_real_),
    start_event_type_label = meta_event_value(event, "event_type_label", NA_character_),
    end_event_type_label = meta_event_value(event, "event_type_label", NA_character_),
    start_class_name = meta_event_value(event, "class_name", NA_character_),
    end_class_name = meta_event_value(event, "class_name", NA_character_),
    reconstruction_status = "complete",
    reconstruction_warning = warning,
    unmatched_start = FALSE,
    unmatched_end = FALSE,
    device_boundary_involved = FALSE,
    stringsAsFactors = FALSE
  )
}

meta_event_value <- function(event, column, default) {
  if (is.null(event) || !column %in% names(event) || nrow(event) == 0) {
    return(default)
  }
  value <- event[[column]][[1]]
  if (length(value) == 0) {
    return(default)
  }
  value
}

first_present_character <- function(...) {
  values <- as.character(c(...))
  values <- values[!is.na(values) & values != ""]
  if (length(values) == 0) {
    return(NA_character_)
  }
  values[[1]]
}

compact_character_values <- function(x) {
  values <- unique(as.character(x))
  values <- values[!is.na(values) & values != ""]
  if (length(values) == 0) {
    return(NA_character_)
  }
  paste(values, collapse = "; ")
}

append_reconstruction_warning <- function(x, flag, label) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- NA_character_
  flag[is.na(flag)] <- FALSE
  idx <- which(flag)
  if (length(idx) == 0) {
    return(x)
  }
  x[idx] <- ifelse(is.na(x[idx]) | x[idx] == "", label, paste(x[idx], label, sep = "; "))
  x
}

second_level_episodes <- function(x, max_episode_ms) {
  if (nrow(x) == 0) {
    return(empty_second_episode_tibble())
  }
  out <- tibble::tibble(
    date = x$date,
    app_name = x$app_name,
    activity_type = classify_activity_type(x$app_name),
    package_name = x$package_name,
    start_ts_ms = x$start_ts_ms,
    end_ts_ms = x$end_ts_ms,
    start_datetime = x$start_datetime,
    end_datetime = x$end_datetime,
    duration_ms = x$duration_ms,
    duration_min = x$duration_ms / 60000,
    duration_text = x$duration_text,
    source_episode_count = 1L,
    source_duration_ms = x$duration_ms,
    merged_gap_ms = 0,
    source_export_type = "line",
    parse_warning = x$parse_warning,
    is_collection_app = x$package_name == "com.w.appusage",
    episode_source = "line",
    pairing_strategy = NA_character_,
    start_event_type = NA_real_,
    end_event_type = NA_real_,
    start_event_type_label = NA_character_,
    end_event_type_label = NA_character_,
    start_class_name = NA_character_,
    end_class_name = NA_character_,
    reconstruction_status = NA_character_,
    reconstruction_warning = NA_character_,
    unmatched_start = FALSE,
    unmatched_end = FALSE,
    device_boundary_involved = FALSE
  )
  out <- add_duration_anomalies(out,
    duration_col = "duration_ms",
    max_duration_ms = max_episode_ms,
    start_col = "start_datetime",
    end_col = "end_datetime"
  )
  conform_second_episode(out[order(out$start_ts_ms, seq_len(nrow(out))), , drop = FALSE])
}

second_level_daily <- function(x, max_daily_app_ms) {
  if (nrow(x) == 0) {
    return(empty_second_daily_tibble())
  }
  export_type <- if ("export_type" %in% names(x)) x$export_type else NA_character_
  daily_source <- daily_source_from_export(export_type, nrow(x))
  out <- tibble::tibble(
    date = x$date,
    weekday = if ("weekday" %in% names(x)) x$weekday else weekday_name(x$date),
    app_name = x$app_name,
    activity_type = classify_activity_type(x$app_name),
    package_name = x$package_name,
    duration_ms = x$duration_ms,
    duration_min = x$duration_ms / 60000,
    open_count = x$open_count,
    notification_count = x$notification_count,
    split_screen_ms = if ("split_screen_ms" %in% names(x)) x$split_screen_ms else NA_real_,
    episode_count = NA_integer_,
    event_count = NA_integer_,
    source_export_type = export_type,
    daily_source = daily_source,
    summary_duration_ms = NA_real_,
    episode_duration_ms = NA_real_,
    duration_diff_ms = NA_real_,
    duration_diff_pct = NA_real_,
    duration_agreement_status = "not_compared",
    complete_episode_count = 0L,
    unmatched_start_count = 0L,
    unmatched_end_count = 0L,
    invalid_pair_count = 0L,
    reconstruction_warning_count = 0L,
    is_all_apps = if ("is_all_apps" %in% names(x)) x$is_all_apps else x$package_name == "ALL",
    is_collection_app = if ("is_collection_app" %in% names(x)) x$is_collection_app else x$package_name == "com.w.appusage",
    parse_warning = x$parse_warning
  )
  out <- add_duration_anomalies(out,
    duration_col = "duration_ms",
    max_duration_ms = max_daily_app_ms
  )
  conform_second_daily(out[order(out$date, seq_len(nrow(out))), , drop = FALSE])
}

second_level_meta_summary <- function(x, max_daily_app_ms) {
  if (nrow(x) == 0) {
    return(empty_second_daily_tibble())
  }
  out <- tibble::tibble(
    date = x$table_date,
    weekday = weekday_name(x$table_date),
    app_name = x$app_name,
    activity_type = classify_activity_type(x$app_name),
    package_name = x$package_name,
    duration_ms = x$total_duration_ms,
    duration_min = x$total_duration_ms / 60000,
    open_count = NA_integer_,
    notification_count = NA_integer_,
    split_screen_ms = NA_real_,
    episode_count = NA_integer_,
    event_count = NA_integer_,
    source_export_type = "meta",
    daily_source = "meta_summary",
    summary_duration_ms = x$total_duration_ms,
    episode_duration_ms = NA_real_,
    duration_diff_ms = NA_real_,
    duration_diff_pct = NA_real_,
    duration_agreement_status = "summary_only",
    complete_episode_count = 0L,
    unmatched_start_count = 0L,
    unmatched_end_count = 0L,
    invalid_pair_count = 0L,
    reconstruction_warning_count = 0L,
    is_all_apps = x$package_name == "ALL",
    is_collection_app = x$package_name == "com.w.appusage",
    parse_warning = x$parse_warning
  )
  out <- add_duration_anomalies(out,
    duration_col = "duration_ms",
    max_duration_ms = max_daily_app_ms
  )
  conform_second_daily(out[order(out$date, seq_len(nrow(out))), , drop = FALSE])
}

daily_from_episodes <- function(x, max_daily_app_ms) {
  if (nrow(x) == 0) {
    return(empty_second_daily_tibble())
  }
  activity_type <- if ("activity_type" %in% names(x)) {
    x$activity_type
  } else {
    classify_activity_type(x$app_name)
  }
  key <- paste(x$date, x$app_name, x$package_name, activity_type, sep = "\r")
  levels <- sort(unique(key))
  group_id <- match(key, levels)
  first_idx <- match(seq_along(levels), group_id)
  durations <- x$duration_ms
  valid_duration <- !is.na(durations) & durations >= 0
  anomaly_any <- if ("anomaly_any" %in% names(x)) x$anomaly_any else rep(FALSE, nrow(x))
  grouped_counts <- rowsum(cbind(
    duration_sum = ifelse(valid_duration, durations, 0),
    valid_count = as.integer(valid_duration),
    anomaly_count = as.integer(anomaly_any %in% TRUE)
  ), group_id, reorder = FALSE)
  grouped_counts <- grouped_counts[as.character(seq_along(levels)), , drop = FALSE]
  duration_ms <- as.numeric(grouped_counts[, "duration_sum"])
  valid_count <- grouped_counts[, "valid_count"]
  duration_ms[valid_count == 0] <- NA_real_
  episode_count <- as.integer(tabulate(group_id, nbins = length(levels)))
  n_anomalies <- as.integer(grouped_counts[, "anomaly_count"])
  parse_warning <- line_daily_parse_warnings(x$parse_warning, group_id, length(levels))
  out <- tibble::tibble(
    date = x$date[first_idx],
    weekday = weekday_name(x$date[first_idx]),
    app_name = x$app_name[first_idx],
    activity_type = activity_type[first_idx],
    package_name = x$package_name[first_idx],
    duration_ms = duration_ms,
    duration_min = duration_ms / 60000,
    open_count = NA_integer_,
    notification_count = NA_integer_,
    split_screen_ms = NA_real_,
    episode_count = episode_count,
    event_count = NA_integer_,
    source_export_type = "line",
    daily_source = "line_episodes",
    summary_duration_ms = NA_real_,
    episode_duration_ms = duration_ms,
    duration_diff_ms = NA_real_,
    duration_diff_pct = NA_real_,
    duration_agreement_status = "not_compared",
    complete_episode_count = episode_count,
    unmatched_start_count = 0L,
    unmatched_end_count = 0L,
    invalid_pair_count = 0L,
    reconstruction_warning_count = 0L,
    is_all_apps = FALSE,
    is_collection_app = x$is_collection_app[first_idx],
    parse_warning = parse_warning,
    n_anomalies = n_anomalies
  )
  out$parse_warning[out$parse_warning == ""] <- NA_character_
  out <- add_duration_anomalies(out,
    duration_col = "duration_ms",
    max_duration_ms = max_daily_app_ms
  )
  out$anomaly_any <- out$anomaly_any | out$n_anomalies > 0
  out$anomaly_reason[out$n_anomalies > 0] <- append_warning(
    out$anomaly_reason[out$n_anomalies > 0],
    "one or more source episodes were anomalous"
  )
  conform_second_daily(out[order(out$date, out$package_name, seq_len(nrow(out))), , drop = FALSE])
}

line_daily_parse_warnings <- function(parse_warning, group_id, n_groups) {
  parse_warning <- as.character(parse_warning)
  out <- rep(NA_character_, n_groups)
  present <- !is.na(parse_warning) & nzchar(parse_warning)
  if (!any(present)) {
    return(out)
  }
  present_idx <- which(present)
  present_groups <- group_id[present_idx]
  present_warnings <- parse_warning[present_idx]
  pair_key <- paste(present_groups, present_warnings, sep = "\r")
  keep <- !duplicated(pair_key)
  present_groups <- present_groups[keep]
  present_warnings <- present_warnings[keep]
  ord <- order(present_groups, seq_along(present_groups))
  present_groups <- present_groups[ord]
  present_warnings <- present_warnings[ord]
  run <- rle(present_groups)
  ends <- cumsum(run$lengths)
  starts <- ends - run$lengths + 1L
  for (i in seq_along(run$values)) {
    out[[run$values[[i]]]] <- paste(present_warnings[starts[[i]]:ends[[i]]], collapse = "; ")
  }
  out
}

aggregate_meta_episodes_daily <- function(episodes, summary_daily = NULL,
                                          max_daily_app_ms = 24 * 60 * 60 * 1000,
                                          compare_to_summary = TRUE) {
  episodes <- conform_second_episode(episodes)
  meta <- episodes[episodes$episode_source == "meta_events", , drop = FALSE]
  if (nrow(meta) == 0) {
    return(empty_second_daily_tibble())
  }

  key <- meta_daily_key(meta)
  groups <- split(seq_len(nrow(meta)), key)
  rows <- lapply(groups, function(idx) {
    x <- meta[idx, , drop = FALSE]
    valid_complete <- x$reconstruction_status == "complete" &
      !is.na(x$duration_ms) &
      x$duration_ms >= 0
    duration_ms <- if (any(valid_complete, na.rm = TRUE)) {
      sum(x$duration_ms[valid_complete], na.rm = TRUE)
    } else {
      NA_real_
    }
    diagnostics <- sum(x$anomaly_any, na.rm = TRUE) +
      sum(x$unmatched_start, na.rm = TRUE) +
      sum(x$unmatched_end, na.rm = TRUE) +
      sum(x$reconstruction_status == "invalid_pair", na.rm = TRUE) +
      sum(!is.na(x$reconstruction_warning) & x$reconstruction_warning != "", na.rm = TRUE)
    data.frame(
      date = first_nonmissing(x$date),
      weekday = weekday_name(first_nonmissing(x$date)),
      app_name = first_nonmissing_character(x$app_name),
      activity_type = first_nonmissing_character(x$activity_type),
      package_name = first_nonmissing_character(x$package_name),
      duration_ms = duration_ms,
      duration_min = duration_ms / 60000,
      open_count = NA_integer_,
      notification_count = NA_integer_,
      split_screen_ms = NA_real_,
      episode_count = sum(valid_complete, na.rm = TRUE),
      event_count = NA_integer_,
      source_export_type = "meta",
      daily_source = "meta_episodes",
      summary_duration_ms = NA_real_,
      episode_duration_ms = duration_ms,
      duration_diff_ms = NA_real_,
      duration_diff_pct = NA_real_,
      duration_agreement_status = "episode_only",
      complete_episode_count = sum(valid_complete, na.rm = TRUE),
      unmatched_start_count = sum(x$unmatched_start, na.rm = TRUE),
      unmatched_end_count = sum(x$unmatched_end, na.rm = TRUE),
      invalid_pair_count = sum(x$reconstruction_status == "invalid_pair", na.rm = TRUE),
      reconstruction_warning_count = sum(!is.na(x$reconstruction_warning) & x$reconstruction_warning != "", na.rm = TRUE),
      is_all_apps = FALSE,
      is_collection_app = any(x$is_collection_app, na.rm = TRUE),
      parse_warning = compact_character_values(c(x$parse_warning, x$reconstruction_warning)),
      n_anomalies = as.integer(diagnostics),
      stringsAsFactors = FALSE
    )
  })
  out <- tibble::as_tibble(do.call(rbind, rows))
  out <- add_duration_anomalies(out,
    duration_col = "duration_ms",
    max_duration_ms = max_daily_app_ms
  )
  out$anomaly_any <- out$anomaly_any | out$n_anomalies > 0
  out$anomaly_reason[out$n_anomalies > 0] <- append_warning(
    out$anomaly_reason[out$n_anomalies > 0],
    "reconstruction_diagnostics"
  )
  out <- conform_second_daily(out[order(out$date, out$package_name, seq_len(nrow(out))), , drop = FALSE])
  if (isTRUE(compare_to_summary) && !is.null(summary_daily)) {
    out <- compare_meta_daily_sources(out, summary_daily)
  }
  out
}

compare_meta_daily_sources <- function(target, reference) {
  target <- conform_second_daily(target)
  reference <- conform_second_daily(reference)
  if (nrow(target) == 0) {
    return(target)
  }
  target_source <- unique(stats::na.omit(target$daily_source))
  if (length(target_source) != 1 || !target_source %in% c("meta_summary", "meta_episodes")) {
    return(target)
  }
  ref_source <- if (identical(target_source, "meta_summary")) "meta_episodes" else "meta_summary"
  ref <- reference[reference$daily_source == ref_source, , drop = FALSE]

  target$duration_agreement_status <- if (identical(target_source, "meta_summary")) {
    "summary_only"
  } else {
    "episode_only"
  }
  if (identical(target_source, "meta_summary")) {
    target$summary_duration_ms <- target$duration_ms
  } else {
    target$episode_duration_ms <- target$duration_ms
  }
  if (nrow(ref) == 0) {
    return(conform_second_daily(target))
  }

  match_idx <- match(meta_daily_key(target), meta_daily_key(ref))
  matched <- !is.na(match_idx)
  if (!any(matched)) {
    return(conform_second_daily(target))
  }
  ref_matched <- ref[match_idx[matched], , drop = FALSE]
  if (identical(target_source, "meta_summary")) {
    target$episode_duration_ms[matched] <- ref_matched$duration_ms
    target$complete_episode_count[matched] <- ref_matched$complete_episode_count
    target$unmatched_start_count[matched] <- ref_matched$unmatched_start_count
    target$unmatched_end_count[matched] <- ref_matched$unmatched_end_count
    target$invalid_pair_count[matched] <- ref_matched$invalid_pair_count
    target$reconstruction_warning_count[matched] <- ref_matched$reconstruction_warning_count
  } else {
    target$summary_duration_ms[matched] <- ref_matched$duration_ms
  }
  target$duration_diff_ms[matched] <- target$episode_duration_ms[matched] - target$summary_duration_ms[matched]
  valid_pct <- matched & !is.na(target$summary_duration_ms) & target$summary_duration_ms != 0
  target$duration_diff_pct[valid_pct] <- target$duration_diff_ms[valid_pct] / target$summary_duration_ms[valid_pct]
  target$duration_agreement_status[matched] <- ifelse(
    is.na(target$duration_diff_ms[matched]),
    "not_compared",
    ifelse(target$duration_diff_ms[matched] == 0, "matched_exact", "matched_with_difference")
  )
  conform_second_daily(target)
}

meta_daily_key <- function(data) {
  paste(
    as.character(data$date),
    reconstruction_key_value(data$app_name),
    reconstruction_key_value(data$package_name),
    reconstruction_key_value(data$activity_type),
    sep = "\r"
  )
}

first_nonmissing <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA)
  }
  x[[1]]
}

first_nonmissing_character <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != ""]
  if (length(x) == 0) {
    return(NA_character_)
  }
  x[[1]]
}

add_duration_anomalies <- function(data, duration_col, max_duration_ms,
                                   start_col = NULL, end_col = NULL) {
  duration <- data[[duration_col]]
  missing_duration <- is.na(duration)
  negative_duration <- !is.na(duration) & duration < 0
  extreme_duration <- !is.na(duration) & duration > max_duration_ms
  cross_date <- rep(FALSE, nrow(data))
  if (!is.null(start_col) && !is.null(end_col) &&
    all(c(start_col, end_col) %in% names(data))) {
    start_date <- as.Date(data[[start_col]])
    end_date <- as.Date(data[[end_col]])
    cross_date <- !is.na(start_date) & !is.na(end_date) & start_date != end_date
  }
  data$anomaly_missing_duration <- missing_duration
  data$anomaly_negative_duration <- negative_duration
  data$anomaly_extreme_duration <- extreme_duration
  data$anomaly_cross_date <- cross_date
  data$anomaly_any <- missing_duration | negative_duration | extreme_duration | cross_date
  data$anomaly_reason <- compose_anomaly_reason(
    missing_duration = missing_duration,
    negative_duration = negative_duration,
    extreme_duration = extreme_duration,
    cross_date = cross_date
  )
  data
}

add_event_anomalies <- function(data) {
  missing_timestamp <- is.na(data$event_ts_ms)
  missing_type <- is.na(data$event_type)
  data$anomaly_missing_timestamp <- missing_timestamp
  data$anomaly_missing_event_type <- missing_type
  data$anomaly_any <- missing_timestamp | missing_type
  data$anomaly_reason <- compose_anomaly_reason(
    missing_timestamp = missing_timestamp,
    missing_event_type = missing_type
  )
  data
}

compose_anomaly_reason <- function(...) {
  values <- list(...)
  n <- length(values[[1]])
  out <- rep(NA_character_, n)
  for (nm in names(values)) {
    flag <- values[[nm]]
    out[flag] <- append_warning(out[flag], nm)
  }
  out
}

filter_collection_app <- function(data) {
  if (!"is_collection_app" %in% names(data)) {
    return(data)
  }
  data[is.na(data$is_collection_app) | !data$is_collection_app, , drop = FALSE]
}

classify_activity_type <- function(app_name) {
  app_name <- as.character(app_name)
  background <- !is.na(app_name) & (
    grepl("\uFF08\u6D41\u5A92\u4F53\uFF09", app_name, fixed = TRUE) |
      grepl("(\u6D41\u5A92\u4F53)", app_name, fixed = TRUE)
  )
  out <- rep("foreground", length(app_name))
  out[background] <- "background"
  out
}

daily_source_from_export <- function(export_type, n) {
  export_type <- as.character(export_type)
  if (length(export_type) == 0 || all(is.na(export_type))) {
    return(rep(NA_character_, n))
  }
  if (length(export_type) == 1) {
    export_type <- rep(export_type, n)
  }
  out <- ifelse(export_type == "app", "app_export",
    ifelse(export_type == "day", "day_export", paste0(export_type, "_export"))
  )
  out[is.na(export_type) | export_type == ""] <- NA_character_
  out
}

canonical_second_episode_schema <- function() {
  list(
    date = as.Date(character()),
    app_name = character(),
    activity_type = character(),
    package_name = character(),
    start_ts_ms = numeric(),
    end_ts_ms = numeric(),
    start_datetime = as.POSIXct(character()),
    end_datetime = as.POSIXct(character()),
    duration_ms = numeric(),
    duration_min = numeric(),
    duration_text = character(),
    source_episode_count = integer(),
    source_duration_ms = numeric(),
    merged_gap_ms = numeric(),
    source_export_type = character(),
    parse_warning = character(),
    is_collection_app = logical(),
    episode_source = character(),
    pairing_strategy = character(),
    start_event_type = numeric(),
    end_event_type = numeric(),
    start_event_type_label = character(),
    end_event_type_label = character(),
    start_class_name = character(),
    end_class_name = character(),
    reconstruction_status = character(),
    reconstruction_warning = character(),
    unmatched_start = logical(),
    unmatched_end = logical(),
    device_boundary_involved = logical(),
    anomaly_missing_duration = logical(),
    anomaly_negative_duration = logical(),
    anomaly_extreme_duration = logical(),
    anomaly_cross_date = logical(),
    anomaly_any = logical(),
    anomaly_reason = character()
  )
}

conform_second_episode <- function(data) {
  data <- tibble::as_tibble(data)
  schema <- canonical_second_episode_schema()
  n <- nrow(data)
  for (nm in names(schema)) {
    if (!nm %in% names(data)) {
      data[[nm]] <- typed_episode_default(schema[[nm]], n, nm)
    }
  }
  data <- data[, names(schema), drop = FALSE]
  for (nm in names(schema)) {
    data[[nm]] <- coerce_episode_column(data[[nm]], schema[[nm]], nm)
  }
  tibble::as_tibble(data)
}

typed_episode_default <- function(template, n, name) {
  if (inherits(template, "Date")) {
    return(as.Date(rep(NA_character_, n)))
  }
  if (inherits(template, "POSIXt")) {
    return(as.POSIXct(rep(NA_real_, n), origin = "1970-01-01"))
  }
  if (is.integer(template)) {
    return(rep(NA_integer_, n))
  }
  if (is.numeric(template)) {
    return(rep(NA_real_, n))
  }
  if (is.logical(template)) {
    return(if (grepl("^anomaly_|^is_|^unmatched_|^device_boundary", name)) rep(FALSE, n) else rep(NA, n))
  }
  rep(NA_character_, n)
}

coerce_episode_column <- function(x, template, name) {
  if (inherits(template, "Date")) {
    return(as.Date(x))
  }
  if (inherits(template, "POSIXt")) {
    if (is.numeric(x)) {
      return(as.POSIXct(x, origin = "1970-01-01"))
    }
    return(as.POSIXct(x))
  }
  if (is.integer(template)) {
    return(as.integer(x))
  }
  if (is.numeric(template)) {
    return(as.numeric(x))
  }
  if (is.logical(template)) {
    x <- as.logical(x)
    if (grepl("^anomaly_|^is_|^unmatched_|^device_boundary", name)) {
      x[is.na(x)] <- FALSE
    }
    return(x)
  }
  as.character(x)
}

canonical_second_daily_schema <- function() {
  list(
    date = as.Date(character()),
    weekday = character(),
    app_name = character(),
    activity_type = character(),
    package_name = character(),
    duration_ms = numeric(),
    duration_min = numeric(),
    open_count = integer(),
    notification_count = integer(),
    split_screen_ms = numeric(),
    episode_count = integer(),
    event_count = integer(),
    source_export_type = character(),
    daily_source = character(),
    summary_duration_ms = numeric(),
    episode_duration_ms = numeric(),
    duration_diff_ms = numeric(),
    duration_diff_pct = numeric(),
    duration_agreement_status = character(),
    complete_episode_count = integer(),
    unmatched_start_count = integer(),
    unmatched_end_count = integer(),
    invalid_pair_count = integer(),
    reconstruction_warning_count = integer(),
    is_all_apps = logical(),
    is_collection_app = logical(),
    parse_warning = character(),
    n_anomalies = integer(),
    anomaly_missing_duration = logical(),
    anomaly_negative_duration = logical(),
    anomaly_extreme_duration = logical(),
    anomaly_cross_date = logical(),
    anomaly_any = logical(),
    anomaly_reason = character()
  )
}

conform_second_daily <- function(data) {
  data <- tibble::as_tibble(data)
  schema <- canonical_second_daily_schema()
  n <- nrow(data)
  for (nm in names(schema)) {
    if (!nm %in% names(data)) {
      data[[nm]] <- typed_daily_default(schema[[nm]], n, nm)
    }
  }
  data <- data[, names(schema), drop = FALSE]
  for (nm in names(schema)) {
    data[[nm]] <- coerce_daily_column(data[[nm]], schema[[nm]], nm)
  }
  tibble::as_tibble(data)
}

typed_daily_default <- function(template, n, name) {
  if (inherits(template, "Date")) {
    return(as.Date(rep(NA_character_, n)))
  }
  if (is.integer(template)) {
    zero_default <- name %in% c(
      "n_anomalies",
      "complete_episode_count",
      "unmatched_start_count",
      "unmatched_end_count",
      "invalid_pair_count",
      "reconstruction_warning_count"
    )
    return(if (zero_default) rep(0L, n) else rep(NA_integer_, n))
  }
  if (is.numeric(template)) {
    return(rep(NA_real_, n))
  }
  if (is.logical(template)) {
    return(if (grepl("^anomaly_|^is_", name)) rep(FALSE, n) else rep(NA, n))
  }
  rep(NA_character_, n)
}

coerce_daily_column <- function(x, template, name) {
  if (inherits(template, "Date")) {
    return(as.Date(x))
  }
  if (is.integer(template)) {
    x <- as.integer(x)
    zero_default <- name %in% c(
      "n_anomalies",
      "complete_episode_count",
      "unmatched_start_count",
      "unmatched_end_count",
      "invalid_pair_count",
      "reconstruction_warning_count"
    )
    if (zero_default) {
      x[is.na(x)] <- 0L
    }
    return(x)
  }
  if (is.numeric(template)) {
    return(as.numeric(x))
  }
  if (is.logical(template)) {
    x <- as.logical(x)
    if (grepl("^anomaly_|^is_", name)) {
      x[is.na(x)] <- FALSE
    }
    return(x)
  }
  as.character(x)
}

empty_second_event_tibble <- function() {
  tibble::tibble(
    date = as.Date(character()),
    table_date = as.Date(character()),
    app_name = character(),
    activity_type = character(),
    package_name = character(),
    class_name = character(),
    event_datetime = as.POSIXct(character()),
    event_ts_ms = numeric(),
    event_type = numeric(),
    event_type_label = character(),
    configuration = character(),
    source_export_type = character(),
    parse_warning = character(),
    is_collection_app = logical(),
    anomaly_missing_timestamp = logical(),
    anomaly_missing_event_type = logical(),
    anomaly_any = logical(),
    anomaly_reason = character()
  )
}

empty_second_episode_tibble <- function() {
  tibble::as_tibble(canonical_second_episode_schema())
}

empty_second_daily_tibble <- function() {
  tibble::as_tibble(canonical_second_daily_schema())
}
