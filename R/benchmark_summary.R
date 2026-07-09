build_preprocessing_benchmark_summary <- function(first = NULL, second = NULL,
                                                  qc = NULL, manifest = NULL,
                                                  first_level_worker_decision = NULL) {
  rows <- list(
    appusage_benchmark_stage_summary(
      summary = appusage_benchmark_attach_manifest(first, manifest),
      stage = "first_level",
      status_col = "status",
      elapsed_cols = c("elapsed_sec"),
      source_size_cols = c("source_size_bytes", "file_size"),
      row_count_cols = c("n_rows"),
      worker_decision = first_level_worker_decision
    ),
    appusage_benchmark_stage_summary(
      summary = second,
      stage = "second_level",
      status_col = "status",
      elapsed_cols = c("second_level_total_elapsed_sec", "elapsed_sec"),
      source_size_cols = c("first_level_rda_size_bytes"),
      row_count_cols = c("n_rows"),
      worker_decision = first_level_worker_decision
    ),
    appusage_benchmark_stage_summary(
      summary = qc,
      stage = "qc",
      status_col = "qc_status",
      elapsed_cols = c("qc_elapsed_sec", "second_level_inline_qc_elapsed_sec"),
      source_size_cols = c("second_level_rda_size_bytes", "first_level_rda_size_bytes"),
      row_count_cols = c("n_rows"),
      worker_decision = first_level_worker_decision
    )
  )
  tibble::as_tibble(do.call(rbind, rows))
}

write_preprocessing_benchmark_summary <- function(project_dir, first = NULL,
                                                  second = NULL, qc = NULL,
                                                  manifest = NULL,
                                                  first_level_worker_decision = NULL,
                                                  file = file.path(project_dir, "preprocessing_benchmark_summary.csv")) {
  benchmark <- build_preprocessing_benchmark_summary(
    first = first,
    second = second,
    qc = qc,
    manifest = manifest,
    first_level_worker_decision = first_level_worker_decision
  )
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(benchmark, file, row.names = FALSE, na = "")
  list(
    file = normalizePath(file, winslash = "/", mustWork = FALSE),
    summary = benchmark
  )
}

appusage_benchmark_attach_manifest <- function(summary, manifest) {
  if (is.null(summary) || nrow(summary) == 0 ||
    is.null(manifest) || nrow(manifest) == 0 ||
    !"source_file" %in% names(summary) ||
    !"source_file" %in% names(manifest) ||
    !"file_size" %in% names(manifest)) {
    return(summary)
  }
  out <- as.data.frame(summary, stringsAsFactors = FALSE)
  source_key <- normalizePath(out$source_file, winslash = "/", mustWork = FALSE)
  manifest_key <- normalizePath(manifest$source_file, winslash = "/", mustWork = FALSE)
  matched <- match(source_key, manifest_key)
  out$source_size_bytes <- suppressWarnings(as.numeric(manifest$file_size[matched]))
  out
}

appusage_benchmark_stage_summary <- function(summary, stage, status_col,
                                             elapsed_cols, source_size_cols,
                                             row_count_cols,
                                             worker_decision = NULL) {
  generated_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  worker <- appusage_benchmark_worker_decision(summary, worker_decision)
  if (is.null(summary) || nrow(summary) == 0) {
    return(appusage_empty_benchmark_stage(stage, worker, generated_at))
  }
  df <- as.data.frame(summary, stringsAsFactors = FALSE)
  status <- appusage_benchmark_character_column(df, status_col, "unknown")
  detected_type <- appusage_benchmark_detected_type(df)
  keys <- unique(data.frame(
    status = status,
    detected_type = detected_type,
    stringsAsFactors = FALSE
  ))
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    idx <- status == keys$status[[i]] & detected_type == keys$detected_type[[i]]
    elapsed <- appusage_benchmark_first_numeric(df, elapsed_cols)[idx]
    source_size <- appusage_benchmark_first_numeric(df, source_size_cols)[idx]
    row_count <- appusage_benchmark_first_numeric(df, row_count_cols)[idx]
    event_rows <- appusage_benchmark_numeric_column(df, "n_event_rows")[idx]
    episode_rows <- appusage_benchmark_numeric_column(df, "n_episode_rows")[idx]
    daily_rows <- appusage_benchmark_numeric_column(df, "n_daily_rows")[idx]
    worker_pid <- appusage_benchmark_first_numeric(df, c("worker_pid", "second_level_worker_pid"))[idx]
    data.frame(
      stage = stage,
      status = keys$status[[i]],
      detected_type = keys$detected_type[[i]],
      n_records = sum(idx, na.rm = TRUE),
      n_success = sum(status[idx] == "success", na.rm = TRUE),
      n_error = sum(status[idx] == "error", na.rm = TRUE),
      n_skipped = sum(status[idx] == "skipped", na.rm = TRUE),
      n_not_processed = sum(status[idx] == "not_processed", na.rm = TRUE),
      n_records_with_elapsed = sum(!is.na(elapsed)),
      elapsed_sec_total = appusage_benchmark_sum(elapsed),
      elapsed_sec_mean = appusage_benchmark_mean(elapsed),
      elapsed_sec_p50 = appusage_benchmark_quantile(elapsed, 0.50),
      elapsed_sec_p95 = appusage_benchmark_quantile(elapsed, 0.95),
      elapsed_sec_max = appusage_benchmark_max(elapsed),
      n_slow_gt_60s = sum(elapsed > 60, na.rm = TRUE),
      n_slow_gt_300s = sum(elapsed > 300, na.rm = TRUE),
      source_size_bytes_total = appusage_benchmark_sum(source_size),
      source_size_bytes_max = appusage_benchmark_max(source_size),
      n_rows_total = appusage_benchmark_sum(row_count),
      n_rows_max = appusage_benchmark_max(row_count),
      n_event_rows_total = appusage_benchmark_sum(event_rows),
      n_episode_rows_total = appusage_benchmark_sum(episode_rows),
      n_daily_rows_total = appusage_benchmark_sum(daily_rows),
      first_level_requested_workers = worker$requested_workers,
      first_level_selected_workers = worker$selected_workers,
      first_level_worker_cap_reason = worker$cap_reason,
      first_level_worker_cap_override = worker$worker_cap_override,
      n_worker_pids = length(unique(stats::na.omit(worker_pid))),
      generated_at = generated_at,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

appusage_empty_benchmark_stage <- function(stage, worker, generated_at) {
  data.frame(
    stage = stage,
    status = "missing",
    detected_type = NA_character_,
    n_records = 0L,
    n_success = 0L,
    n_error = 0L,
    n_skipped = 0L,
    n_not_processed = 0L,
    n_records_with_elapsed = 0L,
    elapsed_sec_total = NA_real_,
    elapsed_sec_mean = NA_real_,
    elapsed_sec_p50 = NA_real_,
    elapsed_sec_p95 = NA_real_,
    elapsed_sec_max = NA_real_,
    n_slow_gt_60s = 0L,
    n_slow_gt_300s = 0L,
    source_size_bytes_total = NA_real_,
    source_size_bytes_max = NA_real_,
    n_rows_total = NA_real_,
    n_rows_max = NA_real_,
    n_event_rows_total = NA_real_,
    n_episode_rows_total = NA_real_,
    n_daily_rows_total = NA_real_,
    first_level_requested_workers = worker$requested_workers,
    first_level_selected_workers = worker$selected_workers,
    first_level_worker_cap_reason = worker$cap_reason,
    first_level_worker_cap_override = worker$worker_cap_override,
    n_worker_pids = 0L,
    generated_at = generated_at,
    stringsAsFactors = FALSE
  )
}

appusage_benchmark_worker_decision <- function(summary, worker_decision = NULL) {
  out <- list(
    requested_workers = NA_integer_,
    selected_workers = NA_integer_,
    cap_reason = NA_character_,
    worker_cap_override = NA
  )
  if (!is.null(worker_decision)) {
    out$requested_workers <- suppressWarnings(as.integer(worker_decision$requested_workers %||% NA_integer_))
    out$selected_workers <- suppressWarnings(as.integer(worker_decision$selected_workers %||% NA_integer_))
    out$cap_reason <- as.character(worker_decision$cap_reason %||% NA_character_)
    out$worker_cap_override <- isTRUE(worker_decision$worker_cap_override)
    return(out)
  }
  if (!is.null(summary) && nrow(summary) > 0) {
    df <- as.data.frame(summary, stringsAsFactors = FALSE)
    out$requested_workers <- appusage_benchmark_first_value(df, "first_level_requested_workers", NA_integer_)
    out$selected_workers <- appusage_benchmark_first_value(df, "first_level_selected_workers", NA_integer_)
    out$cap_reason <- appusage_benchmark_first_value(df, "first_level_worker_cap_reason", NA_character_)
    out$worker_cap_override <- appusage_benchmark_first_value(df, "first_level_worker_cap_override", NA)
  }
  out
}

appusage_benchmark_detected_type <- function(df) {
  for (col in c("detected_type", "filename_export_type", "source_export_type")) {
    if (col %in% names(df)) {
      return(appusage_benchmark_clean_character(df[[col]], "unknown"))
    }
  }
  rep("unknown", nrow(df))
}

appusage_benchmark_character_column <- function(df, col, default) {
  if (col %in% names(df)) {
    return(appusage_benchmark_clean_character(df[[col]], default))
  }
  rep(default, nrow(df))
}

appusage_benchmark_clean_character <- function(x, default) {
  out <- as.character(x)
  out[is.na(out) | !nzchar(out)] <- default
  out
}

appusage_benchmark_first_numeric <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df)) {
      return(suppressWarnings(as.numeric(df[[col]])))
    }
  }
  rep(NA_real_, nrow(df))
}

appusage_benchmark_numeric_column <- function(df, col) {
  if (!col %in% names(df)) {
    return(rep(NA_real_, nrow(df)))
  }
  suppressWarnings(as.numeric(df[[col]]))
}

appusage_benchmark_first_value <- function(df, col, default) {
  if (!col %in% names(df)) {
    return(default)
  }
  value <- df[[col]]
  value <- value[!is.na(value)]
  if (length(value) == 0) {
    return(default)
  }
  value[[1]]
}

appusage_benchmark_sum <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  sum(x, na.rm = TRUE)
}

appusage_benchmark_mean <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  mean(x, na.rm = TRUE)
}

appusage_benchmark_max <- function(x) {
  if (all(is.na(x))) {
    return(NA_real_)
  }
  max(x, na.rm = TRUE)
}

appusage_benchmark_quantile <- function(x, prob) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  as.numeric(stats::quantile(x, probs = prob, names = FALSE, type = 7))
}
