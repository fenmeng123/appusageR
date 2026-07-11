appusage_daily_key_columns <- function(include_daily_source = TRUE) {
  columns <- c("date", "package_name", "app_name", "activity_type")
  if (isTRUE(include_daily_source)) c(columns, "daily_source") else columns
}

appusage_daily_key_component <- function(x) {
  x <- as.character(x)
  out <- rep("-1:", length(x))
  present <- !is.na(x)
  utf8 <- enc2utf8(x[present])
  byte_length <- nchar(utf8, type = "bytes", allowNA = TRUE, keepNA = TRUE)
  out[present] <- paste0(byte_length, ":", utf8)
  out
}

appusage_daily_key <- function(data, include_daily_source = TRUE) {
  data <- tibble::as_tibble(data)
  columns <- appusage_daily_key_columns(include_daily_source)
  values <- lapply(columns, function(column) {
    if (column %in% names(data)) data[[column]] else rep(NA_character_, nrow(data))
  })
  encoded <- lapply(values, appusage_daily_key_component)
  do.call(paste, c(encoded, sep = "|"))
}

appusage_daily_character_order_value <- function(x) {
  x <- as.character(x)
  vapply(x, function(value) {
    if (is.na(value)) return("")
    raw <- charToRaw(enc2utf8(value))
    paste(sprintf("%02x", as.integer(raw)), collapse = "")
  }, character(1))
}

# NA values sort after non-missing values at each key level. Exact ties retain
# their original row order through the final stable index.
appusage_daily_order_index <- function(data) {
  data <- tibble::as_tibble(data)
  if (nrow(data) <= 1L) return(seq_len(nrow(data)))
  date <- data$date
  package <- data$package_name
  app <- data$app_name
  activity <- data$activity_type
  source <- data$daily_source
  order(
    is.na(date), as.numeric(date),
    is.na(package), appusage_daily_character_order_value(package),
    is.na(app), appusage_daily_character_order_value(app),
    is.na(activity), appusage_daily_character_order_value(activity),
    is.na(source), appusage_daily_character_order_value(source),
    seq_len(nrow(data)), method = "radix"
  )
}

appusage_order_daily <- function(data) {
  data <- conform_second_daily(data)
  data[appusage_daily_order_index(data), , drop = FALSE]
}

appusage_order_expected_daily <- function(data, daily_source) {
  data <- tibble::as_tibble(data)
  order_data <- tibble::tibble(
    date = data$date,
    package_name = data$package_name,
    app_name = data$app_name,
    activity_type = data$activity_type,
    daily_source = daily_source
  )
  data[appusage_daily_order_index(order_data), , drop = FALSE]
}

appusage_rowsum_by_daily_key <- function(values, key, expected_keys) {
  values <- as.matrix(values)
  aggregated <- rowsum(values, key, reorder = FALSE, na.rm = TRUE)
  returned_keys <- rownames(aggregated)
  alignment <- match(expected_keys, returned_keys)
  if (anyNA(alignment) || anyDuplicated(returned_keys) ||
      length(returned_keys) != length(expected_keys) ||
      !setequal(returned_keys, expected_keys)) {
    cli::cli_abort(
      "Daily grouping key alignment failed.",
      class = "appusage_daily_key_alignment_error"
    )
  }
  aggregated[alignment, , drop = FALSE]
}

appusage_expected_episode_daily <- function(episodes, source, tz) {
  episodes <- conform_second_episode(episodes)
  selected <- if (identical(source, "line_episodes")) {
    episodes$episode_source == "line"
  } else {
    episodes$episode_source == "meta_events"
  }
  selected[is.na(selected)] <- FALSE
  episodes <- episodes[selected, , drop = FALSE]
  if (nrow(episodes) == 0L) {
    return(list(rows = tibble::tibble(), segmentation = NULL))
  }
  segments <- appusage_interval_segments(episodes, tz = tz)
  key <- appusage_daily_key(segments, include_daily_source = FALSE)
  expected_keys <- unique(key)
  duration <- as.numeric(segments$duration_ms)
  valid <- !is.na(duration) & duration >= 0
  if (identical(source, "meta_episodes")) {
    valid <- valid & segments$reconstruction_status == "complete"
    valid[is.na(valid)] <- FALSE
  }
  grouped <- appusage_rowsum_by_daily_key(
    cbind(
      duration_sum = ifelse(valid, duration, 0),
      valid_duration_count = as.integer(valid)
    ),
    key,
    expected_keys
  )
  first <- match(expected_keys, key)
  pair <- paste(key, segments$.source_row_id, sep = "\r")
  unique_pair <- !duplicated(pair) & valid
  episode_count <- tabulate(
    match(key[unique_pair], expected_keys), nbins = length(expected_keys)
  )
  duration_sum <- as.numeric(grouped[, "duration_sum"])
  duration_sum[grouped[, "valid_duration_count"] == 0] <- NA_real_
  rows <- tibble::tibble(
    key = expected_keys,
    date = segments$date[first],
    package_name = segments$package_name[first],
    app_name = segments$app_name[first],
    activity_type = segments$activity_type[first],
    duration_ms = duration_sum,
    episode_count = as.integer(episode_count),
    source_row_count = as.integer(tabulate(match(key, expected_keys), nbins = length(expected_keys)))
  )
  list(
    rows = rows,
    segmentation = attr(segments, "interval_segmentation_diagnostics", exact = TRUE)
  )
}

appusage_numeric_equal <- function(x, y) {
  tolerance <- sqrt(.Machine$double.eps) * pmax(1, abs(x), abs(y))
  abs(x - y) <= tolerance
}

appusage_validate_second_level_daily <- function(data, tz = "Asia/Shanghai") {
  tz <- appusage_resolve_timezone(tz)
  expected_map <- attr(data, "daily_aggregation_expected", exact = TRUE)
  daily <- conform_second_daily(data$daily %||% empty_second_daily_tibble())
  episodes <- conform_second_episode(data$episode %||% empty_second_episode_tibble())
  daily_key <- appusage_daily_key(daily, include_daily_source = TRUE)
  n_duplicate_daily_keys <- sum(duplicated(daily_key))
  order_violation <- !identical(appusage_daily_order_index(daily), seq_len(nrow(daily)))
  n_missing_daily_duration <- sum(is.na(daily$duration_ms))
  n_unmatched_source_keys <- 0L
  n_nonmissing_numeric_mismatch <- 0L
  n_episode_count_mismatch <- 0L
  n_expected_missing_duration <- 0L
  conservation_difference_ms <- 0
  checked_sources <- character()

  for (source in c("line_episodes", "meta_episodes")) {
    observed <- daily[daily$daily_source == source, , drop = FALSE]
    if (nrow(observed) == 0L) next
    expected <- expected_map[[source]] %||%
      appusage_expected_episode_daily(episodes, source, tz)
    checked_sources <- c(checked_sources, source)
    expected_rows <- expected$rows
    observed_key <- appusage_daily_key(observed, include_daily_source = FALSE)
    match_index <- match(expected_rows$key, observed_key)
    n_unmatched_source_keys <- n_unmatched_source_keys + sum(is.na(match_index))
    matched <- !is.na(match_index)
    if (any(matched)) {
      expected_duration <- expected_rows$duration_ms[matched]
      observed_duration <- observed$duration_ms[match_index[matched]]
      n_expected_missing_duration <- n_expected_missing_duration + sum(is.na(expected_duration))
      comparable <- !is.na(expected_duration) & !is.na(observed_duration)
      mismatch <- rep(FALSE, length(expected_duration))
      mismatch[comparable] <- !appusage_numeric_equal(
        expected_duration[comparable], observed_duration[comparable]
      )
      n_nonmissing_numeric_mismatch <- n_nonmissing_numeric_mismatch + sum(mismatch)
      expected_count <- expected_rows$episode_count[matched]
      observed_count <- observed$episode_count[match_index[matched]]
      count_comparable <- !is.na(expected_count) & !is.na(observed_count)
      n_episode_count_mismatch <- n_episode_count_mismatch + sum(
        count_comparable & expected_count != observed_count
      )
    }
    segment_diff <- expected$segmentation$duration_conservation_diff_ms %||% 0
    conservation_difference_ms <- conservation_difference_ms + segment_diff
  }

  conservation_failure <- !isTRUE(appusage_numeric_equal(
    conservation_difference_ms, 0
  ))
  critical <- n_nonmissing_numeric_mismatch > 0L ||
    n_episode_count_mismatch > 0L || n_duplicate_daily_keys > 0L ||
    isTRUE(order_violation) || isTRUE(conservation_failure)
  warning <- n_unmatched_source_keys > 0L || n_missing_daily_duration > 0L ||
    n_expected_missing_duration > 0L
  list(
    status = if (critical) "error" else if (warning) "warning" else "success",
    rule_version = "0.3.4-E",
    effective_timezone = tz,
    checked_daily_sources = checked_sources,
    n_daily_rows = nrow(daily),
    n_missing_or_unmatched_source_keys = as.integer(n_unmatched_source_keys),
    n_missing_daily_duration = as.integer(n_missing_daily_duration),
    n_expected_missing_source_duration = as.integer(n_expected_missing_duration),
    n_nonmissing_numeric_mismatch = as.integer(n_nonmissing_numeric_mismatch),
    n_episode_count_mismatch = as.integer(n_episode_count_mismatch),
    n_duplicate_daily_keys = as.integer(n_duplicate_daily_keys),
    order_violation = isTRUE(order_violation),
    duration_conservation_difference_ms = as.numeric(conservation_difference_ms),
    conservation_failure = isTRUE(conservation_failure)
  )
}

appusage_stop_on_daily_self_check <- function(result) {
  if (!identical(result$status, "error")) return(invisible(result))
  condition <- structure(
    list(
      message = paste0(
        "Second-level daily aggregation self-check failed: ",
        paste(c(
          if (result$n_nonmissing_numeric_mismatch > 0L) "numeric_mismatch",
          if (result$n_episode_count_mismatch > 0L) "episode_count_mismatch",
          if (result$n_duplicate_daily_keys > 0L) "duplicate_daily_keys",
          if (isTRUE(result$order_violation)) "order_violation",
          if (isTRUE(result$conservation_failure)) "duration_conservation_failure"
        ), collapse = "; ")
      ),
      call = NULL,
      daily_aggregation_self_check = result
    ),
    class = c("appusage_daily_aggregation_error", "error", "condition")
  )
  stop(condition)
}

appusage_daily_self_check_summary_values <- function(metadata_file = NA_character_) {
  empty <- list(
    daily_self_check_status = NA_character_,
    daily_self_check_missing_source_keys = NA_integer_,
    daily_self_check_missing_daily_duration = NA_integer_,
    daily_self_check_numeric_mismatch = NA_integer_,
    daily_self_check_episode_count_mismatch = NA_integer_,
    daily_self_check_duplicate_keys = NA_integer_,
    daily_self_check_order_violation = NA,
    daily_self_check_conservation_diff_ms = NA_real_
  )
  if (!is_present_string(metadata_file) || !file.exists(metadata_file)) return(empty)
  metadata <- tryCatch(
    jsonlite::read_json(metadata_file, simplifyVector = TRUE),
    error = function(e) NULL
  )
  check <- metadata$daily_aggregation_self_check
  if (!is.list(check)) return(empty)
  list(
    daily_self_check_status = as.character(check$status %||% NA_character_),
    daily_self_check_missing_source_keys = as.integer(
      check$n_missing_or_unmatched_source_keys %||% NA_integer_
    ),
    daily_self_check_missing_daily_duration = as.integer(
      check$n_missing_daily_duration %||% NA_integer_
    ),
    daily_self_check_numeric_mismatch = as.integer(
      check$n_nonmissing_numeric_mismatch %||% NA_integer_
    ),
    daily_self_check_episode_count_mismatch = as.integer(
      check$n_episode_count_mismatch %||% NA_integer_
    ),
    daily_self_check_duplicate_keys = as.integer(
      check$n_duplicate_daily_keys %||% NA_integer_
    ),
    daily_self_check_order_violation = as.logical(
      check$order_violation %||% NA
    ),
    daily_self_check_conservation_diff_ms = as.numeric(
      check$duration_conservation_difference_ms %||% NA_real_
    )
  )
}

appusage_attach_daily_self_check_summary <- function(row, metadata_file = NA_character_) {
  values <- appusage_daily_self_check_summary_values(metadata_file)
  for (name in names(values)) row[[name]] <- values[[name]]
  row
}
