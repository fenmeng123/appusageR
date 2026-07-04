#' Parse line-format APP Usage exports
#'
#' Parses episode-level APP Usage records. Episode duration is computed from
#' `end_ts_ms - start_ts_ms`; formatted duration labels are preserved as display
#' text only.
#'
#' @inheritParams parse_day
#'
#' @return A tibble with canonical episode-level columns.
#' @export
parse_line <- function(x, input = c("file", "text", "lines"),
                       participant_id = NULL, source_file = NULL,
                       tz = "Asia/Shanghai", encoding = "auto",
                       strict = FALSE) {
  input <- match.arg(input)
  lines <- read_appusage_lines(x, input = input, encoding = encoding)
  mat <- as_text_matrix(lines)
  required_fields <- line_required_fields()
  header_rows <- find_header_rows(
    mat,
    unname(required_fields)
  )
  diagnostics <- make_parser_diagnostics(
    export_type = "line",
    lines = lines,
    mat = mat,
    header_rows = header_rows,
    required_fields = required_fields
  )

  if (length(header_rows) == 0) {
    msg <- "No line-format APP Usage table header was found."
    if (strict) {
      stop(parser_error(msg, diagnostics, "appusage_missing_required_header"))
    }
    return(attach_parser_diagnostics(empty_line_tibble(), diagnostics))
  }

  source_file <- source_file %||% source_file_label(x, input)
  blocks <- collect_blocks(mat, header_rows)
  raw <- do.call(rbind, lapply(blocks, parse_line_block, mat = mat))
  out <- finalize_line_tibble(raw, participant_id, source_file, tz = tz)
  diagnostics <- finalize_parser_diagnostics(
    diagnostics,
    raw,
    out,
    extras = line_format_diagnostics(raw, out)
  )
  attach_parser_diagnostics(out, diagnostics)
}

#' Parse meta-format APP Usage exports
#'
#' Splits meta exports into app-level summary records and event-level usage-log
#' records.
#'
#' @inheritParams parse_day
#'
#' @return A list with `summary` and `events` tibbles.
#' @export
parse_meta <- function(x, input = c("file", "text", "lines"),
                       participant_id = NULL, source_file = NULL,
                       tz = "Asia/Shanghai", encoding = "auto",
                       strict = FALSE) {
  input <- match.arg(input)
  lines <- read_appusage_lines(x, input = input, encoding = encoding)
  mat <- as_text_matrix(lines)
  marker_text <- apply(mat, 1, paste, collapse = " ")
  table1_rows <- which(stringr::str_detect(marker_text, "\\u8868\\u4e00"))
  table2_rows <- which(stringr::str_detect(marker_text, "\\u8868\\u4e8c"))
  diagnostics <- make_parser_diagnostics(
    export_type = "meta",
    lines = lines,
    mat = mat,
    header_rows = sort(unique(c(table1_rows, table2_rows))),
    required_fields = meta_required_fields(),
    extras = list(
      n_table1_markers = length(table1_rows),
      n_table2_markers = length(table2_rows)
    )
  )

  if (length(table1_rows) == 0 || length(table2_rows) == 0) {
    msg <- "No meta-format APP Usage table markers were found."
    if (strict) {
      stop(parser_error(msg, diagnostics, "appusage_missing_required_header"))
    }
    return(attach_parser_diagnostics(list(
      summary = attach_parser_diagnostics(empty_meta_summary_tibble(), diagnostics),
      events = attach_parser_diagnostics(empty_meta_events_tibble(), diagnostics)
    ), diagnostics))
  }

  source_file <- source_file %||% source_file_label(x, input)
  pairs <- meta_table_pairs(table1_rows, table2_rows, nrow(mat))
  raw_summary <- do.call(rbind, lapply(pairs, parse_meta_summary_pair, mat = mat))
  raw_events <- do.call(rbind, lapply(pairs, parse_meta_events_pair, mat = mat))
  summary <- finalize_meta_summary_tibble(raw_summary, participant_id, source_file, tz = tz)
  events <- finalize_meta_events_tibble(raw_events, participant_id, source_file, tz = tz)
  out <- list(
    summary = summary,
    events = events
  )
  diagnostics <- finalize_parser_diagnostics(
    diagnostics,
    list(summary = raw_summary, events = raw_events),
    out,
    extras = meta_format_diagnostics(pairs, summary, events)
  )
  out$summary <- attach_parser_diagnostics(out$summary, diagnostics)
  out$events <- attach_parser_diagnostics(out$events, diagnostics)

  attach_parser_diagnostics(out, diagnostics)
}

line_required_fields <- function() {
  list(
    start_ts_ms = "\\u5f00\\u59cb\\u65f6\\u95f4\\uff08ms\\uff09",
    start_time_text = "\\u5f00\\u59cb\\u65f6\\u95f4",
    app_name = "\\u5e94\\u7528\\u540d\\u79f0",
    package_name = "\\u5e94\\u7528\\u6807\\u8bc6|\\u5e94\\u7528\\u5305\\u540d",
    duration_text = "\\u4f7f\\u7528\\u65f6\\u957f",
    end_ts_ms = "\\u7ed3\\u675f\\u65f6\\u95f4\\uff08ms\\uff09",
    end_time_text = "\\u7ed3\\u675f\\u65f6\\u95f4"
  )
}

meta_required_fields <- function() {
  list(
    table1_marker = "\\u8868\\u4e00",
    table2_marker = "\\u8868\\u4e8c",
    app_name = "\\u5e94\\u7528\\u540d\\u79f0",
    package_name = "\\u5e94\\u7528\\u5305\\u540d|\\u5e94\\u7528\\u6807\\u8bc6",
    class_name = "\\u5177\\u4f53\\u9875\\u9762",
    timestamp = "\\u65f6\\u95f4\\u6233",
    event_type = "\\u7c7b\\u578b",
    configuration = "\\u914d\\u7f6e"
  )
}

line_format_diagnostics <- function(raw, out) {
  raw_timestamps <- character()
  if (is.data.frame(raw)) {
    timestamp_cols <- intersect(c("start_ts_ms", "end_ts_ms"), names(raw))
    raw_timestamps <- unlist(raw[timestamp_cols], use.names = FALSE)
  }
  list(
    n_t_prefixed_timestamps = sum(grepl("^T:", raw_timestamps), na.rm = TRUE),
    n_zero_duration_episode = if (is.data.frame(out)) sum(out$duration_ms == 0, na.rm = TRUE) else 0L,
    n_negative_episode_duration = if (is.data.frame(out)) sum(out$duration_ms < 0, na.rm = TRUE) else 0L,
    n_cross_date_episode = if (is.data.frame(out) && nrow(out) > 0) {
      sum(as.Date(out$start_datetime) != as.Date(out$end_datetime), na.rm = TRUE)
    } else {
      0L
    }
  )
}

meta_format_diagnostics <- function(pairs, summary, events) {
  event_types <- if (is.data.frame(events) && nrow(events) > 0) {
    table(events$event_type, useNA = "no")
  } else {
    integer()
  }
  event_type_counts <- as.list(as.integer(event_types))
  names(event_type_counts) <- names(event_types)
  unknown_event_types <- if (is.data.frame(events) && nrow(events) > 0) {
    unique(events$event_type[grepl("^EVENT_TYPE_", events$event_type_label)])
  } else {
    numeric()
  }
  list(
    n_table_pairs = length(pairs),
    n_meta_summary_rows = if (is.data.frame(summary)) nrow(summary) else 0L,
    n_meta_event_rows = if (is.data.frame(events)) nrow(events) else 0L,
    table1_empty = !is.data.frame(summary) || nrow(summary) == 0,
    table2_empty = !is.data.frame(events) || nrow(events) == 0,
    event_type_counts = event_type_counts,
    unknown_event_types = as.numeric(unknown_event_types),
    n_null_configuration = if (is.data.frame(events) && "configuration" %in% names(events)) {
      sum(is.na(events$configuration))
    } else {
      NA_integer_
    },
    n_missing_class_name = if (is.data.frame(events) && "class_name" %in% names(events)) {
      sum(is.na(events$class_name))
    } else {
      NA_integer_
    }
  )
}

parse_line_block <- function(block, mat) {
  header <- mat[block$header_row, ]
  pos <- list(
    start_ts_ms = header_position(header, "^\\u5f00\\u59cb\\u65f6\\u95f4\\uff08ms\\uff09$"),
    start_time_text = header_position(header, "^\\u5f00\\u59cb\\u65f6\\u95f4$"),
    app_name = header_position(header, "^\\u5e94\\u7528\\u540d\\u79f0$"),
    package_name = header_position(header, "^\\u5e94\\u7528\\u6807\\u8bc6$|^\\u5e94\\u7528\\u5305\\u540d$"),
    duration_text = header_position(header, "^\\u4f7f\\u7528\\u65f6\\u957f$"),
    end_ts_ms = header_position(header, "^\\u7ed3\\u675f\\u65f6\\u95f4\\uff08ms\\uff09$"),
    end_time_text = header_position(header, "^\\u7ed3\\u675f\\u65f6\\u95f4$")
  )
  date <- extract_row_date(header, previous_date(mat, block$header_row))
  data <- rows_for_block(mat, block)
  rows <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    if (!valid_data_row(row) ||
      row_contains_any(row, c("\\u5f00\\u673a\\u81f3\\u4eca\\u6b65\\u6570\\u4fe1\\u606f", "^STEP", "\\u5f00\\u59cb\\u65f6\\u95f4\\uff08ms\\uff09", "\\u7ed3\\u675f\\u65f6\\u95f4\\uff08ms\\uff09"))) {
      next
    }
    values <- list(
      date = extract_row_date(row, date),
      app_name = first_present(row, pos$app_name),
      package_name = first_present(row, pos$package_name),
      start_ts_ms = first_present(row, pos$start_ts_ms),
      end_ts_ms = first_present(row, pos$end_ts_ms),
      start_time_text = first_present(row, pos$start_time_text),
      end_time_text = first_present(row, pos$end_time_text),
      duration_text = first_present(row, pos$duration_text)
    )
    if (all(is.na(unlist(values[c("app_name", "package_name", "start_ts_ms", "end_ts_ms")])))) {
      next
    }
    rows[[i]] <- data.frame(
      date = values$date,
      app_name = values$app_name,
      package_name = values$package_name,
      start_ts_ms = values$start_ts_ms,
      end_ts_ms = values$end_ts_ms,
      start_time_text = values$start_time_text,
      end_time_text = values$end_time_text,
      duration_text = values$duration_text,
      parse_warning = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

finalize_line_tibble <- function(data, participant_id, source_file, tz) {
  if (is.null(data) || nrow(data) == 0) {
    return(empty_line_tibble())
  }
  start_ts_ms <- parse_t_timestamp(data$start_ts_ms)
  end_ts_ms <- parse_t_timestamp(data$end_ts_ms)
  duration_ms <- end_ts_ms - start_ts_ms
  package_name <- standardize_package_name(data$package_name)
  parse_warning <- data$parse_warning
  bad_duration <- is.na(duration_ms) | duration_ms < 0
  parse_warning[bad_duration] <- append_warning(
    parse_warning[bad_duration],
    "duration_ms could not be computed from timestamps"
  )

  out <- tibble::tibble(
    participant_id = participant_id,
    source_file = source_file,
    export_type = "line",
    date = safe_as_date(data$date),
    app_name = blank_to_na(data$app_name),
    package_name = package_name,
    start_ts_ms = start_ts_ms,
    end_ts_ms = end_ts_ms,
    start_datetime = ms_to_datetime(start_ts_ms, tz = tz),
    end_datetime = ms_to_datetime(end_ts_ms, tz = tz),
    start_time_text = blank_to_na(data$start_time_text),
    end_time_text = blank_to_na(data$end_time_text),
    duration_text = blank_to_na(data$duration_text),
    duration_ms = duration_ms,
    duration_min = duration_ms / 60000,
    is_collection_app = package_name == "com.w.appusage",
    parse_warning = parse_warning
  )
  out[order(out$start_ts_ms, seq_len(nrow(out))), , drop = FALSE]
}

meta_table_pairs <- function(table1_rows, table2_rows, n_rows) {
  pairs <- vector("list", length(table1_rows))
  for (i in seq_along(table1_rows)) {
    t1 <- table1_rows[[i]]
    t2_candidates <- table2_rows[table2_rows > t1]
    t2 <- if (length(t2_candidates) == 0) NA_integer_ else t2_candidates[[1]]
    next_t1_candidates <- table1_rows[table1_rows > t1]
    end <- if (length(next_t1_candidates) == 0) n_rows else next_t1_candidates[[1]] - 1
    pairs[[i]] <- list(table1 = t1, table2 = t2, end = end)
  }
  pairs
}

parse_meta_summary_pair <- function(pair, mat) {
  if (is.na(pair$table2) || pair$table1 >= pair$table2) {
    return(empty_meta_summary_data_frame())
  }
  header <- mat[pair$table1, ]
  table_date <- extract_row_date(header)
  if ((pair$table1 + 1) > (pair$table2 - 1)) {
    data <- mat[0, , drop = FALSE]
  } else {
    data <- mat[(pair$table1 + 1):(pair$table2 - 1), , drop = FALSE]
  }
  rows <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    if (!valid_data_row(row) || row_contains_any(row, c("\\u8868\\u4e00", "\\u8868\\u4e8c"))) {
      next
    }
    rows[[i]] <- data.frame(
      table_date = table_date,
      app_name = first_present(row, 2),
      package_name = first_present(row, 3),
      start_datetime = first_present(row, 4),
      end_datetime = first_present(row, 5),
      last_datetime = first_present(row, 6),
      total_duration_text = first_present(row, 7),
      start_ts_ms = first_present(row, 8),
      end_ts_ms = first_present(row, 9),
      last_ts_ms = first_present(row, 10),
      total_duration_ms = first_present(row, 11),
      parse_warning = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

parse_meta_events_pair <- function(pair, mat) {
  if (is.na(pair$table2)) {
    return(empty_meta_events_data_frame())
  }
  header <- mat[pair$table2, ]
  table_date <- extract_row_date(header)
  if ((pair$table2 + 1) > pair$end) {
    data <- mat[0, , drop = FALSE]
  } else {
    data <- mat[(pair$table2 + 1):pair$end, , drop = FALSE]
  }
  rows <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    if (!valid_data_row(row) || row_contains_any(row, c("\\u8868\\u4e00", "\\u8868\\u4e8c"))) {
      next
    }
    rows[[i]] <- data.frame(
      table_date = table_date,
      app_name = first_present(row, 2),
      package_name = first_present(row, 3),
      class_name = first_present(row, 4),
      event_datetime = first_present(row, 5),
      event_ts_ms = first_present(row, 6),
      event_type = first_present(row, 7),
      configuration = first_present(row, 8),
      parse_warning = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

finalize_meta_summary_tibble <- function(data, participant_id, source_file, tz) {
  if (is.null(data) || nrow(data) == 0) {
    return(empty_meta_summary_tibble())
  }
  package_name <- standardize_package_name(data$package_name)
  out <- tibble::tibble(
    participant_id = participant_id,
    source_file = source_file,
    export_type = "meta",
    table_date = safe_as_date(data$table_date),
    app_name = blank_to_na(data$app_name),
    package_name = package_name,
    start_datetime = safe_as_datetime(data$start_datetime, tz = tz),
    end_datetime = safe_as_datetime(data$end_datetime, tz = tz),
    last_datetime = safe_as_datetime(data$last_datetime, tz = tz),
    total_duration_text = blank_to_na(data$total_duration_text),
    total_duration_ms = parse_t_timestamp(data$total_duration_ms),
    start_ts_ms = parse_t_timestamp(data$start_ts_ms),
    end_ts_ms = parse_t_timestamp(data$end_ts_ms),
    last_ts_ms = parse_t_timestamp(data$last_ts_ms),
    parse_warning = data$parse_warning
  )
  out[order(out$table_date, out$last_ts_ms, seq_len(nrow(out))), , drop = FALSE]
}

finalize_meta_events_tibble <- function(data, participant_id, source_file, tz) {
  if (is.null(data) || nrow(data) == 0) {
    return(empty_meta_events_tibble())
  }
  event_type <- parse_count(data$event_type)
  out <- tibble::tibble(
    participant_id = participant_id,
    source_file = source_file,
    export_type = "meta",
    table_date = safe_as_date(data$table_date),
    app_name = blank_to_na(data$app_name),
    package_name = standardize_package_name(data$package_name),
    class_name = blank_to_na(data$class_name),
    event_datetime = safe_as_datetime(data$event_datetime, tz = tz),
    event_ts_ms = parse_t_timestamp(data$event_ts_ms),
    event_type = event_type,
    event_type_label = label_event_type(event_type),
    configuration = blank_to_na(data$configuration),
    parse_warning = data$parse_warning
  )
  out[order(out$event_ts_ms, seq_len(nrow(out))), , drop = FALSE]
}

empty_line_tibble <- function() {
  tibble::tibble(
    participant_id = character(),
    source_file = character(),
    export_type = character(),
    date = as.Date(character()),
    app_name = character(),
    package_name = character(),
    start_ts_ms = numeric(),
    end_ts_ms = numeric(),
    start_datetime = as.POSIXct(character()),
    end_datetime = as.POSIXct(character()),
    start_time_text = character(),
    end_time_text = character(),
    duration_text = character(),
    duration_ms = numeric(),
    duration_min = numeric(),
    is_collection_app = logical(),
    parse_warning = character()
  )
}

empty_meta_summary_data_frame <- function() {
  data.frame(
    table_date = character(),
    app_name = character(),
    package_name = character(),
    start_datetime = character(),
    end_datetime = character(),
    last_datetime = character(),
    total_duration_text = character(),
    total_duration_ms = character(),
    start_ts_ms = character(),
    end_ts_ms = character(),
    last_ts_ms = character(),
    parse_warning = character(),
    stringsAsFactors = FALSE
  )
}

empty_meta_events_data_frame <- function() {
  data.frame(
    table_date = character(),
    app_name = character(),
    package_name = character(),
    class_name = character(),
    event_datetime = character(),
    event_ts_ms = character(),
    event_type = character(),
    configuration = character(),
    parse_warning = character(),
    stringsAsFactors = FALSE
  )
}

empty_meta_summary_tibble <- function() {
  tibble::tibble(
    participant_id = character(),
    source_file = character(),
    export_type = character(),
    table_date = as.Date(character()),
    app_name = character(),
    package_name = character(),
    start_datetime = as.POSIXct(character()),
    end_datetime = as.POSIXct(character()),
    last_datetime = as.POSIXct(character()),
    total_duration_text = character(),
    total_duration_ms = numeric(),
    start_ts_ms = numeric(),
    end_ts_ms = numeric(),
    last_ts_ms = numeric(),
    parse_warning = character()
  )
}

empty_meta_events_tibble <- function() {
  tibble::tibble(
    participant_id = character(),
    source_file = character(),
    export_type = character(),
    table_date = as.Date(character()),
    app_name = character(),
    package_name = character(),
    class_name = character(),
    event_datetime = as.POSIXct(character()),
    event_ts_ms = numeric(),
    event_type = numeric(),
    event_type_label = character(),
    configuration = character(),
    parse_warning = character()
  )
}
