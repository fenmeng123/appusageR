as_text_matrix <- function(lines) {
  pieces <- strsplit(lines, ",", fixed = TRUE)
  width <- max(lengths(pieces), 1)
  mat <- matrix(NA_character_, nrow = length(pieces), ncol = width)
  for (i in seq_along(pieces)) {
    values <- stringr::str_trim(pieces[[i]])
    values[values == ""] <- NA_character_
    mat[i, seq_along(values)] <- values
  }
  drop_leading_empty_cols(mat)
}

drop_leading_empty_cols <- function(mat) {
  if (ncol(mat) == 0) {
    attr(mat, "n_leading_empty_columns") <- 0L
    return(mat)
  }
  keep <- rep(TRUE, ncol(mat))
  n_dropped <- 0L
  for (j in seq_len(ncol(mat))) {
    if (all(is.na(mat[, j]) | mat[, j] == "")) {
      keep[j] <- FALSE
      n_dropped <- n_dropped + 1L
    } else {
      break
    }
  }
  out <- mat[, keep, drop = FALSE]
  attr(out, "n_leading_empty_columns") <- n_dropped
  out
}

row_contains_all <- function(row, patterns) {
  text <- paste(row, collapse = "\n")
  all(vapply(patterns, stringr::str_detect, logical(1), string = text))
}

find_header_rows <- function(mat, required_patterns) {
  which(apply(mat, 1, row_contains_all, patterns = required_patterns))
}

row_contains_any <- function(row, patterns) {
  text <- paste(row, collapse = "\n")
  any(vapply(patterns, stringr::str_detect, logical(1), string = text))
}

extract_row_date <- function(row, fallback = NA_character_) {
  date <- stringr::str_extract(
    paste(row, collapse = " "),
    "[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}"
  )
  ifelse(is.na(date), fallback, date)
}

previous_date <- function(mat, row_index) {
  if (row_index <= 1) {
    return(NA_character_)
  }
  for (i in rev(seq_len(row_index - 1))) {
    date <- extract_row_date(mat[i, ])
    if (!is.na(date)) {
      return(date)
    }
  }
  NA_character_
}

collect_blocks <- function(mat, header_rows) {
  if (length(header_rows) == 0) {
    return(list())
  }
  lapply(seq_along(header_rows), function(i) {
    start <- header_rows[[i]]
    end <- if (i < length(header_rows)) header_rows[[i + 1]] - 1 else nrow(mat)
    list(header_row = start, start = start + 1, end = end)
  })
}

rows_for_block <- function(mat, block) {
  if (block$start > block$end) {
    return(mat[0, , drop = FALSE])
  }
  mat[block$start:block$end, , drop = FALSE]
}

valid_data_row <- function(row) {
  any(!is.na(row) & row != "")
}

header_position <- function(header, patterns) {
  idx <- which(vapply(header, function(cell) {
    any(stringr::str_detect(cell, patterns), na.rm = TRUE)
  }, logical(1)))
  if (length(idx) == 0) {
    NA_integer_
  } else {
    idx[[1]]
  }
}

first_present <- function(row, position) {
  if (is.na(position) || position < 1 || position > length(row)) {
    return(NA_character_)
  }
  value <- blank_to_na(row[[position]])
  if (length(value) == 0) {
    NA_character_
  } else {
    value
  }
}

parse_count <- function(x) {
  suppressWarnings(as.numeric(stringr::str_extract(
    as.character(x),
    "-?[0-9]+(?:\\.[0-9]+)?"
  )))
}

append_warning <- function(existing, warning) {
  ifelse(
    is.na(existing) | existing == "",
    warning,
    paste(existing, warning, sep = "; ")
  )
}

make_parser_diagnostics <- function(export_type, lines, mat, header_rows,
                                    required_fields = list(),
                                    optional_fields = list(),
                                    extras = list()) {
  text <- paste(mat, collapse = "\n")
  required_hits <- parser_field_hits(text, required_fields)
  optional_hits <- parser_field_hits(text, optional_fields)
  required_found <- names(required_hits)[required_hits > 0]
  required_missing <- names(required_hits)[required_hits == 0]
  optional_found <- names(optional_hits)[optional_hits > 0]
  optional_missing <- names(optional_hits)[optional_hits == 0]

  diagnostics <- list(
    input_profile = list(
      n_lines = length(lines),
      n_nonempty_lines = sum(nzchar(trimws(lines)), na.rm = TRUE),
      n_columns = if (is.null(dim(mat))) NA_integer_ else ncol(mat),
      n_leading_empty_columns = attr(mat, "n_leading_empty_columns", exact = TRUE) %||% 0L
    ),
    header_profile = list(
      n_header_rows = length(header_rows),
      header_rows = as.integer(header_rows),
      required_fields_found = unname(required_found),
      required_fields_missing = unname(required_missing),
      optional_fields_found = unname(optional_found),
      optional_fields_missing = unname(optional_missing),
      required_marker_hits = as.list(as.integer(required_hits)),
      optional_marker_hits = as.list(as.integer(optional_hits))
    ),
    data_presence = list(
      has_required_headers = length(header_rows) > 0,
      has_data_rows = NA,
      empty_reason = parser_empty_reason(lines, header_rows, NA_integer_)
    ),
    parse_quality = list(
      n_rows_in = NA_integer_,
      n_rows_out = NA_integer_,
      n_rows_with_parse_warning = NA_integer_
    ),
    format_specific = extras
  )
  names(diagnostics$header_profile$required_marker_hits) <- names(required_hits)
  names(diagnostics$header_profile$optional_marker_hits) <- names(optional_hits)
  diagnostics$export_type <- export_type
  diagnostics
}

parser_field_hits <- function(text, fields) {
  if (length(fields) == 0) {
    return(stats::setNames(integer(), character()))
  }
  hits <- vapply(fields, function(pattern) {
    sum(stringr::str_detect(text, pattern), na.rm = TRUE)
  }, integer(1))
  hits
}

parser_empty_reason <- function(lines, header_rows, n_rows_out) {
  if (length(lines) == 0 || sum(nzchar(trimws(lines)), na.rm = TRUE) == 0) {
    return("empty_file")
  }
  if (length(header_rows) == 0) {
    return("missing_required_headers")
  }
  if (!is.na(n_rows_out) && n_rows_out == 0) {
    return("headers_only_or_no_record_rows")
  }
  NA_character_
}

finalize_parser_diagnostics <- function(diagnostics, raw_data, parsed_data,
                                        extras = list()) {
  if (is.null(diagnostics)) {
    diagnostics <- list()
  }
  n_raw <- if (is.data.frame(raw_data)) nrow(raw_data) else NA_integer_
  n_out <- parser_total_rows(parsed_data)
  diagnostics$data_presence$has_data_rows <- !is.na(n_out) && n_out > 0
  diagnostics$data_presence$empty_reason <- parser_empty_reason(
    rep("x", max(1L, diagnostics$input_profile$n_nonempty_lines %||% 0L)),
    diagnostics$header_profile$header_rows %||% integer(),
    n_out
  )
  diagnostics$parse_quality <- parser_parse_quality(parsed_data)
  diagnostics$parse_quality$n_rows_in <- n_raw
  diagnostics$format_specific <- utils::modifyList(
    diagnostics$format_specific %||% list(),
    extras
  )
  diagnostics
}

parser_total_rows <- function(x) {
  if (is.data.frame(x)) {
    return(nrow(x))
  }
  if (is.list(x)) {
    rows <- vapply(x, function(item) {
      if (is.data.frame(item)) nrow(item) else 0L
    }, integer(1))
    return(sum(rows, na.rm = TRUE))
  }
  NA_integer_
}

parser_parse_quality <- function(x) {
  frames <- parser_data_frames(x)
  if (length(frames) == 0) {
    return(list(
      n_rows_out = 0L,
      n_rows_with_parse_warning = 0L
    ))
  }
  all_rows <- sum(vapply(frames, nrow, integer(1)), na.rm = TRUE)
  warnings <- sum(vapply(frames, function(frame) {
    if ("parse_warning" %in% names(frame)) {
      sum(!is.na(frame$parse_warning) & frame$parse_warning != "")
    } else {
      0L
    }
  }, integer(1)), na.rm = TRUE)
  out <- list(
    n_rows_out = all_rows,
    n_rows_with_parse_warning = warnings,
    n_missing_app_name = parser_missing_count(frames, "app_name"),
    n_missing_package_name = parser_missing_count(frames, "package_name"),
    n_collection_app_rows = parser_collection_count(frames),
    n_date_parse_fail = parser_missing_date_count(frames),
    n_timestamp_parse_fail = parser_missing_timestamp_count(frames),
    n_duration_parse_fail = parser_duration_warning_count(frames),
    n_negative_duration = parser_negative_duration_count(frames)
  )
  date_range <- parser_date_range(frames)
  timestamp_range <- parser_timestamp_range(frames)
  utils::modifyList(out, c(date_range, timestamp_range))
}

parser_data_frames <- function(x) {
  if (is.data.frame(x)) {
    return(list(x))
  }
  if (!is.list(x)) {
    return(list())
  }
  Filter(is.data.frame, unname(x))
}

parser_missing_count <- function(frames, column) {
  sum(vapply(frames, function(frame) {
    if (!column %in% names(frame)) {
      return(NA_integer_)
    }
    sum(is.na(frame[[column]]) | frame[[column]] == "")
  }, integer(1)), na.rm = TRUE)
}

parser_collection_count <- function(frames) {
  sum(vapply(frames, function(frame) {
    if ("is_collection_app" %in% names(frame)) {
      return(sum(isTRUE_VECTOR(frame$is_collection_app), na.rm = TRUE))
    }
    if ("package_name" %in% names(frame)) {
      return(sum(frame$package_name == "com.w.appusage", na.rm = TRUE))
    }
    0L
  }, integer(1)), na.rm = TRUE)
}

isTRUE_VECTOR <- function(x) {
  !is.na(x) & x
}

parser_missing_date_count <- function(frames) {
  sum(vapply(frames, function(frame) {
    cols <- intersect(c("date", "table_date"), names(frame))
    if (length(cols) == 0) {
      return(0L)
    }
    sum(vapply(cols, function(col) sum(is.na(frame[[col]])), integer(1)))
  }, integer(1)), na.rm = TRUE)
}

parser_missing_timestamp_count <- function(frames) {
  sum(vapply(frames, function(frame) {
    cols <- names(frame)[grepl("(^|_)ts_ms$", names(frame))]
    if (length(cols) == 0) {
      return(0L)
    }
    sum(vapply(cols, function(col) sum(is.na(frame[[col]])), integer(1)))
  }, integer(1)), na.rm = TRUE)
}

parser_duration_warning_count <- function(frames) {
  sum(vapply(frames, function(frame) {
    if (!"parse_warning" %in% names(frame)) {
      return(0L)
    }
    sum(grepl("duration_ms could not be parsed|duration_ms could not be computed", frame$parse_warning))
  }, integer(1)), na.rm = TRUE)
}

parser_negative_duration_count <- function(frames) {
  sum(vapply(frames, function(frame) {
    cols <- intersect(c("duration_ms", "total_duration_ms"), names(frame))
    if (length(cols) == 0) {
      return(0L)
    }
    sum(vapply(cols, function(col) sum(frame[[col]] < 0, na.rm = TRUE), integer(1)))
  }, integer(1)), na.rm = TRUE)
}

parser_date_range <- function(frames) {
  dates <- unlist(lapply(frames, function(frame) {
    cols <- intersect(c("date", "table_date"), names(frame))
    unlist(lapply(cols, function(col) as.character(frame[[col]])), use.names = FALSE)
  }), use.names = FALSE)
  dates <- dates[!is.na(dates) & nzchar(dates)]
  list(
    date_min = if (length(dates) == 0) NA_character_ else min(dates),
    date_max = if (length(dates) == 0) NA_character_ else max(dates)
  )
}

parser_timestamp_range <- function(frames) {
  timestamps <- unlist(lapply(frames, function(frame) {
    cols <- names(frame)[grepl("(^|_)ts_ms$", names(frame))]
    unlist(lapply(cols, function(col) frame[[col]]), use.names = FALSE)
  }), use.names = FALSE)
  timestamps <- timestamps[!is.na(timestamps)]
  list(
    timestamp_min = if (length(timestamps) == 0) NA_real_ else min(timestamps),
    timestamp_max = if (length(timestamps) == 0) NA_real_ else max(timestamps)
  )
}

attach_parser_diagnostics <- function(x, diagnostics) {
  attr(x, "parser_diagnostics") <- diagnostics
  x
}

parser_diagnostics <- function(x) {
  attr(x, "parser_diagnostics", exact = TRUE)
}

parser_error <- function(message, diagnostics, class = "appusage_parser_error") {
  structure(
    list(message = message, parser_diagnostics = diagnostics),
    class = c(class, "error", "condition")
  )
}

condition_parser_diagnostics <- function(error) {
  if (is.null(error)) {
    return(NULL)
  }
  error$parser_diagnostics %||% attr(error, "parser_diagnostics", exact = TRUE)
}
