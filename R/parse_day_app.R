#' Parse day-format APP Usage exports
#'
#' Parses date-by-app daily APP Usage exports into the canonical day-level
#' schema. `duration_ms` is used as the authoritative analytic duration field;
#' formatted duration labels are preserved as display text only.
#'
#' @param x File path, raw text, or character vector of lines.
#' @param input One of `"file"`, `"text"`, or `"lines"`.
#' @param participant_id Optional participant identifier.
#' @param source_file Optional source-file label.
#' @param tz Time zone. Reserved for API consistency with later parsers.
#' @param encoding Source encoding.
#' @param strict If `TRUE`, malformed input raises an error.
#'
#' @return A tibble with canonical day-level columns.
#' @export
parse_day <- function(x, input = c("file", "text", "lines"),
                      participant_id = NULL, source_file = NULL,
                      tz = "Asia/Shanghai", encoding = "auto",
                      strict = FALSE) {
  input <- match.arg(input)
  lines <- read_appusage_lines(x, input = input, encoding = encoding)
  mat <- as_text_matrix(lines)
  required_fields <- day_required_fields()
  optional_fields <- day_optional_fields()
  header_rows <- find_header_rows(
    mat,
    unname(required_fields)
  )
  diagnostics <- make_parser_diagnostics(
    export_type = "day",
    lines = lines,
    mat = mat,
    header_rows = header_rows,
    required_fields = required_fields,
    optional_fields = optional_fields
  )

  if (length(header_rows) == 0) {
    msg <- "No day-format APP Usage table header was found."
    if (strict) {
      stop(parser_error(msg, diagnostics, "appusage_missing_required_header"))
    }
    return(attach_parser_diagnostics(empty_day_tibble(), diagnostics))
  }

  source_file <- source_file %||% source_file_label(x, input)
  blocks <- collect_blocks(mat, header_rows)
  raw <- do.call(rbind, lapply(blocks, parse_day_block, mat = mat))
  out <- finalize_day_tibble(raw, participant_id, source_file, "day")
  diagnostics <- finalize_parser_diagnostics(
    diagnostics,
    raw,
    out,
    extras = day_format_diagnostics(blocks, raw, out)
  )

  attach_parser_diagnostics(out, diagnostics)
}

#' Parse app-format APP Usage exports
#'
#' Parses app-specific longitudinal daily APP Usage exports into the canonical
#' day-level schema where possible.
#'
#' @inheritParams parse_day
#'
#' @return A tibble with canonical day-level columns.
#' @export
parse_app <- function(x, input = c("file", "text", "lines"),
                      participant_id = NULL, source_file = NULL,
                      tz = "Asia/Shanghai", encoding = "auto",
                      strict = FALSE) {
  input <- match.arg(input)
  lines <- read_appusage_lines(x, input = input, encoding = encoding)
  mat <- as_text_matrix(lines)
  required_fields <- app_required_fields()
  header_rows <- find_header_rows(
    mat,
    unname(required_fields)
  )
  diagnostics <- make_parser_diagnostics(
    export_type = "app",
    lines = lines,
    mat = mat,
    header_rows = header_rows,
    required_fields = required_fields
  )

  if (length(header_rows) == 0) {
    msg <- "No app-format APP Usage table header was found."
    if (strict) {
      stop(parser_error(msg, diagnostics, "appusage_missing_required_header"))
    }
    return(attach_parser_diagnostics(empty_day_tibble(), diagnostics))
  }

  source_file <- source_file %||% source_file_label(x, input)
  blocks <- collect_blocks(mat, header_rows)
  raw <- do.call(rbind, lapply(blocks, parse_app_block, mat = mat))
  out <- finalize_day_tibble(raw, participant_id, source_file, "app")
  diagnostics <- finalize_parser_diagnostics(
    diagnostics,
    raw,
    out,
    extras = app_format_diagnostics(raw, out)
  )

  attach_parser_diagnostics(out, diagnostics)
}

day_required_fields <- function() {
  list(
    app_name = "\\u5e94\\u7528\\u540d\\u79f0",
    package_name = "\\u5e94\\u7528\\u6807\\u8bc6|\\u5e94\\u7528\\u5305\\u540d",
    duration_text = "\\u683c\\u5f0f\\u5316\\u65f6\\u95f4",
    duration_ms = "\\u4f7f\\u7528\\u65f6\\u957f\\uff08ms\\uff09",
    open_count = "\\u542f\\u52a8\\u6b21\\u6570",
    notification_count = "\\u901a\\u77e5\\u6b21\\u6570"
  )
}

day_optional_fields <- function() {
  list(
    split_screen_ms = "\\u5206\\u5c4f"
  )
}

app_required_fields <- function() {
  list(
    date = "\\u65e5\\u671f",
    duration_text = "\\u683c\\u5f0f\\u5316\\u65f6\\u95f4",
    duration_ms = "\\u4f7f\\u7528\\u65f6\\u957f\\uff08ms\\uff09",
    open_count = "\\u542f\\u52a8\\u6b21\\u6570",
    notification_count = "\\u901a\\u77e5\\u6b21\\u6570"
  )
}

day_format_diagnostics <- function(blocks, raw, out) {
  raw_duration <- if (is.data.frame(raw) && "duration_ms" %in% names(raw)) {
    as.character(raw$duration_ms)
  } else {
    character()
  }
  list(
    n_date_blocks = length(blocks),
    n_all_rows = if (is.data.frame(out)) sum(out$is_all_apps, na.rm = TRUE) else 0L,
    n_split_screen_rows = if (is.data.frame(out) && "split_screen_ms" %in% names(out)) {
      sum(!is.na(out$split_screen_ms))
    } else {
      0L
    },
    n_duration_ms_scientific_notation = sum(grepl("[Ee][+-]?[0-9]+", raw_duration), na.rm = TRUE),
    all_row_vs_sum_app_diff_ms = all_row_vs_sum_app_diff(out)$diff_ms,
    all_row_vs_sum_app_diff_pct = all_row_vs_sum_app_diff(out)$diff_pct
  )
}

app_format_diagnostics <- function(raw, out) {
  raw_duration <- if (is.data.frame(raw) && "duration_ms" %in% names(raw)) {
    as.character(raw$duration_ms)
  } else {
    character()
  }
  list(
    target_app_name = if (is.data.frame(out) && nrow(out) > 0) out$app_name[[1]] else NA_character_,
    target_package_name = if (is.data.frame(out) && nrow(out) > 0) out$package_name[[1]] else NA_character_,
    n_daily_rows = if (is.data.frame(out)) nrow(out) else 0L,
    n_scientific_duration_values = sum(grepl("[Ee][+-]?[0-9]+", raw_duration), na.rm = TRUE),
    contains_all_record = if (is.data.frame(out)) any(out$is_all_apps, na.rm = TRUE) else FALSE,
    is_app_specific_export = TRUE,
    should_not_infer_total_daily_use = if (is.data.frame(out)) !any(out$is_all_apps, na.rm = TRUE) else TRUE
  )
}

all_row_vs_sum_app_diff <- function(out) {
  if (!is.data.frame(out) || nrow(out) == 0 || !"is_all_apps" %in% names(out)) {
    return(list(diff_ms = NA_real_, diff_pct = NA_real_))
  }
  all_rows <- out[out$is_all_apps %in% TRUE, , drop = FALSE]
  app_rows <- out[is.na(out$is_all_apps) | !out$is_all_apps, , drop = FALSE]
  if (nrow(all_rows) == 0 || nrow(app_rows) == 0) {
    return(list(diff_ms = NA_real_, diff_pct = NA_real_))
  }
  all_total <- sum(all_rows$duration_ms, na.rm = TRUE)
  app_total <- sum(app_rows$duration_ms, na.rm = TRUE)
  diff_ms <- all_total - app_total
  list(
    diff_ms = diff_ms,
    diff_pct = if (all_total == 0) NA_real_ else diff_ms / all_total
  )
}

parse_day_block <- function(block, mat) {
  header <- mat[block$header_row, ]
  pos <- list(
    app_name = header_position(header, "^\\u5e94\\u7528\\u540d\\u79f0$"),
    package_name = header_position(header, "^\\u5e94\\u7528\\u6807\\u8bc6$|^\\u5e94\\u7528\\u5305\\u540d$"),
    duration_text = header_position(header, "^\\u683c\\u5f0f\\u5316\\u65f6\\u95f4$|^\\u4f7f\\u7528\\u65f6\\u957f$"),
    duration_ms = header_position(header, "^\\u4f7f\\u7528\\u65f6\\u957f\\uff08ms\\uff09$|^\\u603b\\u65f6\\u95f4\\uff08ms\\uff09$"),
    open_count = header_position(header, "^\\u542f\\u52a8\\u6b21\\u6570$"),
    notification_count = header_position(header, "^\\u901a\\u77e5\\u6b21\\u6570$")
  )
  split_pos <- header_position(header, "\\u5206\\u5c4f")
  if (is.na(split_pos) && !is.na(pos$notification_count)) {
    split_pos <- pos$notification_count + 1
  }

  date <- extract_row_date(header, previous_date(mat, block$header_row))
  data <- rows_for_block(mat, block)
  rows <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    if (!valid_data_row(row) ||
      row_contains_any(row, c("\\u5e94\\u7528\\u540d\\u79f0", "\\u4f7f\\u7528\\u65f6\\u957f\\uff08ms\\uff09"))) {
      next
    }
    rows[[i]] <- data.frame(
      date = extract_row_date(row, date),
      app_name = first_present(row, pos$app_name),
      package_name = first_present(row, pos$package_name),
      duration_text = first_present(row, pos$duration_text),
      duration_ms = first_present(row, pos$duration_ms),
      open_count = first_present(row, pos$open_count),
      notification_count = first_present(row, pos$notification_count),
      split_screen_ms = first_present(row, split_pos),
      parse_warning = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

parse_app_block <- function(block, mat) {
  header <- mat[block$header_row, ]
  date_pos <- header_position(header, "^\\u65e5\\u671f$")
  if (is.na(date_pos) || date_pos < 3) {
    return(empty_day_data_frame())
  }

  pos <- list(
    date = date_pos,
    duration_text = header_position(header, "^\\u683c\\u5f0f\\u5316\\u65f6\\u95f4$|^\\u4f7f\\u7528\\u65f6\\u957f$"),
    duration_ms = header_position(header, "^\\u4f7f\\u7528\\u65f6\\u957f\\uff08ms\\uff09$"),
    open_count = header_position(header, "^\\u542f\\u52a8\\u6b21\\u6570$"),
    notification_count = header_position(header, "^\\u901a\\u77e5\\u6b21\\u6570$")
  )
  app_name <- first_present(header, 1)
  package_name <- first_present(header, 2)
  data <- rows_for_block(mat, block)
  rows <- vector("list", nrow(data))

  for (i in seq_len(nrow(data))) {
    row <- data[i, ]
    if (!valid_data_row(row) ||
      row_contains_any(row, c("^\\u65e5\\u671f$", "\\u4f7f\\u7528\\u65f6\\u957f\\uff08ms\\uff09"))) {
      next
    }
    rows[[i]] <- data.frame(
      date = first_present(row, pos$date),
      app_name = app_name,
      package_name = package_name,
      duration_text = first_present(row, pos$duration_text),
      duration_ms = first_present(row, pos$duration_ms),
      open_count = first_present(row, pos$open_count),
      notification_count = first_present(row, pos$notification_count),
      split_screen_ms = NA_character_,
      parse_warning = NA_character_,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, rows[!vapply(rows, is.null, logical(1))])
}

finalize_day_tibble <- function(data, participant_id, source_file, export_type) {
  if (is.null(data) || nrow(data) == 0) {
    return(empty_day_tibble())
  }

  duration_ms <- parse_ms_value(data$duration_ms)
  package_name <- standardize_package_name(data$package_name)
  parse_warning <- data$parse_warning
  parse_warning[is.na(duration_ms)] <- append_warning(
    parse_warning[is.na(duration_ms)],
    "duration_ms could not be parsed"
  )

  tibble::tibble(
    participant_id = participant_id,
    source_file = source_file,
    export_type = export_type,
    date = safe_as_date(data$date),
    weekday = weekday_name(data$date),
    app_name = blank_to_na(data$app_name),
    package_name = package_name,
    duration_text = blank_to_na(data$duration_text),
    duration_ms = duration_ms,
    duration_min = duration_ms / 60000,
    open_count = parse_count(data$open_count),
    notification_count = parse_count(data$notification_count),
    split_screen_ms = parse_split_screen_ms(data$split_screen_ms),
    is_all_apps = package_name == "ALL" | blank_to_na(data$app_name) == "\\u6240\\u6709\\u5e94\\u7528",
    is_collection_app = package_name == "com.w.appusage",
    parse_warning = parse_warning
  )[order(safe_as_date(data$date), seq_len(nrow(data))), , drop = FALSE]
}

empty_day_data_frame <- function() {
  data.frame(
    date = character(),
    app_name = character(),
    package_name = character(),
    duration_text = character(),
    duration_ms = character(),
    open_count = character(),
    notification_count = character(),
    split_screen_ms = character(),
    parse_warning = character(),
    stringsAsFactors = FALSE
  )
}

empty_day_tibble <- function() {
  tibble::tibble(
    participant_id = character(),
    source_file = character(),
    export_type = character(),
    date = as.Date(character()),
    weekday = character(),
    app_name = character(),
    package_name = character(),
    duration_text = character(),
    duration_ms = numeric(),
    duration_min = numeric(),
    open_count = numeric(),
    notification_count = numeric(),
    split_screen_ms = numeric(),
    is_all_apps = logical(),
    is_collection_app = logical(),
    parse_warning = character()
  )
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
