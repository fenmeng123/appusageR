#' Read APP Usage export lines
#'
#' Reads APP Usage export content from a file, raw text string, or character
#' vector of lines. File input supports UTF-8, UTF-8 with BOM, GB18030, and GBK
#' through automatic encoding detection.
#'
#' @param x File path, raw text, or lines.
#' @param input One of `"file"`, `"text"`, or `"lines"`.
#' @param encoding Source encoding. Use `"auto"` to try common encodings.
#'
#' @return A UTF-8 character vector of lines.
#' @export
read_appusage_lines <- function(x, input = c("file", "text", "lines"),
                                encoding = "auto") {
  input <- match.arg(input)

  if (input == "file") {
    if (length(x) != 1 || !file.exists(x)) {
      cli::cli_abort("APP Usage file does not exist: {.path {x}}")
    }
    bytes <- readBin(x, what = "raw", n = file.info(x)$size)
    bytes <- bytes[bytes != as.raw(0)]
    text <- decode_raw_text(bytes, encoding = encoding)
    lines <- split_lines(text)
    attr(lines, "encoding_diagnostics") <- attr(text, "encoding_diagnostics")
    return(lines)
  }

  if (input == "text") {
    if (length(x) != 1) {
      x <- paste(x, collapse = "\n")
    }
    return(split_lines(normalize_encoding(x, encoding = encoding)))
  }

  normalize_encoding(as.character(x), encoding = encoding)
}

#' Normalize text encoding
#'
#' Converts character data to UTF-8. File input should usually be handled by
#' `read_appusage_lines()`.
#'
#' @param x Character vector.
#' @param encoding Source encoding or `"auto"`.
#'
#' @return A UTF-8 character vector.
#' @export
normalize_encoding <- function(x, encoding = "auto") {
  x <- as.character(x)
  if (identical(encoding, "auto")) {
    x <- enc2utf8(x)
  } else {
    converted <- iconv(x, from = encoding, to = "UTF-8", sub = "")
    converted[is.na(converted)] <- x[is.na(converted)]
    x <- converted
  }
  stringr::str_remove(x, "^\ufeff")
}

decode_raw_text <- function(bytes, encoding = "auto",
                            detected_candidates = NULL,
                            available_encodings = iconvlist(),
                            converter = iconv) {
  detected <- if (identical(encoding, "auto")) {
    detected_candidates %||% tryCatch(
      stringi::stri_enc_detect(bytes)[[1]]$Encoding,
      error = function(e) character()
    )
  } else {
    encoding
  }
  candidates <- if (identical(encoding, "auto")) {
    unique(c(detected, "UTF-8", "GB18030", "GBK", "CP936"))
  } else {
    unique(as.character(detected))
  }
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  available_encodings <- unique(as.character(available_encodings))
  supported_index <- match(toupper(candidates), toupper(available_encodings))
  supported <- candidates[!is.na(supported_index)]
  unsupported <- candidates[is.na(supported_index)]
  canonical <- available_encodings[stats::na.omit(supported_index)]
  conversion_failures <- list()
  raw_text <- rawToChar(bytes)
  selected <- NA_character_
  converted <- NA_character_

  for (i in seq_along(supported)) {
    attempt <- tryCatch(
      suppressWarnings(converter(raw_text, from = canonical[[i]], to = "UTF-8")),
      error = function(e) e
    )
    if (inherits(attempt, "condition")) {
      conversion_failures[[length(conversion_failures) + 1L]] <- list(
        candidate = supported[[i]],
        message = conditionMessage(attempt),
        condition_class = class(attempt)
      )
      next
    }
    if (length(attempt) == 0L || is.na(attempt[[1]])) {
      conversion_failures[[length(conversion_failures) + 1L]] <- list(
        candidate = supported[[i]],
        message = "iconv returned NA",
        condition_class = "iconv_na"
      )
      next
    }
    selected <- supported[[i]]
    converted <- attempt[[1]]
    break
  }

  if (is.na(converted)) {
    converted <- enc2utf8(raw_text)
  }
  converted <- stringr::str_remove(converted, "^\ufeff")
  attr(converted, "encoding_diagnostics") <- list(
    attempted_candidates = candidates,
    supported_candidates = supported,
    unsupported_candidates = unsupported,
    selected_encoding = selected,
    conversion_failures = conversion_failures,
    fallback_used = is.na(selected)
  )
  converted
}

split_lines <- function(text) {
  text <- stringr::str_replace_all(text, "\r\n?", "\n")
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  stringr::str_remove(lines, "^\ufeff")
}

#' Parse millisecond values
#'
#' Parses APP Usage millisecond fields, including `"T:"` prefixes and
#' scientific notation such as `1.1447863E7`.
#'
#' @param x Values to parse.
#'
#' @return Numeric vector.
#' @export
parse_ms_value <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x <- stringr::str_remove(x, "^T:")
  x <- stringr::str_replace_all(x, ",", "")
  x[x %in% c("", "NA", "NULL", "null", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

#' Parse split-screen duration
#'
#' Extracts millisecond values from APP Usage split-screen labels such as
#' `"38541（分屏时长）"`.
#'
#' @param x Values to parse.
#'
#' @return Numeric milliseconds.
#' @export
parse_split_screen_ms <- function(x) {
  x <- as.character(x)
  value <- stringr::str_extract(x, "[0-9]+(?:\\.[0-9]+)?(?:[Ee][+-]?[0-9]+)?")
  parse_ms_value(value)
}

#' Safely parse APP Usage dates
#'
#' @param x Date values.
#'
#' @return Date vector.
#' @export
safe_as_date <- function(x) {
  x <- as.character(x)
  x <- stringr::str_extract(x, "[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}")
  x <- stringr::str_replace_all(x, "/", "-")
  suppressWarnings(as.Date(x))
}

#' Add weekday labels
#'
#' @param data Data frame with a date column.
#' @param date_col Date column name.
#'
#' @return `data` with a `weekday` column.
#' @export
add_weekday <- function(data, date_col = "date") {
  data$weekday <- weekday_name(data[[date_col]])
  data
}

#' Standardize Android package names
#'
#' @param x Package names.
#'
#' @return Lowercase trimmed package names. The artificial `ALL` package is
#'   standardized to `"ALL"`.
#' @export
standardize_package_name <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x[x %in% c("", "NA", "NULL", "null")] <- NA_character_
  out <- stringr::str_to_lower(x)
  out[!is.na(x) & stringr::str_to_upper(x) == "ALL"] <- "ALL"
  out
}

#' Parse APP Usage timestamp values
#'
#' Removes the APP Usage `"T:"` prefix and parses millisecond timestamps.
#'
#' @param x Timestamp values.
#'
#' @return Numeric milliseconds since the Unix epoch.
#' @export
parse_t_timestamp <- function(x) {
  parse_ms_value(x)
}

#' Safely parse APP Usage datetimes
#'
#' @param x Datetime values such as `"2023-11-12 08:43:46:870"`.
#' @param tz Time zone.
#'
#' @return POSIXct vector.
#' @export
safe_as_datetime <- function(x, tz = "Asia/Shanghai") {
  tz <- appusage_resolve_timezone(tz)
  x <- as.character(x)
  x[x %in% c("", "NA", "NULL", "null")] <- NA_character_
  x <- stringr::str_replace(x, ":(\\d{3})$", ".\\1")
  suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS", tz = tz))
}

#' Label Android UsageEvents event types
#'
#' @param x Numeric event type.
#'
#' @return Character labels. Unknown values are preserved as
#'   `EVENT_TYPE_<number>`.
#' @export
label_event_type <- function(x) {
  x_chr <- as.character(x)
  labels <- c(
    "0" = "NONE",
    "1" = "ACTIVITY_RESUMED_OR_MOVE_TO_FOREGROUND",
    "2" = "ACTIVITY_PAUSED_OR_MOVE_TO_BACKGROUND",
    "3" = "END_OF_DAY",
    "4" = "CONTINUE_PREVIOUS_DAY",
    "5" = "CONFIGURATION_CHANGE",
    "6" = "SYSTEM_INTERACTION",
    "7" = "USER_INTERACTION",
    "8" = "SHORTCUT_INVOCATION",
    "9" = "CHOOSER_ACTION",
    "10" = "NOTIFICATION_SEEN",
    "11" = "STANDBY_BUCKET_CHANGED",
    "12" = "NOTIFICATION_INTERRUPTION",
    "13" = "SLICE_PINNED_PRIV",
    "14" = "SLICE_PINNED",
    "15" = "SCREEN_INTERACTIVE",
    "16" = "SCREEN_NON_INTERACTIVE",
    "17" = "KEYGUARD_SHOWN",
    "18" = "KEYGUARD_HIDDEN",
    "19" = "FOREGROUND_SERVICE_START",
    "20" = "FOREGROUND_SERVICE_STOP",
    "21" = "CONTINUING_FOREGROUND_SERVICE",
    "22" = "ROLLOVER_FOREGROUND_SERVICE",
    "23" = "ACTIVITY_STOPPED",
    "24" = "ACTIVITY_DESTROYED",
    "25" = "FLUSH_TO_DISK",
    "26" = "DEVICE_SHUTDOWN",
    "27" = "DEVICE_STARTUP",
    "28" = "USER_UNLOCKED",
    "29" = "USER_STOPPED",
    "30" = "LOCUS_ID_SET",
    "31" = "APP_COMPONENT_USED"
  )

  out <- unname(labels[x_chr])
  unknown <- is.na(out) & !is.na(x_chr) & nzchar(x_chr)
  out[unknown] <- paste0("EVENT_TYPE_", x_chr[unknown])
  out[is.na(x_chr) | x_chr == ""] <- NA_character_
  out
}

weekday_name <- function(x) {
  date <- safe_as_date(x)
  out <- rep(NA_character_, length(date))
  valid <- !is.na(date)
  out[valid] <- c(
    "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
  )[as.POSIXlt(date[valid])$wday + 1]
  out
}

blank_to_na <- function(x) {
  x <- as.character(x)
  x <- stringr::str_trim(x)
  x[x %in% c("", "NA", "NULL", "null")] <- NA_character_
  x
}

source_file_label <- function(x, input) {
  if (input == "file") {
    normalizePath(x, winslash = "/", mustWork = FALSE)
  } else {
    NA_character_
  }
}

ms_to_datetime <- function(x, tz = "Asia/Shanghai") {
  tz <- appusage_resolve_timezone(tz)
  ms <- parse_t_timestamp(x)
  as.POSIXct(ms / 1000, origin = "1970-01-01", tz = tz)
}
