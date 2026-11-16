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
#' @details
#' File reads return text with encoding diagnostics in attributes. Numeric
#' values are parsed later by [run_first_level_appusage()].
#' @seealso [appusage_demo], [appusage_synthetic_project]
#' @examples
#' data(appusage_demo)
#' folder <- system.file("extdata", "synth", package = "appusageR", mustWork = TRUE)
#' path <- file.path(folder, appusage_demo$sources$source_relpath[1L])
#' lines <- read_appusage_lines(path)
#' head(lines, 3)
#' stopifnot(is.character(lines), length(lines) > 0L)
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
      x <- appusage_text_paste(x, collapse = "\n")
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
#' @details
#' Vector length and missing values are retained. A leading byte-order mark
#' is removed after UTF-8 conversion. This text helper accepts decoded strings;
#' [read_appusage_lines()] handles file bytes and encoding detection.
#' @seealso [appusage_demo], [appusage_synthetic_project], [appusage_configuration]
#' @examples
#' data(appusage_demo)
#' names <- appusage_demo$records$line_seven_days$data$daily$app_name[1:2]
#' normalized <- normalize_encoding(paste0(intToUtf8(0xfeff), names), encoding = "UTF-8")
#' stopifnot(identical(normalized, names))
#' normalized
#' @export
normalize_encoding <- function(x, encoding = "auto") {
  x <- as.character(x)
  if (identical(encoding, "auto")) {
    x <- stringi::stri_enc_toutf8(x)
  } else {
    converted <- appusage_text_encode_skip(x, from = encoding)
    converted[is.na(converted)] <- x[is.na(converted)]
    x <- converted
  }
  appusage_text_remove(x, "^\ufeff")
}

decode_raw_text <- function(bytes, encoding = "auto",
                            detected_candidates = NULL,
                            available_encodings = appusage_text_encoding_names(),
                            converter = appusage_text_encode_strict) {
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
  candidates <- candidates[!is.na(candidates) & appusage_text_nzchar(candidates)]
  available_encodings <- unique(as.character(available_encodings))
  supported_index <- match(appusage_text_upper(candidates), appusage_text_upper(available_encodings))
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
        message = "ICU strict conversion returned NA",
        condition_class = "iconv_na"
      )
      next
    }
    selected <- supported[[i]]
    converted <- attempt[[1]]
    break
  }

  if (is.na(converted)) {
    converted <- stringi::stri_enc_toutf8(raw_text)
  }
  converted <- appusage_text_remove(converted, "^\ufeff")
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
  text <- appusage_text_str_replace_all(text, "\r\n?", "\n")
  lines <- appusage_text_split(text, "\n", fixed = TRUE)[[1]]
  appusage_text_remove(lines, "^\ufeff")
}

#' Parse millisecond values
#'
#' Parses APP Usage millisecond fields, including `"T:"` prefixes and
#' scientific notation such as `1.1447863E7`.
#'
#' @param x Values to parse.
#'
#' @return Numeric vector.
#' @details
#' Whitespace, commas and an initial `T:` are removed before numeric parsing.
#' Empty, missing and nonnumeric values become `NA_real_`. Values remain in
#' milliseconds; formatted duration labels are not interpreted as clock times.
#' @seealso [appusage_demo], [appusage_synthetic_project], [appusage_configuration]
#' @examples
#' data(appusage_demo)
#' ms <- appusage_demo$records$line_seven_days$data$daily$duration_ms[1:2]
#' text <- c(paste0("T:", ms), "", NA_character_)
#' parsed <- parse_ms_value(text)
#' stopifnot(identical(parsed[1:2], ms), all(is.na(parsed[3:4])))
#' parsed
#' @export
parse_ms_value <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trim_both(x)
  x <- appusage_text_remove(x, "^T:")
  x <- appusage_text_str_replace_all(x, ",", "")
  x[x %in% c("", "NA", "NULL", "null", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

#' Parse split-screen duration
#'
#' Extracts millisecond values from APP Usage split-screen labels such as
#' a millisecond count followed by the export's split-screen annotation.
#'
#' @param x Values to parse.
#'
#' @return Numeric milliseconds.
#' @details
#' The first unsigned numeric token, including scientific notation, is
#' returned in milliseconds. Missing or unrecognized labels return `NA_real_`.
#' This extraction helper does not add split-screen use to foreground totals.
#' @seealso [appusage_demo], [appusage_synthetic_project], [appusage_configuration]
#' @examples
#' data(appusage_demo)
#' ms <- appusage_demo$records$line_seven_days$data$daily$duration_ms[[1L]]
#' # Illustrate the annotation syntax using a duration from the teaching record.
#' parsed <- parse_split_screen_ms(c(paste(ms, "(split-screen)"), "", NA_character_))
#' stopifnot(parsed[[1L]] == ms, all(is.na(parsed[2:3])))
#' parsed
#' @export
parse_split_screen_ms <- function(x) {
  x <- as.character(x)
  value <- stringi::stri_extract_first_regex(x, "[0-9]+(?:\\.[0-9]+)?(?:[Ee][+-]?[0-9]+)?")
  parse_ms_value(value)
}

#' Safely parse APP Usage dates
#'
#' @param x Date values.
#'
#' @return Date vector. Missing or invalid dates are returned as `NA`.
#' @details
#' Extracts a year-month-day date from character input, accepting hyphens
#' or slashes. It returns one `Date` per input and retains invalid/missing
#' positions as `NA`. Timestamp-to-date conversion should first use the
#' intended timezone.
#' @seealso [appusage_demo], [appusage_synthetic_project], [appusage_configuration]
#' @examples
#' data(appusage_demo)
#' dates <- appusage_demo$records$line_seven_days$data$daily$date
#' parsed <- safe_as_date(gsub("-", "/", as.character(dates), fixed = TRUE))
#' stopifnot(identical(parsed, dates), is.na(safe_as_date("missing")))
#' head(parsed)
#' @export
safe_as_date <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_extract_first_regex(x, "[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}")
  x <- appusage_text_str_replace_all(x, "/", "-")
  suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
}

#' Add weekday labels
#'
#' @param data Data frame with a date column.
#' @param date_col Date column name.
#'
#' @return `data` with a `weekday` column.
#' @details
#' Returns a copy of the input with English weekday names in `weekday`.
#' Existing weekday values are replaced. Row order and all other columns
#' are retained; missing dates give missing weekday labels.
#' @seealso [appusage_demo], [appusage_synthetic_project], [appusage_configuration]
#' @examples
#' data(appusage_demo)
#' daily <- appusage_demo$records$line_seven_days$data$daily
#' labelled <- add_weekday(daily)
#' stopifnot(nrow(labelled) == nrow(daily),
#'           identical(labelled$duration_ms, daily$duration_ms),
#'           length(unique(labelled$weekday)) == 7L)
#' unique(labelled[, c("date", "weekday")])
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
#' @details
#' Whitespace is trimmed and package identifiers are lowercased. Blank and
#' textual missing markers become `NA_character_`. The total-row sentinel is
#' normalized to uppercase `ALL`; it is not an application identity.
#' @seealso [appusage_demo], [appusage_synthetic_project], [appusage_configuration]
#' @examples
#' data(appusage_demo)
#' packages <- appusage_demo$records$line_seven_days$data$daily$package_name[1:2]
#' normalized <- standardize_package_name(c(paste0(" ", toupper(packages), " "), "all", ""))
#' stopifnot(identical(normalized[1:2], packages), normalized[[3L]] == "ALL",
#'           is.na(normalized[[4L]]))
#' normalized
#' @export
standardize_package_name <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trim_both(x)
  x[x %in% c("", "NA", "NULL", "null")] <- NA_character_
  out <- stringi::stri_trans_tolower(x, locale = "en")
  out[!is.na(x) & stringi::stri_trans_toupper(x, locale = "en") == "ALL"] <- "ALL"
  out
}

#' Parse APP Usage timestamp values
#'
#' Removes the APP Usage `"T:"` prefix and parses millisecond timestamps.
#'
#' @param x Timestamp values.
#'
#' @return Numeric milliseconds since the Unix epoch.
#' @details
#' Returns numeric Unix-epoch milliseconds. Divide by 1000 when constructing
#' a `POSIXct` value with an explicit epoch origin and timezone; the helper
#' itself leaves the authoritative millisecond values numeric.
#' @seealso [appusage_demo], [appusage_synthetic_project], [appusage_configuration]
#' @examples
#' data(appusage_demo)
#' ms <- appusage_demo$records$line_seven_days$data$episode$start_ts_ms[1:2]
#' parsed <- parse_t_timestamp(paste0("T:", ms))
#' stopifnot(identical(parsed, ms))
#' as.POSIXct(parsed / 1000, origin = "1970-01-01", tz = appusage_demo$info$timezone)
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
#' @details
#' Input strings represent wall-clock time in `tz`. Both a decimal second
#' separator and the export's trailing `:mmm` milliseconds are supported.
#' The return value is `POSIXct`; invalid or missing values become `NA`.
#' @seealso [appusage_demo], [appusage_synthetic_project], [appusage_configuration]
#' @examples
#' data(appusage_demo)
#' times <- safe_as_datetime(appusage_demo$survey$submitted_at,
#'                           tz = appusage_demo$info$timezone)
#' stopifnot(inherits(times, "POSIXct"), length(times) == 17L,
#'           all(format(times, "%H:%M:%S", tz = appusage_demo$info$timezone) == "11:00:00"))
#' head(times)
#' @export
safe_as_datetime <- function(x, tz = "Asia/Shanghai") {
  tz <- appusage_resolve_timezone(tz)
  x <- as.character(x)
  x[x %in% c("", "NA", "NULL", "null")] <- NA_character_
  x <- appusage_text_str_replace(x, ":(\\d{3})$", ".\\1")
  suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%OS", tz = tz))
}

#' Label Android UsageEvents event types
#'
#' @param x Numeric event type.
#'
#' @return Character labels. Unknown values are preserved as
#'   `EVENT_TYPE_<number>`.
#' @details
#' Returns labels parallel to the numeric Android UsageEvents codes.
#' Code 1 denotes activity resumption/foreground transition and code 2 denotes
#' pause/background transition. Labels describe events; duration reconstruction
#' is handled separately by [reconstruct_meta_episodes()].
#' @seealso [appusage_demo], [appusage_synthetic_project], [appusage_configuration]
#' @examples
#' data(appusage_demo)
#' events <- appusage_demo$records$meta_seven_days$data$event
#' labels <- label_event_type(events$event_type)
#' stopifnot(length(labels) == 28L, !anyNA(labels))
#' unique(data.frame(event_type = events$event_type, label = labels))
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
  unknown <- is.na(out) & !is.na(x_chr) & appusage_text_nzchar(x_chr)
  out[unknown] <- appusage_text_paste0("EVENT_TYPE_", x_chr[unknown])
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
  x <- stringi::stri_trim_both(x)
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
  ms_to_datetime_validated(x, appusage_resolve_timezone(tz))
}

ms_to_datetime_validated <- function(x, tz) {
  ms <- parse_t_timestamp(x)
  as.POSIXct(ms / 1000, origin = "1970-01-01", tz = tz)
}
