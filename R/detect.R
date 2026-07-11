#' Detect APP Usage export type
#'
#' Detects whether APP Usage export content is `line`, `meta`, `day`, `app`, or
#' `unknown`.
#'
#' @param x File path, raw text, or character vector of lines.
#' @param input One of `"file"`, `"text"`, or `"lines"`.
#' @param encoding Source encoding. Use `"auto"` for common UTF-8 and Chinese
#'   encodings.
#'
#' @return A single character value: `"line"`, `"meta"`, `"day"`, `"app"`, or
#'   `"unknown"`.
#' @export
detect_appusage_type <- function(x, input = c("file", "text", "lines"),
                                 encoding = "auto") {
  input <- match.arg(input)
  lines <- read_appusage_lines(x, input = input, encoding = encoding)
  components <- appusage_detect_components_from_lines(lines)
  priority <- c("meta", "line", "app", "day")
  selected <- priority[priority %in% components]
  if (length(selected) > 0L) selected[[1]] else "unknown"
}

appusage_detect_components_from_lines <- function(lines) {
  mat <- as_text_matrix(lines)
  text <- paste(mat, collapse = "\n")
  components <- character()

  is_meta <- all(stringr::str_detect(text, c("\u8868\u4e00", "\u8868\u4e8c"))) &&
    all(stringr::str_detect(
      text,
      c("\u5177\u4f53\u9875\u9762", "\u65f6\u95f4\u6233", "\u7c7b\u578b", "\u914d\u7f6e")
    ))
  if (is_meta) components <- c(components, "meta")

  is_line <- all(stringr::str_detect(
    text,
    c(
      "\u5f00\u59cb\u65f6\u95f4\uff08ms\uff09", "\u5f00\u59cb\u65f6\u95f4",
      "\u5e94\u7528\u540d\u79f0", "\u5e94\u7528\u6807\u8bc6", "\u4f7f\u7528\u65f6\u957f",
      "\u7ed3\u675f\u65f6\u95f4\uff08ms\uff09", "\u7ed3\u675f\u65f6\u95f4"
    )
  ))
  if (is_line) components <- c(components, "line")

  app_header_rows <- find_header_rows(
    mat,
    c(
      "\u65e5\u671f", "\u683c\u5f0f\u5316\u65f6\u95f4",
      "\u4f7f\u7528\u65f6\u957f\uff08ms\uff09", "\u542f\u52a8\u6b21\u6570",
      "\u901a\u77e5\u6b21\u6570"
    )
  )
  if (length(app_header_rows) > 0L) {
    is_app <- any(vapply(app_header_rows, function(i) {
      header <- mat[i, ]
      date_pos <- which(stringr::str_detect(header, "^\u65e5\u671f$"))
      length(date_pos) > 0L && date_pos[[1]] >= 3L &&
        all(!is.na(header[seq_len(date_pos[[1]] - 1L)]))
    }, logical(1)))
    if (is_app) components <- c(components, "app")
  }

  day_tokens <- c(
    "\u5e94\u7528\u540d\u79f0", "\u5e94\u7528\u6807\u8bc6",
    "\u683c\u5f0f\u5316\u65f6\u95f4", "\u4f7f\u7528\u65f6\u957f\uff08ms\uff09",
    "\u542f\u52a8\u6b21\u6570", "\u901a\u77e5\u6b21\u6570"
  )
  is_day <- any(vapply(seq_len(nrow(mat)), function(i) {
    row_text <- paste(stats::na.omit(mat[i, ]), collapse = "\t")
    first <- mat[i, 1]
    is_present_string(first) && !is.na(safe_as_date(first)) &&
      all(stringr::str_detect(row_text, day_tokens))
  }, logical(1)))
  if (is_day) components <- c(components, "day")

  unique(components)
}
