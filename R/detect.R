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
  mat <- as_text_matrix(lines)
  text <- paste(mat, collapse = "\n")

  is_meta <- all(stringr::str_detect(text, c("\\u8868\\u4e00", "\\u8868\\u4e8c"))) &&
    all(stringr::str_detect(text, c("\\u5177\\u4f53\\u9875\\u9762", "\\u65f6\\u95f4\\u6233", "\\u7c7b\\u578b", "\\u914d\\u7f6e")))
  if (is_meta) {
    return("meta")
  }

  is_line <- all(stringr::str_detect(
    text,
    c("\\u5f00\\u59cb\\u65f6\\u95f4\\uff08ms\\uff09", "\\u5f00\\u59cb\\u65f6\\u95f4", "\\u5e94\\u7528\\u540d\\u79f0", "\\u5e94\\u7528\\u6807\\u8bc6", "\\u4f7f\\u7528\\u65f6\\u957f", "\\u7ed3\\u675f\\u65f6\\u95f4\\uff08ms\\uff09", "\\u7ed3\\u675f\\u65f6\\u95f4")
  ))
  if (is_line) {
    return("line")
  }

  app_header_rows <- find_header_rows(
    mat,
    c("\\u65e5\\u671f", "\\u683c\\u5f0f\\u5316\\u65f6\\u95f4", "\\u4f7f\\u7528\\u65f6\\u957f\\uff08ms\\uff09", "\\u542f\\u52a8\\u6b21\\u6570", "\\u901a\\u77e5\\u6b21\\u6570")
  )
  if (length(app_header_rows) > 0) {
    first_header <- mat[app_header_rows[[1]], ]
    date_pos <- which(stringr::str_detect(first_header, "^\\u65e5\\u671f$"))
    has_app_prefix <- length(date_pos) > 0 &&
      date_pos[[1]] >= 3 &&
      all(!is.na(first_header[seq_len(date_pos[[1]] - 1)]))
    if (has_app_prefix) {
      return("app")
    }
  }

  is_day <- all(stringr::str_detect(
    text,
    c("\\u5e94\\u7528\\u540d\\u79f0", "\\u5e94\\u7528\\u6807\\u8bc6", "\\u683c\\u5f0f\\u5316\\u65f6\\u95f4", "\\u4f7f\\u7528\\u65f6\\u957f\\uff08ms\\uff09", "\\u542f\\u52a8\\u6b21\\u6570", "\\u901a\\u77e5\\u6b21\\u6570")
  ))
  if (is_day) {
    return("day")
  }

  "unknown"
}
