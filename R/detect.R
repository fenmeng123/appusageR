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
  preflight <- appusage_source_preflight(
    lines,
    input = "lines",
    encoding = encoding,
    filename_type = NA_character_
  )
  if (identical(preflight$status, "ok") &&
    is_present_string(preflight$selected_component)) {
    preflight$selected_component
  } else {
    "unknown"
  }
}

appusage_detect_components_from_lines <- function(lines) {
  mat <- appusage_parse_context(lines)
  boundaries <- appusage_structural_boundaries(mat)
  intersect(c("meta", "line", "app", "day"), boundaries$component)
}
