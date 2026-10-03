# Private, file-local storage. Views share this environment but only the cache is
# mutable. Neither the environment nor its views are attached to output data.
appusage_parse_context <- function(lines) {
  if (inherits(lines, "appusage_parse_context")) return(lines)
  pieces <- appusage_text_split(lines, ",", fixed = TRUE)
  sizes <- lengths(pieces)
  values <- stringi::stri_trim_both(unlist(pieces, use.names = FALSE))
  values[values == ""] <- NA_character_
  columns <- sequence(sizes)
  width <- max(sizes, 1L)
  present <- !is.na(values)
  leading <- if (any(present)) min(columns[present]) - 1L else width
  store <- new.env(parent = emptyenv())
  store$lines <- lines
  store$values <- values
  store$sizes <- sizes
  store$offsets <- c(0, cumsum(sizes))
  store$leading <- as.integer(leading)
  store$width <- as.integer(width - leading)
  store$cache <- new.env(parent = emptyenv())
  text_values <- values
  text_values[is.na(text_values)] <- "NA"
  row_text <- rep("", length(pieces))
  nonzero <- sizes > 0L
  row_text[nonzero] <- stringi::stri_join_list(utils::relist(text_values, pieces)[nonzero], sep = "\n")
  if (leading > 0L) {
    row_text <- stringi::stri_replace_first_regex(row_text,
      appusage_text_paste0("^(?:NA\\n){", leading, "}"), "")
    row_text[sizes <= leading] <- ""
  }
  padding <- sizes < width & store$width > 0L
  nonempty <- padding & row_text != ""
  row_text[nonempty] <- stringi::stri_join(row_text[nonempty], "\nNA")
  row_text[padding & !nonempty] <- "NA"
  store$cache$row_text <- row_text
  structure(list(store = store, rows = seq_along(lines)),
    class = "appusage_parse_context",
    n_leading_empty_columns = as.integer(leading))
}

#' @export
dim.appusage_parse_context <- function(x) c(length(x$rows), x$store$width)

#' @export
`[.appusage_parse_context` <- function(x, i, j, drop = TRUE) {
  if (!missing(i)) x$rows <- x$rows[i]
  if (missing(j) && !drop) return(x)
  columns <- if (missing(j)) seq_len(x$store$width) else seq_len(x$store$width)[j]
  out <- matrix(NA_character_, length(x$rows), length(columns))
  for (k in seq_along(columns)) out[, k] <- appusage_context_column(x, columns[[k]])
  if (drop) drop(out) else out
}

appusage_context_column <- function(x, position) {
  if (!inherits(x, "appusage_parse_context")) {
    if (is.na(position) || position < 1L || position > ncol(x)) return(rep(NA_character_, nrow(x)))
    return(blank_to_na(x[, position]))
  }
  out <- rep(NA_character_, length(x$rows))
  if (is.na(position) || position < 1L || position > x$store$width) return(out)
  position <- position + x$store$leading
  ok <- x$store$sizes[x$rows] >= position
  out[ok] <- x$store$values[x$store$offsets[x$rows[ok]] + position]
  out
}

appusage_context_row_text <- function(x) {
  if (!inherits(x, "appusage_parse_context")) {
    if (!ncol(x)) return(rep("", nrow(x)))
    return(do.call(appusage_text_paste, c(lapply(seq_len(ncol(x)), function(j) x[, j]), list(sep = "\n"))))
  }
  s <- x$store
  s$cache$row_text[x$rows]
}

appusage_context_all_text <- function(x) {
  if (!inherits(x, "appusage_parse_context")) return(appusage_text_paste(x, collapse = "\n"))
  if (is.null(x$store$cache$all_text)) x$store$cache$all_text <- appusage_text_paste(x$store$values, collapse = "\n")
  x$store$cache$all_text
}

appusage_context_hits <- function(x, pattern) {
  if (!inherits(x, "appusage_parse_context")) return(appusage_text_detect(appusage_context_row_text(x), pattern))
  key <- appusage_text_paste0("pattern:", pattern)
  cache <- x$store$cache
  if (!exists(key, envir = cache, inherits = FALSE)) {
    all <- x
    all$rows <- seq_along(x$store$lines)
    assign(key, appusage_text_detect(appusage_context_row_text(all), pattern), envir = cache)
  }
  get(key, envir = cache, inherits = FALSE)[x$rows]
}

appusage_context_match <- function(x, patterns, all = TRUE) {
  hit <- rep(all, nrow(x))
  for (pattern in patterns) {
    found <- appusage_context_hits(x, pattern)
    hit <- if (all) hit & found else hit | found
  }
  hit
}

appusage_context_dates <- function(x, fallback = NA_character_) {
  if (inherits(x, "appusage_parse_context")) {
    if (is.null(x$store$cache$date_values)) x$store$cache$date_values <- stringi::stri_extract_first_regex(x$store$cache$row_text, "[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}")
    dates <- x$store$cache$date_values[x$rows]
  } else dates <- stringi::stri_extract_first_regex(appusage_context_row_text(x), "[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}")
  ifelse(is.na(dates), fallback, dates)
}

appusage_context_previous_date <- function(x, row_index) {
  if (row_index <= 1L) return(NA_character_)
  s <- x$store
  if (is.null(s$cache$dates)) {
    all <- x; all$rows <- seq_along(s$lines)
    s$cache$dates <- appusage_context_dates(all)
    s$cache$date_rows <- which(!is.na(s$cache$dates))
  }
  index <- findInterval(x$rows[[row_index]] - 1L, s$cache$date_rows)
  if (!index) NA_character_ else s$cache$dates[s$cache$date_rows[[index]]]
}

appusage_context_valid <- function(x) {
  if (!inherits(x, "appusage_parse_context")) return(rowSums(!is.na(x) & x != "") > 0L)
  s <- x$store
  if (is.null(s$cache$nonempty)) {
    row <- rep.int(seq_along(s$sizes), s$sizes)
    s$cache$nonempty <- tabulate(row[!is.na(s$values)], length(s$sizes)) > 0L
  }
  s$cache$nonempty[x$rows]
}

appusage_context_records <- function(x) {
  s <- x$store
  if (is.null(s$cache$candidate)) {
    row <- rep.int(seq_along(s$sizes), s$sizes)
    cells <- s$values
    keep <- !is.na(cells) & appusage_text_nzchar(cells)
    cells <- cells[keep]; row <- row[keep]
    counts <- tabulate(row, length(s$sizes))
    package <- appusage_text_grepl("^(?:[A-Za-z][A-Za-z0-9_-]*[.])+[A-Za-z0-9_.-]+$|^ALL$", cells)
    timed <- appusage_text_grepl("^T:[0-9]{6,}$|^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}|^[0-9]+(?:[.][0-9]+)?(?:[Ee][+-]?[0-9]+)?$", cells)
    tokens <- appusage_text_grepl(appusage_text_paste(c("\u5f00\u59cb\u65f6\u95f4", "\u7ed3\u675f\u65f6\u95f4", "\u5e94\u7528\u540d\u79f0", "\u5e94\u7528\u6807\u8bc6", "\u683c\u5f0f\u5316\u65f6\u95f4", "\u4f7f\u7528\u65f6\u957f", "\u542f\u52a8\u6b21\u6570", "\u901a\u77e5\u6b21\u6570", "\u5177\u4f53\u9875\u9762", "\u65f6\u95f4\u6233", "\u914d\u7f6e"), collapse = "|"), cells)
    has_package <- tabulate(row[package], length(counts)) > 0L
    has_time <- tabulate(row[timed], length(counts)) > 0L
    headers <- tabulate(row[tokens], length(counts))
    s$cache$candidate <- counts >= 2L & ((has_package & has_time) | (has_time & counts >= 3L & headers < 2L))
  }
  which(s$cache$candidate[x$rows])
}

appusage_context_input <- function(x, input, encoding) {
  if (inherits(x, "appusage_parse_context")) x else appusage_parse_context(read_appusage_lines(x, input = input, encoding = encoding))
}

appusage_prepare_source <- function(...) {
  sink <- new.env(parent = emptyenv())
  preflight <- appusage_source_preflight(..., .context_sink = sink)
  list(preflight = preflight, context = sink$context)
}
