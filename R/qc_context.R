# A call-local cache. Never attach this environment to published QC or data.
appusage_qc_context <- function(grains, config) {
  context <- new.env(parent = emptyenv())
  context$grains <- grains
  context$config <- config
  context$columns <- new.env(parent = emptyenv())
  context$dates <- new.env(parent = emptyenv())
  context$views <- new.env(parent = emptyenv())
  context
}

appusage_qc_col <- function(x, col, kind, context = NULL,
                            grain = "episode", rows = NULL) {
  getter <- switch(kind, num = appusage_num_col, chr = appusage_chr_col,
    lgl = appusage_lgl_col)
  if (is.null(context)) return(getter(x, col))
  key <- stringi::stri_join(grain, kind, col, sep = "\r")
  if (!exists(key, context$columns, inherits = FALSE)) {
    context$columns[[key]] <- getter(context$grains[[grain]], col)
  }
  value <- context$columns[[key]]
  if (is.null(rows)) value else value[rows]
}

appusage_qc_date <- function(x, col, context = NULL, grain = "episode",
                             rows = NULL, tz = "Asia/Shanghai", ms = FALSE,
                             offset = 0) {
  if (is.null(context)) {
    if (ms) return(appusage_date_from_ms(appusage_num_col(x, col) + offset, tz))
    return(appusage_date_col(x, col, tz))
  }
  # Endpoint offset is part of the key: end and end-minus-0.001 differ at midnight.
  key <- stringi::stri_join(grain, col, tz, ms, offset, sep = "\r")
  if (!exists(key, context$dates, inherits = FALSE)) {
    context$dates[[key]] <- if (ms) {
      value <- appusage_qc_col(x, col, "num", context, grain) + offset
      appusage_date_from_datetime_validated(ms = value, tz = tz)
    } else {
      appusage_date_col(context$grains[[grain]], col, tz)
    }
  }
  value <- context$dates[[key]]
  if (is.null(rows)) value else value[rows]
}

appusage_qc_episode_view <- function(episode, kind, context = NULL) {
  if (!is.null(context) && exists(kind, context$views, inherits = FALSE)) {
    return(context$views[[kind]])
  }
  source <- appusage_qc_col(episode, "episode_source", "chr", context)
  rows <- if (kind == "line") {
    export <- appusage_qc_col(episode, "source_export_type", "chr", context)
    which(export %in% "line" | source %in% "line")
  } else {
    which(!is.na(source) & source == "meta_events")
  }
  view <- list(rows = rows, data = episode[rows, , drop = FALSE])
  if (!is.null(context)) context$views[[kind]] <- view
  view
}

appusage_qc_valid_intervals <- function(start, end, duration, context = NULL,
                                        rows = NULL) {
  if (is.null(context)) {
    return(!is.na(start) & !is.na(end) & !is.na(duration) & end >= start & duration >= 0)
  }
  if (is.null(context$valid_intervals)) {
    x <- context$grains$episode
    a <- appusage_qc_col(x, "start_ts_ms", "num", context)
    b <- appusage_qc_col(x, "end_ts_ms", "num", context)
    d <- appusage_qc_col(x, "duration_ms", "num", context)
    context$valid_intervals <- !is.na(a) & !is.na(b) & !is.na(d) & b >= a & d >= 0
  }
  if (is.null(rows)) context$valid_intervals else context$valid_intervals[rows]
}
