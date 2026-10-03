from implement_structures_035 import read,write,get,replace,ROOT

def main():
    text=read('second_level.R')
    f=get(text,'reconstruct_meta_episodes')
    start=f.index('  rows <- list()'); end=f.index('  out <- add_duration_anomalies',start)
    f=f[:start]+'''  pairs <- appusage_meta_pair_indices(events, start_event_types, end_event_types)
  diagnostics[names(pairs$diagnostics)] <- pairs$diagnostics
  if (!length(pairs$from)) {
    out <- empty_second_episode_tibble()
    return(attach_meta_reconstruction_diagnostics(out,
      finalize_meta_reconstruction_diagnostics(diagnostics, out)))
  }
  out <- appusage_meta_rows_from_indices(events, pairs, pairing, tz)
'''+f[end:]
    text=replace(text,'reconstruct_meta_episodes',f)
    f=get(text,'clip_overlapping_meta_timeline')
    start=f.index('  previous <- ordered[[1]]'); end=f.index('  episodes <- conform_second_episode(episodes[order(',start)
    f=f[:start]+'''  previous <- utils::head(ordered, -1L)
  current <- ordered[-1L]
  clipped <- episodes$start_ts_ms[current] < episodes$end_ts_ms[previous]
  index <- previous[clipped]
  new_end <- episodes$start_ts_ms[current[clipped]]
  if (length(index)) {
    amounts <- episodes$end_ts_ms[index] - new_end
    episodes$end_ts_ms[index] <- new_end
    episodes$end_datetime[index] <- meta_episode_datetime_from_ms(new_end, episodes$end_datetime[index])
    duration <- new_end - episodes$start_ts_ms[index]
    episodes$duration_ms[index] <- duration
    episodes$duration_min[index] <- duration / 60000
    episodes$duration_text[index] <- NA_character_
    flag <- seq_len(nrow(episodes)) %in% index
    episodes$reconstruction_warning <- append_reconstruction_warning(episodes$reconstruction_warning, flag, "timeline_clipped")
    episodes$anomaly_reason <- append_reconstruction_warning(episodes$anomaly_reason, flag, "timeline_clipped")
    episodes$anomaly_any[index] <- TRUE
    invalid <- index[is.na(duration) | duration <= 0]
    if (length(invalid)) {
      episodes$reconstruction_status[invalid] <- "invalid_pair"
      episodes$duration_ms[invalid] <- NA_real_
      episodes$duration_min[invalid] <- NA_real_
      flag <- seq_len(nrow(episodes)) %in% invalid
      episodes$reconstruction_warning <- append_reconstruction_warning(episodes$reconstruction_warning, flag, "timeline_clipped_to_nonpositive")
      episodes$anomaly_reason <- append_reconstruction_warning(episodes$anomaly_reason, flag, "timeline_clipped_to_nonpositive")
    }
    diagnostics$n_timeline_clipped_episodes <- length(index)
    diagnostics$total_timeline_clipped_ms <- Reduce(`+`, amounts, init = 0)
    diagnostics$n_timeline_clipped_to_nonpositive <- length(invalid)
  }
'''+f[end:]
    text=replace(text,'clip_overlapping_meta_timeline',f)
    f=get(text,'merge_contiguous_meta_episodes')
    start=f.index('  prev <- seq_len'); end=f.index('  out$.original_order <- NULL',start)
    f=f[:start]+'''  same_key <- appusage_meta_merge_edges(episodes, merge_gap_ms)
  group <- cumsum(!same_key)
  groups <- split(seq_len(n), group)
  first <- vapply(groups, `[`, integer(1), 1L)
  last <- vapply(groups, function(i) i[[length(i)]], integer(1))
  out <- episodes[first, , drop = FALSE]
  many <- which(lengths(groups) > 1L)
  single <- which(lengths(groups) == 1L)
  out$source_episode_count[single[is.na(out$source_episode_count[single])]] <- 1L
  missing <- single[is.na(out$source_duration_ms[single])]
  out$source_duration_ms[missing] <- out$duration_ms[missing]
  out$merged_gap_ms[single[is.na(out$merged_gap_ms[single])]] <- 0
  for (column in c("end_ts_ms", "end_datetime", "end_event_type", "end_event_type_label", "end_class_name")) {
    out[[column]][many] <- episodes[[column]][last[many]]
  }
  if (length(many)) {
    selected <- groups[many]
    duration <- vapply(selected, function(i) sum(episodes$duration_ms[i], na.rm = TRUE), numeric(1))
    out$duration_ms[many] <- duration
    out$duration_min[many] <- duration / 60000
    out$source_duration_ms[many] <- duration
    out$parse_warning[many] <- vapply(selected, function(i) compact_character_values(c(episodes$parse_warning[i], episodes$reconstruction_warning[i])), character(1))
    for (column in c("is_collection_app", "device_boundary_involved", "anomaly_missing_duration", "anomaly_negative_duration", "anomaly_extreme_duration", "anomaly_cross_date", "anomaly_any")) {
      out[[column]][many] <- vapply(selected, function(i) any(episodes[[column]][i], na.rm = TRUE), logical(1))
    }
    for (column in c("anomaly_reason", "reconstruction_warning")) {
      out[[column]][many] <- vapply(selected, function(i) compact_character_values(episodes[[column]][i]), character(1))
    }
    out$source_episode_count[many] <- vapply(selected, function(i) {
      count <- sum(episodes$source_episode_count[i], na.rm = TRUE)
      if (count > 0) count else length(i)
    }, numeric(1))
    out$merged_gap_ms[many] <- vapply(selected, function(i) sum(pmax(episodes$start_ts_ms[i[-1L]] - episodes$end_ts_ms[utils::head(i, -1L)], 0), na.rm = TRUE), numeric(1))
  }
'''+f[end:]
    text=replace(text,'merge_contiguous_meta_episodes',f)
    write('second_level.R',text)
    text=read('anomaly_qc.R').replace('  for (key in unique(package)) {\n    idx <- which(package %in% key)','  groups <- split(seq_len(n), match(package, unique(package)))\n  for (idx in groups) {')
    write('anomaly_qc.R',text)
    text=read('timezone.R')
    text=text.replace('  if (!tz %in% OlsonNames()) {','  if (!tz %in% appusage_timezone_names()) {')
    text+='''
# Refresh when the source of the timezone database changes; invalid zone names
# still pass through the same resolver error on every public entry.
appusage_timezone_names <- local({
  cached <- NULL
  source <- NULL
  function() {
    current <- Sys.getenv("TZDIR", unset = "")
    if (is.null(cached) || !identical(source, current)) {
      cached <<- OlsonNames()
      source <<- current
    }
    cached
  }
})

appusage_midnight_ms <- function(date, tz) {
  dates <- unique(date)
  values <- as.numeric(as.POSIXct(paste(dates, "00:00:00"),
    format = "%Y-%m-%d %H:%M:%S", tz = tz)) * 1000
  values[match(date, dates)]
}
'''
    old='''    midnight_ms <- as.numeric(as.POSIXct(
      paste(segment_date, "00:00:00"),
      format = "%Y-%m-%d %H:%M:%S", tz = tz
    )) * 1000
    next_midnight_ms <- as.numeric(as.POSIXct(
      paste(segment_date + 1L, "00:00:00"),
      format = "%Y-%m-%d %H:%M:%S", tz = tz
    )) * 1000'''
    assert old in text
    text=text.replace(old,'    midnight_ms <- appusage_midnight_ms(segment_date, tz)\n    next_midnight_ms <- appusage_midnight_ms(segment_date + 1L, tz)')
    write('timezone.R',text)
    text=read('source_anomaly_qc.R')
    text=replace(text,'appusage_source_qc_interval_segments','''appusage_source_qc_interval_segments <- function(x, tz) {
  n <- nrow(x)
  if (!n) return(NULL)
  tz <- appusage_resolve_timezone(tz)
  start <- as.numeric(x$start_ts_ms)
  end <- as.numeric(x$end_ts_ms)
  duration <- as.numeric(x$duration_ms)
  start_date <- appusage_date_from_datetime(ms = start, tz = tz)
  end_date <- appusage_date_from_datetime(ms = end - 0.001, tz = tz)
  invalid <- which(is.na(start_date) | is.na(end_date) | end_date < start_date)
  # Keep seq.Date's existing error behavior for unrepresentable/reversed input.
  if (length(invalid)) for (i in invalid) seq(start_date[[i]], end_date[[i]], by = "day")
  cross <- which(start_date != end_date)
  counts <- rep(1L, n)
  parts <- vector("list", length(cross))
  for (j in seq_along(cross)) {
    i <- cross[[j]]
    dates <- seq(start_date[[i]], end_date[[i]], by = "day")
    boundaries <- appusage_midnight_ms(dates[-1L], tz)
    endpoints <- c(start[[i]], boundaries[boundaries > start[[i]] & boundaries < end[[i]]], end[[i]])
    wall <- diff(endpoints)
    allocated <- if (sum(wall) > 0) duration[[i]] * wall / sum(wall) else duration[[i]]
    if (length(allocated) > 1L) allocated[[length(allocated)]] <- duration[[i]] - sum(allocated[-length(allocated)])
    counts[[i]] <- length(allocated)
    parts[[j]] <- list(start = utils::head(endpoints, -1L), end = endpoints[-1L], duration = allocated)
  }
  row <- rep.int(seq_len(n), counts)
  a <- start[row]; b <- end[row]; d <- duration[row]
  positive <- b > a
  d[positive] <- d[positive] * (b[positive] - a[positive]) / (b[positive] - a[positive])
  offsets <- c(0L, cumsum(counts))
  for (j in seq_along(cross)) {
    i <- cross[[j]]
    index <- offsets[[i]] + seq_len(counts[[i]])
    a[index] <- parts[[j]]$start; b[index] <- parts[[j]]$end; d[index] <- parts[[j]]$duration
  }
  data.frame(source_row = row, date = appusage_date_from_datetime(ms = a, tz = tz),
    start_ts_ms = a, end_ts_ms = b, duration_ms = d,
    package_name = appusage_chr_col(x, "package_name")[row], stringsAsFactors = FALSE)
}''')
    write('source_anomaly_qc.R',text)

if __name__=='__main__': main()
