# Keep event selection sequential, but keep state and output as integer indexes.
appusage_meta_pair_indices <- function(events, start_event_types, end_event_types) {
  n <- nrow(events)
  from <- to <- integer(n)
  inferred <- rep(NA_real_, n)
  reason <- rep(NA_character_, n)
  boundary <- logical(n)
  size <- 0L
  dropped <- dropped_start <- dropped_end <- inferred_count <- 0L
  types <- events$event_type
  times <- events$event_ts_ms
  durations <- events$event_duration_ms
  add <- function(a, b, device = FALSE, duration = NA_real_, why = NA_character_) {
    size <<- size + 1L
    from[[size]] <<- a; to[[size]] <<- b
    inferred[[size]] <<- duration; reason[[size]] <<- why
    boundary[[size]] <<- device
  }
  unmatched <- function(i, why) {
    duration <- durations[[i]]
    if (!is.na(duration) && duration >= 0) {
      add(i, i, duration = duration, why = why)
      inferred_count <<- inferred_count + 1L
    } else {
      dropped <<- dropped + 1L
      if (why == "unmatched_start") dropped_start <<- dropped_start + 1L else dropped_end <<- dropped_end + 1L
    }
  }
  boundaries <- which(types %in% c(26, 27))
  groups <- split(seq_len(n), events$.reconstruction_key)
  for (idx in groups) {
    if (length(boundaries)) {
      idx <- unique(c(idx, boundaries))
      idx <- idx[order(times[idx], events$.row_order[idx], na.last = TRUE)]
    }
    open <- pending <- 0L
    for (i in idx) {
      type <- types[[i]]
      if (!is.na(type) && type == 26) {
        if (pending) { add(open, pending); open <- pending <- 0L }
        if (open) { add(open, i, device = TRUE); open <- pending <- 0L }
        next
      }
      if (!is.na(type) && type == 27) next
      stop_event <- !is.na(type) && type == 23
      if (pending && !stop_event) { add(open, pending); open <- pending <- 0L }
      if (!is.na(type) && type %in% start_event_types) {
        if (open) unmatched(open, "unmatched_start")
        open <- i; pending <- 0L
      } else if (!is.na(type) && type == 2) {
        if (!open) unmatched(i, "unmatched_end") else pending <- i
      } else if (stop_event) {
        if (!open) {
          unmatched(i, "unmatched_end")
        } else {
          if (pending) {
            if (is.na(times[[pending]]) || (!is.na(times[[i]]) && times[[i]] >= times[[pending]])) pending <- i
            add(open, pending)
          } else add(open, i)
          open <- pending <- 0L
        }
      } else if (!is.na(type) && type %in% end_event_types) {
        if (!open) unmatched(i, "unmatched_end") else {
          add(open, i); open <- pending <- 0L
        }
      }
    }
    if (pending) { add(open, pending); open <- pending <- 0L }
    if (open) unmatched(open, "unmatched_start")
  }
  take <- seq_len(size)
  list(from = from[take], to = to[take], inferred = inferred[take],
    reason = reason[take], boundary = boundary[take],
    diagnostics = list(n_dropped_unmatched_events = dropped,
      n_dropped_unmatched_starts = dropped_start, n_dropped_unmatched_ends = dropped_end,
      n_duration_inferred_episodes = inferred_count))
}

appusage_pair_text <- function(a, b, combine = FALSE) {
  a <- as.character(a); b <- as.character(b)
  a[!is.na(a) & a == ""] <- NA_character_
  b[!is.na(b) & b == ""] <- NA_character_
  out <- a
  out[is.na(a)] <- b[is.na(a)]
  if (combine) {
    both <- !is.na(a) & !is.na(b) & a != b
    out[both] <- appusage_text_paste(a[both], b[both], sep = "; ")
  }
  out
}

appusage_meta_rows_from_indices <- function(events, pairs, pairing, tz) {
  a <- pairs$from; b <- pairs$to; n <- length(a)
  inferred <- !is.na(pairs$inferred)
  start <- events$event_ts_ms[a]; end <- events$event_ts_ms[b]
  start_datetime <- events$event_datetime[a]; end_datetime <- events$event_datetime[b]
  duration <- end - start
  end[inferred] <- start[inferred] + pairs$inferred[inferred]
  duration[inferred] <- pairs$inferred[inferred]
  end_datetime[inferred] <- ms_to_datetime(end[inferred], tz = tz)
  date <- appusage_date_from_datetime(start_datetime, tz = tz)
  fallback <- is.na(date) & !inferred
  date[fallback] <- appusage_date_from_datetime(end_datetime[fallback], tz = tz)
  source_date <- events$source_table_date[a]
  fallback <- is.na(source_date) & !inferred
  source_date[fallback] <- events$source_table_date[b[fallback]]
  app <- appusage_pair_text(events$app_name[a], events$app_name[b])
  package <- appusage_pair_text(events$package_name[a], events$package_name[b])
  # Inferred rows use the original values, including empty strings.
  app[inferred] <- events$app_name[a[inferred]]
  package[inferred] <- events$package_name[a[inferred]]
  warning <- appusage_pair_text(events$parse_warning[a], events$parse_warning[b], TRUE)
  warning[inferred] <- events$parse_warning[a[inferred]]
  reconstruction <- rep(NA_character_, n)
  reconstruction[inferred] <- appusage_text_paste0("duration_inferred_from_", pairs$reason[inferred])
  tibble::tibble(date = date, source_table_date = source_date,
    source_date_timestamp_date_mismatch = !is.na(source_date) & !is.na(date) & source_date != date,
    app_name = app, activity_type = classify_activity_type(app), package_name = package,
    start_ts_ms = start, end_ts_ms = end, start_datetime = start_datetime, end_datetime = end_datetime,
    duration_ms = duration, duration_min = duration / 60000, duration_text = NA_character_,
    source_episode_count = 1L, source_duration_ms = duration, merged_gap_ms = 0,
    source_export_type = "meta", parse_warning = warning,
    is_collection_app = !is.na(package) & package == "com.w.appusage",
    episode_source = "meta_events", pairing_strategy = pairing,
    start_event_type = events$event_type[a], end_event_type = events$event_type[b],
    start_event_type_label = events$event_type_label[a], end_event_type_label = events$event_type_label[b],
    start_class_name = events$class_name[a], end_class_name = events$class_name[b],
    reconstruction_status = "complete", reconstruction_warning = reconstruction,
    unmatched_start = FALSE, unmatched_end = FALSE, device_boundary_involved = pairs$boundary)
}

appusage_meta_merge_edges <- function(episodes, merge_gap_ms) {
  n <- nrow(episodes)
  if (n < 2L || is.na(merge_gap_ms) || merge_gap_ms < 0) return(rep(FALSE, n))
  a <- seq_len(n - 1L); b <- a + 1L
  eligible <- episodes$episode_source %in% "meta_events" & episodes$reconstruction_status %in% "complete" & !episodes$device_boundary_involved %in% TRUE
  same <- eligible[a] & eligible[b]
  for (column in c("app_name", "package_name", "activity_type", "date")) {
    value <- as.character(episodes[[column]])
    present <- !is.na(value) & value != ""
    same <- same & present[a] & present[b] & value[a] == value[b]
  }
  gap <- episodes$start_ts_ms[b] - episodes$end_ts_ms[a]
  same <- same & !is.na(gap) & gap >= 0 & gap <= merge_gap_ms
  same[is.na(same)] <- FALSE
  c(FALSE, same)
}
