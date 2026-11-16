# Count positive-span half-open pairs without constructing a pair matrix.
appusage_geometry_counts <- function(a, b) {
  counts <- c(identical_endpoints = 0, same_start = 0, same_end = 0,
              strict_containment = 0, partial_crossing = 0)
  if (length(a) < 2L) return(counts)
  order <- order(a, b)
  a <- a[order]
  b <- b[order]
  pairs <- function(n) sum(as.double(n) * (n - 1) / 2)
  run <- c(TRUE, diff(a) != 0 | diff(b) != 0)
  frequency <- diff(c(which(run), length(run) + 1))
  counts[1] <- pairs(frequency)
  counts[2] <- pairs(rle(a)$lengths) - counts[1]
  counts[3] <- pairs(tabulate(match(b, unique(b)))) - counts[1]
  # Within each equal-start group ends are ascending, so a strict end
  # inversion always has a strictly earlier start. Compress duplicate keys.
  ends <- b[run]
  if (is.unsorted(ends)) {
    rank <- match(ends, sort(unique(ends)))
    size <- max(rank)
    tree <- numeric(size)
    seen <- 0
    for (i in seq_along(rank)) {
      j <- rank[i]
      le <- 0
      while (j > 0L) {
        le <- le + tree[j]
        j <- j - bitwAnd(j, -j)
      }
      counts[4] <- counts[4] + frequency[i] * (seen - le)
      j <- rank[i]
      while (j <= size) {
        tree[j] <- tree[j] + frequency[i]
        j <- j + bitwAnd(j, -j)
      }
      seen <- seen + frequency[i]
    }
  }
  total <- appusage_exact_count(appusage_overlap_pair_count(a, b))
  counts[5] <- total - sum(counts[1:4])
  counts
}

appusage_pair_breakdown <- function(a, b, package) {
  total <- appusage_geometry_counts(a, b)
  known <- !is.na(package)
  identified <- if (all(known)) total else appusage_geometry_counts(a[known], b[known])
  same <- total * 0
  groups <- split(which(known), package[known])
  if (length(groups) == 1L) same <- identified else
    for (idx in groups[lengths(groups) > 1L])
      same <- same + appusage_geometry_counts(a[idx], b[idx])
  data.frame(type = names(total), total = unname(total), same_package = unname(same),
    cross_package = unname(identified - same), unknown_package = unname(total - identified))
}

appusage_package_spans <- function(a, b, package) {
  # Merge overlapping/touching rows within each known app before allocating.
  o <- order(package, a, b)
  a <- a[o]; b <- b[o]; package <- package[o]
  groups <- split(seq_along(a), package)
  parts <- lapply(groups, function(i) {
    ends <- cummax(b[i])
    new <- c(TRUE, a[i[-1L]] > utils::head(ends, -1L))
    data.frame(package = package[i[1L]], start = a[i][new], end = ends[c(new[-1L], TRUE)])
  })
  do.call(rbind, unname(parts))
}

appusage_allocate_slices <- function(a, b, package, times, width, strategy, priority) {
  known <- !is.na(package)
  if (strategy == "priority") known <- known & package %in% priority
  spans <- appusage_package_spans(a[known], b[known], package[known])
  n <- length(times)
  coverage <- function(left, right) cumsum(tabulate(left, n) - tabulate(right, n))[-n]
  blocked <- coverage(match(a[!known], times), match(b[!known], times)) > 0L
  unassigned <- sum(width[blocked])
  if (is.null(spans)) return(list(by_app = numeric(), unassigned = unassigned))
  left <- match(spans$start, times)
  right <- match(spans$end, times)
  if (strategy == "equal_share") {
    k <- coverage(left, right)
    integral <- c(0, cumsum(ifelse(k > 0 & !blocked, width / pmax(1, k), 0)))
    shares <- integral[right] - integral[left]
  } else {
    # Assign each atom once in priority order. The successor table skips
    # settled atoms; path compression avoids rescanning the settled prefix.
    next_free <- seq_len(n)
    shares <- numeric(length(left))
    for (i in order(match(spans$package, priority))) {
      j <- left[i]
      while (j < right[i]) {
        if (next_free[j] == j) {
          if (!blocked[j]) shares[i] <- shares[i] + width[j]
          next_free[j] <- j + 1L
        }
        next_free[j] <- next_free[next_free[j]]
        j <- next_free[j]
      }
    }
  }
  list(by_app = tapply(shares, spans$package, sum), unassigned = unassigned)
}

appusage_geometry_day <- function(seg, strategy, priority) {
  a <- seg$start_ts_ms
  b <- seg$end_ts_ms
  times <- sort(unique(c(a, b)))
  n <- length(times)
  depth <- cumsum(tabulate(match(a, times), n) - tabulate(match(b, times), n))[-n]
  width <- diff(times)
  union <- sum(width[depth > 0])
  excess <- sum(width * pmax(0, depth - 1))
  stored <- if (anyNA(seg$duration_ms)) NA_real_ else sum(seg$duration_ms)
  allocation <- if (strategy %in% c("equal_share", "priority"))
    appusage_allocate_slices(a, b, seg$package_name, times, width, strategy, priority)
  list(metrics = data.frame(date = seg$date[1L], segments = length(a),
      stored_duration_sum = stored, endpoint_span_sum = sum(b - a), endpoint_union = union,
      multiplicity_excess = excess, multiple_coverage_ms = sum(width[depth >= 2L])),
    pairs = appusage_pair_breakdown(a, b, seg$package_name), allocation = allocation)
}

# Resolve minute labels by matching actual UTC instants against the local clock.
# Nonexistent spring minutes and repeated autumn minutes have no unique match.
appusage_local_minutes <- function(date, label, tz) {
  date <- as.character(safe_as_date(date))
  label <- as.character(label)
  ms <- rep(NA_real_, length(label))
  for (idx in split(seq_along(date), date)) {
    anchor <- as.numeric(as.POSIXct(date[idx[1L]], tz = "UTC"))
    utc <- anchor + seq(-86400, 2 * 86400, by = 60)
    wall <- format(as.POSIXct(utc, origin = "1970-01-01", tz = tz), "%Y-%m-%d %H:%M", tz = tz)
    pos <- match(paste(date[idx], label[idx]), wall)
    unique_minute <- !is.na(pos) & !(duplicated(wall) | duplicated(wall, fromLast = TRUE))[pos]
    ms[idx[unique_minute]] <- utc[pos[unique_minute]] * 1000
  }
  ms
}

appusage_map_intervals <- function(x, columns, tz) {
  allowed <- c("date", "end_date", "start_time", "end_time", "duration_seconds",
    "duration_ms", "start_ts_ms", "end_ts_ms", "package_name", "app_name", "activity_type",
    "is_collection_app", "participant_id", "source_file")
  if (!is.character(columns) || is.null(names(columns)) || anyDuplicated(names(columns)) ||
      !all(names(columns) %in% allowed) || !all(columns %in% names(x)))
    stop("Supply named canonical-to-input column names present in the table.")
  # Read from the original table so that mapping one key cannot overwrite another.
  x[names(columns)] <- x[unname(columns)]
  if ("duration_seconds" %in% names(columns)) {
    if ("duration_ms" %in% names(columns)) stop("Map one stored duration unit.")
    x$duration_ms <- 1000 * appusage_audit_numeric(x, "duration_seconds")
  }
  if (any(c("start_time", "end_time") %in% names(columns))) {
    if (!all(c("date", "start_time", "end_time") %in% names(columns)) ||
        any(c("start_ts_ms", "end_ts_ms") %in% names(columns)))
      stop("Map date, start_time and end_time together for displayed minutes.")
    end_date <- if ("end_date" %in% names(columns)) x$end_date else x$date
    x$start_ts_ms <- appusage_local_minutes(x$date, x$start_time, tz)
    x$end_ts_ms <- appusage_local_minutes(end_date, x$end_time, tz)
  }
  x
}

#' Audit interval overlap on a local-calendar timeline
#'
#' Positive half-open intervals are split at local midnight. Counts describe
#' same-day segment pairs. A pair spanning two dates can contribute twice.
#' Stored duration is apportioned by span when a record crosses midnight.
#' Endpoint-derived values describe the geometry of the supplied representation.
#' @inheritParams audit_appusage_days
#' @param data One source: a proc-1 or proc-2 result, cache RDA, or a mapped
#'   data frame with numeric start_ts_ms, end_ts_ms, duration_ms, package_name
#'   and app_name or activity_type. Values use milliseconds since the Unix epoch.
#' @param strategy One strategy for the whole selection. Equal share counts
#'   distinct active apps. Priority uses the first listed active package.
#'   Unknown and (for priority) unlisted packages leave a slice unassigned.
#'   The exclusion view removes whole source rows involved in overlap.
#'   The exploratory hybrid subtracts endpoint excess from eligible stored
#'   duration and reports a negative result as unavailable.
#' @param columns Optional named character mapping from canonical fields to
#'   original data-frame columns, e.g. c(package_name = "App_UUID",
#'   date = "day", start_time = "begin", end_time = "finish",
#'   duration_seconds = "seconds", app_name = "label"). Local HH:MM labels use
#'   display_minute precision. Supply end_date for a known overnight interval;
#'   a backward clock with only one date remains undetermined. Ambiguous or
#'   nonexistent local minutes have no unique endpoint.
#' @param priority Ordered vector of unique package names for priority allocation.
#' @param precision Endpoint representation. Native line data retain their export
#'   precision. For a mapped table declare native_export_ms or display_minute;
#'   unknown precision is recorded as unevaluated.
#' @param tz IANA time zone for midnight boundaries; defaults to paired metadata,
#'   then to the package time zone.
#' @return Selection and exclusions, five pair types by date and package relation,
#'   daily and total timeline measures (ms), and the optional strategy view.
#' @examples
#' data(appusage_demo)
#' record <- appusage_demo$records$overlapping_intervals
#' audit <- audit_appusage_overlap(record, metadata = record$metadata)
#' stopifnot(audit$totals[["stored_duration_sum"]] == 44000,
#'           audit$totals[["endpoint_union"]] == 14000,
#'           audit$totals[["multiplicity_excess"]] == 30000)
#' audit$totals
#' @export
audit_appusage_overlap <- function(data, metadata = NULL,
    strategy = c("diagnose", "stored_sum", "endpoint_union", "equal_share",
      "priority", "exclude_flagged_view", "hybrid_exploratory"),
    activity = c("foreground", "background", "all"), include_collection_app = FALSE,
    packages = NULL, priority = NULL, precision = NULL, tz = NULL, columns = NULL) {
  strategy <- match.arg(strategy)
  activity <- match.arg(activity)
  input <- appusage_audit_input(data, metadata)
  d <- input$data
  native <- !is.data.frame(d)
  x <- if (native) d$line %||% appusage_line_episode_rows(d$episode) else d
  if (is.null(x)) x <- data.frame()
  tz <- appusage_resolve_timezone(tz %||% input$metadata$export$timezone %||% appusage_default_timezone())
  if (!is.null(columns)) {
    if (native) stop("Supply a single data frame for explicit column mapping.")
    x <- appusage_map_intervals(x, columns, tz)
  }
  display <- "start_time" %in% names(columns)
  if (display && !is.null(precision) && precision != "display_minute")
    stop("Mapped wall-clock minutes require display_minute precision.")
  if (display) precision <- "display_minute"
  appusage_audit_one_source(x)
  native_line <- native && (!is.null(d$line) ||
    identical(input$metadata$export$detected_type, "line") ||
    (nrow(x) > 0L && all(x$source_export_type %in% "line") && "source_export_type" %in% names(x)))
  precision <- match.arg(precision %||% if (native_line) "native_export_ms" else "unknown",
    c("native_export_ms", "display_minute", "unknown"))
  if (strategy == "priority" && (!is.character(priority) || !length(priority) ||
      anyNA(priority) || any(!nzchar(priority)) || anyDuplicated(priority)))
    stop("Supply a unique ordered package priority list.")
  # Preserve unidentified active intervals for allocation. They make their
  # slices unassigned, even when positively identified collection rows are excluded.
  selected <- appusage_audit_filter(x, activity, include_collection_app, packages,
    retain_unknown_package = TRUE)
  a <- appusage_audit_numeric(x, "start_ts_ms")
  b <- appusage_audit_numeric(x, "end_ts_ms")
  duration <- appusage_audit_numeric(x, "duration_ms")
  duration[!is.finite(duration) | duration < 0] <- NA_real_
  package <- blank_to_na(appusage_chr_col(x, "package_name"))
  reason <- rep("eligible", nrow(x))
  reason[b == a & is.finite(a) & is.finite(b)] <- "zero_span"
  reason[which(b == a & duration > 0 & precision == "display_minute")] <- "same_minute_positive_stored"
  reason[which(b < a)] <- if (display && !"end_date" %in% names(columns))
    "cross_midnight_undetermined" else "inverted_endpoints"
  reason[!is.finite(a) | !is.finite(b)] <- "missing_endpoints"
  if (precision == "unknown") reason[] <- "unknown_precision"
  reason[selected %in% FALSE] <- "filtered"
  reason[is.na(selected)] <- "unknown_selection"
  idx <- which(reason == "eligible")
  pieces <- if (length(idx)) appusage_source_qc_interval_segments(data.frame(
    start_ts_ms = a[idx], end_ts_ms = b[idx], duration_ms = duration[idx],
    package_name = package[idx]), tz)
  dates <- if (length(idx)) split(seq_len(nrow(pieces)), as.character(pieces$date)) else list()
  days <- lapply(dates, function(i) appusage_geometry_day(pieces[i, , drop = FALSE], strategy, priority))
  metrics <- do.call(rbind, lapply(days, "[[", "metrics"))
  cols <- c("segments", "stored_duration_sum", "endpoint_span_sum", "endpoint_union",
    "multiplicity_excess", "multiple_coverage_ms")
  totals <- if (length(days)) colSums(metrics[cols]) else stats::setNames(rep(0, length(cols)), cols)
  pairs <- if (length(days)) Reduce(function(x, y) { x[-1] <- x[-1] + y[-1]; x },
    lapply(days, "[[", "pairs")) else appusage_pair_breakdown(numeric(), numeric(), character())
  appusage_exact_count(sum(pairs$total))
  selected_ms <- sum(duration[selected %in% TRUE])
  view <- switch(strategy, stored_sum = selected_ms, endpoint_union = totals[["endpoint_union"]],
    hybrid_exploratory = totals[["stored_duration_sum"]] - totals[["multiplicity_excess"]],
    equal_share = , priority = lapply(days, "[[", "allocation"), NULL)
  flagged <- integer()
  if (strategy == "exclude_flagged_view") {
    # Detect affected original rows on the uncut timeline.
    o <- idx[order(a[idx], b[idx])]
    if (length(o)) {
      hit <- a[o] < c(-Inf, utils::head(cummax(b[o]), -1L)) |
        b[o] > c(a[o][-1L], Inf)
      flagged <- sort(o[hit])
    }
    view <- sum(duration[setdiff(idx, flagged)])
  }
  status <- if (!length(idx)) "no_eligible_intervals" else "ok"
  if (strategy %in% c("hybrid_exploratory", "stored_sum", "exclude_flagged_view") && is.na(view))
    status <- "incomplete_stored_duration"
  if (strategy == "hybrid_exploratory" && isTRUE(view < 0)) {
    view <- NA_real_; status <- "negative_hybrid"
  }
  list(status = status, strategy = strategy, precision = precision, timezone = tz,
    activity = activity, include_collection_app = include_collection_app, packages = packages,
    priority = priority, columns = columns, pair_unit = "same_day_segment_pairs", selected_stored_sum_ms = selected_ms,
    missing_selected_duration = sum(is.na(duration) & selected %in% TRUE),
    row_status = reason, daily = metrics, pairs = pairs, daily_pairs = lapply(days, "[[", "pairs"),
    totals = totals, view = view, flagged_source_rows = flagged)
}
