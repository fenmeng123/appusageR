# Count nonblank rows inside a native block. A standalone streaming section
# heading in the date column of a line table has no application payload.
appusage_day_candidate_count <- function(mat, block, component, header) {
  rows <- rows_for_block(mat, block)
  valid <- appusage_context_valid(rows)
  if (component == "line") {
    date_col <- which(!is.na(header) & grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}$", header))
    if (length(date_col) == 1L) {
      marker <- which(appusage_context_column(rows, date_col) %in% "\u6d41\u5a92\u4f53")
      if (length(marker)) {
        # Strip only empty CSV fields and whitespace around this exact heading.
        heading <- stringi::stri_detect_regex(rows$store$lines[rows$rows[marker]],
          "^\\s*(?:,\\s*)*\u6d41\u5a92\u4f53\\s*(?:,\\s*)*$")
        valid[marker[heading]] <- FALSE
      }
    }
  }
  sum(valid)
}

# Record header evidence while the native parser still knows each block boundary.
# The scientific tables retain their original columns and attributes.
appusage_parse_blocks <- function(blocks, mat, component, parser, ...) {
  facts <- vector("list", length(blocks))
  rows <- vector("list", length(blocks))
  for (i in seq_along(blocks)) {
    block <- blocks[[i]]
    raw <- parser(block, mat = mat, ...)
    rows[i] <- list(raw)
    header <- appusage_header_cells(mat, block$header_row)
    header_date <- extract_row_date(header)
    candidates <- appusage_day_candidate_count(mat, block, component, header)
    parsed <- NROW(raw)
    dates <- as.character(safe_as_date(header_date))
    counts <- parsed
    if (component == "app" && parsed > 0L) {
      # An app export has one header per app and one date on each record.
      row_date <- as.character(safe_as_date(raw$date))
      dates <- unique(row_date)
      counts <- tabulate(match(row_date, dates), length(dates))
    }
    identified <- if (parsed) !is.na(blank_to_na(raw$app_name)) |
      !is.na(blank_to_na(raw$package_name)) else logical()
    state <- if (parsed != candidates || any(!identified)) "partial" else if (parsed)
      "recorded" else "explicit_empty"
    facts[[i]] <- data.frame(component = component, header_row = block$header_row,
      last_row = block$end, header_date = header_date, date = dates,
      candidate_rows_in_block = candidates, parsed_rows = counts,
      state = ifelse(is.na(dates), "unknown", state), stringsAsFactors = FALSE)
  }
  if (length(facts)) mat$store$cache$day_blocks[[component]] <- do.call(rbind, facts)
  do.call(rbind, rows)
}

appusage_coverage_facts <- function(context) {
  tables <- context$store$cache$day_blocks
  blocks <- do.call(rbind, unname(tables))
  list(version = 1L, blocks = blocks, hint = appusage_blank_hint(blocks))
}

appusage_failed_coverage <- function(context) {
  # Preflight can reject a file before a parser has visited any table. Only a
  # bounded heading with no nonblank payload is evidence of an explicit empty.
  bounds <- appusage_structural_boundaries(context)
  kind <- c(line_header = "line", day_header = "day", app_header = "app",
    meta_table1 = "meta_summary", meta_table2 = "meta_events")
  headers <- bounds[bounds$boundary_type %in% names(kind), , drop = FALSE]
  ranges <- appusage_table_blocks(context, headers$row)
  facts <- lapply(seq_along(ranges), function(i) {
    b <- ranges[[i]]
    header <- appusage_header_cells(context, b$header_row)
    token <- extract_row_date(header)
    date <- as.character(safe_as_date(token))
    n <- appusage_day_candidate_count(context, b, unname(kind[headers$boundary_type[i]]), header)
    data.frame(component = unname(kind[headers$boundary_type[i]]), header_row = b$header_row,
      last_row = b$end, header_date = token, date = date, candidate_rows_in_block = n,
      parsed_rows = if (n) NA_integer_ else 0L,
      state = if (is.na(date)) "unknown" else if (n) "partial" else "explicit_empty")
  })
  blocks <- do.call(rbind, facts)
  list(version = 1L, blocks = blocks, hint = appusage_blank_hint(blocks))
}

appusage_blank_hint <- function(blocks) {
  if (!NROW(blocks)) return("not_evaluated")
  components <- split(blocks, blocks$component)
  states <- vapply(components, function(b) {
    days <- safe_as_date(b$date)
    known <- !is.na(days)
    damaged <- any(!known | b$state %in% c("partial", "unknown"))
    if (!any(known)) return("not_evaluated")
    grouped <- split(b$state[known], as.character(days[known]))
    empty <- any(vapply(grouped, function(s) all(s == "explicit_empty"), logical(1)))
    gap <- !damaged && any(diff(sort(unique(days[known]))) > 1)
    if (empty || gap) "detected" else if (damaged) "not_evaluated" else "none_detected"
  }, character(1))
  if ("detected" %in% states) "detected" else if ("not_evaluated" %in% states)
    "not_evaluated" else "none_detected"
}

appusage_audit_input <- function(data, metadata) {
  if (is.character(data) && length(data) == 1L) {
    path <- data
    if (is.null(metadata)) {
      sidecar <- sub("[.]rda$", ".json", path, ignore.case = TRUE)
      if (!identical(path, sidecar) && file.exists(sidecar)) metadata <- sidecar
    }
    data <- load_appusage_data_object(path)
  } else if (inherits(data, c("appusage_first_level", "appusage_second_level"))) {
    metadata <- metadata %||% data[["metadata"]]
    if (is.null(metadata) && is_present_string(data$metadata_file)) metadata <- data$metadata_file
    if (is.null(metadata) && is_present_string(data$audit_metadata_file)) metadata <- data$audit_metadata_file
    data <- data$data
  }
  if (is.character(metadata)) metadata <- appusage_read_cache_json(metadata)
  list(data = data, metadata = metadata)
}

appusage_audit_filter <- function(x, activity, include_collection_app, packages,
                                  retain_unknown_package = FALSE) {
  n <- nrow(x)
  pkg <- appusage_chr_col(x, "package_name")
  name <- appusage_chr_col(x, "app_name")
  type <- if ("activity_type" %in% names(x)) as.character(x$activity_type) else {
    inferred <- classify_activity_type(name)
    inferred[is.na(name)] <- NA_character_
    inferred
  }
  keep <- rep(TRUE, n)
  if (!identical(activity, "all")) {
    keep <- type == activity
    keep[!type %in% c("foreground", "background")] <- NA
  }
  if (!include_collection_app) {
    collection <- if ("is_collection_app" %in% names(x)) x$is_collection_app else pkg == "com.w.appusage"
    if (retain_unknown_package) collection[is.na(collection) & is.na(blank_to_na(pkg))] <- FALSE
    keep <- keep & !collection
  }
  if (!is.null(packages)) keep <- keep & ifelse(is.na(pkg), NA, pkg %in% packages)
  keep[pkg %in% "ALL"] <- FALSE
  keep
}

appusage_audit_numeric <- function(x, field) {
  v <- x[[field]]
  if (is.null(v)) return(rep(NA_real_, nrow(x)))
  if (inherits(v, c("Date", "POSIXt", "difftime")))
    stop("Supply explicit numeric timestamp or duration units for ", field, ".")
  if (is.factor(v)) v <- as.character(v)
  suppressWarnings(as.numeric(v))
}

appusage_audit_one_source <- function(x) {
  for (key in intersect(c("source_file", "source_record_key", "participant_id"), names(x))) {
    if (length(unique(stats::na.omit(x[[key]]))) > 1L)
      stop("Supply one source and one participant per audit.")
  }
}

appusage_day_blocks <- function(metadata) {
  facts <- metadata$day_coverage
  blocks <- facts$blocks
  if (!isTRUE(facts$version == 1L) || !length(blocks)) return(NULL)
  if (!is.data.frame(blocks))
    blocks <- do.call(rbind, lapply(blocks, function(x) {
      x$date <- x$date %||% NA_character_
      x$header_date <- x$header_date %||% NA_character_
      as.data.frame(x, stringsAsFactors = FALSE)
    }))
  blocks
}

#' Audit dated blocks and compare daily recorded-duration denominators
#'
#' Audits one source/component. A calendar day refers to the original dated table
#' (app exports carry the date on each record). Line duration is attributed to
#' its source table date. Empty and absent tables require an explicit zero
#' assumption. Event-only and cumulative meta tables have no daily duration.
#' @param data A first- or second-level result, data list, cache RDA path, or
#'   one explicitly selected table with date, duration_ms, package_name and
#'   app_name or activity_type. Supply paired metadata for block evidence.
#' @param metadata Paired metadata list or JSON path.
#' @param window Two inclusive Date limits. Defaults to the range of dated
#'   blocks as a file-range proxy.
#' @param strategy Recorded complete days, or an explicit blank-day sensitivity
#'   strategy. Edge exclusion removes the first and last recorded dates once.
#' @param component Native component. Defaults to the selected line/day/app
#'   export or meta events.
#' @param activity Selected activity ("foreground", "background" or "all").
#' @param include_collection_app Include the collection app in durations.
#' @param packages Optional vector of selected package names.
#' @param exclude_edges Also exclude the first and last recorded dates.
#' @return A list with the daily calendar and reasons, original block facts,
#'   window source, numerator, actual denominator and mean in milliseconds.
#' @examples
#' data(appusage_demo)
#' record <- appusage_demo$records$recorded_blank_absent_days
#' observed <- audit_appusage_days(record, metadata = record$metadata)
#' zero_window <- audit_appusage_days(record, metadata = record$metadata,
#'                                  strategy = "zero_window")
#' observed$calendar
#' # Compare denominators without modifying stored daily durations or QC.
#' stopifnot(observed$denominator_days == 2L, observed$mean_ms == 150000,
#'           zero_window$denominator_days == 4L, zero_window$mean_ms == 75000)
#' observed[c("denominator_days", "mean_ms")]
#' zero_window[c("denominator_days", "mean_ms")]
#' @export
audit_appusage_days <- function(data, metadata = NULL, window = NULL,
                                strategy = c("observed_complete", "zero_internal",
                                  "zero_boundary", "zero_window", "exclude_observed_edges"),
                                component = NULL, activity = c("foreground", "background", "all"),
                                include_collection_app = FALSE, packages = NULL,
                                exclude_edges = FALSE) {
  strategy <- match.arg(strategy)
  activity <- match.arg(activity)
  input <- appusage_audit_input(data, metadata)
  data <- input$data
  blocks <- appusage_day_blocks(input$metadata)
  if (is.null(component)) {
    component <- input$metadata$export$detected_type
    if (!is_present_string(component) || !component %in% c("line", "day", "app", "meta")) {
      native <- intersect(c("line", "day", "app", "meta_events", "meta_summary"), names(data))
      if (!is.data.frame(data) && !length(native) && is.data.frame(data$daily))
        native <- unique(stats::na.omit(data$daily$source_export_type))
      if (!length(native)) native <- unique(blocks$component)
      component <- if (length(native)) native[1L] else "line"
    }
  }
  if (identical(component, "meta")) component <- "meta_events"
  component <- match.arg(component, c("line", "day", "app", "meta_events", "meta_summary"))
  blocks <- blocks[blocks$component == component, , drop = FALSE]
  x <- if (is.data.frame(data)) data else switch(component, line = data$line %||% data$episode,
    day = data$day %||% data$daily, app = data$app %||% data$daily,
    meta_events = data$meta_events %||% data$event, meta_summary = data$meta_summary)
  if (is.null(x)) x <- data.frame()
  if ("app_name" %in% names(x) && "activity_type" %in% names(x)) {
    # Standardization may assign foreground to a missing application name.
    x$activity_type[is.na(x$app_name)] <- NA_character_
  }
  appusage_audit_one_source(x)
  if (!is.data.frame(data) && component %in% c("day", "app") && "source_export_type" %in% names(x))
    x <- x[x$source_export_type %in% component, , drop = FALSE]
  if (component == "line" && "episode_source" %in% names(x))
    x <- appusage_line_episode_rows(x)
  dates <- safe_as_date(x[["source_table_date"]] %||% x[["table_date"]] %||% x[["date"]] %||% character())
  if (length(dates) != nrow(x)) stop("A record date column is required.")
  days <- safe_as_date(blocks$date %||% character())
  window_source <- if (is.null(window)) "file_range_proxy" else "external"
  scope <- c(days, dates)
  if (is.null(window)) window <- if (any(!is.na(scope))) range(scope, na.rm = TRUE) else as.Date(character())
  window <- safe_as_date(window)
  if (length(window) && (length(window) != 2L || anyNA(window) || window[1] > window[2]))
    stop("Supply two valid, ordered window dates.")
  calendar <- if (length(window)) seq(window[1], window[2], by = "day") else as.Date(character())
  index <- split(seq_along(dates), as.character(dates))
  headers <- split(seq_along(days), as.character(days))
  key <- as.character(calendar)
  n <- length(key)
  coverage <- rep("unknown", n)
  measured <- rep("unavailable", n)
  value <- rep(NA_real_, n)
  duration <- appusage_audit_numeric(x, "duration_ms")
  selected <- appusage_audit_filter(x, activity, include_collection_app, packages)
  ambiguous_date <- anyNA(days) || anyNA(dates)
  reliable_absence <- NROW(blocks) > 0 && !ambiguous_date
  duration_supported <- !startsWith(component, "meta")
  for (i in seq_along(key)) {
    h <- headers[[key[i]]]
    r <- index[[key[i]]]
    states <- blocks$state[h]
    recorded <- length(r) > 0L
    coverage[i] <- if (any(states %in% c("partial", "unknown"))) "partial" else if (
      recorded || "recorded" %in% states) "recorded" else if ("explicit_empty" %in% states)
      "explicit_empty" else if (reliable_absence) "no_block" else "unknown"
    if (!recorded || !duration_supported) next
    known <- duration[r][selected[r] %in% TRUE]
    ok <- is.finite(known) & known >= 0
    expected_rows <- blocks$parsed_rows[h]
    incomplete <- length(h) && (anyNA(expected_rows) || sum(expected_rows) != length(r))
    # A known zero on a selected record is measurable. Inferring an empty
    # foreground selection also needs a complete block and individual app rows.
    empty_unverified <- !length(known) && (!length(h) || all(x$package_name[r] %in% "ALL"))
    uncertain <- anyNA(selected[r]) || any(!ok) || coverage[i] == "partial" ||
      ambiguous_date || incomplete || empty_unverified
    if (!uncertain) {
      measured[i] <- "complete"
      value[i] <- sum(known)
    } else if (any(ok)) measured[i] <- "partial"
  }
  position <- rep("undetermined", n)
  recorded_dates <- c(dates, days[blocks$parsed_rows > 0L])
  recorded_dates <- recorded_dates[!is.na(recorded_dates) & recorded_dates %in% calendar]
  if (length(recorded_dates)) {
    ends <- range(recorded_dates)
    position <- ifelse(calendar < ends[1], "leading",
      ifelse(calendar > ends[2], "trailing", "internal"))
  }
  blank <- coverage %in% c("explicit_empty", "no_block")
  fill <- blank & duration_supported & reliable_absence & switch(strategy, zero_window = TRUE,
    zero_internal = position == "internal", zero_boundary = position %in% c("leading", "trailing"), FALSE)
  reason <- ifelse(measured == "complete", "recorded_duration", ifelse(coverage == "recorded", measured, coverage))
  value[fill] <- 0
  reason[fill] <- strategy
  if ((exclude_edges || strategy == "exclude_observed_edges") && length(recorded_dates)) {
    edge <- calendar %in% range(recorded_dates)
    value[edge] <- NA_real_
    reason[edge] <- "excluded_observed_edge"
  }
  denominator <- sum(!is.na(value))
  numerator <- if (denominator) sum(value, na.rm = TRUE) else NA_real_
  list(component = component, window = window, window_source = window_source,
    strategy = strategy, exclude_edges = exclude_edges || strategy == "exclude_observed_edges",
    activity = activity, include_collection_app = include_collection_app, packages = packages,
    outside_window_records = sum(!is.na(dates) & !dates %in% calendar),
    undated_records = sum(is.na(dates)), undated_blocks = sum(is.na(days)),
    blocks = blocks, calendar = data.frame(date = calendar, coverage = coverage,
      position = position, duration_status = measured, value_ms = value, reason = reason),
    numerator_ms = numerator, denominator_days = denominator,
    mean_ms = if (denominator) numerator / denominator else NA_real_,
    reason = if (!denominator) "no_eligible_days" else "ok")
}
