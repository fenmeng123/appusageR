audit_intervals <- function(start, end, package = rep("a", length(start)), duration = end - start) {
  data.frame(start_ts_ms = as.double(start), end_ts_ms = as.double(end),
    duration_ms = as.double(duration), package_name = package,
    activity_type = rep("foreground", length(start)), stringsAsFactors = FALSE)
}

test_that("tibble day and app exports use available date columns without warnings", {
  day <- as.Date("2024-01-01")
  x <- tibble::tibble(date = day, duration_ms = 1000, package_name = "org.example",
    app_name = "Example", activity_type = "foreground")
  for (component in c("day", "app")) {
    frame <- x
    for (column in c("date", "table_date", "source_table_date")) {
      frame[[column]] <- day + match(column, c("date", "table_date", "source_table_date"))
      check <- function(y) audit_appusage_days(y, component = component)
      expect_no_warning(audit <- check(frame))
      expect_identical(audit, check(as.data.frame(frame)))
      expect_identical(audit$calendar$date, frame[[column]])
    }
  }
  x$date <- NULL
  expect_error(audit_appusage_days(x, component = "day"), "record date column")
})

geometry <- function(x, ...) audit_appusage_overlap(x, precision = "native_export_ms",
  include_collection_app = TRUE, tz = "UTC", ...)

audit_oracle <- function(x) {
  out <- matrix(0, 5, 4)
  if (nrow(x) < 2) return(out)
  for (i in seq_len(nrow(x) - 1L)) for (j in (i + 1L):nrow(x)) {
    a <- x$start_ts_ms[c(i, j)]
    b <- x$end_ts_ms[c(i, j)]
    if (max(a) >= min(b)) next
    k <- if (a[1] == a[2] && b[1] == b[2]) 1 else if (a[1] == a[2]) 2 else if (
      b[1] == b[2]) 3 else if (which.min(a) == which.max(b)) 4 else 5
    pkg <- x$package_name[c(i, j)]
    rel <- if (anyNA(pkg)) 4 else if (pkg[1] == pkg[2]) 2 else 3
    out[k, c(1, rel)] <- out[k, c(1, rel)] + 1
  }
  out
}

test_that("five geometries and package relations agree with independent enumeration", {
  set.seed(311)
  for (i in 1:25) {
    a <- sample.int(12, 25, replace = TRUE)
    x <- audit_intervals(a, a + sample.int(7, 25, replace = TRUE),
      sample(c("a", "b", NA_character_), 25, replace = TRUE))
    expect_equal(unname(as.matrix(geometry(x)$pairs[-1])), audit_oracle(x))
  }
  adj <- geometry(audit_intervals(c(0, 2), c(2, 3)))
  expect_equal(sum(adj$pairs$total), 0)
  triple <- geometry(audit_intervals(c(0, 2, 4), c(10, 8, 6)))
  expect_equal(unname(triple$totals[3:6]), c(18, 10, 8, 6))
})

test_that("allocation operates on distinct apps and unassigned slices", {
  x <- audit_intervals(c(0, 1, 1, 2), c(6, 5, 5, 4), c("a", "b", "b", NA))
  share <- geometry(x, strategy = "equal_share")$view[[1]]
  expect_equal(as.numeric(share$by_app[c("a", "b")]), c(3, 1))
  expect_equal(share$unassigned, 2)
  default_filter <- audit_appusage_overlap(x, precision = "native_export_ms",
    strategy = "equal_share", tz = "UTC")
  expect_equal(default_filter$view[[1]]$unassigned, 2)
  priority <- geometry(x, strategy = "priority", priority = c("b", "a"))$view[[1]]
  expect_equal(as.numeric(priority$by_app[c("a", "b")]), c(2, 2))
  expect_equal(priority$unassigned, 2)
  unlisted <- geometry(x, strategy = "priority", priority = "a")$view[[1]]
  expect_equal(unlisted$unassigned, 4)
  expect_error(geometry(x, strategy = "priority", priority = c("a", "a")), "unique")
  expect_equal(geometry(x, strategy = "exclude_flagged_view")$flagged_source_rows, 1:4)
  x$duration_ms <- 0
  expect_identical(geometry(x, strategy = "hybrid_exploratory")$status, "negative_hybrid")
})

test_that("half-open midnight splits preserve duration across DST and count daily pairs", {
  ts <- function(x) as.numeric(as.POSIXct(x, tz = "America/New_York")) * 1000
  for (dates in list(c("2024-03-09", "2024-03-10", "2024-03-11"),
                     c("2024-11-02", "2024-11-03", "2024-11-04"))) {
    t <- ts(dates)
    x <- audit_intervals(rep(t[2] - 30000, 2), rep(t[3], 2),
      c("a", "b"), c(90000, 180000))
    y <- audit_appusage_overlap(x, precision = "native_export_ms", tz = "America/New_York")
    hours <- if (dates[1] == "2024-03-09") 23 else 25
    expect_equal(y$daily$endpoint_union, c(30000, hours * 3600000))
    expect_equal(sum(y$daily$stored_duration_sum), sum(x$duration_ms))
    expect_equal(y$pairs$total[1], 2)
    expect_equal(y$daily$segments, c(2, 2))
    expect_equal(y$totals[["endpoint_span_sum"]], sum(x$end_ts_ms - x$start_ts_ms))
  }
})

test_that("stored and geometry scopes explain unknown precision and excluded records", {
  x <- audit_intervals(c(4, NA, 2, 1), c(4, 2, 1, 4), duration = c(5, 1, 2, 4))
  expect_identical(audit_appusage_overlap(x)$status, "no_eligible_intervals")
  y <- audit_appusage_overlap(x, precision = "display_minute", strategy = "stored_sum")
  expect_equal(y$row_status, c("same_minute_positive_stored", "missing_endpoints",
    "inverted_endpoints", "eligible"))
  expect_equal(y$view, 12)
  expect_equal(y$totals[["stored_duration_sum"]], 4)
  expect_equal(sum(geometry(x[FALSE, ])$pairs$total), 0)
  x$duration_ms[4] <- NA_real_
  expect_true(is.na(geometry(x)$totals[["stored_duration_sum"]]))
})

test_that("mapped local minutes retain display precision and unresolved midnight/DST", {
  x <- data.frame(day = c("2024-10-08", "2024-10-08", "2024-11-03", "2024-03-10"),
    from = c("23:59", "00:01", "01:35", "02:00"), to = c("00:01", "00:01", "02:00", "03:00"),
    next_day = c("2024-10-09", "2024-10-08", "2024-11-03", "2024-03-10"),
    App_UUID = "org.example.app", seconds = 33, name = "Demo")
  columns <- c(package_name = "App_UUID", app_name = "name", start_time = "from",
    end_time = "to", date = "day", duration_seconds = "seconds")
  y <- audit_appusage_overlap(x, columns = columns, tz = "America/New_York")
  expect_equal(y$precision, "display_minute")
  expect_equal(y$row_status, c("cross_midnight_undetermined", "same_minute_positive_stored",
    "missing_endpoints", "missing_endpoints"))
  y <- audit_appusage_overlap(x, columns = c(columns, end_date = "next_day"), tz = "America/New_York")
  expect_equal(y$daily$endpoint_union, c(60000, 60000))
  expect_equal(y$daily$stored_duration_sum, c(16500, 16500))
  expect_equal(y$selected_stored_sum_ms, 132000)
  x$seconds <- factor(x$seconds)
  expect_equal(audit_appusage_overlap(x, columns = columns)$selected_stored_sum_ms, 132000)
  expect_error(audit_appusage_overlap(x, columns = columns, precision = "native_export_ms"),
    "display_minute")
})

test_that("large counts survive daily QC, aggregation, report projection and anomaly sums", {
  n <- 65537L
  x <- audit_intervals(rep(1, n), rep(2, n), rep(c("a", "b"), length.out = n))
  x$episode_source <- "line"
  x$is_collection_app <- FALSE
  exact <- 2147516416
  y <- geometry(x)
  expect_identical(y$pairs$total[1], exact)
  old <- appusage_line_overlap_qc(x, appusage_source_qc_config(tz = "UTC"))
  expect_identical(old$daily[[1]]$overlap_count, exact)
  expect_identical(old$overlap_count, exact)
  expect_equal(old$cross_package_overlap_count, 32769 * 32768)
  m <- list(source_anomaly_qc = list(line_foreground_overlap = old))
  expect_identical(appusage_source_qc_summary_from_metadata(m)$line_overlap_count, exact)
  total <- appusage_add_anomaly(appusage_empty_anomaly_qc(), "episode", "overlap", "critical", exact)
  expect_identical(total$n_anomalies_by_severity$critical, exact)
  total <- appusage_add_anomaly(total, "episode", "overlap", "critical", 1L)
  expect_identical(total$n_anomalies_by_type$overlap, exact + 1)
  expect_equal(sum(geometry(x[1:65536, ])$pairs$total), 2147450880)
})

test_that("day denominators separate recorded, empty, absent and damaged evidence", {
  d <- as.Date("2024-10-01") + 0:7
  blocks <- data.frame(component = "line", date = as.character(d[c(2,3,4,5,5,6,7)]),
    parsed_rows = c(1,0,1,1,0,2,1),
    state = c("recorded","explicit_empty","recorded","recorded","partial","recorded","recorded"))
  meta <- list(day_coverage = list(version = 1L, blocks = blocks))
  rows <- data.frame(date = d[c(2,4,5,6,6,7)], duration_ms = c(0,9,20,4,NA,NA),
    activity_type = c("foreground","background", "foreground","foreground","foreground","foreground"),
    package_name = "a")
  run <- function(s, ...) audit_appusage_days(rows, meta, range(d), s, ...)
  observed <- run("observed_complete")
  expect_equal(observed$calendar$coverage,
    c("no_block","recorded","explicit_empty","recorded","partial","recorded","recorded","no_block"))
  expect_equal(observed$calendar$duration_status[c(2,4,5,6,7)],
    c("complete","complete","partial","partial","unavailable"))
  expect_equal(observed$denominator_days, 2)
  expect_equal(run("zero_internal")$denominator_days, 3)
  expect_equal(run("zero_boundary")$denominator_days, 4)
  expect_equal(run("zero_window")$denominator_days, 5)
  expect_equal(run("exclude_observed_edges")$denominator_days, 1)
  expect_equal(audit_appusage_days(rows[2, ])$denominator_days, 0)
  subset <- rows[-5, ]
  expect_equal(audit_appusage_days(subset, meta, range(d))$calendar$duration_status[6], "partial")
  rows$activity_type[1] <- NA_character_
  expect_equal(run("zero_window")$calendar$reason[2], "unavailable")
  expect_equal(audit_appusage_days(rows[1, ], window = range(d), strategy = "zero_window")$denominator_days, 0)
  expect_true(is.na(audit_appusage_days(rows[1, ], strategy = "exclude_observed_edges")$mean_ms))
  other <- list(daily = transform(rows, source_export_type = "line"))
  expect_equal(audit_appusage_days(other, component = "app", window = range(d))$denominator_days, 0)
  blocks$date[2] <- NA_character_
  meta$day_coverage$blocks <- blocks
  expect_false(any(run("zero_window")$calendar$reason == "zero_window"))
})
