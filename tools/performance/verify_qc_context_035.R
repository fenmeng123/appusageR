# Correctness only: pre-refactor 0.3.5 QC functions, with no performance timing.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 1L)
work <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
devtools::load_all(quiet = TRUE)
ns <- asNamespace("appusageR")
oracle <- new.env(parent = ns)
for (f in c("anomaly_qc.R", "source_anomaly_qc.R", "qc_metadata.R")) {
  sys.source(file.path(work, "candidate-source", "R", f), envir = oracle)
}
normalize <- function(x) {
  x$created_at <- "<TIME>"
  x$source_anomaly_qc$created_at <- "<TIME>"
  x
}
check <- function(data, zone, metadata = NULL) {
  call <- list(data = data, metadata = metadata,
    source_qc_config = list(effective_timezone = zone))
  capture <- function(fun) tryCatch(list(value = normalize(do.call(fun, call))),
    error = function(e) list(error = conditionMessage(e), class = class(e)))
  old <- capture(oracle$qc_appusage_anomalies)
  new <- capture(get("qc_appusage_anomalies", ns))
  if (!identical(old, new)) {
    saveRDS(list(old = old, new = new), file.path(work, "qc-context-mismatch.rds"))
    stop("QC context oracle mismatch")
  }
  invisible(TRUE)
}
set.seed(35035)
n <- 36L
episode <- data.frame(start_ts_ms = 1710046800000 + sample(c(-3600000, 0, 60000, 86400000), n, TRUE),
  duration_ms = sample(c(0, 100, 90000000, NA, -1), n, TRUE),
  package_name = sample(c("p.a", "p.b", NA), n, TRUE),
  app_name = sample(c("a", "b", ""), n, TRUE),
  episode_source = rep(c("line", "meta_events", NA), length.out = n),
  source_export_type = rep(c("line", "meta", "unknown"), length.out = n),
  reconstruction_status = rep(c("complete", "invalid_pair", NA), length.out = n),
  reconstruction_warning = rep(c("timeline_clipped_to_nonpositive", "duration_inferred", NA), length.out = n),
  activity_type = rep(c("foreground", "background", "", NA), length.out = n),
  is_collection_app = rep(c(TRUE, FALSE, NA), length.out = n),
  source_table_date = as.Date("2024-03-10"),
  source_date_timestamp_date_mismatch = rep(c(TRUE, FALSE, NA), length.out = n))
episode$end_ts_ms <- episode$start_ts_ms + episode$duration_ms
event <- data.frame(event_ts_ms = episode$start_ts_ms, package_name = episode$package_name)
daily <- data.frame(date = as.Date("2024-03-10") + 0:3, duration_ms = c(100, NA, -1, 0))
cases <- list(list(), episode, event, daily, list(episode = episode, event = event, daily = daily))
for (i in seq_len(8)) {
  e <- episode
  if (i %% 2 == 0) e$anomaly_cross_date <- rep(TRUE, n)
  if (i %% 3 == 0) e$start_ts_ms[c(1, 3)] <- c(NA, Inf)
  if (i %% 4 == 0) e$end_ts_ms[c(1, 3)] <- c(-Inf, NA)
  if (i %% 5 == 0) e$end_ts_ms <- as.character(e$end_ts_ms)
  if (i %% 6 == 0) e$source_table_date <- as.character(e$source_table_date)
  if (i %% 7 == 0) e <- e[0, ]
  if (i %% 8 == 0) e$duration_ms <- NULL
  cases[[length(cases) + 1L]] <- list(episode = e, event = event, daily = daily)
}
synthetic <- 0L
for (zone in c("Asia/Shanghai", "UTC", "America/New_York")) {
  for (x in cases) {
    check(x, zone, list(native_export_created_at = "2024-04-01"))
    synthetic <- synthetic + 1L
  }
}
real <- character()
for (id in sprintf("raw%02d", 1:4)) {
  source <- file.path(work, "golden", id)
  first <- readRDS(file.path(source, "private_first_summary.rds"))
  second <- readRDS(file.path(source, "private_second_summary.rds"))
  data <- get("load_appusage_data_object", ns)(second$second_level_data_file[[1L]])
  metadata <- jsonlite::read_json(first$metadata_file[[1L]])
  check(data, "Asia/Shanghai", metadata)
  real <- c(real, id)
  cat(id, "exact QC object equality\n")
}
jsonlite::write_json(list(status = "success", synthetic_cases = synthetic,
  real_qc_objects_exact = real, oracle = "frozen pre-addition 0.3.5 QC source",
  normalized = c("created_at", "source_anomaly_qc/created_at"), performance_measured = FALSE),
  file.path(work, "qc-context-equivalence.json"), auto_unbox = TRUE, pretty = TRUE)
cat(synthetic, "synthetic cases exact\n")
