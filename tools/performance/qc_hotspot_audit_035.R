# Diagnostic-only QC profiling. Does not modify frozen libraries or campaign outputs.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 3L)
root <- normalizePath(args[[1L]], winslash = "/", mustWork = TRUE)
role <- match.arg(args[[2L]], c("baseline", "candidate"))
mode <- match.arg(args[[3L]], c("synthetic", sprintf("raw%02d", 1:5)))
stopifnot(mode == "synthetic" || role == "candidate")
state <- jsonlite::read_json(file.path(root, "campaign/campaign-status.json"))
stopifnot(state$phase == "waiting_for_baseline_and_candidate_readiness")
lib <- file.path(root, paste0(role, "-lib"))
.libPaths(c(lib, .libPaths()))
library(appusageR)
stopifnot(normalizePath(find.package("appusageR"), winslash = "/") == paste0(lib, "/appusageR"))
ns <- asNamespace("appusageR")
fun <- function(name) get(name, ns)
out <- file.path(root, "qc-audit", paste(mode, role, sep = "-"))
stopifnot(!dir.exists(out))
dir.create(out, recursive = TRUE)
profile <- function(expr) {
  path <- file.path(out, "Rprof.out")
  gc()
  started <- proc.time()
  Rprof(path, interval = 0.01, memory.profiling = TRUE, gc.profiling = TRUE)
  value <- tryCatch(force(expr), finally = Rprof(NULL))
  elapsed <- proc.time() - started
  p <- summaryRprof(path, memory = "both")
  write.csv(p$by.total, file.path(out, "profile-total.csv"))
  write.csv(p$by.self, file.path(out, "profile-self.csv"))
  saveRDS(value, file.path(out, "private-result.rds"))
  list(value = value, wall_sec = unname(elapsed[["elapsed"]]),
    cpu_sec = unname(sum(elapsed[c("user.self", "sys.self")])),
    sampled_sec = p$sampling.time)
}

if (mode == "synthetic") {
  n <- 32L
  start <- as.numeric(as.POSIXct("2024-10-07 12:00:00", tz = "Asia/Shanghai"))*1000 + seq_len(n)*60000
  x <- data.frame(start_ts_ms = start, end_ts_ms = start + 30000,
    duration_ms = rep(30000, n), package_name = "com.example.synthetic")
  result <- profile(fun("appusage_source_qc_interval_segments")(x, "Asia/Shanghai"))
  .qc_olson_calls <- 0L
  .qc_zone_calls <- 0L
  trace("OlsonNames", where = baseenv(), print = FALSE,
    tracer = quote(.GlobalEnv$.qc_olson_calls <- .GlobalEnv$.qc_olson_calls + 1L))
  trace("appusage_resolve_timezone", where = ns, print = FALSE,
    tracer = quote(.GlobalEnv$.qc_zone_calls <- .GlobalEnv$.qc_zone_calls + 1L))
  invisible(fun("appusage_source_qc_interval_segments")(x[seq_len(4L), ], "Asia/Shanghai"))
  untrace("OlsonNames", where = baseenv())
  untrace("appusage_resolve_timezone", where = ns)
  evidence <- list(mode = mode, role = role, rows = n, wall_sec = result$wall_sec,
    cpu_sec = result$cpu_sec, sampled_sec = result$sampled_sec,
    four_row_timezone_validations = .qc_zone_calls, four_row_olson_calls = .qc_olson_calls)
} else {
  source <- file.path(root, "golden", mode)
  stopifnot(jsonlite::read_json(file.path(source, "result.json"))$status == "success")
  first <- readRDS(file.path(source, "private_first_summary.rds"))
  second <- readRDS(file.path(source, "private_second_summary.rds"))
  metadata_file <- list.files(source, pattern = "_proc-2[.]json$", recursive = TRUE, full.names = TRUE)
  stopifnot(length(metadata_file) == 1L)
  metadata <- jsonlite::read_json(metadata_file)
  first_metadata <- jsonlite::read_json(first$metadata_file[[1L]])
  data_env <- new.env(parent = emptyenv())
  stopifnot(identical(load(second$second_level_data_file[[1L]], data_env), "data"))
  callback <- function(stage, status, details = NULL) {
    cat(stage, status, format(Sys.time(), "%H:%M:%OS3"), "\n")
  }
  result <- profile(fun("run_qc_for_second_level_data")(
    data = data_env$data, second_level_rda = second$second_level_data_file[[1L]],
    participant_id = first$participant_id[[1L]], require_all_weekdays = TRUE,
    min_nonempty_days = 7, use_all_apps_row = FALSE, include_collection_app = TRUE,
    drop_likely_total_all_rows = TRUE, all_row_tolerance = 0.10,
    metadata = first_metadata, stage_callback = callback))
  value <- result$value
  new <- list(anomaly_qc = value$anomaly_qc, qc = value$qc, counts = value$counts)
  serialized <- file.path(out, "private-qc.json")
  fun("write_metadata_json")(new, serialized)
  new <- jsonlite::read_json(serialized)
  old <- list(anomaly_qc = metadata$anomaly_qc, qc = metadata$qc[names(new$qc)], counts = metadata$counts[names(new$counts)])
  # Only these two generated timestamps are normalized for this QC-only comparison.
  old$anomaly_qc$created_at <- new$anomaly_qc$created_at <- "<GENERATED_TIME>"
  old$anomaly_qc$source_anomaly_qc$created_at <- new$anomaly_qc$source_anomaly_qc$created_at <- "<GENERATED_TIME>"
  equal <- vapply(names(old), function(k) identical(old[[k]], new[[k]]), logical(1))
  saveRDS(list(old = old, new = new), file.path(out, "private-comparison.rds"))
  evidence <- list(mode = mode, role = role, wall_sec = result$wall_sec,
    cpu_sec = result$cpu_sec, sampled_sec = result$sampled_sec,
    event_rows = nrow(data_env$data$event),
    episode_rows = nrow(data_env$data$episode), daily_rows = nrow(data_env$data$daily),
    exact_published_qc_fields = as.list(equal),
    old_recorded_inline_qc_sec = metadata$second_level$profiling$inline_qc_elapsed_sec)
}
jsonlite::write_json(evidence, file.path(out, "evidence.json"), auto_unbox = TRUE, pretty = TRUE)
print(evidence)
