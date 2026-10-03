args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(if (length(args)) args[[1L]] else ".", quiet = TRUE)
candidate <- asNamespace("appusageR")
baseline <- new.env(parent = globalenv())
root <- "E:/mSens_AppUsage/reference/workflow_test/performance_035"
for (path in list.files(file.path(root, "baseline-source/R"), full.names = TRUE, pattern = "[.]R$")) sys.source(path, baseline)
records <- list()
capture <- function(f, args) {
  warnings <- character()
  value <- tryCatch(withCallingHandlers(do.call(f, args), warning = function(w) {
    warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
  }), error = function(e) list(error_class = class(e), error_message = conditionMessage(e)))
  list(value = value, warnings = warnings)
}
compare <- function(name, args, label) {
  old <- capture(get(name, baseline), args)
  new <- capture(get(name, candidate), args)
  equal <- identical(old, new)
  records[[length(records) + 1L]] <<- data.frame(function_name = name, case = label, identical = equal)
  if (!equal) {
    saveRDS(list(old = old, new = new, args = args), file.path(root, "differential_failure.rds"))
    print(all.equal(old, new))
    stop(name, ": ", label)
  }
  new$value
}
for (type in c("line", "meta", "day", "app")) {
  lines <- readLines(file.path("tests/testthat/fixtures", paste0(type, "_sample.txt")), encoding = "UTF-8", warn = FALSE)
  variants <- list(original = lines, repeated = rep(lines, 2L), leading = paste0(",,", lines),
    wide = c(lines, paste(rep("extra", 1000L), collapse = ",")), blank = c("", lines, "", ",,"))
  for (label in names(variants)) {
    x <- variants[[label]]
    parsed <- compare(paste0("parse_", type), list(x = x, input = "lines", participant_id = "synthetic"), label)
    compare("appusage_source_preflight", list(x = x, input = "lines"), paste(type, label))
    if (label == "original") {
      data <- if (type == "meta") parsed else setNames(list(parsed), type)
      compare("make_second_level_appusage", list(data = data), type)
    }
  }
}
set.seed(35035)
for (tz in c("Asia/Shanghai", "UTC", "America/New_York")) {
  origin <- as.numeric(as.POSIXct("2024-03-09 23:30:00", tz = tz)) * 1000
  events <- data.frame(package_name = sample(c("org.a", "org.b", NA_character_), 35, TRUE),
    app_name = sample(c("a", "b", ""), 35, TRUE), class_name = sample(c("main", "other"), 35, TRUE),
    event_type = sample(c(1, 2, 23, 26, 27, 7, NA), 35, TRUE),
    event_ts_ms = origin + sample(0:15, 35, TRUE) * 1000,
    event_duration_ms = sample(c(NA, 0, -1, 1500.25), 35, TRUE),
    parse_warning = sample(c("", NA, "w1", "w2"), 35, TRUE))
  for (pairing in c("package", "package_class")) {
    for (merge in c(FALSE, TRUE)) compare("reconstruct_meta_episodes", list(events = events,
      tz = tz, pairing = pairing, merge_contiguous = merge), paste(tz, pairing, merge))
  }
  starts <- origin + c(0, 3600000, 7200000, 86400000)
  x <- data.frame(start_ts_ms = starts, end_ts_ms = starts + c(3600000, 10.125, 90000000, 3),
    duration_ms = c(1350.75, 3.14159, 100001.125, 2), package_name = c("a", NA, "b", "a"))
  compare("appusage_source_qc_interval_segments", list(x = x, tz = tz), tz)
}
label <- if (length(args) > 1L) args[[2L]] else "structure_differential"
write.csv(do.call(rbind, records), file.path(root, paste0(label, ".csv")), row.names = FALSE)
cat(length(records), "exact differential cases passed\n")
