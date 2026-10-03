# Private source manifest is supplied by the caller; no participant data here.
args <- commandArgs(trailingOnly = TRUE)
stopifnot(length(args) == 5L)
lib <- normalizePath(args[[1]], winslash = "/", mustWork = TRUE)
manifest <- read.csv(args[[2]], check.names = FALSE, fileEncoding = "UTF-8")
id <- args[[3]]
run <- args[[4]]
role <- args[[5]]
stopifnot(!dir.exists(run))
dir.create(run, recursive = TRUE)
.libPaths(c(lib, .libPaths()))
library(appusageR)
stopifnot(as.character(packageVersion("appusageR")) == if (role == "baseline") "0.3.4" else "0.3.5")
stopifnot(identical(normalizePath(find.package("appusageR"), winslash = "/"),
                    normalizePath(file.path(lib, "appusageR"), winslash = "/")))
stopifnot(!is.na(Sys.setlocale("LC_ALL", "Chinese_China.utf8")))
entry <- manifest[manifest$benchmark_id == id, , drop = FALSE]
stopifnot(nrow(entry) == 1L, file.info(entry$source_file)$size == entry$size_bytes,
          unname(tools::md5sum(entry$source_file)) == entry$source_md5,
          as.character(packageVersion("stringi")) == "1.8.7",
          stringi::stri_info()$ICU.version == "74.1")
dependency_lock <- Sys.getenv("APPUSAGER_BENCHMARK_DEPENDENCY_LOCK", "")
dependencies <- if (nzchar(dependency_lock)) {
  expected <- unlist(jsonlite::read_json(dependency_lock, simplifyVector = TRUE), use.names = TRUE)
  actual <- vapply(names(expected), function(p) as.character(packageVersion(p)), character(1))
  stopifnot(identical(actual, expected))
  actual
} else NULL
res <- list(id = id, role = role, status = "running", stage = "first",
  package = as.character(packageVersion("appusageR")), library = lib,
  source_md5 = entry$source_md5, source_bytes = entry$size_bytes,
  locale = Sys.getlocale(), timezone = "Asia/Shanghai", workers = 1L,
  provenance = appusageR:::appusage_build_run_provenance(),
  dependency_versions = dependencies, r_runtime = R.version.string,
  cache_policy = "Source hashed before APIs; OS cache not flushed")
persist <- function() jsonlite::write_json(res, file.path(run, "result.json"),
  auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
persist()
writeLines(capture.output(sessionInfo()), file.path(run, "sessionInfo.txt"))
ok <- tryCatch({
  total <- proc.time()
  t0 <- proc.time()
  first <- read_appusage_batch(entry$source_file, ids = id, output_dir = run,
    project_name = "performance_035", project_id = id, tz = "Asia/Shanghai",
    progress = FALSE, parallel = FALSE, n_cores = 1L, overwrite = FALSE,
    retry_memory_allocation = FALSE)
  res$first_wall_sec <- unname((proc.time() - t0)[["elapsed"]])
  saveRDS(first, file.path(run, "private_first_summary.rds"))
  res$first_status <- first$status[[1]]
  res$stage <- "second"
  persist()
  stopifnot(identical(first$status[[1]], "success"))
  t0 <- proc.time()
  second <- write_second_level_batch(first, output_dir = file.path(run, "proclevel-2"),
    tz = "Asia/Shanghai", overwrite = FALSE, parallel = FALSE, n_cores = 1L,
    progress = FALSE, inline_qc = TRUE, meta_daily_source = "summary")
  res$second_wall_sec <- unname((proc.time() - t0)[["elapsed"]])
  res$total_wall_sec <- unname((proc.time() - total)[["elapsed"]])
  res$total_cpu_sec <- sum((proc.time() - total)[c("user.self", "sys.self")])
  saveRDS(second, file.path(run, "private_second_summary.rds"))
  stopifnot(nrow(second) == 1L, identical(second$status[[1L]], "success"))
  for (path in c(first$data_file, second$second_level_data_file)) {
    e <- new.env(parent = emptyenv())
    stopifnot(identical(load(path, e), "data"))
  }
  stopifnot(unname(tools::md5sum(entry$source_file)) == entry$source_md5)
  res$status <- "success"
  res$stage <- "complete"
  TRUE
}, error = function(e) {
  res$status <<- "error"
  res$error_class <<- class(e)
  res$error_message <<- conditionMessage(e)
  FALSE
})
persist()
quit(save = "no", status = if (ok) 0L else 1L)
