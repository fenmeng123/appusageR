Sys.setenv(R_USER_CACHE_DIR = "E:/mSens_AppUsage/.r-cache", XDG_CACHE_HOME = "E:/mSens_AppUsage/.cache")
devtools::document()
results <- devtools::test(reporter = "summary")
saveRDS(results, "E:/mSens_AppUsage/reference/workflow_test/performance_035/test-results.rds")
if (any(vapply(results, function(x) any(vapply(x$results,
  function(r) inherits(r, c("expectation_failure", "expectation_error")), logical(1))), logical(1)))) quit(status = 1L)
