library(testthat)
library(appusageR)

run_appusage_tests <- function() {
  if (identical(Sys.getenv("NOT_CRAN"), "true")) return(test_check("appusageR"))
  # Keep this fixture around the entire installed-package test run, rather than
  # a helper's shorter lifetime. Policy/fallback tests still provide their own
  # explicit inputs; test-memory-probe.R also exercises the saved real detector.
  probe <- function() list(
    detected_total_memory_bytes = 64 * 1024^3,
    detected_free_memory_bytes = 32 * 1024^3,
    memory_detected = TRUE,
    memory_source = "synthetic_cran_test_fixture"
  )
  attr(probe, "real_probe") <- getFromNamespace("appusage_detect_memory_info", "appusageR")
  with_mocked_bindings(test_check("appusageR"),
    appusage_detect_memory_info = probe, .package = "appusageR")
}
run_appusage_tests()
