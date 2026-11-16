test_that("the real memory detector returns measured or explicit unavailable values", {
  memory <- appusage_test_real_memory_probe()
  expect_named(memory, names(appusage_test_memory_fixture))
  expect_type(memory$memory_detected, "logical")
  expect_length(memory$memory_detected, 1L)
  expect_false(is.na(memory$memory_detected))
  if (isTRUE(memory$memory_detected)) {
    expect_true(is.finite(memory$detected_total_memory_bytes))
    expect_gt(memory$detected_total_memory_bytes, 0)
    expect_true(is.finite(memory$detected_free_memory_bytes))
    expect_gte(memory$detected_free_memory_bytes, 0)
    expect_lte(memory$detected_free_memory_bytes, memory$detected_total_memory_bytes)
  } else {
    expect_true(is.na(memory$detected_total_memory_bytes))
    expect_true(is.na(memory$detected_free_memory_bytes))
  }
  expect_false(identical(memory$memory_source, "synthetic_cran_test_fixture"))
})

test_that("CRAN's synthetic host does not replace local acceptance memory detection", {
  if (appusage_test_uses_memory_fixture) {
    expect_identical(appusage_detect_memory_info(), appusage_test_memory_fixture)
  } else {
    expect_identical(appusage_detect_memory_info, appusage_test_real_memory_probe)
  }
})

test_that("serial worker selection needs no host-memory subprocess", {
  local_mocked_bindings(appusage_detect_memory_info = function()
    stop("Serial selection must not query host memory"), .package = "appusageR")
  decision <- appusage_first_level_worker_decision(FALSE, 8L, character(),
    available_cores = 16L)
  expect_identical(decision$selected_workers, 1L)
  expect_identical(decision$cap_reason, "serial_parallel_false")
  expect_identical(decision$memory_source, "not_requested_serial")
  expect_false(decision$memory_detected)
  expect_true(is.na(decision$detected_total_memory_bytes))
  expect_true(is.na(decision$detected_free_memory_bytes))

  # Explicit caller-supplied measurements retain their original provenance.
  measured <- list(detected_total_memory_bytes = 16 * 1024^3,
    detected_free_memory_bytes = 2 * 1024^3, memory_detected = TRUE,
    memory_source = "explicit_test_measurement")
  supplied <- appusage_first_level_worker_decision(FALSE, 8L, character(),
    available_cores = 16L, memory_info = measured)
  expect_identical(supplied[names(measured)], measured)
})

test_that("parallel worker decisions always refresh memory and retain low-memory caps", {
  calls <- 0L
  local_mocked_bindings(appusage_detect_memory_info = function() {
    calls <<- calls + 1L
    list(detected_total_memory_bytes = 64 * 1024^3,
      detected_free_memory_bytes = if (calls == 1L) 32 * 1024^3 else 2 * 1024^3,
      memory_detected = TRUE, memory_source = "changing_test_measurement")
  }, .package = "appusageR")
  sizes <- list(total_source_size_bytes = 120, max_source_size_bytes = 10,
    average_source_size_bytes = 10, n_sized_files = 12L)
  select <- function() appusage_first_level_worker_decision(TRUE, 12L,
    rep("synthetic.txt", 12L), available_cores = 16L, size_info = sizes)
  ample <- select()
  low <- select()
  expect_identical(calls, 2L)
  expect_identical(ample$selected_workers, 12L)
  expect_identical(low$selected_workers, 4L)
  expect_identical(low$cap_reason, "low_free_memory")
  expect_identical(low$detected_free_memory_bytes, 2 * 1024^3)
})
