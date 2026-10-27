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
