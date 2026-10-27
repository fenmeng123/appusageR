test_that("duplicate-source transactions work below the legacy Windows path limit", {
  root <- tempfile("cache-path-")
  root <- paste0(root, paste(rep("x", max(0L, 100L - nchar(root))), collapse = ""))
  dir.create(root, recursive = TRUE)
  files <- file.path(root, c("AppUsage_line_2024_2_1_3_4_5.txt",
                            "AppUsage_line_2024_2_2_3_4_5.txt"))
  file.copy(test_path("fixtures", "line_sample.txt"), files)
  first <- read_appusage_batch(files, ids = rep("same-participant", 2),
    output_dir = root, project_name = "DuplicateSources", project_id = "B1", progress = FALSE)
  second_dir <- file.path(first$project_root[[1]], "proclevel-2")
  paths <- second_level_expected_paths(first$data_file[[1]], second_dir)
  transaction <- appusage_second_level_transaction_paths(paths$rda_file, paths$json_file)
  breadcrumb <- appusage_second_level_breadcrumb_path(paths$rda_file)
  breadcrumb_transaction <- appusage_compact_transaction_paths(breadcrumb,
    appusage_second_level_transaction_id())
  expect_true(all(nchar(unlist(c(transaction, breadcrumb_transaction))) < 260L))

  expect_no_warning(second <- write_second_level_batch(first, second_dir, progress = FALSE))
  expect_identical(second$second_level_status, rep("success", 2))
  expect_identical(second$source_record_key, first$source_record_key)
  expect_length(unique(second$second_level_data_file), 2L)
  pair <- c(second$second_level_data_file, second$second_level_metadata_file)
  hashes <- tools::md5sum(pair)
  expect_no_warning(resumed <- write_second_level_batch(first, second_dir,
    resume = TRUE, overwrite = FALSE, progress = FALSE))
  expect_identical(resumed$second_level_status, rep("success", 2))
  expect_identical(tools::md5sum(pair), hashes)
})

test_that("compact transaction cleanup preserves other owners and legacy recovery", {
  root <- tempfile("transaction-owners-"); dir.create(root)
  rda <- file.path(root, "sub-one_type-line_proc-2.rda")
  json <- sub("[.]rda$", ".json", rda)
  own <- appusage_second_level_transaction_paths(rda, json, token = "test-transaction")
  other_rda <- sub("sub-one", "sub-two", rda)
  other <- appusage_second_level_transaction_paths(other_rda,
    sub("[.]rda$", ".json", other_rda), token = "test-transaction")
  legacy <- file.path(root, paste0(".", basename(rda), ".appusage-tmp-old"))
  for (path in c(unlist(own), unlist(other), legacy)) writeLines("evidence", path)
  appusage_cleanup_second_level_transaction_artifacts(rda, json)
  expect_false(any(file.exists(c(unlist(own), legacy))))
  expect_true(all(file.exists(unlist(other))))

  breadcrumb <- appusage_second_level_breadcrumb_path(rda)
  tmp <- appusage_compact_transaction_paths(breadcrumb, "test-transaction")
  legacy_tmp <- paste0(breadcrumb, ".tmp-old")
  writeLines("durable progress", breadcrumb)
  for (path in c(unlist(tmp), legacy_tmp)) writeLines("partial", path)
  appusage_cleanup_second_level_breadcrumb_temps(breadcrumb)
  expect_false(any(file.exists(c(unlist(tmp), legacy_tmp))))
  expect_identical(readLines(breadcrumb), "durable progress")
  expect_true(all(file.exists(unlist(other))))
})

test_that("a failed status write retains the original second-level error", {
  root <- tempfile("status-error-"); dir.create(root)
  first <- read_appusage_batch(test_path("fixtures", "line_sample.txt"), ids = "p",
    output_dir = root, project_name = "Error", project_id = "1", progress = FALSE)
  failing_writer <- write_second_level_appusage
  body(failing_writer) <- quote(stop("original processing failure"))
  local_mocked_bindings(
    write_second_level_appusage = failing_writer,
    write_second_level_status_metadata = function(...) stop("status write failure")
  )
  error <- tryCatch(write_second_level_batch(first, progress = FALSE), error = identity)
  expect_s3_class(error, "appusage_second_level_status_write_error")
  expect_identical(conditionMessage(error$parent), "original processing failure")
  expect_identical(conditionMessage(error$metadata_error), "status write failure")
  expect_match(conditionMessage(error), "original processing failure.*status write failure")
  expect_match(error$metadata_file, "proc-2[.]json$")
})
