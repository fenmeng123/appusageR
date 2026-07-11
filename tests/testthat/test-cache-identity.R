cache_identity_project_fixture <- function() {
  root <- tempfile("appusage-cache-identity-")
  dir.create(root, recursive = TRUE)
  source <- file.path(root, "AppUsage_line_2024_1_2_3_4_5.txt")
  file.copy(testthat::test_path("fixtures", "line_sample.txt"), source)
  first <- read_appusage_batch(
    source,
    ids = "same-participant",
    output_dir = root,
    project_name = "Identity",
    project_id = "B1",
    progress = FALSE
  )
  project_root <- unique(first$project_root)[[1]]
  second_dir <- file.path(project_root, "proclevel-2")
  second <- write_second_level_batch(
    first,
    output_dir = second_dir,
    progress = FALSE
  )
  list(root = root, project_root = project_root, first = first,
       second = second, second_dir = second_dir)
}

test_that("source record keys are stable, order independent, and distinct", {
  root <- tempfile("appusage-source-key-")
  dir.create(root)
  paths <- file.path(root, c(
    "AppUsage_line_2024_1_2_3_4_5.txt",
    "AppUsage_line_2024_1_3_3_4_5.txt"
  ))
  file.copy(testthat::test_path("fixtures", "line_sample.txt"), paths)
  id_info <- resolve_participant_ids(paths, ids = rep("p1", 2), input = "file")
  keys <- vapply(seq_along(paths), function(i) {
    appusage_source_identity(
      paths[[i]], "file", id_info[i, , drop = FALSE], "p1", "line"
    )$source_record_key
  }, character(1))
  reverse_keys <- vapply(rev(seq_along(paths)), function(i) {
    appusage_source_identity(
      paths[[i]], "file", id_info[i, , drop = FALSE], "p1", "line"
    )$source_record_key
  }, character(1))

  expect_length(unique(keys), 2L)
  expect_identical(keys, rev(reverse_keys))
  expect_match(keys[[1]], "sub=p1=type=line=time=20240102030405=fp=")

  summary <- read_appusage_batch(
    paths,
    ids = rep("p1", 2),
    output_dir = root,
    project_name = "DuplicateType",
    project_id = "B1",
    progress = FALSE
  )
  expect_length(unique(summary$source_record_key), 2L)
  expect_length(unique(summary$data_file), 2L)
  expect_true(all(grepl("_src-", basename(summary$data_file), fixed = TRUE)))

  detected_cores <- parallel::detectCores(logical = TRUE)
  if (!is.na(detected_cores) && detected_cores >= 2L) {
    serial <- read_appusage_batch(
      paths, ids = rep("p1", 2), progress = FALSE
    )
    parallel_result <- read_appusage_batch(
      rev(paths), ids = rep("p1", 2), progress = FALSE,
      parallel = TRUE, n_cores = 2
    )
    serial_keys <- stats::setNames(serial$source_record_key, basename(serial$source_file))
    parallel_keys <- stats::setNames(
      parallel_result$source_record_key,
      basename(parallel_result$source_file)
    )
    expect_identical(
      serial_keys[sort(names(serial_keys))],
      parallel_keys[sort(names(parallel_keys))]
    )
  }
})

test_that("same participant and type publish distinct proc-2 pairs and resume", {
  root <- tempfile("appusage-duplicate-source-workflow-")
  dir.create(root)
  paths <- file.path(root, c(
    "AppUsage_line_2024_2_1_3_4_5.txt",
    "AppUsage_line_2024_2_2_3_4_5.txt"
  ))
  file.copy(testthat::test_path("fixtures", "line_sample.txt"), paths)

  first <- read_appusage_batch(
    paths,
    ids = rep("same-participant", 2),
    output_dir = root,
    project_name = "DuplicateSources",
    project_id = "B1",
    progress = FALSE
  )
  expect_equal(nrow(first), 2L)
  expect_length(unique(first$source_record_key), 2L)
  expect_length(unique(first$data_file), 2L)
  expect_length(unique(first$metadata_file), 2L)
  expect_true(all(file.exists(first$data_file)))
  expect_true(all(file.exists(first$metadata_file)))

  second_dir <- file.path(unique(first$project_root)[[1]], "proclevel-2")
  second <- write_second_level_batch(
    first,
    output_dir = second_dir,
    progress = FALSE
  )
  expect_identical(second$second_level_status, c("success", "success"))
  expect_identical(second$source_record_key, first$source_record_key)
  expect_length(unique(second$second_level_data_file), 2L)
  expect_length(unique(second$second_level_metadata_file), 2L)
  expect_true(all(file.exists(second$second_level_data_file)))
  expect_true(all(file.exists(second$second_level_metadata_file)))

  for (i in seq_len(nrow(second))) {
    metadata <- jsonlite::read_json(
      second$second_level_metadata_file[[i]],
      simplifyVector = FALSE
    )
    expect_true(appusage_normalized_paths_equal(
      metadata$outputs$second_level_rda,
      second$second_level_data_file[[i]]
    ))
    expect_true(appusage_normalized_paths_equal(
      metadata$outputs$metadata_json,
      second$second_level_metadata_file[[i]]
    ))
    expect_identical(
      metadata$identity$source_record_key,
      first$source_record_key[[i]]
    )
    expect_identical(
      metadata$source$source_fingerprint,
      first$source_fingerprint[[i]]
    )
  }

  resumed <- write_second_level_batch(
    first,
    output_dir = second_dir,
    resume = TRUE,
    overwrite = FALSE,
    progress = FALSE
  )
  expect_equal(nrow(resumed), nrow(first))
  expect_identical(resumed$source_record_key, first$source_record_key)
  expect_identical(resumed$second_level_status, c("success", "success"))
  expect_identical(
    resumed$second_level_metadata_file,
    second$second_level_metadata_file
  )
})

test_that("second-level pair state classifies durable and partial states", {
  fixture <- cache_identity_project_fixture()
  first <- fixture$first
  status <- second_level_existing_cache_status(
    first$data_file[[1]], fixture$second_dir, first, 1L
  )
  expect_identical(status$pair_state, "complete_valid_pair")

  unlink(status$json_file)
  status <- second_level_existing_cache_status(
    first$data_file[[1]], fixture$second_dir, first, 1L
  )
  expect_identical(status$pair_state, "rda_only_incomplete")

  write_second_level_appusage(
    first$data_file[[1]], fixture$second_dir, overwrite = TRUE
  )
  paths <- second_level_expected_paths(first$data_file[[1]], fixture$second_dir)
  metadata <- jsonlite::read_json(paths$json_file, simplifyVector = FALSE)
  unlink(paths$rda_file)
  status <- second_level_existing_cache_status(
    first$data_file[[1]], fixture$second_dir, first, 1L
  )
  expect_identical(status$pair_state, "json_success_missing_rda")

  metadata$processing$second_level_status <- "error"
  jsonlite::write_json(metadata, paths$json_file, auto_unbox = TRUE, na = "null")
  status <- second_level_existing_cache_status(
    first$data_file[[1]], fixture$second_dir, first, 1L
  )
  expect_identical(status$pair_state, "json_only_error")

  writeLines("{not-json", paths$json_file)
  status <- second_level_existing_cache_status(
    first$data_file[[1]], fixture$second_dir, first, 1L
  )
  expect_identical(status$pair_state, "corrupt_json")
})

test_that("pair ownership rejects stale fingerprints and true key collisions", {
  fixture <- cache_identity_project_fixture()
  first <- fixture$first
  paths <- second_level_expected_paths(first$data_file[[1]], fixture$second_dir)
  metadata <- jsonlite::read_json(paths$json_file, simplifyVector = FALSE)
  metadata$source$source_fingerprint <- "stale-fingerprint"
  jsonlite::write_json(metadata, paths$json_file, auto_unbox = TRUE, na = "null")
  status <- second_level_existing_cache_status(
    first$data_file[[1]], fixture$second_dir, first, 1L
  )
  expect_identical(status$pair_state, "stale_source_identity")
  expect_identical(status$reason, "source_fingerprint_mismatch")

  metadata <- jsonlite::read_json(paths$json_file, simplifyVector = FALSE)
  metadata$source$source_fingerprint <- first$source_fingerprint[[1]]
  metadata$identity$source_record_key <- "different-source-record-key"
  jsonlite::write_json(metadata, paths$json_file, auto_unbox = TRUE, na = "null")
  key_mismatch <- second_level_existing_cache_status(
    first$data_file[[1]], fixture$second_dir, first, 1L
  )
  expect_identical(key_mismatch$pair_state, "source_key_collision")
  expect_identical(key_mismatch$reason, "source_record_key_mismatch")

  metadata$identity$source_record_key <- first$source_record_key[[1]]
  metadata$outputs$second_level_rda <- file.path(fixture$second_dir, "stale.rda")
  jsonlite::write_json(metadata, paths$json_file, auto_unbox = TRUE, na = "null")
  stale_path <- second_level_existing_cache_status(
    first$data_file[[1]], fixture$second_dir, first, 1L
  )
  expect_identical(stale_path$pair_state, "stale_source_identity")
  expect_true(stale_path$owned_by_task)
  rebuilt <- write_second_level_batch(
    first, fixture$second_dir, resume = TRUE, progress = FALSE
  )
  expect_identical(rebuilt$second_level_status[[1]], "success")

  duplicated <- dplyr::bind_rows(first, first)
  duplicated$index <- seq_len(nrow(duplicated))
  collision <- second_level_existing_cache_status(
    duplicated$data_file[[1]], fixture$second_dir, duplicated, 1L
  )
  expect_identical(collision$pair_state, "source_key_collision")
  expect_identical(collision$reason, "duplicate_source_record_key")
})

test_that("unambiguous legacy pairs remain readable", {
  fixture <- cache_identity_project_fixture()
  legacy_first_dir <- tempfile("legacy-proc1-")
  legacy_second_dir <- tempfile("legacy-proc2-")
  dir.create(legacy_first_dir)
  dir.create(legacy_second_dir)
  data <- load_appusage_data_object(fixture$first$data_file[[1]])
  legacy_first <- file.path(
    legacy_first_dir,
    build_appusage_filename("legacy", "line", proc = 1, extension = "rda")
  )
  save(data, file = legacy_first)
  write_second_level_appusage(
    legacy_first, legacy_second_dir, inline_qc = FALSE
  )
  legacy_summary <- tibble::tibble(
    index = 1L, participant_id = "legacy", detected_type = "line",
    status = "success", data_file = legacy_first
  )
  state <- second_level_existing_cache_status(
    legacy_first, legacy_second_dir, legacy_summary, 1L
  )
  expect_identical(state$pair_state, "legacy_unambiguous_pair")
  expect_identical(state$status, "complete")
})

test_that("resume reconstructs valid success and summary cardinality follows first level", {
  fixture <- cache_identity_project_fixture()
  first <- fixture$first
  failed <- first
  failed$index <- 2L
  failed$status <- "error"
  failed$data_file <- NA_character_
  failed$metadata_file <- NA_character_
  failed$source_record_key <- paste0(first$source_record_key, "-failed")
  failed$source_fingerprint <- paste0(first$source_fingerprint, "failed")
  failed$source_cache_key <- paste0(first$source_cache_key, "-failed")
  failed$error_message <- "synthetic first-level failure"
  combined <- dplyr::bind_rows(first, failed)

  summary <- write_second_level_batch(
    combined,
    output_dir = fixture$second_dir,
    resume = TRUE,
    overwrite = FALSE,
    progress = FALSE
  )
  expect_equal(nrow(summary), nrow(combined))
  expect_identical(summary$source_record_key, combined$source_record_key)
  expect_identical(summary$second_level_status, c("success", "skipped"))
  expect_identical(summary$skip_reason[[2]], "upstream_first_level_error")
})

test_that("metadata-only refresh preserves order and matching fields", {
  fixture <- cache_identity_project_fixture()
  first <- fixture$first
  second <- fixture$second
  old <- second
  old$self_report_match_status <- "matched"
  old <- old[nrow(old):1, , drop = FALSE]

  refreshed <- refresh_second_level_summary_from_metadata(
    first,
    fixture$second_dir,
    rows = list(),
    previous_summary = old
  )
  expect_equal(nrow(refreshed), nrow(first))
  expect_identical(refreshed$source_record_key, first$source_record_key)
  expect_identical(refreshed$self_report_match_status, "matched")
})
