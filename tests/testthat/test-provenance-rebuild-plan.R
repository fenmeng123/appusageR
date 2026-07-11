test_that("implementation provenance fingerprints are deterministic and auditable", {
  sha <- paste(rep("a", 40), collapse = "")
  one <- appusageR:::appusage_build_run_provenance(
    workflow_run_id = "run-fixed", git_sha = sha, git_dirty = TRUE,
    package_root = tempdir()
  )
  two <- appusageR:::appusage_build_run_provenance(
    workflow_run_id = "run-fixed", git_sha = sha, git_dirty = TRUE,
    package_root = tempdir()
  )
  fields <- c(
    "parser_implementation_fingerprint",
    "second_level_implementation_fingerprint",
    "source_qc_config_fingerprint"
  )
  expect_identical(one[fields], two[fields])
  expect_equal(one$effective_timezone, "Asia/Shanghai")
  expect_equal(one$git_commit_sha, sha)
  expect_equal(one$git_build_marker, "dirty")
  expect_equal(one$output_schema_version, "0.3.4")

  changed <- appusageR:::appusage_build_run_provenance(
    workflow_run_id = "run-fixed", git_sha = sha, git_dirty = FALSE,
    source_qc_config = list(line_overlap_warning_ratio = 0.02),
    package_root = tempdir()
  )
  expect_false(identical(
    one$source_qc_config_fingerprint,
    changed$source_qc_config_fingerprint
  ))
  unavailable <- appusageR:::appusage_git_build_info(
    package_root = tempfile(), git_sha = NA_character_, git_dirty = NULL
  )
  expect_equal(unavailable$git_build_marker, "unavailable")
  expect_true(is.na(unavailable$git_commit_sha))
})

test_that("workflow configuration preserves current and cache-creating run provenance", {
  old <- appusageR:::appusage_build_run_provenance(
    workflow_run_id = "run-old", git_sha = paste(rep("b", 40), collapse = ""),
    git_dirty = FALSE, package_root = tempdir()
  )
  current <- appusageR:::appusage_build_run_provenance(
    workflow_run_id = "run-current", git_sha = paste(rep("c", 40), collapse = ""),
    git_dirty = FALSE, package_root = tempdir()
  )
  existing <- list(
    created_at = "2024-01-01T00:00:00+0800",
    current_run_provenance = old,
    run_provenance_history = list()
  )
  config <- list(
    created_at = "new",
    current_run_provenance = current,
    workflow_run_id = current$workflow_run_id
  )
  merged <- appusageR:::appusage_merge_existing_workflow_configuration(config, existing)
  expect_equal(merged$current_run_provenance$workflow_run_id, "run-current")
  expect_equal(merged$run_provenance_history[[1]]$workflow_run_id, "run-old")
  expect_equal(merged$created_at, existing$created_at)

  root <- file.path(tempdir(), paste0("provenance-config-", sample.int(1e8, 1)))
  dir.create(root)
  persisted <- list(
    output_study_dir = root,
    current_run_provenance = current,
    workflow_run_id = current$workflow_run_id,
    workflow_state = list(checkpoints = list())
  )
  path <- appusageR:::appusage_write_workflow_configuration(persisted)
  expect_equal(readRDS(path)$current_run_provenance$workflow_run_id, "run-current")
})

test_that("proc metadata and diagnostics carry one supplied run provenance", {
  provenance <- appusageR:::appusage_build_run_provenance(
    workflow_run_id = "run-metadata", git_sha = paste(rep("d", 40), collapse = ""),
    git_dirty = FALSE, package_root = tempdir()
  )
  id_info <- data.frame(
    wenjuanxing_sequence_id = 1L,
    native_export_type_from_filename = "day",
    native_export_type_raw = "day",
    native_export_created_at = "2024-01-01 12:00:00",
    stringsAsFactors = FALSE
  )
  first <- appusageR:::build_metadata(
    participant_id = "1", participant_id_source = "filename",
    id_info = id_info, source_file = NA_character_, export_type = "day",
    export_type_match = TRUE, input = "text", encoding = "UTF-8",
    tz = "Asia/Shanghai", started_at = Sys.time(), finished_at = Sys.time(),
    status = "error", data = NULL, warnings = character(),
    error = simpleError("synthetic"), metadata_file = NA_character_,
    data_file = NA_character_, provenance = provenance
  )
  expect_equal(first$implementation_provenance$workflow_run_id, "run-metadata")
  second <- appusageR:::build_second_level_metadata(
    first_metadata = first, first_level_rda = NA_character_,
    first_level_metadata_file = NA_character_, second_level_rda = NA_character_,
    second_level_metadata_file = tempfile(fileext = ".json"),
    second_level_data = NULL, status = "error", error = simpleError("synthetic"),
    include_collection_app = TRUE, max_episode_ms = 86400000,
    max_daily_app_ms = 86400000, reconstruct_meta = FALSE,
    meta_pairing = "package", meta_start_event_types = 1,
    meta_end_event_types = c(2, 23), merge_meta_episodes = TRUE,
    meta_episode_merge_gap_ms = 30000, meta_daily_source = "summary",
    provenance = provenance, tz = "Asia/Shanghai",
    started_at = Sys.time(), finished_at = Sys.time()
  )
  expect_equal(second$implementation_provenance$workflow_run_id, "run-metadata")
  expect_equal(second$upstream_implementation_provenance$workflow_run_id, "run-metadata")
  diagnostic <- diagnose_appusage_error(
    simpleError("synthetic"), stage = "first_level",
    context = list(implementation_provenance = provenance)
  )
  expect_equal(diagnostic$implementation_provenance$workflow_run_id, "run-metadata")
})

synthetic_rebuild_project <- function(current) {
  root <- file.path(tempdir(), paste0("appusage-rebuild-plan-", sample.int(1e8, 1)))
  dir.create(file.path(root, "diagnostics"), recursive = TRUE)
  dir.create(file.path(root, "proclevel-2"))
  keys <- paste0("source-", 1:8)
  types <- c("line", "line", "line", "line", "meta", "day", "day", "app")
  manifest <- data.frame(
    index = 1:8, is_txt = TRUE, source_record_key = keys,
    source_fingerprint = paste0("fp-", 1:8),
    participant_id = as.character(1:8), detected_type = types,
    source_file = file.path(root, paste0("synthetic-", 1:8, ".txt")),
    stringsAsFactors = FALSE
  )
  first <- manifest
  first$status <- c("error", rep("success", 4), "error", "success", "success")
  first$failure_family <- c("memory_allocation", rep(NA_character_, 4),
    "binary_source", NA_character_, NA_character_)
  first$failure_attribution <- c("package", rep(NA_character_, 7))
  first$mixed_content <- c(FALSE, FALSE, TRUE, rep(FALSE, 5))
  first$structural_quality_critical <- FALSE
  first$data_file <- ifelse(first$status == "success",
    file.path(root, paste0(keys, "-proc1.rda")), NA_character_)
  first$metadata_file <- NA_character_
  second_keys <- keys[c(1:5, 7:8)]
  second <- data.frame(
    source_record_key = second_keys,
    source_fingerprint = paste0("fp-", c(1:5, 7:8)),
    detected_type = types[c(1:5, 7:8)],
    second_level_status = c("skipped", rep("success", 6)),
    status = c("skipped", rep("success", 6)),
    qc_status = c(NA, rep("success", 4), "not_run", "success"),
    daily_self_check_status = "success",
    daily_self_check_numeric_mismatch = c(0, 0, 0, 1, 0, 0, 0),
    daily_self_check_duplicate_keys = 0,
    daily_self_check_conservation_diff_ms = 0,
    second_level_data_file = file.path(root, paste0(second_keys, ".rda")),
    second_level_metadata_file = NA_character_,
    self_report_match_status = c(rep(NA_character_, 6), "matched"),
    stringsAsFactors = FALSE
  )
  second$second_level_data_file[second$source_record_key == "source-8"] <-
    file.path(root, "old-source-8.rda")
  utils::write.csv(manifest, file.path(root, "diagnostics", "project_manifest.csv"),
    row.names = FALSE
  )
  utils::write.csv(first, file.path(root, "analytic_summary_table_proclevel-1.csv"),
    row.names = FALSE
  )
  utils::write.csv(second, file.path(root, "analytic_summary_table_proclevel-2.csv"),
    row.names = FALSE
  )
  list(root = root, current = current)
}

test_that("metadata-only rebuild planner covers targeted actions and cardinality", {
  current <- appusageR:::appusage_build_run_provenance(
    workflow_run_id = "run-plan", git_sha = paste(rep("e", 40), collapse = ""),
    git_dirty = FALSE, package_root = tempdir()
  )
  project <- synthetic_rebuild_project(current)
  before <- sort(list.files(project$root, recursive = TRUE, full.names = TRUE))
  testthat::local_mocked_bindings(
    second_level_existing_cache_status = function(first_level_rda, output_dir,
                                                   batch_summary, index) {
      key <- batch_summary$source_record_key[[index]]
      metadata_provenance <- current
      if (identical(key, "source-5")) {
        metadata_provenance <- current
        metadata_provenance$effective_timezone <- "UTC"
        metadata_provenance$second_level_implementation_fingerprint <- "stale-meta"
      }
      list(
        status = if (identical(key, "source-2")) "collision" else "complete",
        pair_state = if (identical(key, "source-2"))
          "source_key_collision" else "complete_valid_pair",
        reason = if (identical(key, "source-2"))
          "source_record_key_mismatch" else "valid_proc2_pair",
        rda_file = file.path(project$root, paste0(key, ".rda")),
        json_file = file.path(project$root, paste0(key, ".json")),
        metadata = list(implementation_provenance = metadata_provenance)
      )
    },
    load_appusage_data_object = function(...) stop("RDA payload must not be loaded"),
    .package = "appusageR"
  )
  plan <- plan_appusage_project_rebuild(
    project$root, current_provenance = current
  )
  after <- sort(list.files(project$root, recursive = TRUE, full.names = TRUE))
  expect_identical(before, after)
  expect_equal(nrow(plan), 8L)
  expect_equal(plan$source_record_key, paste0("source-", 1:8))
  expect_match(plan$reason_codes[[1]], "package_attributed_first_level_failure")
  expect_match(plan$requested_actions[[2]], "reconcile_cache_identity")
  expect_match(plan$requested_actions[[3]], "rebuild_first_and_second_level")
  expect_match(plan$reason_codes[[4]], "line_daily_self_check_stale")
  expect_match(plan$reason_codes[[5]], "meta_timezone_provenance_stale")
  expect_match(plan$reason_codes[[6]], "first_level_failure_not_represented_as_skip")
  expect_match(plan$requested_actions[[7]], "refresh_qc")
  expect_true(plan$matching_refresh_required[[8]])
  expect_match(plan$requested_actions[[8]], "refresh_matching")
  expect_equal(plan$execution_helper[[4]], "rerun_second_level_project_subset")
})

test_that("mixed content rebuild depends on parser provenance or critical quality", {
  current <- appusageR:::appusage_build_run_provenance(
    workflow_run_id = "run-mixed", git_sha = paste(rep("2", 40), collapse = ""),
    git_dirty = FALSE, package_root = tempdir()
  )
  first <- data.frame(
    status = "success", detected_type = "line", mixed_content = TRUE,
    structural_quality_critical = FALSE, stringsAsFactors = FALSE
  )
  second <- data.frame(
    status = "success", second_level_status = "success", qc_status = "success",
    stringsAsFactors = FALSE
  )
  pair <- list(status = "complete", pair_state = "complete_valid_pair")

  accepted <- appusageR:::appusage_plan_rebuild_decision(
    first, second, pair, current, current, current
  )
  expect_false(grepl("mixed_content", accepted$reason_codes, fixed = TRUE))
  expect_false(grepl(
    "rebuild_first_and_second_level", accepted$requested_actions, fixed = TRUE
  ))

  legacy <- current
  legacy$parser_implementation_fingerprint <- "legacy-parser"
  stale <- appusageR:::appusage_plan_rebuild_decision(
    first, second, pair, legacy, current, current
  )
  expect_match(stale$reason_codes, "mixed_content_legacy_parser_provenance")
  expect_match(stale$requested_actions, "rebuild_first_and_second_level")

  first$structural_quality_critical <- TRUE
  critical <- appusageR:::appusage_plan_rebuild_decision(
    first, second, pair, current, current, current
  )
  expect_match(critical$reason_codes, "critical_structural_quality")
})

test_that("planner detects duplicate and extra proc-2 summary identities", {
  current <- appusageR:::appusage_build_run_provenance(
    workflow_run_id = "run-cardinality",
    git_sha = paste(rep("3", 40), collapse = ""),
    git_dirty = FALSE, package_root = tempdir()
  )
  project <- synthetic_rebuild_project(current)
  summary_path <- file.path(
    project$root, "analytic_summary_table_proclevel-2.csv"
  )
  second <- utils::read.csv(
    summary_path, stringsAsFactors = FALSE, check.names = FALSE
  )
  duplicate <- second[second$source_record_key == "source-2", , drop = FALSE]
  extra <- duplicate
  extra$source_record_key <- "source-extra"
  extra$source_fingerprint <- "fp-extra"
  utils::write.csv(rbind(second, duplicate, extra), summary_path, row.names = FALSE)

  testthat::local_mocked_bindings(
    second_level_existing_cache_status = function(first_level_rda, output_dir,
                                                   batch_summary, index) {
      key <- batch_summary$source_record_key[[index]]
      list(
        status = "complete", pair_state = "complete_valid_pair",
        reason = "valid_proc2_pair",
        rda_file = file.path(project$root, paste0(key, ".rda")),
        json_file = file.path(project$root, paste0(key, ".json")),
        metadata = list(implementation_provenance = current)
      )
    },
    load_appusage_data_object = function(...) stop("RDA payload must not be loaded"),
    .package = "appusageR"
  )
  plan <- plan_appusage_project_rebuild(
    project$root, current_provenance = current
  )

  expect_equal(nrow(plan), 8L)
  row2 <- plan[plan$source_record_key == "source-2", , drop = FALSE]
  expect_equal(row2$proc2_identity_count[[1L]], 2L)
  expect_true(row2$proc2_duplicate_identity[[1L]])
  expect_match(
    row2$proc2_cardinality_reason_codes[[1L]],
    "proc2_summary_duplicate_source_identity"
  )
  expect_true(all(plan$proc2_extra_row_count == 1L))
  expect_true(all(grepl("proc2_summary_extra_rows", plan$reason_codes)))
  expect_true(all(grepl("refresh_proc2_summary", plan$requested_actions)))
  row6 <- plan[plan$source_record_key == "source-6", , drop = FALSE]
  expect_true(row6$proc2_missing_row[[1L]])
  expect_match(
    row6$proc2_cardinality_reason_codes[[1L]], "proc2_summary_missing_source"
  )
})

test_that("planner exposes stale parser, second-level and QC fingerprints", {
  current <- appusageR:::appusage_build_run_provenance(
    workflow_run_id = "run-current", git_sha = paste(rep("f", 40), collapse = ""),
    git_dirty = FALSE, package_root = tempdir()
  )
  stale <- current
  stale$parser_implementation_fingerprint <- "old-parser"
  stale$second_level_implementation_fingerprint <- "old-second"
  stale$source_qc_config_fingerprint <- "old-qc"
  expect_equal(appusageR:::appusage_compare_provenance(
    stale, current, "parser_implementation_fingerprint"
  ), "stale")
  expect_equal(appusageR:::appusage_compare_provenance(
    stale, current, "second_level_implementation_fingerprint"
  ), "stale")
  expect_equal(appusageR:::appusage_compare_provenance(
    stale, current, "source_qc_config_fingerprint"
  ), "stale")
  expect_equal(appusageR:::appusage_compare_provenance(
    list(), current, "source_qc_config_fingerprint"
  ), "missing")
})

test_that("explicit plan writing is opt-in and readable", {
  current <- appusageR:::appusage_build_run_provenance(
    workflow_run_id = "run-write", git_sha = paste(rep("1", 40), collapse = ""),
    git_dirty = FALSE, package_root = tempdir()
  )
  project <- synthetic_rebuild_project(current)
  testthat::local_mocked_bindings(
    second_level_existing_cache_status = function(first_level_rda, output_dir,
                                                   batch_summary, index) {
      list(status = "complete", pair_state = "complete_valid_pair",
        reason = "valid_proc2_pair",
        rda_file = file.path(project$root, paste0(batch_summary$source_record_key[[index]], ".rda")),
        json_file = NA_character_,
        metadata = list(implementation_provenance = current))
    },
    .package = "appusageR"
  )
  path <- file.path(project$root, "preview.csv")
  expect_false(file.exists(path))
  plan <- plan_appusage_project_rebuild(
    project$root, current_provenance = current,
    write_plan = TRUE, plan_file = path
  )
  expect_true(file.exists(path))
  expect_equal(nrow(utils::read.csv(path)), nrow(plan))
})
