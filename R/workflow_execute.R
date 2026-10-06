# Shared execution service. Project/Wenjuanxing and generic APIs differ only in
# input adaptation and reporting callbacks, never in scientific stage execution.
appusage_execute_pipeline <- function(x, output_root, config, ids = NULL,
                                      project_name = NULL, project_id = NULL,
                                      project_dir = NULL, run_second_level = TRUE,
                                      first_options = list(), provenance = NULL,
                                      stage_callback = NULL, result_callback = NULL,
                                      matching_adapter = NULL, runtime_context = NULL) {
  config <- appusage_validate_config(config)
  provenance <- appusage_resolve_run_provenance(provenance, tz = config$time$tz)
  provenance$source_verification <- config$execution$source_verification
  execution <- config$execution
  context <- runtime_context %||% appusage_runtime_context(provenance)
  previous_context <- getOption("appusageR.runtime_context")
  options(appusageR.runtime_context = context)
  on.exit(options(appusageR.runtime_context = previous_context), add = TRUE)
  project_name <- project_name %||% next_study_project_name(output_root)
  project_id <- project_id %||% generate_project_id()
  project_dir <- project_dir %||% file.path(output_root,
    appusage_text_paste0(sanitize_entity_value(project_name), "_", sanitize_entity_value(project_id)))
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  appusage_resume_index_load(project_dir, context)
  plan <- NULL
  if (config$parse$input == "file") {
    manifest <- appusage_workflow_manifest(x, ids)
    x <- manifest$source_file
    if (any(!is.na(manifest$participant_id))) {
      if (anyNA(manifest$participant_id)) cli::cli_abort("Participant IDs must be supplied for every source or omitted for all.")
      ids <- manifest$participant_id
    }
    plan <- appusage_runtime_measure(context, "planning", appusage_prepare_workflow_plan(manifest, project_dir, config,
      verify = execution$source_verification, provenance = provenance, context = context))
    if (!isTRUE(run_second_level)) {
      plan$tasks$action[plan$tasks$stage != "parse"] <- "disabled"
      plan$tasks$reason[plan$tasks$stage != "parse"] <- "outside_requested_scope"
      plan$tasks$reason_code[plan$tasks$stage != "parse"] <- "outside_requested_scope"
      plan$project_tasks$action <- "disabled"
      plan$project_tasks$reason_code <- "outside_requested_scope"
    }
  }
  dir.create(project_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(config, file.path(project_dir, "appusage_configuration.rds"))
  if (!is.null(plan)) {
    manifest_path <- file.path(project_dir, "appusage_manifest.rds")
    saved_manifest <- if (file.exists(manifest_path)) readRDS(manifest_path) else NULL
    manifest <- plan$manifest
    if (!is.null(saved_manifest)) {
      keep <- !saved_manifest$source_file %in% manifest$source_file
      manifest <- bind_appusage_summary_rows(manifest, saved_manifest[keep, , drop = FALSE])
    }
    saveRDS(manifest, manifest_path)
    saveRDS(plan, file.path(project_dir, "appusage_plan.rds"))
  }
  state <- appusage_new_run_record(project_dir, plan)
  first <- NULL
  notify <- function(stage, status, value = NULL) {
    appusage_record_stage(state, stage, status, value)
    if (is.function(stage_callback)) stage_callback(stage, status, value)
  }
  execute <- function(stage, fun) {
    notify(stage, "started")
    tryCatch({
      value <- fun()
      appusage_record_execution(state, stage, value, first)
      if (is.function(result_callback)) value <- result_callback(stage, value)
      notify(stage, "completed", value)
      value
    }, error = function(e) {
      notify(stage, "error", e)
      stop(e)
    })
  }
  first <- execute("first_level", function() {
    if (!is.null(plan)) appusage_rebuild_first_level_summary_if_needed(project_dir, manifest)
    if (!is.null(plan) && execution$resume && !execution$overwrite &&
        execution$source_verification != "metadata" &&
        all(plan$tasks$action[plan$tasks$stage == "parse"] == "reuse")) {
      first <- appusage_read_summary_csv(file.path(project_dir, "analytic_summary_table_proclevel-1.csv"))
      index <- match(normalized_summary_path(x), normalized_summary_path(first$source_file))
      first <- first[index, , drop = FALSE]
      first$index <- seq_len(nrow(first))
      attr(first, "stage_reused") <- TRUE
      attr(first, "execution_actions") <- rep("reuse", nrow(first))
      return(first)
    }
    if (identical(execution$source_verification, "cache_only")) {
      if (is.null(project_dir)) cli::cli_abort("Cache-only execution requires `project_dir`.")
      old <- appusage_read_summary_csv(file.path(project_dir, "analytic_summary_table_proclevel-1.csv"))
      index <- match(normalized_summary_path(x), normalized_summary_path(old$source_file))
      if (anyNA(index)) cli::cli_abort("Cache-only execution has sources without first-level caches.")
      first <- old[index, , drop = FALSE]
      requested <- appusage_parse_contract(config$parse$type, config$parse$input,
        config$parse$encoding, config$time$tz, config$parse$parser_strict, provenance)
      valid <- vapply(seq_len(nrow(first)), function(i) {
        appusage_first_contract_valid(first[i, , drop = FALSE],
          appusage_read_json_safely(first$metadata_file[[i]]), requested, verify = "cache_only")
      }, logical(1))
      if (!all(valid)) cli::cli_abort("Cache-only execution needs compatible, intact first-level caches; use content verification to rebuild.")
      attr(first, "stage_reused") <- TRUE
      attr(first, "execution_actions") <- rep("reuse", nrow(first))
      return(first)
    }
    args <- list(x = x, ids = ids, output_dir = output_root, project_dir = project_dir,
      project_name = project_name, project_id = project_id, type = config$parse$type,
      input = config$parse$input, encoding = config$parse$encoding, tz = config$time$tz,
      parser_strict = config$parse$parser_strict, strict = execution$strict,
      overwrite = execution$overwrite, resume = execution$resume,
      progress = execution$progress, parallel = execution$parallel,
      n_cores = execution$workers, checkpoint_every = execution$checkpoint_every,
      provenance = provenance)
    do.call(read_appusage_batch, utils::modifyList(args, first_options))
  })
  # Recovered legacy summaries may not carry project_root; the adapter's
  # explicit output location remains authoritative.
  first$project_root <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  second <- qc <- categories <- NULL
  options <- appusage_config_second_options(config)
  if (isTRUE(run_second_level)) {
    second <- execute("second_level", function() {
      do.call(write_second_level_batch, c(list(batch_summary = first,
        output_dir = file.path(project_dir, "proclevel-2"),
        overwrite = execution$overwrite, resume = execution$resume,
        progress = execution$progress, parallel = execution$parallel,
        n_cores = execution$workers, provenance = provenance), options))
    })
  }
  if (config$qc$enabled && !is.null(second)) {
    qc <- execute("qc", function() {
      appusage_refresh_qc_stage(project_dir, second, options, provenance,
        strict = execution$strict, progress = execution$progress)
    })
  }
  if (config$category$enabled && !is.null(second)) {
    categories <- execute("category", function() {
      appusage_refresh_category_stage(project_dir, qc %||% second,
        config$category$dictionary, overwrite = config$category$overwrite,
        resume = execution$resume && !execution$overwrite)
    })
  }
  matching <- if (config$matching$enabled) execute("matching", function() {
    matching_first <- appusage_read_summary_csv(file.path(project_dir, "analytic_summary_table_proclevel-1.csv"))
    matching_second <- appusage_project_summary(project_dir, fresh = categories %||% qc %||% second, write = FALSE)
    appusage_run_matching_stage(project_dir, config, matching_first, matching_second,
      adapter = matching_adapter, publish = FALSE)
  }) else NULL
  projected <- if (!is.null(second) || !is.null(matching)) execute("summary", function() {
    appusage_publish_project(project_dir, config, fresh = categories %||% qc %||% second,
      matching = matching, adapter = matching_adapter)
  }) else NULL
  latest <- projected %||% qc %||% second %||% first
  state$record$failures <- list(parse = sum(first$status == "error", na.rm = TRUE),
    research_data = if (is.null(second)) 0L else sum(second$second_level_status == "error", na.rm = TRUE),
    qc = if (is.null(qc)) 0L else sum(qc$qc_status == "error", na.rm = TRUE))
  state$record$status <- if (sum(unlist(state$record$failures)) > 0L) "completed_with_errors" else "completed"
  state$record$finished_at <- appusage_workflow_timestamp()
  state$record$metrics <- appusage_runtime_metrics(context)
  saveRDS(state$record, state$path)
  report_first <- appusage_read_summary_csv(file.path(project_dir, "analytic_summary_table_proclevel-1.csv"))
  if (!nrow(report_first)) report_first <- first
  appusage_resume_index_save(project_dir, context)
  list(project_dir = project_dir, first_level = first, second_level = second,
    qc = qc, categories = categories, matching = matching, latest = latest,
    plan = plan, run_record = state$record,
    overview = appusage_workflow_overview(report_first,
      if (is.null(second) && is.null(matching)) NULL else latest, config = config),
    effective_config = config, implementation_provenance = provenance)
}

appusage_category_contract <- function(dictionary, overwrite = TRUE) {
  dictionary <- as_app_category_dictionary(dictionary)
  list(dictionary = appusage_object_fingerprint(as.data.frame(dictionary)), overwrite = overwrite,
    implementation = appusage_function_fingerprint(c("add_app_categories",
      "add_app_categories_frame", "build_category_lookups", "apply_category_match",
      "standardize_category_dict", "category_name_key", "standardize_package_name")))
}

appusage_refresh_category_stage <- function(project_dir, summary, dictionary,
                                             overwrite = TRUE, resume = TRUE) {
  dictionary <- as_app_category_dictionary(dictionary)
  expected <- appusage_category_contract(dictionary, overwrite)
  paths <- appusage_summary_proc2_paths(summary)
  actions <- rep("blocked", length(paths))
  for (i in seq_along(paths)) {
    path <- paths[[i]]
    if (!is_present_string(path) || !file.exists(path)) next
    metadata <- tryCatch(appusage_read_validation_json(second_level_metadata_path(path)),
      error = function(e) NULL)
    if (resume && appusage_contract_equal(metadata$module_state$category, expected)) {
      actions[[i]] <- "reuse"
      next
    }
    actions[[i]] <- "run"
    result <- write_app_categories_one(path, project_dir, dictionary, overwrite)
    if (identical(result$status[[1]], "error")) cli::cli_abort("Category update failed: {result$error_message[[1]]}")
  }
  result <- if (!any(actions == "run")) summary else appusage_project_summary(project_dir, write = FALSE)
  attr(result, "execution_actions") <- actions
  attr(result, "stage_reused") <- !any(actions == "run")
  result
}

appusage_refresh_qc_stage <- function(project_dir, summary, options,
                                      provenance = NULL, strict = FALSE, progress = FALSE,
                                      resume = TRUE) {
  paths <- appusage_summary_proc2_paths(summary)
  provenance <- appusage_resolve_run_provenance(provenance, tz = options$tz)
  expected <- appusage_qc_contract(options, provenance)
  requested_research <- appusage_research_contract(options, provenance)
  changed <- 0L
  actions <- rep("blocked", length(paths))
  refreshed <- list()
  qc_args <- options[intersect(names(options), setdiff(names(formals(write_qc_metadata_one)),
    c("metadata_file", "overwrite")))]
  for (i in seq_along(paths)) {
    if (!is_present_string(paths[[i]]) || !file.exists(paths[[i]])) next
    json <- second_level_metadata_path(paths[[i]])
    if (!file.exists(json)) next
    metadata <- appusage_read_validation_json(json)
    recorded_research <- metadata$module_state$research_data
    # Duration labels can be synchronized by QC. Other research changes need
    # their upstream stage, rather than falsely recording a new QC contract.
    if (!is.null(recorded_research)) {
      for (name in c("max_episode_ms", "max_daily_app_ms")) {
        recorded_research$configuration[[name]] <- requested_research$configuration[[name]]
      }
      if (!appusage_contract_equal(recorded_research, requested_research)) {
        cli::cli_abort("QC requires compatible research_data; run the research_data stage or the full workflow first.")
      }
    }
    if (isTRUE(resume) && identical(metadata$processing$qc_status, "success") &&
        appusage_contract_equal(metadata$module_state$qc, expected)) {
      actions[[i]] <- "reuse"
      next
    }
    if (progress) message(sprintf("Refreshing QC file %d/%d", i, length(paths)))
    changed <- changed + 1L
    actions[[i]] <- "run"
    do.call(write_qc_metadata_one, c(list(metadata_file = json, overwrite = TRUE, provenance = provenance), qc_args))
    metadata <- appusage_read_json(json, simplifyVector = TRUE)
    if (identical(metadata$processing$qc_status, "success")) {
      metadata$module_state$qc <- expected
      appusage_atomic_write_metadata_json(metadata, json)
    } else if (strict) {
      cli::cli_abort("QC failed for source {i}: {metadata$qc$qc_error_message}")
    }
    refreshed[[length(refreshed) + 1L]] <- qc_summary_row_from_metadata(json, metadata = metadata)
  }
  result <- if (length(refreshed)) appusage_project_summary(project_dir,
    fresh = do.call(bind_appusage_summary_rows, refreshed), write = FALSE) else summary
  attr(result, "stage_reused") <- changed == 0L
  attr(result, "execution_actions") <- actions
  result
}

appusage_config_from_legacy <- function(type = "auto", input = "file", encoding = "auto",
                                        tz = "Asia/Shanghai", second_options = list(),
                                        execution = list(), category = list()) {
  options <- appusage_second_effective_options(second_options, tz = tz)
  defaults <- appusage_config()
  qc <- options[intersect(names(options), names(defaults$qc))]
  qc$enabled <- options$inline_qc
  appusage_config(parse = list(type = type, input = input, encoding = encoding),
    time = list(tz = tz),
    reconstruction = options[intersect(names(options), names(defaults$reconstruction))],
    daily = list(meta_daily_source = options$meta_daily_source),
    qc = qc, execution = execution, category = category)
}
