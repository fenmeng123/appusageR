#' Describe source-level work without running preprocessing
#'
#' The default preview reads summaries and JSON metadata only. It does not
#' read raw content or deserialize RDA payloads. `verify = "content"` checks
#' source bytes and artifact checksums. Execution always revalidates a plan.
#' @param x File paths or a data frame containing `source_file` and optional
#'   `participant_id`. Omit to use the saved project source manifest.
#' @param project_dir Exact output project directory (it need not exist).
#' @param config An [appusage_config()]. Omit to use the saved configuration.
#' @param verify One of `"metadata"`, `"content"`, or `"cache_only"`.
#' @param ids Optional participant IDs for a file vector.
#' @return An `appusage_plan` with manifest, effective configuration, source-level
#'   `tasks` and `project_tasks` for matching/summary. Both tables provide stable
#'   task IDs, action, reason codes, differences and verification scope. Actions
#'   are `reuse`, `run`, `blocked`, or `disabled`. Print and summary methods use
#'   only these stored tables; a preview does not authorize stale cache reuse.
#' @export
plan_appusage_workflow <- function(x = NULL, project_dir, config = NULL,
                                   verify = c("metadata", "content", "cache_only"),
                                   ids = NULL) {
  verify <- match.arg(verify)
  context <- appusage_runtime_context()
  previous_context <- getOption("appusageR.runtime_context")
  options(appusageR.runtime_context = context)
  on.exit(options(appusageR.runtime_context = previous_context), add = TRUE)
  appusage_resume_index_load(project_dir, context)
  appusage_prepare_workflow_plan(x, project_dir, config, verify, ids, context = context)
}

appusage_prepare_workflow_plan <- function(x = NULL, project_dir, config = NULL,
                                           verify = "metadata", ids = NULL,
                                           provenance = NULL, context = NULL) {
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  saved <- file.path(project_dir, "appusage_configuration.rds")
  if (is.null(config) && file.exists(saved)) config <- readRDS(saved)
  config <- appusage_validate_config(config)
  if (is.null(x)) {
    source_file <- file.path(project_dir, "appusage_manifest.rds")
    if (!file.exists(source_file)) cli::cli_abort("Supply `x` or an existing saved manifest.")
    x <- readRDS(source_file)
  }
  manifest <- appusage_workflow_manifest(x, ids)
  first <- appusage_read_summary_csv(file.path(project_dir, "analytic_summary_table_proclevel-1.csv"))
  if (!nrow(first) && dir.exists(file.path(project_dir, "proclevel-1"))) {
    first <- rebuild_first_level_summary_from_cache(project_dir, manifest, write = FALSE)
  }
  provenance <- provenance %||% appusage_build_run_provenance(tz = config$time$tz)
  requested <- appusage_parse_contract(config$parse$type, config$parse$input,
    config$parse$encoding, config$time$tz, config$parse$parser_strict, provenance)
  options <- appusage_config_second_options(config)
  research_contract <- appusage_research_contract(options, provenance)
  qc_contract <- appusage_qc_contract(options, provenance)
  category_contract <- if (config$category$enabled)
    appusage_category_contract(config$category$dictionary, config$category$overwrite) else NULL
  index <- match(normalized_summary_path(manifest$source_file), normalized_summary_path(first[["source_file"]]))
  tasks <- vector("list", nrow(manifest))
  summary_rows <- vector("list", nrow(manifest))
  for (i in seq_len(nrow(manifest))) {
    row <- if (!is.na(index[[i]])) first[index[[i]], , drop = FALSE] else NULL
    metadata <- if (!is.null(row)) appusage_runtime_json(row$metadata_file[[1]], context) else NULL
    evidence <- appusage_parse_evidence(row, metadata, requested,
      if (verify == "content") manifest$source_file[[i]] else NULL,
      config$parse$input, if (verify == "metadata") "preview" else verify, context)
    valid <- evidence$valid
    identity_ok <- is.na(manifest$participant_id[[i]]) ||
      identical(as.character(row$participant_id[[1]]), manifest$participant_id[[i]])
    if (!identity_ok) evidence$reason <- "source_identity_changed"
    valid <- valid && identity_ok
    cache_only <- verify == "cache_only" || config$execution$source_verification == "cache_only"
    forced <- config$execution$overwrite || !config$execution$resume
    if (forced && !cache_only) valid <- FALSE
    parse <- if (valid) "reuse" else if (cache_only) "blocked" else "run"
    research <- if (valid && row$status[[1]] != "success") "blocked" else "run"
    cache <- NULL
    if (valid && row$status[[1]] == "success") {
      cache <- second_level_existing_cache_status(row$data_file[[1]],
        file.path(project_dir, "proclevel-2"), row, 1L, first_metadata = metadata)
      cache <- appusage_second_cache_for_options(cache, options, provenance,
        verify = verify != "metadata", requested = research_contract)
      if (!is.null(cache$metadata)) summary_rows[[i]] <- qc_summary_row_from_metadata(
        cache$json_file, metadata = cache$metadata)
      if (!forced && identical(cache$status, "complete")) research <- "reuse"
      if (identical(cache$status, "collision")) research <- "blocked"
    }
    if (parse == "blocked") research <- "blocked"
    qc <- if (!config$qc$enabled) "disabled" else if (research == "blocked") "blocked" else "run"
    if (qc == "run" && research == "reuse" &&
        appusage_contract_equal(cache$metadata$module_state$qc, qc_contract) &&
        identical(cache$metadata$processing$qc_status, "success")) qc <- "reuse"
    category <- if (!config$category$enabled) "disabled" else if (research == "blocked") "blocked" else "run"
    if (category == "run" && research == "reuse" &&
        appusage_contract_equal(cache$metadata$module_state$category, category_contract)) category <- "reuse"
    stages <- appusage_stage_registry()$stage[appusage_stage_registry()$scope == "source"]
    reasons <- c(if (forced && !cache_only) "forced" else if (valid) "up_to_date" else evidence$reason,
      if (research == "blocked") "upstream_failed_or_identity_conflict" else
        if (research == "reuse") "up_to_date" else cache$reason %||% "upstream_required",
      if (qc == "reuse") "up_to_date" else if (qc == "disabled") "disabled_by_config" else
        if (research == "blocked") "upstream_failed" else "qc_contract_changed_or_missing",
      if (category == "reuse") "up_to_date" else if (category == "disabled") "disabled_by_config" else
        if (research == "blocked") "upstream_failed" else "dictionary_contract_changed_or_missing")
    if (identical(cache$status, "collision")) reasons[[2]] <- "identity_conflict"
    if (forced) reasons[c(parse, research, qc, category) == "run"] <- "forced"
    changes <- list(appusage_contract_details(metadata$module_state$parse, requested),
      appusage_contract_details(cache$metadata$module_state$research_data, research_contract),
      appusage_contract_details(cache$metadata$module_state$qc, qc_contract),
      if (config$category$enabled) appusage_contract_details(cache$metadata$module_state$category, category_contract) else character())
    details <- vapply(seq_along(stages), function(j) if (length(changes[[j]]) &&
      !reasons[[j]] %in% c("up_to_date", "disabled_by_config"))
        appusage_text_paste(changes[[j]], collapse = "; ") else reasons[[j]], character(1))
    tasks[[i]] <- data.frame(source_file = manifest$source_file[[i]],
      task_id = appusage_task_id(manifest$source_file[[i]], stages), scope = "source",
      source_record_key = if (is.null(row)) NA_character_ else appusage_get_col_value(row, "source_record_key", NA_character_),
      stage = stages,
      action = c(parse, research, qc, category),
      reason = c(if (valid) "compatible_parse" else "missing_changed_or_unverified_parse",
        cache$reason %||% "upstream_required", "qc_contract", "dictionary_contract"),
      reason_code = reasons, details = details, verification = verify,
      source_verified = evidence$source_verified, payload_loaded = FALSE,
      stringsAsFactors = FALSE)
  }
  tasks <- tibble::as_tibble(do.call(rbind, tasks))
  summary_rows <- Filter(Negate(is.null), summary_rows)
  second <- if (length(summary_rows)) do.call(bind_appusage_summary_rows, summary_rows) else tibble::tibble()
  project_tasks <- appusage_plan_project_tasks(project_dir, config, tasks, first, second,
    verify, context)
  structure(list(project_dir = project_dir, manifest = manifest, config = config,
    tasks = tasks, project_tasks = project_tasks, verification = verify,
    overview = appusage_workflow_overview(first[index[!is.na(index)], , drop = FALSE], second,
      n_sources = nrow(manifest), config = config),
    planned_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")),
    class = c("appusage_plan", "list"))
}

appusage_workflow_manifest <- function(x, ids = NULL) {
  if (is.data.frame(x)) {
    if (!"source_file" %in% names(x)) cli::cli_abort("Manifest requires `source_file`.")
    manifest <- as.data.frame(x, stringsAsFactors = FALSE)
    if (!is.null(ids)) cli::cli_abort("Use manifest$participant_id instead of `ids`.")
  } else {
    if (!is.character(x) || !length(x)) cli::cli_abort("Supply a nonempty file vector or manifest.")
    if (!is.null(ids) && length(ids) != length(x)) cli::cli_abort("`ids` must align with `x`.")
    manifest <- data.frame(source_file = x, stringsAsFactors = FALSE)
    if (is.null(ids) && !is.null(names(x)) && all(appusage_text_nzchar(names(x)))) ids <- names(x)
    if (!is.null(ids)) manifest$participant_id <- as.character(ids)
  }
  if (!nrow(manifest) || anyNA(manifest$source_file)) cli::cli_abort("Source manifest must be nonempty without missing paths.")
  manifest$source_file <- normalizePath(manifest$source_file, winslash = "/", mustWork = FALSE)
  if (anyDuplicated(manifest$source_file)) cli::cli_abort("Source manifest contains duplicate paths.")
  if (!"participant_id" %in% names(manifest)) manifest$participant_id <- NA_character_
  manifest$participant_id <- as.character(manifest$participant_id)
  manifest
}

#' Run one APP Usage cache stage independently
#'
#' Uses the same scientific and storage functions as the workflow. Existing
#' caches supply upstream data; raw files are never read by downstream stages.
#' @param stage One of `"parse"`, `"research_data"`, `"qc"`, `"category"`,
#'   `"matching"`, or `"summary"`.
#' @param project_dir Exact output project directory.
#' @param config An [appusage_config()]. Defaults to the saved configuration.
#' @param x Optional manifest/file vector selecting sources. Required for a new
#'   parse stage; omitted for all existing sources.
#' @return The stage's compact summary or matching result, with a `run_record`
#'   attribute describing actual scope, actions and timings. Project projections
#'   are refreshed through the same service used by the full workflow.
#' @export
run_appusage_stage <- function(stage, project_dir, config = NULL, x = NULL) {
  stage <- match.arg(stage, c("parse", "research_data", "qc", "category", "matching", "summary"))
  saved <- file.path(project_dir, "appusage_configuration.rds")
  if (is.null(config) && file.exists(saved)) config <- readRDS(saved)
  config <- appusage_validate_config(config)
  if (stage == "parse") {
    return(run_appusage_workflow(x, project_dir = project_dir, config = config,
      run_second_level = FALSE)$first_level)
  }
  saved_manifest <- file.path(project_dir, "appusage_manifest.rds")
  manifest <- if (file.exists(saved_manifest)) readRDS(saved_manifest) else NULL
  appusage_rebuild_first_level_summary_if_needed(project_dir, manifest)
  provenance <- appusage_build_run_provenance(tz = config$time$tz)
  context <- appusage_runtime_context(provenance)
  previous_context <- getOption("appusageR.runtime_context")
  options(appusageR.runtime_context = context)
  on.exit(options(appusageR.runtime_context = previous_context), add = TRUE)
  appusage_resume_index_load(project_dir, context)
  first <- appusage_read_summary_csv(file.path(project_dir, "analytic_summary_table_proclevel-1.csv"))
  if (!nrow(first)) cli::cli_abort("No first-level summary is available.")
  plan <- appusage_prepare_workflow_plan(x, project_dir, config,
    verify = "cache_only", provenance = provenance, context = context)
  state <- appusage_new_run_record(project_dir, plan)
  state$record$requested_stage <- stage
  outside <- state$record$tasks$stage != stage
  state$record$tasks$actual_action[outside] <- "disabled"
  state$record$tasks$deviation_reason[outside] <- "outside_requested_scope"
  if (!is.null(x)) {
    manifest <- appusage_workflow_manifest(x)
    index <- match(manifest$source_file, normalized_summary_path(first$source_file))
    if (anyNA(index)) cli::cli_abort("Selected sources are absent from the project.")
    first <- first[index, , drop = FALSE]
  }
  options <- appusage_config_second_options(config)
  legacy <- appusage_stage_registry()$legacy_stage[match(stage, appusage_stage_registry()$stage)]
  appusage_record_stage(state, legacy, "started")
  result <- tryCatch({
    if (stage %in% c("research_data", "qc", "category")) {
      invalid <- plan$tasks$source_file[plan$tasks$stage == "parse" & plan$tasks$action == "blocked"]
      invalid <- intersect(invalid, first$source_file[first$status == "success"])
      if (length(invalid)) {
        appusage_record_tasks(state, stage, "blocked", "blocked", sources = invalid)
        cli::cli_abort("The selected stage requires compatible, intact first-level caches; run parse or the full workflow first.")
      }
    }
    if (stage == "research_data") {
      value <- do.call(write_second_level_batch, c(list(batch_summary = first,
      output_dir = file.path(project_dir, "proclevel-2"),
      overwrite = config$execution$overwrite, resume = config$execution$resume,
      progress = config$execution$progress, parallel = config$execution$parallel,
      n_cores = config$execution$workers, provenance = provenance), options))
    } else {
      second <- appusage_project_summary(project_dir, write = FALSE)
      if (stage %in% c("qc", "category")) {
        first_paths <- normalized_summary_path(first$data_file)
        second <- second[normalized_summary_path(second$first_level_data_file) %in% first_paths, , drop = FALSE]
        for (path in appusage_summary_proc2_paths(second)) {
          if (!is_present_string(path)) next
          metadata <- appusage_read_json_safely(second_level_metadata_path(path))
          if (!appusage_artifact_valid(path, metadata$module_state$artifact))
            cli::cli_abort("Downstream stages require intact research_data; run research_data first.")
        }
      }
      value <- switch(stage,
        qc = appusage_refresh_qc_stage(project_dir, second, options, provenance,
          strict = config$execution$strict, progress = config$execution$progress,
          resume = config$execution$resume && !config$execution$overwrite),
        category = appusage_refresh_category_stage(project_dir, second,
          config$category$dictionary, config$category$overwrite, config$execution$resume),
        matching = appusage_run_matching_stage(project_dir, config,
          appusage_read_summary_csv(file.path(project_dir, "analytic_summary_table_proclevel-1.csv")),
          second, publish = FALSE),
        summary = appusage_publish_project(project_dir, config))
    }
    appusage_record_execution(state, legacy, value, first)
    appusage_record_stage(state, legacy, "completed")
    if (stage != "summary") {
      appusage_record_stage(state, "summary", "started")
      projected <- appusage_publish_project(project_dir, config,
        fresh = if (is.data.frame(value)) value else NULL,
        matching = if (stage == "matching") value else NULL)
      appusage_record_execution(state, "summary", projected)
      appusage_record_stage(state, "summary", "completed")
    }
    value
  }, error = function(e) {
    appusage_record_stage(state, legacy, "error", e)
    stop(e)
  })
  state$record$status <- "completed"
  state$record$finished_at <- appusage_workflow_timestamp()
  state$record$metrics <- appusage_runtime_metrics(context)
  saveRDS(state$record, state$path)
  attr(result, "run_record") <- state$record
  appusage_resume_index_save(project_dir, context)
  result
}
