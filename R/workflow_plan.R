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
#' @return An `appusage_plan` with manifest, effective configuration and a task
#'   table. Actions are `reuse`, `run`, `blocked`, or `disabled`.
#' @export
plan_appusage_workflow <- function(x = NULL, project_dir, config = NULL,
                                   verify = c("metadata", "content", "cache_only"),
                                   ids = NULL) {
  verify <- match.arg(verify)
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
  provenance <- appusage_build_run_provenance(tz = config$time$tz)
  requested <- appusage_parse_contract(config$parse$type, config$parse$input,
    config$parse$encoding, config$time$tz, config$parse$parser_strict, provenance)
  options <- appusage_config_second_options(config)
  index <- match(normalized_summary_path(manifest$source_file), normalized_summary_path(first[["source_file"]]))
  tasks <- vector("list", nrow(manifest))
  for (i in seq_len(nrow(manifest))) {
    row <- if (!is.na(index[[i]])) first[index[[i]], , drop = FALSE] else NULL
    metadata <- if (!is.null(row)) appusage_read_json_safely(row$metadata_file[[1]]) else NULL
    valid <- !is.null(row) && appusage_first_contract_valid(row, metadata, requested,
      if (verify == "content") manifest$source_file[[i]] else NULL,
      config$parse$input, if (verify == "metadata") "preview" else verify)
    valid <- valid && (is.na(manifest$participant_id[[i]]) ||
      identical(as.character(row$participant_id[[1]]), manifest$participant_id[[i]]))
    cache_only <- verify == "cache_only" || config$execution$source_verification == "cache_only"
    forced <- config$execution$overwrite || !config$execution$resume
    if (forced && !cache_only) valid <- FALSE
    parse <- if (valid) "reuse" else if (cache_only) "blocked" else "run"
    research <- if (valid && row$status[[1]] != "success") "blocked" else "run"
    cache <- NULL
    if (valid && row$status[[1]] == "success") {
      cache <- second_level_existing_cache_status(row$data_file[[1]],
        file.path(project_dir, "proclevel-2"), row, 1L)
      cache <- appusage_second_cache_for_options(cache, options, provenance,
        verify = verify != "metadata")
      if (!forced && identical(cache$status, "complete")) research <- "reuse"
      if (identical(cache$status, "collision")) research <- "blocked"
    }
    if (parse == "blocked") research <- "blocked"
    qc <- if (!config$qc$enabled) "disabled" else if (research == "blocked") "blocked" else "run"
    if (qc == "run" && research == "reuse" &&
        appusage_contract_equal(cache$metadata$module_state$qc, appusage_qc_contract(options, provenance)) &&
        identical(cache$metadata$processing$qc_status, "success")) qc <- "reuse"
    category <- if (!config$category$enabled) "disabled" else if (research == "blocked") "blocked" else "run"
    if (category == "run" && research == "reuse" &&
        appusage_contract_equal(cache$metadata$module_state$category,
          appusage_category_contract(config$category$dictionary, config$category$overwrite))) category <- "reuse"
    tasks[[i]] <- data.frame(source_file = manifest$source_file[[i]],
      stage = c("parse", "research_data", "qc", "category"),
      action = c(parse, research, qc, category),
      reason = c(if (valid) "compatible_parse" else "missing_changed_or_unverified_parse",
        cache$reason %||% "upstream_required", "qc_contract", "dictionary_contract"),
      source_verified = verify == "content", payload_loaded = FALSE,
      stringsAsFactors = FALSE)
  }
  structure(list(project_dir = project_dir, manifest = manifest, config = config,
    tasks = tibble::as_tibble(do.call(rbind, tasks)), verification = verify,
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
#' @return The stage's compact summary or matching result.
#' @export
run_appusage_stage <- function(stage, project_dir, config = NULL, x = NULL) {
  stage <- match.arg(stage, c("parse", "research_data", "qc", "category", "matching", "summary"))
  saved <- file.path(project_dir, "appusage_configuration.rds")
  if (is.null(config) && file.exists(saved)) config <- readRDS(saved)
  config <- appusage_validate_config(config)
  if (stage == "summary") return(appusage_project_summary(project_dir))
  if (stage == "parse") {
    return(run_appusage_workflow(x, project_dir = project_dir, config = config,
      run_second_level = FALSE)$first_level)
  }
  first <- appusage_read_summary_csv(file.path(project_dir, "analytic_summary_table_proclevel-1.csv"))
  if (!nrow(first)) cli::cli_abort("No first-level summary is available.")
  if (!is.null(x)) {
    manifest <- appusage_workflow_manifest(x)
    index <- match(manifest$source_file, normalized_summary_path(first$source_file))
    if (anyNA(index)) cli::cli_abort("Selected sources are absent from the project.")
    first <- first[index, , drop = FALSE]
  }
  options <- appusage_config_second_options(config)
  if (stage == "research_data") {
    return(do.call(write_second_level_batch, c(list(batch_summary = first,
      output_dir = file.path(project_dir, "proclevel-2"),
      overwrite = config$execution$overwrite, resume = config$execution$resume,
      progress = config$execution$progress, parallel = config$execution$parallel,
      n_cores = config$execution$workers), options)))
  }
  second <- appusage_project_summary(project_dir, write = FALSE)
  first_paths <- normalized_summary_path(first$data_file)
  second <- second[normalized_summary_path(second$first_level_data_file) %in% first_paths, , drop = FALSE]
  if (stage == "qc") return(appusage_refresh_qc_stage(project_dir, second, options,
    strict = config$execution$strict, progress = config$execution$progress))
  if (stage == "category") return(appusage_refresh_category_stage(project_dir, second,
    config$category$dictionary, config$category$overwrite, config$execution$resume))
  appusage_run_matching_stage(project_dir, config, first, second)
}
