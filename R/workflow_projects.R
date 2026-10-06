# Compact project-level ownership evidence. No questionnaire or RDA payloads.
appusage_projection_implementation <- function() {
  appusage_runtime_implementation("projection", c("appusage_project_summary",
    "appusage_apply_match_projection", "appusage_publish_project", "appusage_write_xlsx"))
}

appusage_matching_implementation <- function() {
  appusage_runtime_implementation("matching", c("appusage_match_self_report_table",
    "extract_wenjuanxing_upload_filenames", "appusage_match_one_self_report_row",
    "appusage_resolve_duplicate_manifest_candidates", "appusage_build_manifest_match_index"))
}

appusage_projection_state <- function(project_dir) {
  path <- file.path(project_dir, "appusage_projection_state.rds")
  if (!file.exists(path)) return(list())
  tryCatch(readRDS(path), error = function(e) list())
}

appusage_save_projection_state <- function(project_dir, name, value) {
  state <- appusage_projection_state(project_dir)
  state[[name]] <- value
  saveRDS(state, file.path(project_dir, "appusage_projection_state.rds"))
  invisible(value)
}

appusage_matching_adapter <- function(project_dir, settings) {
  path <- file.path(project_dir, "workflow_configuration.rds")
  if (!file.exists(path) || !is_present_string(settings$self_report_file)) return(NULL)
  adapter <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!identical(normalized_summary_path(adapter$resolved_self_report_file),
      normalized_summary_path(settings$self_report_file))) return(NULL)
  adapter
}

appusage_matching_request <- function(project_dir, config, first, second,
                                       verify = "content", adapter = NULL) {
  settings <- config$matching
  adapter <- adapter %||% appusage_matching_adapter(project_dir, settings)
  input <- settings$self_report %||% settings$self_report_file
  source <- if (is.data.frame(input)) {
    list(object = appusage_object_fingerprint(input))
  } else if (is_present_string(input) && file.exists(input)) {
    if (verify == "metadata") list(stat = appusage_source_stat(input)) else
      list(artifact = appusage_artifact_signature(input))
  } else NULL
  references <- data.frame(source_file = character(), second_level_rda = character())
  if (nrow(first)) {
    manifest <- appusage_manifest_with_proc2_paths(
      data.frame(source_file = first[["source_file"]]), first, second, project_dir)
    references <- manifest[c("source_file", "second_level_rda")]
    references <- references[order(references$source_file), , drop = FALSE]
    rownames(references) <- NULL
  }
  list(settings = settings[setdiff(names(settings), c("self_report", "self_report_file"))],
    read_options = if (!is.null(adapter)) adapter[c("self_report_sheet", "self_report_n_max",
      "self_report_guess_max", "self_report_col_types", "project_id", "project_name")] else NULL,
    input = source, references = appusage_object_fingerprint(references),
    implementation = appusage_matching_implementation())
}

appusage_matching_request_valid <- function(state, requested, project_dir, verify) {
  if (is.null(state) || is.null(requested$input)) return(FALSE)
  recorded <- state$request
  if (verify == "metadata" && !is.null(requested$input$stat)) recorded$input <- list(stat = state$input_stat)
  appusage_contract_equal(recorded, requested) && appusage_artifact_valid(
    file.path(project_dir, "self_report_link_result.rds"), state$owner, verify != "metadata")
}

appusage_projection_dependencies <- function(project_dir) {
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  paths <- c(file.path(project_dir, "analytic_summary_table_proclevel-1.csv"),
    sort(list.files(file.path(project_dir, "proclevel-2"), pattern = "_proc-2[.]json$", full.names = TRUE)),
    file.path(project_dir, "self_report_link_result.rds"))
  paths <- paths[file.exists(paths)]
  # These are metadata/relationship owners, never raw or scientific RDA files.
  as.list(appusage_file_md5(paths))
}

appusage_projection_valid <- function(state, project_dir, verify = TRUE) {
  if (is.null(state) || !length(state$outputs)) return(FALSE)
  if (!identical(state$implementation, appusage_projection_implementation())) return(FALSE)
  if (!appusage_contract_equal(state$dependencies, appusage_projection_dependencies(project_dir))) return(FALSE)
  all(vapply(names(state$outputs), function(path)
    appusage_artifact_valid(path, state$outputs[[path]], verify), logical(1)))
}

appusage_plan_project_tasks <- function(project_dir, config, source_tasks, first,
                                        second, verify, context = NULL) {
  state <- appusage_projection_state(project_dir)
  recorded_request <- state$matching$request
  matching <- if (config$matching$enabled) "run" else "disabled"
  reason <- if (matching == "disabled") "disabled_by_config" else "matching_contract_changed_or_missing"
  upstream <- any(source_tasks$action[source_tasks$stage %in% c("parse", "research_data")] == "run")
  forced <- config$execution$overwrite || !config$execution$resume
  if (matching == "run") {
    requested <- appusage_matching_request(project_dir, config, first, second, verify)
    if (verify == "metadata" && !is.null(requested$input$stat) && !is.null(recorded_request))
      recorded_request$input <- list(stat = state$matching$input_stat)
    if (is.null(requested$input)) {
      matching <- "blocked"
      reason <- "questionnaire_missing"
    } else if (!upstream && !forced && appusage_matching_request_valid(state$matching, requested, project_dir, verify)) {
      matching <- "reuse"
      reason <- "up_to_date"
    } else if (!is.null(state$matching)) {
      reason <- if (!appusage_contract_equal(recorded_request$input, requested$input)) "questionnaire_changed" else
        if (!identical(recorded_request$implementation, requested$implementation)) "implementation_changed" else
        if (!appusage_contract_equal(recorded_request$settings, requested$settings) ||
            !identical(recorded_request$read_options, requested$read_options)) "configuration_changed" else
        if (!identical(recorded_request$references, requested$references)) "source_availability_changed" else "matching_artifact_missing_or_corrupt"
    }
  }
  if (upstream && matching == "run") reason <- "upstream_required"
  if (forced && matching == "run") reason <- "forced"
  unchanged <- !any(source_tasks$action == "run") && matching %in% c("reuse", "disabled") && !forced
  summary <- if (unchanged && appusage_projection_valid(state$summary, project_dir,
      verify = verify != "metadata")) "reuse" else "run"
  stages <- appusage_stage_registry()$stage[appusage_stage_registry()$scope == "project"]
  codes <- c(reason, if (summary == "reuse") "up_to_date" else if (forced) "forced" else
    if (!is.null(state$summary) && !identical(state$summary$implementation,
      appusage_projection_implementation())) "implementation_changed" else "projection_changed_or_missing")
  difference <- if (matching %in% c("run", "blocked") && !is.null(state$matching))
    appusage_text_paste(appusage_contract_details(recorded_request, requested), collapse = "; ") else reason
  tibble::tibble(source_file = NA_character_, source_record_key = NA_character_,
    task_id = appusage_task_id(project_dir, stages), scope = "project", stage = stages,
    action = c(matching, summary), reason = codes,
    reason_code = codes, details = c(difference, codes[[2]]), verification = verify,
    source_verified = FALSE, payload_loaded = FALSE)
}

appusage_matching_export_path <- function(project_dir, config, adapter = NULL) {
  input <- config$matching$self_report_file
  if (!is_present_string(input) || !config$matching$enabled) return(NA_character_)
  adapter <- adapter %||% appusage_matching_adapter(project_dir, config$matching)
  id <- adapter$project_id %||% project_info_from_root(project_dir)$project_id
  if (!is_present_string(id)) return(file.path(project_dir, "self_report_matched.xlsx"))
  appusage_matched_excel_path(input, project_dir, id)
}

appusage_publish_project <- function(project_dir, config, fresh = NULL, matching = NULL,
                                      adapter = NULL, force = FALSE) {
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  state <- appusage_projection_state(project_dir)
  if (!force && appusage_projection_valid(state$summary, project_dir)) {
    result <- appusage_read_summary_csv(file.path(project_dir, "analytic_summary_table_proclevel-2.csv"))
    attr(result, "stage_reused") <- TRUE
    return(result)
  }
  result <- appusage_project_summary(project_dir, fresh = fresh)
  outputs <- file.path(project_dir, "analytic_summary_table_proclevel-2.csv")
  export <- appusage_matching_export_path(project_dir, config, adapter)
  if (is_present_string(export)) {
    owner <- file.path(project_dir, "self_report_link_result.rds")
    if (is.null(matching) && file.exists(owner)) matching <- readRDS(owner)
    if (!is.null(matching)) {
      old <- state$summary
      same_link <- identical(old$dependencies[[owner]], as.character(appusage_file_md5(owner)[[1]]))
      if (!identical(old$implementation, appusage_projection_implementation()) ||
          !same_link || !appusage_artifact_valid(export, old$outputs[[export]])) {
        appusage_write_xlsx(matching$matched_self_report, export)
      }
      outputs <- c(outputs, export)
    }
  }
  signatures <- lapply(outputs, appusage_artifact_signature)
  names(signatures) <- outputs
  appusage_save_projection_state(project_dir, "summary", list(
    implementation = appusage_projection_implementation(),
    dependencies = appusage_projection_dependencies(project_dir), outputs = signatures,
    columns = names(result),
    annotations = result[intersect(c("source_record_key", "diagnostic_report", "diagnostic_json"), names(result))]))
  attr(result, "stage_reused") <- FALSE
  result
}
