#' Run a serial collection of independently resumable projects
#'
#' Each project uses the existing workflow and per-source cache contracts.
#' Projects execute in table order; file processing inside a project uses the
#' requested workers. A content check precedes every reuse decision. Checkpoints
#' contain compact project status and paths, never scientific payloads.
#' @param projects Data frame with unique character `project_id`, `project_name`,
#'   `project_dir` (raw folder), and `output_dir` (exact output project directory).
#'   Optional `manifest` and `config` list columns supply source manifests and
#'   per-project configurations. Wenjuanxing fields include `self_report_file`,
#'   `upload_col`, `sequence_col`, `submit_time_col`, and `sheet`.
#'   A logical `read_only` column protects selected existing outputs: incomplete
#'   delivery is reported as blocked without attempting repairs or projections.
#' @param batch_dir Directory for collection checkpoints and delivery reports.
#' @param config Default [appusage_config()], overridden by a row's configuration.
#' @param workers Per-project file workers. Defaults to eight; projects are serial.
#' @param continue_on_error Continue other projects after a project failure.
#' @param protect_completed Prevent automatic scientific rebuilds of existing
#'   caches whose implementation/configuration/identity evidence is insufficient.
#'   Missing or damaged artifacts can still be repaired by the existing workflow.
#' @return An `appusage_projects` object with project statuses, validation and
#'   workflow times, paths, and compact stage records. Reordered calls revalidate
#'   evidence and preserve previously checkpointed, unselected projects.
#' @export
run_appusage_projects <- function(projects, batch_dir, config = NULL, workers = 8L,
                                  continue_on_error = TRUE, protect_completed = TRUE) {
  config <- appusage_validate_config(config)
  projects <- appusage_collection_registry(projects, config)
  if (length(workers) != 1L || !is.numeric(workers) || is.na(workers) ||
      workers < 1 || workers %% 1 != 0) cli::cli_abort("`workers` must be a positive integer.")
  if (!is.logical(continue_on_error) || length(continue_on_error) != 1L || is.na(continue_on_error) ||
      !is.logical(protect_completed) || length(protect_completed) != 1L || is.na(protect_completed))
    cli::cli_abort("Collection policies must be TRUE or FALSE.")
  batch_dir <- appusage_collection_path(batch_dir)
  if (any(vapply(projects$output_dir, function(path) appusage_collection_overlap(path, batch_dir), logical(1))) ||
      any(vapply(projects$project_dir, function(path) appusage_collection_overlap(path, batch_dir), logical(1))))
    cli::cli_abort("`batch_dir` must be separate from raw and project output directories.")
  if (any(vapply(projects$manifest, function(manifest) any(vapply(manifest$source_file,
      function(path) appusage_collection_overlap(path, batch_dir), logical(1))), logical(1))))
    cli::cli_abort("Manifest sources must not overlap `batch_dir`.")
  dir.create(batch_dir, recursive = TRUE, showWarnings = FALSE)
  checkpoint <- file.path(batch_dir, "appusage_projects.rds")
  prior <- if (file.exists(checkpoint)) tryCatch(readRDS(checkpoint), error = function(e) NULL) else NULL
  prior_rows <- prior$projects %||% tibble::tibble()
  keep <- if (nrow(prior_rows)) !prior_rows$project_id %in% projects$project_id else logical()
  result <- structure(list(projects = prior_rows[keep, , drop = FALSE], stage_records = list(),
    source_workers = as.integer(workers), project_parallel = FALSE,
    started_at = appusage_workflow_timestamp(), batch_dir = batch_dir),
    class = c("appusage_projects", "list"))
  # Preserve compact records of projects not selected in this call.
  result$stage_records <- prior$stage_records[intersect(names(prior$stage_records), prior_rows[["project_id"]][keep])] %||% list()
  write_checkpoint <- function(row) {
    old <- result$projects
    if (nrow(old)) old <- old[old$project_id != row$project_id[[1]], , drop = FALSE]
    result$projects <<- bind_appusage_summary_rows(old, row)
    saveRDS(result, checkpoint)
  }
  project_row <- function(i, status = "pending") data.frame(
    project_id = projects$project_id[[i]], project_name = projects$project_name[[i]],
    project_key = projects$project_key[[i]], output_dir = projects$output_dir[[i]],
    status = status, action = if (status == "pending") "pending" else "inspect",
    n_sources = nrow(projects$manifest[[i]]), source_issues = NA_integer_,
    validation_elapsed_sec = 0, workflow_elapsed_sec = 0, elapsed_sec = 0,
    requested_workers = as.integer(workers), started_at = NA_character_,
    finished_at = NA_character_, error = NA_character_, stringsAsFactors = FALSE)
  for (i in seq_len(nrow(projects))) write_checkpoint(project_row(i))
  for (i in seq_len(nrow(projects))) {
    entry <- projects[i, , drop = FALSE]
    id <- entry$project_id[[1]]
    effective <- entry$config[[1]]
    effective$execution$parallel <- workers > 1L
    effective$execution$workers <- as.integer(workers)
    manifest <- entry$manifest[[1]]
    clock <- proc.time()[["elapsed"]]
    row <- project_row(i, "running")
    row$started_at <- appusage_workflow_timestamp()
    write_checkpoint(row)
    failure <- tryCatch({
      report <- validate_appusage_project(entry$output_dir[[1]], manifest, effective, "content")
      row$validation_elapsed_sec <- report$elapsed_sec
      first_path <- file.path(entry$output_dir[[1]], "analytic_summary_table_proclevel-1.csv")
      first <- appusage_read_summary_csv(first_path)
      if (!nrow(first) && dir.exists(file.path(entry$output_dir[[1]], "proclevel-1"))) {
        # The planner resolves the same metadata without writing a missing CSV.
        # Use that evidence for protection before permitting the workflow.
        first <- rebuild_first_level_summary_from_cache(entry$output_dir[[1]],
          manifest, write = FALSE)
      }
      requested_adapter <- appusage_collection_adapter(entry, effective)
      if (report$complete && !is.null(requested_adapter)) {
        second <- appusage_read_summary_csv(file.path(entry$output_dir[[1]], "analytic_summary_table_proclevel-2.csv"))
        requested <- appusage_matching_request(entry$output_dir[[1]], effective, first, second,
          adapter = requested_adapter)
        state <- appusage_projection_state(entry$output_dir[[1]])
        if (!appusage_matching_request_valid(state$matching, requested, entry$output_dir[[1]], "content")) {
          report$complete <- FALSE; report$status <- "incomplete"
          report$checks <- bind_appusage_summary_rows(report$checks, data.frame(
            check = "collection_matching_configuration", status = "failed",
            detail = "Requested project/worksheet differs from saved matching settings.", source_file = NA_character_))
          report$project_tasks$action <- "run"
          report$project_tasks$reason_code <- c("configuration_changed", "upstream_required")
          report$project_tasks$details <- "Collection matching settings changed."
        }
      }
      protected <- appusage_collection_protected_sources(report, first, entry$output_dir[[1]])
      if (report$complete) {
        row$status <- "skipped_complete"; row$action <- "reuse"
        result$stage_records[[id]] <- list(status = "reused", source_tasks =
          as.data.frame(table(report$tasks$stage, report$tasks$action)),
          project_tasks = report$project_tasks[c("stage", "action", "reason_code")])
      } else if (entry$read_only[[1]]) {
        row$status <- "blocked"; row$action <- "blocked"
        row$error <- appusage_text_paste0("Read-only project delivery requires review: ",
          appusage_text_paste(unique(report$checks$detail[report$checks$status == "failed"]), collapse = "; "))
      } else if (protect_completed && length(protected)) {
        row$status <- "blocked"; row$action <- "blocked"
        row$error <- appusage_text_paste0("Existing scientific caches need explicit rebuild approval: ",
          appusage_text_paste(unique(protected), collapse = "; "))
      } else {
        work_clock <- proc.time()[["elapsed"]]
        adapter <- NULL
        if (effective$matching$enabled && is_present_string(effective$matching$self_report_file)) {
          read <- appusage_read_self_report_workbook(effective$matching$self_report_file,
            sheet = requested_adapter$self_report_sheet,
            n_max = requested_adapter$self_report_n_max,
            guess_max = requested_adapter$self_report_guess_max,
            col_types = requested_adapter$self_report_col_types)
          adapter <- list(data = read$data, workbook_read = read[c("diagnostics", "diagnostics_file")],
            project_id = id, project_name = entry$project_name[[1]],
            resolved_self_report_file = effective$matching$self_report_file,
            self_report_sheet = requested_adapter$self_report_sheet,
            self_report_n_max = requested_adapter$self_report_n_max,
            self_report_guess_max = requested_adapter$self_report_guess_max,
            self_report_col_types = requested_adapter$self_report_col_types, manifest = manifest,
            sequence_col = effective$matching$sequence_col, upload_col = effective$matching$upload_col,
            submit_time_col = effective$matching$submit_time_col,
            export_type_priority = effective$matching$export_type_priority)
          dir.create(entry$output_dir[[1]], recursive = TRUE, showWarnings = FALSE)
          saved_adapter <- adapter; saved_adapter$data <- saved_adapter$manifest <- NULL
          saveRDS(saved_adapter, file.path(entry$output_dir[[1]], "workflow_configuration.rds"))
          appusage_write_project_manifest(manifest, file.path(entry$output_dir[[1]], "diagnostics"))
        }
        workflow <- appusage_execute_pipeline(manifest, dirname(entry$output_dir[[1]]),
          effective, project_name = entry$project_name[[1]], project_id = id,
          project_dir = entry$output_dir[[1]], matching_adapter = adapter,
          result_callback = appusage_collection_projection_callback(entry$output_dir[[1]]),
          first_options = list(worker_cap_override = TRUE))
        row$workflow_elapsed_sec <- proc.time()[["elapsed"]] - work_clock
        result$stage_records[[id]] <- workflow$run_record[c("status", "stages", "failures", "metrics")]
        rm(workflow)
        report <- validate_appusage_project(entry$output_dir[[1]], manifest, effective, "content")
        row$validation_elapsed_sec <- row$validation_elapsed_sec + report$elapsed_sec
        row$action <- "run"
        row$status <- if (report$complete) report$status else "incomplete"
        if (!report$complete) row$error <- "Delivery checks remain incomplete; inspect the saved report."
      }
      row$source_issues <- sum(report$overview$execution$n[
        report$overview$execution$dimension == "parse" & report$overview$execution$state == "error"])
      saveRDS(report, file.path(batch_dir, appusage_text_paste0("delivery-", sanitize_entity_value(id), ".rds")))
      NULL
    }, error = function(e) conditionMessage(e))
    if (!is.null(failure)) { row$status <- "failed"; row$error <- failure }
    row$finished_at <- appusage_workflow_timestamp()
    row$elapsed_sec <- proc.time()[["elapsed"]] - clock
    write_checkpoint(row)
    result$projects <- result$projects[order(match(result$projects$project_id,
      c(projects$project_id, prior_rows[["project_id"]][keep]))), , drop = FALSE]
    saveRDS(result, checkpoint)
    if (!continue_on_error && row$status %in% c("failed", "blocked", "incomplete")) break
  }
  result$finished_at <- appusage_workflow_timestamp()
  saveRDS(result, checkpoint)
  invisible(result)
}

appusage_collection_projection_callback <- function(project_dir) {
  saved <- appusage_projection_state(project_dir)$summary
  if (!length(saved$columns) || !identical(saved$implementation,
      appusage_projection_implementation())) return(NULL)
  annotations <- intersect(c("diagnostic_report", "diagnostic_json"),
    names(saved$annotations))
  function(stage, value) {
    if (!stage %in% c("second_level", "qc", "category") || !is.data.frame(value))
      return(value)
    before <- attributes(value)
    same_owners <- appusage_contract_equal(saved$dependencies,
      appusage_projection_dependencies(project_dir))
    at <- match(value$source_record_key, saved$annotations$source_record_key)
    for (name in annotations) {
      if (!name %in% names(value))
        value[[name]] <- typed_summary_na(saved$annotations[[name]], nrow(value))
      if (same_owners) {
        selected <- which(!is.na(at) & appusage_missing_summary_value(value[[name]]))
        value[[name]][selected] <- saved$annotations[[name]][at[selected]]
      }
    }
    # Multiple source-stage projections can occur while Excel is still missing.
    # Carry verified diagnostic layout through them until final publication.
    order <- c(intersect(saved$columns, names(value)), setdiff(names(value), saved$columns))
    value <- value[order]
    before$names <- names(value)
    attributes(value) <- before
    value
  }
}

appusage_collection_adapter <- function(entry, config) {
  if (!config$matching$enabled || !is_present_string(config$matching$self_report_file)) return(NULL)
  adapter <- appusage_matching_adapter(entry$output_dir[[1]], config$matching) %||% list(
    self_report_n_max = Inf, self_report_guess_max = Inf, self_report_col_types = NULL)
  sheets <- readxl::excel_sheets(config$matching$self_report_file)
  resolve <- function(sheet) if (is.numeric(sheet)) sheets[[sheet]] else sheet
  desired <- entry$sheet[[1]]
  recorded <- adapter$self_report_sheet %||% 1
  adapter$self_report_sheet <- if (identical(resolve(recorded), resolve(desired))) recorded else desired
  # Preserve an equivalent legacy scalar type without losing character IDs.
  id <- entry$project_id[[1]]
  if (!identical(as.character(adapter$project_id), id)) adapter$project_id <- id
  adapter$project_name <- entry$project_name[[1]]
  adapter$self_report_n_max <- adapter$self_report_n_max %||% Inf
  adapter
}

appusage_collection_protected_sources <- function(report, first, project_dir) {
  if (!nrow(first)) {
    # Without an identity-bearing summary, existing payloads cannot be assigned
    # safely to current sources. Never silently overwrite orphan scientific data.
    artifacts <- unlist(lapply(c("proclevel-1", "proclevel-2"), function(stage)
      list.files(file.path(project_dir, stage), pattern = "[.]rda$",
        recursive = TRUE, full.names = TRUE)), use.names = FALSE)
    return(if (length(artifacts)) "unidentified_scientific_artifacts" else character())
  }
  if (!nrow(report$tasks)) return(character())
  orphaned <- first[["data_file"]][first$status %in% "incomplete" &
    file.exists(first[["data_file"]]) & (is.na(first[["source_file"]]) |
      !appusage_text_nzchar(first[["source_file"]]))]
  if (length(orphaned)) return("unidentified_scientific_artifacts")
  reasons <- c("verification_insufficient", "implementation_changed", "configuration_changed",
    "source_identity_changed", "identity_conflict")
  tasks <- report$tasks
  paths <- first$source_file[first$status %in% "success" & file.exists(first$data_file)]
  unique(tasks$source_file[tasks$source_file %in% paths &
    tasks$stage %in% c("parse", "research_data") & tasks$action == "run" & tasks$reason_code %in% reasons])
}

appusage_collection_overlap <- function(a, b) {
  # Windows paths are case insensitive; normalize trailing separators first.
  a <- appusage_text_lower(appusage_text_gsub("[/\\\\]+$", "", a))
  b <- appusage_text_lower(appusage_text_gsub("[/\\\\]+$", "", b))
  identical(a, b) || stringi::stri_startswith_fixed(a, appusage_text_paste0(b, "/")) ||
    stringi::stri_startswith_fixed(b, appusage_text_paste0(a, "/"))
}

appusage_collection_path <- function(paths) {
  # Resolve the existing ancestor, then append missing components. This also
  # canonicalizes relative paths on platforms with missing-path fallbacks.
  vapply(paths, function(path) {
    suffix <- character()
    while (!file.exists(path)) {
      parent <- dirname(path)
      if (identical(parent, path)) cli::cli_abort("Path has no accessible existing ancestor: {path}")
      suffix <- c(basename(path), suffix)
      path <- parent
    }
    path <- normalizePath(path, winslash = "/", mustWork = TRUE)
    for (part in suffix) {
      if (part == "..") path <- dirname(path) else if (part != ".") path <- file.path(path, part)
    }
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }, character(1), USE.NAMES = FALSE)
}

appusage_collection_registry <- function(projects, config) {
  required <- c("project_id", "project_name", "project_dir", "output_dir")
  if (!is.data.frame(projects) || !nrow(projects) || !all(required %in% names(projects)))
    cli::cli_abort("`projects` requires project_id, project_name, project_dir and output_dir.")
  projects <- as.data.frame(projects, stringsAsFactors = FALSE)
  if (!is.character(projects$project_id) || anyNA(projects$project_id) ||
      any(!appusage_text_nzchar(projects$project_id)) || anyDuplicated(projects$project_id))
    cli::cli_abort("ProjectIDs must be unique nonempty character values, preserving leading zeros.")
  if (anyDuplicated(vapply(projects$project_id, sanitize_entity_value, character(1))))
    cli::cli_abort("ProjectIDs collide after filename normalization.")
  for (name in setdiff(required, "project_id")) {
    if (!is.character(projects[[name]]) || anyNA(projects[[name]]) ||
        any(!appusage_text_nzchar(projects[[name]]))) cli::cli_abort("`{name}` must contain nonempty strings.")
  }
  projects$project_dir <- appusage_collection_path(projects$project_dir)
  projects$output_dir <- appusage_collection_path(projects$output_dir)
  for (i in seq_len(nrow(projects))) {
    for (j in seq_len(nrow(projects))) {
      if (appusage_collection_overlap(projects$project_dir[[i]], projects$output_dir[[j]]))
        cli::cli_abort("Raw and output project directories must not overlap.")
      if (i < j && (appusage_collection_overlap(projects$output_dir[[i]], projects$output_dir[[j]]) ||
          appusage_collection_overlap(projects$project_dir[[i]], projects$project_dir[[j]])))
        cli::cli_abort("Project directories must be independent and nonoverlapping.")
    }
  }
  cell <- function(name, i, default = NULL) {
    if (!name %in% names(projects)) return(default)
    value <- projects[[name]][[i]]
    if (is.null(value) || (length(value) == 1L && is.atomic(value) && is.na(value))) default else value
  }
  manifests <- configs <- vector("list", nrow(projects))
  sheets <- vector("list", nrow(projects))
  read_only <- logical(nrow(projects))
  for (i in seq_len(nrow(projects))) {
    policy <- cell("read_only", i, FALSE)
    if (!is.logical(policy) || length(policy) != 1L || is.na(policy))
      cli::cli_abort("`read_only` must contain TRUE or FALSE for each project.")
    read_only[[i]] <- policy
    effective <- appusage_validate_config(cell("config", i, config))
    questionnaire <- cell("self_report_file", i)
    if (!is.null(questionnaire)) {
      if (!is.character(questionnaire) || length(questionnaire) != 1L || !file.exists(questionnaire))
        cli::cli_abort("Questionnaire path must resolve to one existing workbook.")
      effective$matching <- utils::modifyList(effective$matching, list(enabled = TRUE,
        self_report_file = normalizePath(questionnaire, winslash = "/"), self_report = NULL,
        sequence_col = cell("sequence_col", i, effective$matching$sequence_col),
        upload_col = cell("upload_col", i, effective$matching$upload_col),
        submit_time_col = cell("submit_time_col", i, effective$matching$submit_time_col)), keep.null = TRUE)
    }
    sheets[[i]] <- cell("sheet", i, 1)
    effective <- appusage_validate_config(effective)
    if (effective$matching$enabled && is_present_string(effective$matching$self_report_file)) {
      header <- names(readxl::read_excel(effective$matching$self_report_file,
        sheet = sheets[[i]], n_max = 0, .name_repair = "minimal"))
      fields <- c(effective$matching$sequence_col, effective$matching$upload_col,
        effective$matching$submit_time_col)
      if (any(vapply(fields, function(field) sum(header == field) != 1L, logical(1))))
        cli::cli_abort("Questionnaire columns must resolve uniquely for each project.")
    }
    manifest <- cell("manifest", i)
    if (is.null(manifest)) {
      manifest <- build_appusage_project_manifest(projects$project_dir[[i]], questionnaire)
      manifest <- manifest[manifest$is_txt %in% TRUE, , drop = FALSE]
    }
    manifest <- appusage_workflow_manifest(manifest)
    manifest$source_file <- appusage_collection_path(manifest$source_file)
    if (any(vapply(projects$output_dir, function(output) any(vapply(manifest$source_file,
        function(path) appusage_collection_overlap(path, output), logical(1))), logical(1))))
      cli::cli_abort("Manifest sources must not overlap any project output directory.")
    manifests[[i]] <- manifest
    configs[[i]] <- effective
  }
  projects$manifest <- manifests; projects$config <- configs; projects$sheet <- sheets
  projects$read_only <- read_only
  projects$project_key <- vapply(seq_len(nrow(projects)), function(i)
    appusage_text_paste(projects$project_id[[i]], projects$output_dir[[i]], sep = "|"), character(1))
  projects
}

#' @rdname run_appusage_projects
#' @param object,x A saved collection result.
#' @param ... Reserved for method compatibility.
#' @export
summary.appusage_projects <- function(object, ...) {
  list(projects = object$projects, stage_records = object$stage_records,
    source_workers = object$source_workers, project_parallel = object$project_parallel)
}

#' @rdname run_appusage_projects
#' @export
print.appusage_projects <- function(x, ...) {
  cat("APP Usage project collection (serial projects,", x$source_workers, "file workers)\n")
  print(x$projects[c("project_id", "status", "n_sources", "elapsed_sec")])
  invisible(x)
}
