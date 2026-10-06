# Fixed business stages. This describes execution; it does not alter science.
appusage_stage_registry <- function() {
  data.frame(stage = c("parse", "research_data", "qc", "category", "matching", "summary"),
    scope = c(rep("source", 4L), rep("project", 2L)),
    legacy_stage = c("first_level", "second_level", "qc", "category", "matching", "summary"),
    owner = c("proc1", "proc2_science", "qc", "category", "link_result", "projection"),
    inputs = c("raw", "proc1", "proc2", "proc2,dictionary", "questionnaire,available_sources", "source_metadata,link_result"),
    configuration = c("parse,time", "reconstruction,daily,time,duration_labels", "qc,time", "category",
      "matching", "projection_layout"),
    reuse_evidence = c("source,parse_contract,proc1_integrity", "parse_contract,research_contract,proc2_integrity",
      "research_values,qc_contract,success", "research_values,dictionary_contract",
      "questionnaire,references,matching_contract,link_integrity", "owner_signatures,output_signatures"),
    stringsAsFactors = FALSE)
}

# A call-local observer, never saved in a plan, provenance or worker result.
appusage_runtime_context <- function(provenance = NULL) {
  context <- new.env(parent = emptyenv())
  context$provenance <- provenance
  context$enabled <- isTRUE(getOption("appusageR.diagnostics", FALSE))
  context$metrics <- list()
  context$implementations <- list()
  context$validation_json <- new.env(parent = emptyenv())
  context$summary_csv <- new.env(parent = emptyenv())
  context$index_projects <- character()
  context
}

appusage_runtime_implementation <- function(key, functions) {
  context <- getOption("appusageR.runtime_context")
  cached <- if (!is.null(context)) context$implementations[[key]] else NULL
  if (!is.null(cached)) return(cached)
  value <- appusage_function_fingerprint(functions)
  if (!is.null(context)) context$implementations[[key]] <- value
  value
}

appusage_runtime_measure <- function(context, operation, expr, bytes = 0) {
  if (is.null(context) || !isTRUE(context$enabled)) return(force(expr))
  start <- proc.time()
  on.exit({
    elapsed <- proc.time() - start
    old <- context$metrics[[operation]] %||% c(calls = 0, elapsed_sec = 0, cpu_sec = 0, bytes = 0)
    context$metrics[[operation]] <- old + c(calls = 1, elapsed_sec = unname(elapsed[["elapsed"]]),
      cpu_sec = unname(elapsed[["user.self"]] + elapsed[["sys.self"]]), bytes = bytes)
  }, add = TRUE)
  force(expr)
}

appusage_runtime_json <- function(path, context = NULL) {
  if (!is_present_string(path) || !file.exists(path)) return(NULL)
  appusage_runtime_measure(context, "json_read",
    tryCatch(appusage_read_validation_json(path), error = function(e) NULL),
    bytes = unname(file.info(path)$size[[1]]))
}

appusage_runtime_metrics <- function(context) {
  if (is.null(context) || !length(context$metrics)) return(tibble::tibble())
  rows <- lapply(names(context$metrics), function(name) {
    data.frame(operation = name, as.list(context$metrics[[name]]), stringsAsFactors = FALSE)
  })
  tibble::as_tibble(do.call(rbind, rows))
}

appusage_count <- function(operation, bytes = 0) {
  context <- getOption("appusageR.runtime_context")
  if (is.null(context) || !isTRUE(context$enabled)) return(invisible(NULL))
  old <- context$metrics[[operation]] %||% c(calls = 0, elapsed_sec = 0, cpu_sec = 0, bytes = 0)
  amount <- bytes
  context$metrics[[operation]] <- old + c(calls = 1, elapsed_sec = 0, cpu_sec = 0,
    bytes = if (length(amount)) sum(amount, na.rm = TRUE) else 0)
  invisible(NULL)
}

appusage_read_json <- function(path, ...) {
  appusage_count("json_parse", if (is.character(path)) file.info(path)$size else 0)
  jsonlite::read_json(path, ...)
}

appusage_file_md5 <- function(files) {
  appusage_count("file_md5", file.info(files)$size)
  tools::md5sum(files)
}

appusage_task_id <- function(source, stage) {
  # Paths identify tasks within this project, not scientific source identities.
  appusage_text_paste(source, stage, sep = "::")
}

appusage_new_run_record <- function(project_dir, plan = NULL) {
  state <- new.env(parent = emptyenv())
  tasks <- if (is.null(plan)) tibble::tibble() else
    bind_appusage_summary_rows(plan$tasks, plan$project_tasks)
  if (nrow(tasks)) {
    tasks$planned_action <- tasks$action
    tasks$actual_action <- ifelse(tasks$action == "disabled", "disabled", NA_character_)
    tasks$final_status <- "not_run"
    tasks$result_status <- NA_character_
    tasks$deviation_reason <- NA_character_
    tasks$started_at <- tasks$finished_at <- NA_character_
    tasks$elapsed_sec <- NA_real_
  }
  state$record <- list(started_at = appusage_workflow_timestamp(), status = "running",
    stages = list(), tasks = tasks, plan_file = if (!is.null(plan)) "appusage_plan.rds" else NULL)
  state$clocks <- list()
  state$path <- file.path(project_dir, "appusage_run_record.rds")
  state
}

appusage_record_stage <- function(state, stage, status, value = NULL) {
  item <- state$record$stages[[stage]] %||% list()
  now <- appusage_workflow_timestamp()
  if (status == "started") {
    state$clocks[[stage]] <- proc.time()
    item$started_at <- now
  } else {
    clock <- state$clocks[[stage]]
    item$finished_at <- now
    if (!is.null(clock)) {
      elapsed <- proc.time() - clock
      item$elapsed_sec <- unname(elapsed[["elapsed"]])
      item$cpu_sec <- unname(elapsed[["user.self"]] + elapsed[["sys.self"]])
    }
  }
  item$status <- status
  item$updated_at <- now
  state$record$stages[[stage]] <- item
  registry <- appusage_stage_registry()
  selected <- (state$record$tasks[["stage"]] %||% character()) == registry$stage[match(stage, registry$legacy_stage)]
  for (field in c("started_at", "finished_at", "elapsed_sec")) {
    if (!is.null(item[[field]])) state$record$tasks[[field]][selected] <- item[[field]]
  }
  if (status == "error") {
    state$record$status <- "error"
    state$record$error <- conditionMessage(value)
  }
  saveRDS(state$record, state$path)
  invisible(NULL)
}

appusage_record_tasks <- function(state, stage, actions, statuses = NULL, sources = NULL) {
  tasks <- state$record$tasks
  at <- which((tasks[["stage"]] %||% character()) == stage)
  if (!is.null(sources)) at <- at[match(sources, tasks$source_file[at], nomatch = 0L)]
  if (!length(at)) return(invisible(NULL))
  actions <- rep_len(actions, length(at))
  tasks$actual_action[at] <- actions
  tasks$result_status[at] <- rep_len(statuses %||% ifelse(actions == "blocked", "blocked",
    ifelse(actions == "disabled", "not_run", "success")), length(at))
  tasks$final_status[at] <- ifelse(actions == "reuse", "reused", tasks$result_status[at])
  different <- tasks$planned_action[at] != actions
  tasks$deviation_reason[at[different]] <- "execution_revalidation_or_upstream_result"
  state$record$tasks <- tasks
  invisible(NULL)
}

appusage_record_execution <- function(state, legacy_stage, value, first = NULL) {
  registry <- appusage_stage_registry()
  stage <- registry$stage[match(legacy_stage, registry$legacy_stage)]
  if (is.na(stage)) return(invisible(NULL))
  if (registry$scope[match(stage, registry$stage)] == "project") {
    return(appusage_record_tasks(state, stage,
      if (isTRUE(attr(value, "stage_reused"))) "reuse" else "run"))
  }
  first <- if (stage == "parse") value else first
  first <- as.data.frame(first)
  n <- nrow(first)
  actions <- attr(value, "execution_actions") %||%
    rep(if (isTRUE(attr(value, "stage_reused"))) "reuse" else "run", n)
  value <- as.data.frame(value)
  if (stage == "parse") {
    statuses <- first$status
  } else {
    if (!is.null(value$first_level_data_file) || !is.null(value$first_level_rda)) {
      index <- match(normalized_summary_path(first$data_file),
        normalized_summary_path(value$first_level_data_file %||% value$first_level_rda))
    } else index <- seq_len(n)
    field <- switch(stage, research_data = "second_level_status", qc = "qc_status", category = "app_category_status")
    statuses <- (value[[field]] %||% value$status)[index]
    actions[first$status != "success"] <- "blocked"
    statuses[first$status != "success"] <- "blocked"
    statuses[is.na(statuses)] <- "not_run"
  }
  appusage_record_tasks(state, stage, actions, statuses, sources = first$source_file)
}

# Avoid touching unchanged projections; serialization also preserves the legacy
# CSV representation (blank missing cells, column order and numeric formatting).
appusage_write_csv_if_changed <- function(data, path) {
  temporary <- tempfile("appusage-summary-", tmpdir = dirname(path), fileext = ".csv")
  on.exit(unlink(temporary), add = TRUE)
  utils::write.csv(data, temporary, row.names = FALSE, na = "")
  same <- file.exists(path) && identical(unname(tools::md5sum(path)), unname(tools::md5sum(temporary)))
  if (!same) {
    appusage_runtime_invalidate(path)
    if (!file.copy(temporary, path, overwrite = TRUE)) cli::cli_abort("Cannot write summary: {.path {path}}")
  }
  invisible(!same)
}

appusage_contract_details <- function(recorded, requested, prefix = "") {
  if (is.null(recorded) && is.null(requested)) return(character())
  if (appusage_contract_equal(recorded, requested)) return(character())
  if (is.list(recorded) || is.list(requested)) {
    keys <- union(names(recorded), names(requested))
    if (length(keys)) return(unlist(lapply(keys, function(key) {
      appusage_contract_details(recorded[[key]], requested[[key]],
        if (prefix == "") key else appusage_text_paste(prefix, key, sep = "."))
    }), use.names = FALSE))
  }
  show <- function(x) if (is.null(x)) "<missing>" else appusage_text_paste(as.character(x), collapse = ",")
  appusage_text_paste0(prefix, ": ", show(recorded), " -> ", show(requested))
}
