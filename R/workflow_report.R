appusage_workflow_overview <- function(first = NULL, second = NULL,
                                       n_sources = nrow(first), config = NULL) {
  first <- as.data.frame(first %||% list())
  second <- as.data.frame(second %||% list())
  n_sources <- n_sources %||% 0L
  count <- function(values, states, dimension, denominator) {
    values[is.na(values)] <- "not_run"
    states <- unique(ifelse(is.na(states), "not_run", states))
    values <- c(values, rep("not_run", max(0L, denominator - length(values))))
    data.frame(dimension = dimension, state = states,
      n = vapply(states, function(state) sum(values %in% state), integer(1)),
      denominator = denominator, stringsAsFactors = FALSE)
  }
  execution <- bind_appusage_summary_rows(
    count(first$status, c("success", "error", "incomplete", "not_run"), "parse", n_sources),
    count(second$second_level_status %||% second$status, c("success", "error", "skipped", "not_run"), "research_data", n_sources),
    count(second$qc_status, c("success", "error", "not_run", "skipped"), "qc_execution", n_sources))
  completed <- second$qc_status %in% "success"
  qc <- data.frame(state = c("pass", "fail", "not_evaluated"),
    n = c(sum(completed & second$pass_qc %in% TRUE),
      sum(completed & second$pass_qc %in% FALSE),
      n_sources - sum(completed & !is.na(second$pass_qc))),
    denominator = n_sources, stringsAsFactors = FALSE)
  grains <- lapply(c("event", "episode", "daily"), function(grain) {
    rows <- second[[appusage_text_paste0("n_", grain, "_rows")]] %||% rep(NA_real_, nrow(second))
    eligible <- second[[appusage_text_paste0("analysis_eligible_", grain)]] %||% rep(NA, nrow(second))
    unsupported <- if (grain == "event") second$detected_type %in% c("line", "day", "app") else
      if (grain == "episode") second$detected_type %in% c("day", "app") else rep(FALSE, nrow(second))
    disabled <- grain == "episode" && identical(config$reconstruction$reconstruct_meta, FALSE)
    disabled <- rep(disabled, nrow(second)) & second$detected_type %in% "meta"
    success <- (second$second_level_status %||% second$status) %in% "success"
    data.frame(grain = grain, available = sum(success & rows > 0, na.rm = TRUE),
      empty = sum(success & !unsupported & !disabled & rows == 0, na.rm = TRUE),
      not_applicable = sum(success & unsupported), disabled = sum(success & disabled),
      not_generated = n_sources - sum(success),
      eligible = sum(completed & eligible %in% TRUE),
      ineligible = sum(completed & eligible %in% FALSE),
      eligibility_not_evaluated = n_sources - sum(completed & !is.na(eligible)),
      denominator = n_sources, stringsAsFactors = FALSE)
  })
  reasons <- intersect(c("source_record_key", "participant_id", "qc_status", "pass_qc",
    "source_qc_status", "source_qc_severity", "source_qc_episode_reasons", "source_qc_daily_reasons",
    "analysis_eligible_event", "analysis_eligible_episode", "analysis_eligible_daily"), names(second))
  list(execution = tibble::as_tibble(execution), coverage_qc = tibble::as_tibble(qc),
    source_qc = count(second$source_qc_status, unique(c("not_run", second$source_qc_status)), "source_qc", n_sources),
    eligibility_reasons = tibble::as_tibble(second[reasons]),
    grains = tibble::as_tibble(do.call(rbind, grains)))
}

appusage_inspection_commands <- function(project_dir) {
  argument <- encodeString(project_dir, quote = '"')
  c(workflow = sprintf("run_appusage_workflow(project_dir = %s)", argument),
    summary = sprintf("run_appusage_stage(\"summary\", project_dir = %s)", argument),
    qc = sprintf("run_appusage_stage(\"qc\", project_dir = %s)", argument))
}

appusage_plan_artifacts <- function(tasks, verification) {
  out <- tasks[tasks$stage %in% c("parse", "research_data"),
    c("source_file", "stage", "reason_code"), drop = FALSE]
  out$state <- "unverified"
  reasons <- out$reason_code
  out$state[appusage_text_grepl("missing", reasons)] <- "missing"
  out$state[appusage_text_grepl("corrupt|malformed", reasons)] <- "corrupt"
  out$state[appusage_text_grepl("changed|conflict|collision", reasons)] <- "incompatible"
  if (verification != "metadata") out$state[reasons == "up_to_date"] <- "complete"
  out$verification <- verification
  out
}

#' Inspect an APP Usage plan or completed workflow without file access
#'
#' These methods use only the object's stored compact tables. They never read
#' sources, load scientific payloads or repair outputs. QC execution success,
#' coverage screening and grain-specific analysis eligibility are separate.
#' @param object,x An APP Usage plan or workflow result.
#' @param ... Reserved for method compatibility.
#' @return Summary methods return a list of compact tables. Print methods return
#'   their input invisibly.
#' @name appusage_workflow_inspection
NULL

#' @rdname appusage_workflow_inspection
#' @export
summary.appusage_plan <- function(object, ...) {
  list(tasks = object$tasks, project_tasks = object$project_tasks,
    verification = object$verification, overview = object$overview,
    artifacts = appusage_plan_artifacts(object$tasks, object$verification),
    suggested_calls = appusage_inspection_commands(object$project_dir))
}

#' @rdname appusage_workflow_inspection
#' @export
print.appusage_plan <- function(x, ...) {
  cat("APP Usage workflow plan (", x$verification, " verification)\n", sep = "")
  if (identical(x$verification, "metadata")) cat("Source, payload and questionnaire content are not verified. Execution revalidates.\n")
  tasks <- bind_appusage_summary_rows(x$tasks, x$project_tasks)
  print(as.data.frame(table(stage = tasks$stage, action = tasks$action)))
  pending <- unique(tasks$stage[tasks$action %in% c("run", "blocked")])
  if (length(pending)) cat("Inspect reason_code and details before running the workflow or selected stage.\n")
  cat("Resume: ", appusage_inspection_commands(x$project_dir)[["workflow"]], "\n", sep = "")
  invisible(x)
}

#' @rdname appusage_workflow_inspection
#' @export
summary.appusage_workflow_result <- function(object, ...) {
  list(status = object$run_record$status, stages = object$run_record$stages,
    tasks = object$run_record$tasks,
    suggested_calls = appusage_inspection_commands(object$project_dir),
    overview = object$overview %||% appusage_workflow_overview(object$first_level,
      object$qc %||% object$second_level, config = object$effective_config))
}

#' @rdname appusage_workflow_inspection
#' @export
summary.appusage_project_workflow <- function(object, ...) {
  summary.appusage_workflow_result(object, ...)
}

#' @rdname appusage_workflow_inspection
#' @export
print.appusage_workflow_result <- function(x, ...) {
  overview <- summary.appusage_workflow_result(x)
  cat("APP Usage workflow:", overview$status %||% "not_run", "\n")
  print(overview$overview$execution)
  cat("Coverage QC (not execution success):\n")
  print(overview$overview$coverage_qc)
  cat("Grain availability and analysis eligibility:\n")
  print(overview$overview$grains)
  invisible(x)
}

#' @rdname appusage_workflow_inspection
#' @export
print.appusage_project_workflow <- function(x, ...) {
  print.appusage_workflow_result(x, ...)
}
