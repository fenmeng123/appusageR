#' Inspect project delivery and current cache compatibility
#'
#' Checks saved source identities, stage contracts and project projections.
#' Content verification additionally reads source bytes, checks cache signatures,
#' and inspects one scientific RDA at a time. Metadata verification leaves
#' payloads and questionnaire cells unopened. The report is read-only.
#' @param project_dir Exact output project directory.
#' @param x Optional current source manifest or file vector. Omit to inspect the
#'   saved manifest; supply current sources to detect additions and removals.
#' @param config An [appusage_config()]. Defaults to the saved configuration.
#' @param verify `"metadata"` for a preview or `"content"` for full checks.
#' @return An `appusage_delivery` report with `complete`, `status`, verification
#'   scope, checks, source/project tasks, overview, and suggested recovery calls.
#'   Diagnosed upstream failures remain in its source denominator. Complete
#'   delivery can include these failures; metadata previews remain unverified.
#' @export
validate_appusage_project <- function(project_dir, x = NULL, config = NULL,
                                      verify = c("metadata", "content")) {
  verify <- match.arg(verify)
  started <- proc.time()[["elapsed"]]
  project_dir <- normalizePath(project_dir, winslash = "/", mustWork = FALSE)
  previous_context <- getOption("appusageR.runtime_context")
  context <- previous_context %||% appusage_runtime_context()
  options(appusageR.runtime_context = context)
  on.exit(options(appusageR.runtime_context = previous_context), add = TRUE)
  appusage_resume_index_load(project_dir, context)
  config <- appusage_delivery_config(project_dir, config)
  first <- appusage_read_summary_csv(file.path(project_dir, "analytic_summary_table_proclevel-1.csv"))
  second <- appusage_read_summary_csv(file.path(project_dir, "analytic_summary_table_proclevel-2.csv"))
  if (is.null(x)) {
    path <- file.path(project_dir, "appusage_manifest.rds")
    x <- if (file.exists(path)) readRDS(path) else first[intersect("source_file", names(first))]
  }
  checks <- list()
  add <- function(check, passed, detail, source_file = NA_character_) {
    checks[[length(checks) + 1L]] <<- data.frame(check = check,
      status = if (isTRUE(passed)) "passed" else "failed", detail = detail,
      source_file = source_file, stringsAsFactors = FALSE)
  }
  plan <- tryCatch(appusage_prepare_workflow_plan(x, project_dir, config, verify,
    context = context), error = identity)
  if (inherits(plan, "error")) {
    add("workflow_evidence", FALSE, conditionMessage(plan))
    manifest <- if (is.data.frame(x)) x else data.frame(source_file = x)
    tasks <- project_tasks <- tibble::tibble()
    overview <- appusage_workflow_overview(first, second, n_sources = nrow(manifest), config = config)
  } else {
    manifest <- plan$manifest; tasks <- plan$tasks; project_tasks <- plan$project_tasks
    overview <- plan$overview
    sources <- normalized_summary_path(manifest$source_file)
    first_sources <- normalized_summary_path(first[["source_file"]])
    add("first_source_inventory", !anyDuplicated(first_sources) && setequal(sources, first_sources),
      "Current source identities and first-level summary must correspond exactly.")
    # Failed parses also have proc-2 skipped rows and must remain represented.
    second_keys <- as.character(second[["source_record_key"]])
    first_keys <- as.character(first[["source_record_key"]])
    add("second_source_inventory", length(first_keys) == nrow(first) &&
      length(second_keys) == nrow(second) && !anyNA(first_keys) &&
      all(appusage_text_nzchar(first_keys)) && !anyDuplicated(first_keys) &&
      !anyDuplicated(second_keys) && setequal(first_keys, second_keys),
      "First and second summaries must contain the same unique source_record_key values.")
    parse <- tasks[tasks$stage == "parse", , drop = FALSE]
    failed <- first_sources[first[["status"]] %in% "error"]
    expected_block <- tasks$source_file %in% failed & tasks$stage != "parse" &
      tasks$source_file %in% parse$source_file[parse$action == "reuse"]
    pending <- tasks$action == "run" | (tasks$action == "blocked" & !expected_block)
    add("source_stages", !any(pending), "Required stages must be reusable or diagnosed upstream failures.")
    add("project_stages", all(project_tasks$action %in% c("reuse", "disabled")),
      "Matching and summary projections must satisfy their ownership contracts.")
    for (i in seq_len(nrow(first))) {
      row <- first[i, , drop = FALSE]
      path <- row$source_file[[1]]
      meta <- appusage_read_validation_json(row$metadata_file[[1]])
      identity <- appusage_metadata_source_identity(meta)
      add("source_metadata_identity", is_present_string(identity$source_record_key) &&
        identical(identity$source_record_key, appusage_get_col_value(row, "source_record_key", NA_character_)) &&
        identical(identity$source_fingerprint, appusage_get_col_value(row, "source_fingerprint", NA_character_)),
        "JSON source identity must agree with the first-level summary.", path)
      if (row$status[[1]] != "success") {
        diagnosed <- appusage_read_json_safely(row$metadata_file[[1]])
        j <- match(appusage_get_col_value(row, "source_record_key", NA_character_), second[["source_record_key"]])
        add("upstream_failure", row$status[[1]] == "error" && length(diagnosed$errors) > 0L &&
          !is.na(j) && second$second_level_status[[j]] == "skipped",
          "Failed input retains diagnostics and its skipped downstream row.", path)
        next
      }
      if (verify != "content") next
      failure <- tryCatch({
        raw <- load_appusage_data_object(row$data_file[[1]])
        appusage_delivery_raw_contract(raw, row$detected_type[[1]])
        rm(raw)
        j <- match(row$source_record_key[[1]], second[["source_record_key"]])
        if (is.na(j)) stop("Missing second-level identity.")
        row2 <- second[j, , drop = FALSE]
        rda <- appusage_summary_proc2_paths(row2)[[1]]
        data <- load_appusage_data_object(rda)
        appusage_delivery_research_contract(data)
        meta2 <- appusage_read_validation_json(second_level_metadata_path(rda))
        if (!identical(appusage_metadata_source_identity(meta2)$source_record_key, row$source_record_key[[1]]))
          stop("Second-level metadata source identity differs.")
        counts <- second_level_qc_counts(data)
        metadata_counts <- if (inherits(meta2, "appusage_validation_metadata"))
          attr(meta2, "appusage_summary_row") else meta2$counts
        for (name in c("n_event_rows", "n_episode_rows", "n_daily_rows"))
          if (!isTRUE(all.equal(as.numeric(counts[[name]]), as.numeric(metadata_counts[[name]]))))
            stop("Second-level row counts differ from JSON: ", name)
        rm(data)
        NULL
      }, error = function(e) conditionMessage(e))
      add("scientific_payload_contract", is.null(failure), failure %||%
        "Single data object, raw/research frame columns, types and metadata counts verified.", path)
    }
    if (verify == "content" && config$matching$enabled) {
      failure <- tryCatch(appusage_delivery_matching_contract(project_dir, config),
        error = function(e) conditionMessage(e))
      add("questionnaire_export", is.null(failure), failure %||%
        "All questionnaire rows, source links and exported cells verified.")
    }
  }
  checks <- if (length(checks)) tibble::as_tibble(do.call(rbind, checks)) else tibble::tibble()
  complete <- verify == "content" && nrow(checks) > 0L && all(checks$status == "passed")
  legacy <- nrow(tasks) > 0L && any(tasks$reason_code == "verification_insufficient")
  status <- if (complete) {
    if (any(first$status == "error")) "completed_with_source_issues" else "completed"
  } else if (verify == "metadata") "unverified" else if (legacy) "legacy_unverified" else "incomplete"
  structure(list(project_dir = project_dir, verification = verify, complete = complete,
    status = status, n_sources = nrow(manifest), checks = checks, tasks = tasks,
    project_tasks = project_tasks, overview = overview,
    elapsed_sec = proc.time()[["elapsed"]] - started,
    suggested_calls = appusage_inspection_commands(project_dir)),
    class = c("appusage_delivery", "list"))
}

appusage_delivery_config <- function(project_dir, config = NULL) {
  path <- file.path(project_dir, "appusage_configuration.rds")
  if (is.null(config) && file.exists(path)) config <- readRDS(path)
  appusage_validate_config(config)
}

appusage_delivery_raw_contract <- function(data, type) {
  prototypes <- switch(type, line = list(line = empty_line_tibble()),
    meta = list(meta_summary = empty_meta_summary_tibble(), meta_events = empty_meta_events_tibble()),
    day = list(day = empty_day_tibble()), app = list(app = empty_day_tibble()))
  if (is.null(prototypes) || !is.list(data) || !identical(names(data), names(prototypes)))
    stop("Raw data table structure differs from its export type.")
  for (name in names(prototypes)) appusage_delivery_frame_contract(data[[name]],
    strip_individual_columns(prototypes[[name]]), exact = TRUE)
  invisible(TRUE)
}

appusage_delivery_frame_contract <- function(frame, prototype, exact = FALSE) {
  if (!is.data.frame(frame) || !all(names(prototype) %in% names(frame)))
    stop("Missing scientific table or required columns.")
  selected <- names(frame)[names(frame) %in% names(prototype)]
  if (!identical(selected, names(prototype)) || (exact && !identical(names(frame), selected)) ||
      !identical(lapply(frame[selected], class), lapply(prototype, class)))
    stop("Scientific column order or R types differ from the schema.")
  invisible(TRUE)
}

appusage_delivery_research_contract <- function(data) {
  if (!is.list(data) || !identical(names(data), c("event", "episode", "daily")))
    stop("Research data must contain event, episode and daily tables.")
  prototypes <- list(event = empty_second_event_tibble(),
    episode = empty_second_episode_tibble(), daily = empty_second_daily_tibble())
  for (name in names(prototypes)) appusage_delivery_frame_contract(data[[name]], prototypes[[name]])
  invisible(TRUE)
}

appusage_delivery_matching_contract <- function(project_dir, config) {
  link_path <- file.path(project_dir, "self_report_link_result.rds")
  if (!file.exists(link_path)) stop("Missing LinkResult.")
  link <- readRDS(link_path)
  adapter <- appusage_matching_adapter(project_dir, config$matching)
  settings <- config$matching
  questionnaire <- settings$self_report
  guess_max <- adapter$self_report_guess_max
  if (length(guess_max) == 1L && is.na(guess_max)) guess_max <- NULL
  if (is.null(questionnaire)) questionnaire <- appusage_read_self_report_workbook(
    settings$self_report_file, sheet = adapter$self_report_sheet %||% 1,
    n_max = adapter$self_report_n_max %||% Inf, guess_max = guess_max,
    col_types = adapter$self_report_col_types %||% NULL, emit_warning = FALSE)$data
  matched <- link$matched_self_report
  if (!is.data.frame(matched) || nrow(matched) != nrow(questionnaire) ||
      !identical(names(matched)[seq_len(ncol(questionnaire))], names(questionnaire)))
    stop("Questionnaire row or column inventory differs from LinkResult.")
  for (name in names(questionnaire)) if (!isTRUE(all.equal(questionnaire[[name]], matched[[name]])))
    stop("Questionnaire values or row order differ: ", name)
  paths <- matched$moSens_data_dir[matched$moSens_match_status %in% "matched"]
  if (anyNA(paths) || any(!appusage_text_nzchar(paths))) stop("Matched rows lack data paths.")
  absolute <- vapply(paths, function(path) appusage_text_grepl("^([A-Za-z]:|/|\\\\)", path), logical(1))
  if (any(absolute) || any(appusage_text_grepl("(^|[/\\\\])[.][.]([/\\\\]|$)", paths)) ||
      any(!file.exists(file.path(project_dir, paths)))) stop("Matched paths must resolve inside the project.")
  export <- readxl::read_excel(appusage_matching_export_path(project_dir, config),
    col_types = "text", .name_repair = "minimal", trim_ws = FALSE)
  headers <- appusage_delivery_excel_strings(names(matched))
  headers[is.na(headers)] <- ""
  if (!identical(names(export), headers) ||
      nrow(export) != nrow(matched))
    stop("Excel row/column inventory differs from LinkResult.")
  # Excel serializes date/time and missing cells; compare their serialized cells.
  expected <- matched
  for (column in seq_along(expected)) {
    name <- names(expected)[[column]]
    if (!appusage_delivery_excel_cells_equal(export[[column]], expected[[column]]))
      stop("Excel cells differ from LinkResult: ", name)
  }
  NULL
}

appusage_delivery_excel_cells_equal <- function(actual, expected) {
  actual <- as.character(actual)
  desired <- appusage_delivery_excel_text(expected)
  actual[is.na(actual)] <- ""; desired[is.na(desired)] <- ""
  if (identical(actual, desired)) return(TRUE)
  if (length(actual) != length(desired) || !is.numeric(expected) ||
      inherits(expected, c("Date", "POSIXt"))) return(FALSE)
  # Numeric Excel cells can use decimal or exponent notation for the same
  # number. Compare only valid finite numeric tokens, with no tolerance.
  mismatch <- which(actual != desired)
  number <- "\\A[+-]?(?:[0-9]+(?:\\.[0-9]*)?|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?\\z"
  numeric <- stringi::stri_detect_regex(actual[mismatch], number) &
    stringi::stri_detect_regex(desired[mismatch], number)
  if (!all(numeric)) return(FALSE)
  left <- as.numeric(actual[mismatch]); right <- as.numeric(desired[mismatch])
  all(is.finite(left) & is.finite(right) & left == right)
}

appusage_delivery_excel_text <- function(x) {
  if (inherits(x, "Date")) return(as.character(as.numeric(x) + 25569))
  if (inherits(x, "POSIXt")) {
    # openxlsx writes the displayed calendar time as an Excel serial value.
    zone <- attr(x, "tzone") %||% ""
    local <- as.POSIXct(format(x, "%Y-%m-%d %H:%M:%OS", tz = zone[[1]]), tz = "UTC")
    return(as.character(as.numeric(local) / 86400 + 25569))
  }
  if (is.logical(x)) return(ifelse(is.na(x), NA_character_, ifelse(x, "TRUE", "FALSE")))
  appusage_delivery_excel_strings(as.character(x))
}

appusage_delivery_excel_strings <- function(x) {
  # Windows text-mode workbook writing expands each LF once. Model the written
  # representation on the expected side, preserving all existing CR and spaces.
  if (.Platform$OS.type == "windows") x <- stringi::stri_replace_all_fixed(x, "\n", "\r\n")
  # The XML reader treats a raw whitespace-only text node as a blank cell before
  # ST_Xstring decoding. Encoded spaces/control characters are decoded later.
  x[which(stringi::stri_detect_regex(x, "^[ \\t\\r\\n]*$"))] <- NA_character_
  selected <- which(stringi::stri_detect_regex(x, "_x[0-9A-Fa-f]{4}_"))
  if (length(selected)) {
    # readxl decodes ST_Xstring escapes once. Protect literal backslashes before
    # passing only these generated Unicode escapes through stringi.
    text <- stringi::stri_replace_all_fixed(x[selected], "\\", "\\\\")
    text <- stringi::stri_replace_all_regex(text, "_x([0-9A-Fa-f]{4})_", "\\\\u$1")
    x[selected] <- stringi::stri_unescape_unicode(text)
  }
  x
}

#' @rdname validate_appusage_project
#' @param object A saved delivery report.
#' @param ... Reserved for method compatibility.
#' @export
summary.appusage_delivery <- function(object, ...) {
  object[c("status", "complete", "verification", "n_sources", "checks", "overview",
    "elapsed_sec", "suggested_calls")]
}

#' @rdname validate_appusage_project
#' @export
print.appusage_delivery <- function(x, ...) {
  cat("APP Usage project delivery:", x$status, "\n")
  cat("Sources:", x$n_sources, " Verification:", x$verification, "\n")
  print(as.data.frame(table(x$checks$status)))
  invisible(x)
}
