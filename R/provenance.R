appusage_output_schema_version <- function() {
  "0.3.4"
}

appusage_new_workflow_run_id <- function() {
  seed <- appusage_text_paste(
    format(Sys.time(), "%Y%m%dT%H%M%OS6"),
    Sys.getpid(),
    sprintf("%08x", sample.int(.Machine$integer.max, 1L)),
    sep = "-"
  )
  appusage_text_paste0("run-", appusage_text_gsub("[^A-Za-z0-9-]", "", seed))
}

appusage_canonical_object <- function(x) {
  if (is.list(x)) {
    if (!is.null(names(x))) x <- x[order(names(x))]
    return(lapply(x, appusage_canonical_object))
  }
  if (inherits(x, "POSIXt")) return(format(x, "%Y-%m-%dT%H:%M:%OS6%z"))
  if (inherits(x, "Date")) return(as.character(x))
  x
}

appusage_object_fingerprint <- function(x) {
  text <- appusage_text_paste(utils::capture.output(dput(appusage_canonical_object(x))), collapse = "\n")
  appusage_stable_text_md5(text)
}

appusage_function_fingerprint <- function(function_names) {
  namespace <- asNamespace("appusageR")
  # Include private helpers reachable from the entry functions, so an internal
  # scientific fix cannot leave a stage incorrectly reusable. Base/imported
  # functions are covered by recorded dependency versions, not package bodies.
  pending <- unique(function_names)
  expanded <- character()
  while (length(pending)) {
    name <- pending[[1L]]
    pending <- pending[-1L]
    if (name %in% expanded) next
    expanded <- c(expanded, name)
    fun <- get0(name, envir = namespace, inherits = FALSE)
    if (!is.function(fun)) next
    symbols <- all.names(body(fun), functions = TRUE, unique = TRUE)
    local <- symbols[vapply(symbols, function(symbol)
      is.function(get0(symbol, envir = namespace, inherits = FALSE)), logical(1))]
    pending <- unique(c(pending, setdiff(local, expanded)))
  }
  function_names <- expanded
  text <- unlist(lapply(sort(unique(function_names)), function(name) {
    fun <- get0(name, envir = namespace, inherits = FALSE)
    if (!is.function(fun)) return(c(name, "<unavailable>"))
    c(
      appusage_text_paste0("function=", name),
      appusage_text_paste(utils::capture.output(dput(formals(fun))), collapse = "\n"),
      appusage_text_paste(deparse(body(fun), width.cutoff = 500L), collapse = "\n")
    )
  }), use.names = FALSE)
  appusage_stable_text_md5(text)
}

appusage_parser_implementation_fingerprint <- function() {
  appusage_function_fingerprint(c(appusage_text_implementation_functions(),
    "appusage_source_preflight", "first_level_detect_type", "normalize_first_level_input",
    "appusage_component_boundary_diagnostics", "header_position",
    "parse_line", "parse_meta", "parse_day", "parse_app", "parse_first_level_by_type",
    "parse_line_block", "line_structural_quality", "appusage_parse_context",
    "appusage_context_row_text", "appusage_context_column", "appusage_context_hits",
    "appusage_context_records", "appusage_prepare_source", "appusage_structural_boundaries",
    "appusage_parse_line_context", "appusage_parse_meta_context",
    "appusage_parse_day_context", "appusage_parse_app_context",
    "appusage_context_all_text", "appusage_context_match", "appusage_context_dates",
    "appusage_context_previous_date", "appusage_context_valid", "appusage_context_input",
    "[.appusage_parse_context", "dim.appusage_parse_context", "decode_raw_text",
    "normalize_encoding", "split_lines", "parse_meta_summary_pair", "parse_meta_events_pair",
    "parse_day_block", "parse_app_block"
  ))
}

appusage_second_level_implementation_fingerprint <- function() {
  appusage_function_fingerprint(c(appusage_text_implementation_functions(),
    "make_second_level_appusage", "standardize_appusage", "build_appusage_daily",
    "appusage_frame_timezone", "coerce_episode_column", "daily_from_episodes",
    "reconstruct_meta_episodes", "clip_overlapping_meta_timeline",
    "aggregate_meta_episodes_daily", "appusage_interval_segments",
    "appusage_validate_second_level_daily", "appusage_order_daily",
    "appusage_meta_pair_indices", "appusage_meta_rows_from_indices",
    "appusage_meta_merge_edges", "merge_contiguous_meta_episodes",
    "appusage_source_qc_interval_segments", "appusage_midnight_ms", "appusage_timezone_names",
    "appusage_pair_text", "appusage_resolve_timezone", "ms_to_datetime", "appusage_date_from_datetime",
    "appusage_daily_segment_index", "appusage_interval_calendar", "ms_to_datetime_validated",
    "appusage_date_from_datetime_validated",
    "appusage_qc_context", "appusage_qc_col", "appusage_qc_date",
    "appusage_qc_episode_view", "appusage_qc_valid_intervals",
    "qc_appusage_anomalies", "run_qc_for_second_level_data",
    "appusage_source_anomaly_qc", "appusage_check_episode_anomalies",
    "appusage_check_daily_anomalies", "appusage_daily_total_by_date",
    "appusage_check_export_span_anomalies", "appusage_observed_dates",
    "appusage_line_overlap_qc", "appusage_line_timestamp_qc",
    "appusage_meta_reconstruction_qc"
  ))
}

appusage_text_implementation_functions <- function() {
  # Include every compatibility adapter, including finite codec/case deltas.
  all <- ls(asNamespace("appusageR"), all.names = TRUE)
  all[stringi::stri_startswith_fixed(all, "appusage_text_") &
    all != "appusage_text_implementation_functions"]
}

appusage_git_directory <- function(package_root) {
  marker <- file.path(package_root, ".git")
  if (dir.exists(marker)) return(marker)
  if (!file.exists(marker)) return(NA_character_)
  line <- tryCatch(readLines(marker, n = 1L, warn = FALSE), error = function(e) "")
  if (!length(line) || !appusage_text_grepl("^gitdir:", line)) return(NA_character_)
  value <- appusage_text_trim(appusage_text_sub("^gitdir:", "", line))
  if (!appusage_text_grepl("^[A-Za-z]:[/\\\\]|^/", value)) value <- file.path(package_root, value)
  normalizePath(value, winslash = "/", mustWork = FALSE)
}

appusage_git_commit_from_files <- function(package_root) {
  git_dir <- appusage_git_directory(package_root)
  if (!is_present_string(git_dir) || !dir.exists(git_dir)) return(NA_character_)
  head <- tryCatch(readLines(file.path(git_dir, "HEAD"), n = 1L, warn = FALSE),
    error = function(e) ""
  )
  if (!length(head) || !appusage_text_nzchar(head)) return(NA_character_)
  if (appusage_text_grepl("^[0-9a-fA-F]{40}$", head)) return(appusage_text_lower(head))
  if (!appusage_text_grepl("^ref:", head)) return(NA_character_)
  ref <- appusage_text_trim(appusage_text_sub("^ref:", "", head))
  sha <- tryCatch(readLines(file.path(git_dir, ref), n = 1L, warn = FALSE),
    error = function(e) ""
  )
  if (length(sha) && appusage_text_grepl("^[0-9a-fA-F]{40}$", sha)) appusage_text_lower(sha) else NA_character_
}

appusage_git_build_info <- function(package_root = NULL, git_sha = NULL,
                                     git_dirty = NULL) {
  package_root <- package_root %||% tryCatch(
    appusage_package_root_for_workers(),
    error = function(e) system.file(package = "appusageR")
  )
  env_sha <- Sys.getenv("APPUSAGER_GIT_COMMIT", unset = "")
  sha <- git_sha %||% if (appusage_text_nzchar(env_sha)) env_sha else
    appusage_git_commit_from_files(package_root)
  if (!is_present_string(sha) || !appusage_text_grepl("^[0-9a-fA-F]{7,40}$", sha)) sha <- NA_character_
  env_dirty <- Sys.getenv("APPUSAGER_GIT_DIRTY", unset = "")
  if (is.null(git_dirty) && appusage_text_nzchar(env_dirty)) {
    git_dirty <- appusage_text_lower(env_dirty) %in% c("1", "true", "yes", "dirty")
  }
  marker <- if (isTRUE(git_dirty)) {
    "dirty"
  } else if (identical(git_dirty, FALSE)) {
    "clean"
  } else if (is_present_string(sha)) {
    "commit_available_dirty_state_unavailable"
  } else {
    "unavailable"
  }
  list(
    git_commit_sha = if (is_present_string(sha)) appusage_text_lower(sha) else NA_character_,
    git_build_marker = marker
  )
}

appusage_build_run_provenance <- function(
    tz = appusage_default_timezone(), source_qc_config = NULL,
    workflow_run_id = NULL, git_sha = NULL, git_dirty = NULL,
    package_root = NULL) {
  tz <- appusage_resolve_timezone(tz)
  git <- appusage_git_build_info(package_root, git_sha, git_dirty)
  source_qc <- appusage_source_qc_config(source_qc_config, tz = tz)
  list(
    package_version = as.character(utils::packageVersion("appusageR")),
    output_schema_version = appusage_output_schema_version(),
    workflow_run_id = workflow_run_id %||% appusage_new_workflow_run_id(),
    effective_timezone = tz,
    git_commit_sha = git$git_commit_sha,
    git_build_marker = git$git_build_marker,
    parser_implementation_fingerprint = appusage_parser_implementation_fingerprint(),
    second_level_implementation_fingerprint = appusage_second_level_implementation_fingerprint(),
    research_implementation_fingerprint = appusage_research_implementation_fingerprint(),
    qc_implementation_fingerprint = appusage_qc_implementation_fingerprint(),
    source_qc_config_fingerprint = appusage_object_fingerprint(source_qc),
    source_qc_rule_version = source_qc$rule_version,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z")
  )
}

appusage_resolve_run_provenance <- function(provenance = NULL,
                                             tz = appusage_default_timezone(),
                                             source_qc_config = NULL) {
  if (is.null(provenance)) {
    return(appusage_build_run_provenance(tz, source_qc_config))
  }
  if (!is.list(provenance)) cli::cli_abort("`provenance` must be NULL or a provenance list.")
  required <- c(
    "package_version", "output_schema_version", "workflow_run_id",
    "effective_timezone", "parser_implementation_fingerprint",
    "second_level_implementation_fingerprint", "source_qc_config_fingerprint"
  )
  missing <- required[!vapply(required, function(name) {
    is_present_string(provenance[[name]])
  }, logical(1))]
  if (length(missing)) {
    cli::cli_abort("Provenance is missing required field(s): {paste(missing, collapse = ', ')}")
  }
  provenance$effective_timezone <- appusage_resolve_timezone(provenance$effective_timezone)
  provenance
}

appusage_metadata_provenance <- function(metadata) {
  provenance <- metadata$implementation_provenance %||% metadata$provenance
  if (!is.list(provenance)) list() else provenance
}

appusage_provenance_summary_values <- function(provenance = list()) {
  value <- function(name) {
    x <- provenance[[name]]
    if (is.null(x) || length(x) == 0L) NA_character_ else as.character(x[[1L]])
  }
  list(
    provenance_package_version = value("package_version"),
    provenance_output_schema_version = value("output_schema_version"),
    workflow_run_id = value("workflow_run_id"),
    provenance_effective_timezone = value("effective_timezone"),
    provenance_git_commit_sha = value("git_commit_sha"),
    provenance_git_build_marker = value("git_build_marker"),
    parser_implementation_fingerprint = value("parser_implementation_fingerprint"),
    second_level_implementation_fingerprint = value("second_level_implementation_fingerprint"),
    source_qc_config_fingerprint = value("source_qc_config_fingerprint")
  )
}

appusage_attach_provenance_summary <- function(row, provenance = list()) {
  values <- appusage_provenance_summary_values(provenance)
  for (name in names(values)) {
    if (!name %in% names(row)) {
      row[[name]] <- values[[name]]
      next
    }
    missing <- is.na(row[[name]]) | (is.character(row[[name]]) & !appusage_text_nzchar(row[[name]]))
    row[[name]][missing] <- values[[name]]
  }
  row
}

appusage_provenance_summary_values_from_file <- function(metadata_file) {
  if (!is_present_string(metadata_file) || !file.exists(metadata_file)) {
    return(appusage_provenance_summary_values())
  }
  metadata <- tryCatch(
    appusage_read_json(metadata_file, simplifyVector = TRUE),
    error = function(e) list()
  )
  appusage_provenance_summary_values(appusage_metadata_provenance(metadata))
}

appusage_compare_provenance <- function(cache, current, field) {
  old <- cache[[field]]
  new <- current[[field]]
  if (!is_present_string(old)) return("missing")
  if (!is_present_string(new)) return("current_unavailable")
  if (identical(as.character(old), as.character(new))) "match" else "stale"
}

appusage_read_project_provenance <- function(project_dir) {
  config_file <- file.path(project_dir, "workflow_configuration.rds")
  if (!file.exists(config_file)) return(list())
  config <- tryCatch(readRDS(config_file), error = function(e) list())
  config$current_run_provenance %||% list()
}
