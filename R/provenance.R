appusage_output_schema_version <- function() {
  "0.3.4"
}

appusage_new_workflow_run_id <- function() {
  seed <- paste(
    format(Sys.time(), "%Y%m%dT%H%M%OS6"),
    Sys.getpid(),
    sprintf("%08x", sample.int(.Machine$integer.max, 1L)),
    sep = "-"
  )
  paste0("run-", gsub("[^A-Za-z0-9-]", "", seed))
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
  text <- paste(capture.output(dput(appusage_canonical_object(x))), collapse = "\n")
  appusage_stable_text_md5(text)
}

appusage_function_fingerprint <- function(function_names) {
  namespace <- asNamespace("appusageR")
  text <- unlist(lapply(sort(unique(function_names)), function(name) {
    fun <- get0(name, envir = namespace, inherits = FALSE)
    if (!is.function(fun)) return(c(name, "<unavailable>"))
    c(
      paste0("function=", name),
      paste(capture.output(dput(formals(fun))), collapse = "\n"),
      paste(deparse(body(fun), width.cutoff = 500L), collapse = "\n")
    )
  }), use.names = FALSE)
  appusage_stable_text_md5(text)
}

appusage_parser_implementation_fingerprint <- function() {
  appusage_function_fingerprint(c(
    "appusage_source_preflight", "first_level_detect_type",
    "parse_line", "parse_meta", "parse_day", "parse_app",
    "parse_line_block", "appusage_line_structural_quality"
  ))
}

appusage_second_level_implementation_fingerprint <- function() {
  appusage_function_fingerprint(c(
    "make_second_level_appusage", "daily_from_episodes",
    "reconstruct_meta_episodes", "clip_meta_episode_timeline",
    "aggregate_meta_episodes_daily", "appusage_interval_segments",
    "appusage_validate_second_level_daily", "appusage_order_daily"
  ))
}

appusage_git_directory <- function(package_root) {
  marker <- file.path(package_root, ".git")
  if (dir.exists(marker)) return(marker)
  if (!file.exists(marker)) return(NA_character_)
  line <- tryCatch(readLines(marker, n = 1L, warn = FALSE), error = function(e) "")
  if (!length(line) || !grepl("^gitdir:", line)) return(NA_character_)
  value <- trimws(sub("^gitdir:", "", line))
  if (!grepl("^[A-Za-z]:[/\\\\]|^/", value)) value <- file.path(package_root, value)
  normalizePath(value, winslash = "/", mustWork = FALSE)
}

appusage_git_commit_from_files <- function(package_root) {
  git_dir <- appusage_git_directory(package_root)
  if (!is_present_string(git_dir) || !dir.exists(git_dir)) return(NA_character_)
  head <- tryCatch(readLines(file.path(git_dir, "HEAD"), n = 1L, warn = FALSE),
    error = function(e) ""
  )
  if (!length(head) || !nzchar(head)) return(NA_character_)
  if (grepl("^[0-9a-fA-F]{40}$", head)) return(tolower(head))
  if (!grepl("^ref:", head)) return(NA_character_)
  ref <- trimws(sub("^ref:", "", head))
  sha <- tryCatch(readLines(file.path(git_dir, ref), n = 1L, warn = FALSE),
    error = function(e) ""
  )
  if (length(sha) && grepl("^[0-9a-fA-F]{40}$", sha)) tolower(sha) else NA_character_
}

appusage_git_build_info <- function(package_root = NULL, git_sha = NULL,
                                     git_dirty = NULL) {
  package_root <- package_root %||% tryCatch(
    appusage_package_root_for_workers(),
    error = function(e) system.file(package = "appusageR")
  )
  env_sha <- Sys.getenv("APPUSAGER_GIT_COMMIT", unset = "")
  sha <- git_sha %||% if (nzchar(env_sha)) env_sha else
    appusage_git_commit_from_files(package_root)
  if (!is_present_string(sha) || !grepl("^[0-9a-fA-F]{7,40}$", sha)) sha <- NA_character_
  env_dirty <- Sys.getenv("APPUSAGER_GIT_DIRTY", unset = "")
  if (is.null(git_dirty) && nzchar(env_dirty)) {
    git_dirty <- tolower(env_dirty) %in% c("1", "true", "yes", "dirty")
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
    git_commit_sha = if (is_present_string(sha)) tolower(sha) else NA_character_,
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
    missing <- is.na(row[[name]]) | (is.character(row[[name]]) & !nzchar(row[[name]]))
    row[[name]][missing] <- values[[name]]
  }
  row
}

appusage_provenance_summary_values_from_file <- function(metadata_file) {
  if (!is_present_string(metadata_file) || !file.exists(metadata_file)) {
    return(appusage_provenance_summary_values())
  }
  metadata <- tryCatch(
    jsonlite::read_json(metadata_file, simplifyVector = TRUE),
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
