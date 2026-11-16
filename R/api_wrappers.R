#' Read APP Usage text into normalized lines
#'
#' Module 1 convenience wrapper for raw APP Usage text input. It keeps only the
#' normalized line vector plus lightweight source metadata, so the result can be
#' passed directly to `run_first_level_appusage()`.
#'
#' @param x File path, single text string, or character vector of lines.
#' @param input Input mode: `"file"`, `"text"`, or `"lines"`.
#' @param encoding Source encoding or `"auto"`.
#'
#' @return A lightweight `appusage_text` list.
#' @details
#' The result contains `lines`, source basename/path, requested encoding,
#' line count and read warnings. Passing it to the first-level wrapper keeps
#' source identity while reusing the decoded text.
#' @seealso [appusage_demo], [appusage_synthetic_project]
#' @examples
#' data(appusage_demo)
#' folder <- system.file("extdata", "synth", package = "appusageR", mustWork = TRUE)
#' path <- file.path(folder, appusage_demo$sources$source_relpath[1L])
#' text <- read_appusage_text(path)
#' text[c("source_file", "n_lines", "requested_encoding")]
#' first <- run_first_level_appusage(text, strict = TRUE)
#' stopifnot(first$status == "success", nrow(first$data$line) == 14L)
#' @export
read_appusage_text <- function(x, input = c("file", "text", "lines"),
                               encoding = "auto") {
  input <- match.arg(input)
  warnings <- character()
  lines <- withCallingHandlers(
    read_appusage_lines(x, input = input, encoding = encoding),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  source_path <- if (identical(input, "file")) {
    normalizePath(x, winslash = "/", mustWork = FALSE)
  } else {
    NA_character_
  }
  out <- list(
    lines = lines,
    input = input,
    source_file = if (identical(input, "file")) basename(x) else NA_character_,
    source_path = source_path,
    encoding = encoding,
    requested_encoding = encoding,
    n_lines = length(lines),
    warnings = unique(warnings),
    read_warning = if (length(warnings) == 0) {
      NA_character_
    } else {
      appusage_text_paste(unique(warnings), collapse = "\n")
    }
  )
  class(out) <- c("appusage_text", "list")
  out
}

#' Run first-level APP Usage preprocessing for one source
#'
#' Module 2 wrapper around content detection and the faithful raw parsers. When
#' cache writing is requested, it writes the same BIDS-like `proc-1` RDA/JSON
#' pair used by batch preprocessing.
#'
#' @param x File path, text, lines, or an `appusage_text` object from
#'   `read_appusage_text()`.
#' @param input Input mode when `x` is not an `appusage_text` object.
#' @param type `"auto"` parses bounded line and meta blocks together. A format
#'   selects that component. `"legacy"` applies the earlier single-component selection.
#' @param participant_id Optional participant ID for metadata and cache naming.
#' @param tz Time zone passed to parsers.
#' @param encoding Source encoding or `"auto"`.
#' @param strict Whether parser errors should be strict.
#' @param output_dir Optional directory for first-level cache files.
#' @param write_cache Whether to write first-level cache files.
#' @param overwrite Whether existing cache files may be overwritten.
#' @param parser_strict Parser tolerance, separate from stop-on-error behavior.
#'   Defaults to `strict` for compatibility with the original single-source API.
#'
#' @return An `appusage_first_level` list with parsed data and metadata.
#' @details
#' Inspect `status` before using `data`. Successful results contain faithful
#' format-specific tables, `metadata` and optional cache paths. `strict = FALSE`
#' returns a failed-source result; `parser_strict = TRUE` retains the parser
#' quality gate. File names identify the source; content determines its format.
#' @seealso [appusage_demo], [appusage_synthetic_project]
#' @examples
#' data(appusage_demo)
#' folder <- system.file("extdata", "synth", package = "appusageR", mustWork = TRUE)
#' path <- file.path(folder, appusage_demo$sources$source_relpath[1L])
#' first <- run_first_level_appusage(path, strict = TRUE)
#' first$data$line[, c("app_name", "duration_ms")]
#' stopifnot(sum(first$data$line$duration_ms) == 12600000)
#' # Explicitly select a malformed synthetic input and retain its diagnosis.
#' bad <- file.path(folder, appusage_demo$sources$source_relpath[15L])
#' failed <- run_first_level_appusage(bad, strict = FALSE, parser_strict = TRUE)
#' stopifnot(failed$status == "error")
#' @export
run_first_level_appusage <- function(x, input = c("file", "text", "lines"),
                                     type = c("auto", "line", "meta", "day", "app", "legacy"),
                                     participant_id = NULL,
                                     tz = "Asia/Shanghai",
                                     encoding = "auto",
                                     strict = FALSE,
                                     output_dir = NULL,
                                     write_cache = !is.null(output_dir),
                                     overwrite = FALSE, parser_strict = strict) {
  input <- match.arg(input)
  type <- match.arg(type)
  tz <- appusage_resolve_timezone(tz)
  normalized <- normalize_first_level_input(x, input = input, encoding = encoding)
  id_info <- first_level_wrapper_id_info(normalized$source_file,
    normalized$metadata_input, participant_id)
  id_info$native_export_type_raw <- id_info$native_export_type_from_filename
  if (isTRUE(write_cache) && is.null(output_dir)) {
    cli::cli_abort("`output_dir` is required when `write_cache = TRUE`.")
  }
  identity_x <- if (normalized$metadata_input == "file") normalized$source_file else {
    if (inherits(x, "appusage_text")) x$lines else x
  }
  out <- preprocess_one_appusage(
    x = normalized$x, id_info = id_info, type = type, input = normalized$input,
    output_dir = if (isTRUE(write_cache)) output_dir else NULL,
    tz = tz, encoding = encoding, overwrite = overwrite, index = 1L,
    return_result = TRUE, parser_strict = parser_strict,
    source_ref = list(source_file = normalized$source_file,
      input = normalized$metadata_input, identity_x = identity_x)
  )
  if (isTRUE(strict) && !is.null(out$condition)) stop(out$condition)
  if (inherits(out$condition, "appusage_cache_exists")) stop(out$condition)
  out$condition <- NULL
  out
}

#' Run second-level APP Usage preprocessing for one source
#'
#' Module 3 wrapper around [make_second_level_appusage()] and
#' [write_second_level_appusage()]. Meta episode reconstruction is explicit and
#' enabled by default in second-level meta processing.
#'
#' @param x First-level wrapper result, first-level RDA path, parser output, or
#'   first-level data list.
#' @param export_type Optional export type override for in-memory data.
#' @param output_dir Optional directory for `proc-2` cache files.
#' @param write_cache Whether to write `proc-2` cache files.
#' @param overwrite Whether existing cache files may be overwritten.
#' @param include_collection_app Whether to keep `com.w.appusage` rows.
#' @param max_episode_ms Episode anomaly threshold.
#' @param max_daily_app_ms Daily-app anomaly threshold.
#' @param reconstruct_meta Whether to explicitly reconstruct meta Table 2 events
#'   into episode records. Defaults to `TRUE` for second-level meta output.
#' @param meta_pairing Pairing strategy for meta event reconstruction.
#' @param meta_start_event_types Event types that start meta episodes.
#' @param meta_end_event_types Event types that end meta episodes.
#' @param merge_meta_episodes Whether to merge adjacent complete reconstructed
#'   meta episodes for the same app when their time gap is small.
#' @param meta_episode_merge_gap_ms Maximum non-negative gap, in milliseconds,
#'   allowed when merging adjacent reconstructed meta episodes.
#' @param meta_daily_source Source for meta daily rows.
#' @param tz Effective IANA time zone for timestamp-derived dates and daily
#'   interval boundaries.
#'
#' @return An `appusage_second_level` list.
#' @details
#' The successful result has `data` (typed event/episode/daily tables),
#' `data_file` and `metadata_file`; paths are missing for in-memory execution.
#' Unavailable grains are typed empty tables. In mixed auto mode, omitted meta
#' options use native line episodes/daily totals and meta events. Explicit meta
#' reconstruction requires selecting the meta component at first level.
#' Durations and timestamps ending in `_ms` are numeric milliseconds.
#' @seealso [appusage_demo], [appusage_synthetic_project]
#' @examples
#' data(appusage_demo)
#' folder <- system.file("extdata", "synth", package = "appusageR", mustWork = TRUE)
#' path <- file.path(folder, appusage_demo$sources$source_relpath[5L])
#' first <- run_first_level_appusage(path, strict = TRUE)
#' second <- run_second_level_appusage(first)
#' vapply(second$data, nrow, integer(1))
#' stopifnot(identical(second$data, appusage_demo$records$mixed_line_meta$data))
#' @export
run_second_level_appusage <- function(x, export_type = NULL,
                                      output_dir = NULL,
                                      write_cache = !is.null(output_dir),
                                      overwrite = FALSE,
                                      include_collection_app = TRUE,
                                      max_episode_ms = 24 * 60 * 60 * 1000,
                                      max_daily_app_ms = 24 * 60 * 60 * 1000,
                                      reconstruct_meta = TRUE,
                                      meta_pairing = c("package", "package_class"),
                                      meta_start_event_types = 1,
                                      meta_end_event_types = c(2, 23),
                                       merge_meta_episodes = TRUE,
                                       meta_episode_merge_gap_ms = 30 * 1000,
                                       meta_daily_source = c("summary", "episodes", "both"),
                                       tz = "Asia/Shanghai") {
  explicit <- list(reconstruct_meta = !missing(reconstruct_meta),
                   meta_daily_source = !missing(meta_daily_source))
  tz <- appusage_resolve_timezone(tz)
  meta_pairing <- match.arg(meta_pairing)
  meta_daily_source <- match.arg(meta_daily_source)
  first_level_rda <- NA_character_
  first_data <- x
  if (inherits(x, "appusage_first_level")) {
    if (!identical(x$status, "success")) {
      cli::cli_abort("Second-level processing requires a successful first-level result.",
        class = "appusage_first_cache_unverified")
    }
    first_level_rda <- x$data_file
    first_data <- x$data
    export_type <- export_type %||% x$type
  } else if (is.character(x) && length(x) == 1 && file.exists(x)) {
    first_level_rda <- x
    if (!isTRUE(write_cache)) first_data <- load_appusage_data_object(x)
    export_type <- export_type %||% parse_appusage_filename(x)$type
  }

  data_file <- NA_character_
  metadata_file <- NA_character_
  args <- appusage_forward_meta(list(include_collection_app = include_collection_app,
    max_episode_ms = max_episode_ms, max_daily_app_ms = max_daily_app_ms,
    reconstruct_meta = reconstruct_meta, meta_pairing = meta_pairing,
    meta_start_event_types = meta_start_event_types, meta_end_event_types = meta_end_event_types,
    merge_meta_episodes = merge_meta_episodes, meta_episode_merge_gap_ms = meta_episode_merge_gap_ms,
    meta_daily_source = meta_daily_source, tz = tz), explicit)
  if (isTRUE(write_cache)) {
    if (!is_present_string(first_level_rda) || !file.exists(first_level_rda)) {
      cli::cli_abort("A first-level RDA path is required when `write_cache = TRUE`.")
    }
    data_file <- do.call(write_second_level_appusage, c(list(
      first_level_rda = first_level_rda, output_dir = output_dir, overwrite = overwrite), args))
    second_data <- load_appusage_data_object(data_file)
    metadata_file <- second_level_metadata_path(data_file)
  } else {
    second_data <- do.call(make_second_level_appusage, c(list(data = quote(first_data), export_type = export_type), args))
  }

  out <- list(
    status = "success",
    data = second_data,
    data_file = ifelse(is.na(data_file), NA_character_, normalizePath(data_file, winslash = "/", mustWork = FALSE)),
    metadata_file = ifelse(is.na(metadata_file), NA_character_, normalizePath(metadata_file, winslash = "/", mustWork = FALSE))
  )
  if (!write_cache && inherits(x, "appusage_first_level"))
    out$metadata <- x$metadata[c("day_coverage", "export")]
  if (!write_cache && is.character(x) && is_present_string(first_level_rda)) {
    paired <- first_level_metadata_path(first_level_rda)
    if (file.exists(paired)) out$audit_metadata_file <- paired
  }
  class(out) <- c("appusage_second_level", "list")
  out
}

#' Run APP Usage QC
#'
#' Module 4 wrapper for in-memory daily QC or project-level QC metadata updates.
#' In project mode this calls [write_qc_metadata_batch()] and updates `proc-2`
#' JSON metadata without creating `proclevel-3`.
#'
#' @param x A daily data frame, second-level data list, or second-level wrapper
#'   result.
#' @param project_dir Optional project directory for metadata-writing QC.
#' @param participant_col Participant column for in-memory daily QC.
#' @param strict,overwrite,progress Passed to [write_qc_metadata_batch()] in
#'   project mode.
#' @param require_all_weekdays,min_nonempty_days,use_all_apps_row,include_collection_app,drop_likely_total_all_rows,all_row_tolerance
#'   Existing QC rule parameters.
#' @param max_episode_ms,max_daily_app_ms,max_daily_total_ms Numeric anomaly
#'   thresholds in milliseconds for project-mode QC metadata.
#' @param max_export_lookback_days Maximum expected lookback in days from native
#'   export timestamp to observed record dates.
#' @param meta_diff_abs_ms,meta_diff_ratio Thresholds for meta summary-vs-episode
#'   duration disagreement checks.
#'
#' @return A QC tibble or project summary tibble.
#' @details
#' The in-memory branch returns coverage QC from [qc_appusage_day()]. If a
#' participant column is absent, it uses a single record owner. For complete
#' in-memory source/anomaly checks use [assess_appusage_qc()]. The project
#' branch updates existing proc-2 metadata using [write_qc_metadata_batch()].
#' @seealso [appusage_demo], [appusage_synthetic_project]
#' @examples
#' data(appusage_demo)
#' qc <- run_qc_appusage(appusage_demo$records$line_seven_days)
#' stopifnot(qc$pass_qc, qc$n_nonempty_days == 7L)
#' qc[, c("participant_id", "n_nonempty_days", "pass_qc")]
#' @export
run_qc_appusage <- function(x = NULL, project_dir = NULL,
                            participant_col = "participant_id",
                            strict = FALSE, overwrite = TRUE,
                            progress = TRUE,
                            require_all_weekdays = TRUE,
                            min_nonempty_days = 7,
                            use_all_apps_row = FALSE,
                            include_collection_app = TRUE,
                            drop_likely_total_all_rows = TRUE,
                            all_row_tolerance = 0.10,
                            max_episode_ms = 24 * 60 * 60 * 1000,
                            max_daily_app_ms = 24 * 60 * 60 * 1000,
                            max_daily_total_ms = 24 * 60 * 60 * 1000,
                            max_export_lookback_days = 31,
                            meta_diff_abs_ms = 60 * 1000,
                            meta_diff_ratio = 0.20) {
  if (!is.null(project_dir)) {
    return(write_qc_metadata_batch(
      project_dir = project_dir,
      strict = strict,
      overwrite = overwrite,
      progress = progress,
      require_all_weekdays = require_all_weekdays,
      min_nonempty_days = min_nonempty_days,
      use_all_apps_row = use_all_apps_row,
      include_collection_app = include_collection_app,
      drop_likely_total_all_rows = drop_likely_total_all_rows,
      all_row_tolerance = all_row_tolerance,
      max_episode_ms = max_episode_ms,
      max_daily_app_ms = max_daily_app_ms,
      max_daily_total_ms = max_daily_total_ms,
      max_export_lookback_days = max_export_lookback_days,
      meta_diff_abs_ms = meta_diff_abs_ms,
      meta_diff_ratio = meta_diff_ratio
    ))
  }
  if (inherits(x, "appusage_second_level")) {
    x <- x$data
  }
  daily <- if (is.list(x) && !is.data.frame(x) && !is.null(x$daily)) {
    x$daily
  } else {
    x
  }
  if (!is.data.frame(daily)) {
    cli::cli_abort("`x` must be a daily data frame, second-level data list, or `project_dir`.")
  }
  if (!participant_col %in% names(daily)) {
    daily[[participant_col]] <- "record-000001"
  }
  qc_appusage_day(
    daily,
    participant_col = participant_col,
    require_all_weekdays = require_all_weekdays,
    min_nonempty_days = min_nonempty_days,
    use_all_apps_row = use_all_apps_row,
    include_collection_app = include_collection_app,
    drop_likely_total_all_rows = drop_likely_total_all_rows,
    all_row_tolerance = all_row_tolerance
  )
}

#' Extract uncoded apps after category enrichment
#'
#' Module 5 helper for dictionary maintenance. It reports rows where either
#' category column is missing and does not infer or assign categories.
#'
#' @param x Data frame or second-level data list after [add_app_categories()].
#'
#' @return A tibble of de-duplicated uncoded app rows.
#' @details
#' Identities are deduplicated across available grains. Duration and `n_rows`
#' come from one canonical usage table, rather than summing event, episode and
#' daily representations. `ALL` rows are excluded. Missing or unusable duration
#' is `NA`, not zero. For population denominators and manual-review priorities
#' use [profile_uncoded_apps()] with an explicit primary-source/person manifest.
#' @seealso [appusage_demo], [appusage_synthetic_project], [appusage_configuration]
#' @examples
#' data(appusage_demo)
#' # Explicit teaching labels for fictional applications, separate from the dictionary.
#' dictionary <- data.frame(
#'   App_UUID = "org.example.reader", App_Name = "Demo Reader",
#'   Level_1_Category = "Education and Business",
#'   Level_2_Category = "Books and Reference")
#' classified <- add_app_categories(appusage_demo$records$line_seven_days$data,
#'                                   dictionary)
#' uncoded <- extract_uncoded_apps(classified)
#' stopifnot(nrow(uncoded) == 1L, uncoded$package_name == "org.example.music",
#'           uncoded$total_duration_ms == 8400000)
#' uncoded
#' @export
extract_uncoded_apps <- function(x) {
  frames <- if (is.data.frame(x)) list(x) else if (is.list(x))
    x[intersect(c("event", "episode", "daily"), names(x))] else
      cli::cli_abort("x must be a data frame or saved second-level data list.")
  identities <- lapply(frames, function(f) {
    if (!is.data.frame(f) || !nrow(f) || !all(c("package_name", "app_name") %in% names(f))) return(NULL)
    missing <- if (all(c("Level_1_Category", "Level_2_Category") %in% names(f)))
      is.na(category_trim(f$Level_1_Category)) | is.na(category_trim(f$Level_2_Category)) else rep(TRUE, nrow(f))
    total <- standardize_package_name(f$package_name) %in% "ALL"
    if ("is_all_apps" %in% names(f)) total <- total | f$is_all_apps %in% TRUE
    keep <- missing & !total
    data.frame(package_name = as.character(f$package_name[keep]), app_name = as.character(f$app_name[keep]),
      source_export_type = if ("source_export_type" %in% names(f)) as.character(f$source_export_type[keep]) else rep(NA_character_, sum(keep)),
      stringsAsFactors = FALSE)
  })
  identities <- Filter(Negate(is.null), identities)
  if (!length(identities)) return(empty_uncoded_apps_tibble())
  data <- unique(do.call(rbind, identities))
  if (!nrow(data)) return(empty_uncoded_apps_tibble())
  table <- appusage_category_usage_table(x)
  keys <- paste(data$package_name, data$app_name, sep = "\r")
  if (nrow(table) && all(c("package_name", "app_name") %in% names(table))) {
    tk <- paste(table$package_name, table$app_name, sep = "\r")
    groups <- split(seq_len(nrow(table)), tk)
  } else groups <- list()
  data$n_rows <- vapply(keys, function(k) length(groups[[k]]), integer(1))
  data$total_duration_ms <- vapply(keys, function(k) {
    rows <- groups[[k]]
    if (!length(rows) || !"duration_ms" %in% names(table)) return(NA_real_)
    values <- table$duration_ms[rows]
    if (any(!is.finite(values) | values < 0)) NA_real_ else sum(values)
  }, double(1))
  tibble::as_tibble(data)
}

#' Run the standard APP Usage preprocessing workflow
#'
#' Shared, configuration-driven execution of faithful parsing, research-data
#' construction, routine QC and optional category/relationship modules.
#'
#' @param x Input vector passed to [read_appusage_batch()].
#' @param output_dir Parent output directory for the BIDS-like project folder.
#' @param ids Optional participant IDs aligned with `x`.
#' @param type,input,tz,encoding,strict Existing first-level batch parameters.
#' @param run_second_level Whether to run second-level preprocessing.
#' @param run_qc Whether to update QC metadata.
#' @param dictionary Optional category dictionary data frame or path.
#' @param run_category Whether to run category enrichment.
#' @param progress,parallel,n_cores Existing batch workflow controls.
#' @param overwrite Whether cache outputs may be overwritten.
#' @param project_name,project_id Optional project folder identifiers.
#' @param reconstruct_meta Whether to explicitly reconstruct meta Table 2 events
#'   during second-level preprocessing. Defaults to `TRUE`.
#' @param meta_pairing Pairing strategy for meta event reconstruction.
#' @param meta_start_event_types Event types that start meta episodes.
#' @param meta_end_event_types Event types that end meta episodes.
#' @param merge_meta_episodes Whether to merge adjacent complete reconstructed
#'   meta episodes for the same app when their time gap is small.
#' @param meta_episode_merge_gap_ms Maximum non-negative gap, in milliseconds,
#'   allowed when merging adjacent reconstructed meta episodes.
#' @param meta_daily_source Source for meta daily rows.
#'
#' @return Invisibly returns an `appusage_workflow_result` list of compact
#'   summaries and paths.
#' @param config Validated [appusage_config()]. When supplied, it replaces the
#'   legacy scientific/execution arguments.
#' @param project_dir Exact output project directory for reproducible resumes.
#' @param plan Optional [plan_appusage_workflow()] result; revalidated at run time.
#' @param resume Whether compatible existing source stages may be reused.
#' @details
#' Results include `first_level`, `second_level`, `qc`, `project_dir`,
#' `plan` and `run_record`. Resume using the same exact output project directory
#' so its manifest/configuration and paired artifacts can be checked.
#' `overwrite` is not needed for an ordinary unchanged resume. A missing or
#' invalid pair schedules the affected stage; upstream compatible work is reused.
#' The example creates and repairs only a temporary teaching cache.
#' @seealso [appusage_demo], [appusage_synthetic_project]
#' @examples
#' local({
#'   data(appusage_demo)
#'   folder <- system.file("extdata", "synth", package = "appusageR", mustWork = TRUE)
#'   path <- file.path(folder, appusage_demo$sources$source_relpath[1L])
#'   output <- tempfile("appusage-example-")
#'   on.exit(unlink(output, recursive = TRUE))
#'   result <- run_appusage_workflow(path, project_dir = output,
#'     parallel = FALSE, n_cores = 1L, progress = FALSE, strict = TRUE)
#'   plan <- plan_appusage_workflow(project_dir = output, verify = "content")
#'   stopifnot(all(plan$tasks$action %in% c("reuse", "disabled")))
#'   resumed <- run_appusage_workflow(project_dir = output)
#'   stopifnot(all(attr(resumed$first_level, "execution_actions") == "reuse"))
#'   # Remove only the generated temporary proc-2 JSON to demonstrate repair.
#'   unlink(result$second_level$second_level_metadata_file[[1L]])
#'   repair <- plan_appusage_workflow(project_dir = output, verify = "content")
#'   stopifnot(repair$tasks$action[repair$tasks$stage == "research_data"] == "run")
#'   restored <- run_appusage_workflow(project_dir = output)
#'   stopifnot(file.exists(restored$second_level$second_level_metadata_file[[1L]]))
#' })
#' @export
run_appusage_workflow <- function(x = NULL, output_dir = NULL, ids = NULL,
                                  type = "auto", input = "file",
                                  tz = "Asia/Shanghai",
                                  encoding = "auto",
                                  strict = FALSE,
                                  run_second_level = TRUE,
                                  run_qc = TRUE,
                                  dictionary = NULL,
                                  run_category = !is.null(dictionary),
                                  progress = TRUE,
                                  parallel = FALSE,
                                  n_cores = 1,
                                  overwrite = FALSE,
                                  project_name = NULL,
                                  project_id = NULL,
                                  reconstruct_meta = TRUE,
                                  meta_pairing = c("package", "package_class"),
                                  meta_start_event_types = 1,
                                  meta_end_event_types = c(2, 23),
                                  merge_meta_episodes = TRUE,
                                  meta_episode_merge_gap_ms = 30 * 1000,
                                  meta_daily_source = c("summary", "episodes", "both"),
                                  config = NULL, project_dir = NULL, plan = NULL,
                                  resume = TRUE) {
  if (!is.null(config) || !is.null(plan)) {
    supplied <- names(match.call())[-1L]
    conflict <- setdiff(supplied, c("x", "output_dir", "ids", "config", "plan",
      "project_dir", "project_name", "project_id", "run_second_level"))
    if (length(conflict)) cli::cli_abort("Configure {paste(conflict, collapse = ', ')} inside `config`; do not mix explicit legacy settings with a configuration/plan.")
  }
  if (!is.null(plan)) {
    if (!inherits(plan, "appusage_plan")) cli::cli_abort("`plan` must be an appusage_plan.")
    x <- x %||% plan$manifest
    config <- config %||% plan$config
    project_dir <- project_dir %||% plan$project_dir
  }
  if (is.null(x) && !is.null(project_dir)) {
    path <- file.path(project_dir, "appusage_manifest.rds")
    if (file.exists(path)) x <- readRDS(path)
    path <- file.path(project_dir, "appusage_configuration.rds")
    if (is.null(config) && file.exists(path)) {
      config <- appusage_validate_config(readRDS(path))
      if (!missing(reconstruct_meta) || !missing(meta_daily_source) || !missing(type)) {
        explicit <- appusage_meta_explicit(config)
        if (!missing(type) && identical(type, "auto") &&
            any(vapply(explicit, identical, logical(1), "legacy_unknown"))) {
          cli::cli_abort("Create a new appusage_config() to use automatic mixed parsing with this legacy project.")
        }
        if (!missing(reconstruct_meta)) {
          config$reconstruction$reconstruct_meta <- reconstruct_meta
          explicit$reconstruct_meta <- TRUE
        }
        if (!missing(meta_daily_source)) {
          config$daily$meta_daily_source <- match.arg(meta_daily_source)
          explicit$meta_daily_source <- TRUE
        }
        if (!missing(type)) config$parse$type <- type
        attr(config, "meta_explicit") <- explicit
      }
    }
  }
  if (is.null(output_dir) && is.null(project_dir)) cli::cli_abort("Supply `output_dir` or `project_dir`.")
  output_dir <- output_dir %||% dirname(project_dir)
  tz <- appusage_resolve_timezone(tz)
  config <- config %||% appusage_config_from_legacy(type, input, encoding, tz,
    second_options = appusage_forward_meta(list(reconstruct_meta = reconstruct_meta,
      meta_pairing = match.arg(meta_pairing), meta_start_event_types = meta_start_event_types,
      meta_end_event_types = meta_end_event_types, merge_meta_episodes = merge_meta_episodes,
      meta_episode_merge_gap_ms = meta_episode_merge_gap_ms,
      meta_daily_source = match.arg(meta_daily_source), inline_qc = run_qc),
      list(reconstruct_meta = !missing(reconstruct_meta), meta_daily_source = !missing(meta_daily_source))),
    execution = list(strict = strict, progress = progress, parallel = parallel,
      workers = n_cores, overwrite = overwrite, resume = resume),
    category = list(enabled = run_category, dictionary = dictionary))
  out <- appusage_execute_pipeline(x, output_dir, config = config, ids = ids,
    project_name = project_name, project_id = project_id, project_dir = project_dir,
    run_second_level = run_second_level)
  class(out) <- c("appusage_workflow_result", "list")
  invisible(out)
}

normalize_first_level_input <- function(x, input, encoding) {
  if (inherits(x, "appusage_text")) {
    return(list(
      # read_appusage_text() has already decoded and normalized these lines.
      x = appusage_parse_context(x$lines),
      input = "lines",
      source_file = x$source_path,
      metadata_input = x$input
    ))
  }
  source_file <- if (identical(input, "file")) {
    normalizePath(x, winslash = "/", mustWork = FALSE)
  } else {
    source_file_label(x, input)
  }
  list(
    x = x,
    input = input,
    source_file = source_file,
    metadata_input = input
  )
}

first_level_wrapper_id_info <- function(source_file, input, participant_id) {
  filename <- if (identical(input, "file") && is_present_string(source_file)) {
    parse_wenjuanxing_upload_filename(source_file)
  } else {
    list()
  }
  resolved_id <- participant_id %||% {
    seq_id <- filename$wenjuanxing_sequence_id %||% NA_integer_
    if (!is.na(seq_id)) as.character(seq_id) else "record-000001"
  }
  data.frame(
    participant_id = resolved_id,
    participant_id_source = if (!is.null(participant_id)) {
      "manual"
    } else if (!is.na(filename$wenjuanxing_sequence_id %||% NA_integer_)) {
      "filename"
    } else {
      "generated"
    },
    wenjuanxing_sequence_id = filename$wenjuanxing_sequence_id %||% NA_integer_,
    filename_parse_status = filename$filename_parse_status %||% NA_character_,
    filename_parse_warning = filename$filename_parse_warning %||% NA_character_,
    native_export_file_name = filename$native_export_file_name %||% NA_character_,
    native_export_type_from_filename =
      filename$native_export_type_from_filename %||% NA_character_,
    native_export_created_at = filename$native_export_created_at %||% NA_character_,
    stringsAsFactors = FALSE
  )
}

parse_first_level_by_type <- function(x, input, type, participant_id,
                                      source_file, tz, encoding, strict) {
  switch(type,
    line_meta = appusage_parse_line_meta_context(x, input, participant_id, source_file,
                                                 tz, encoding, strict),
    line = appusage_parse_line_context(
      x,
      input = input,
      participant_id = participant_id,
      source_file = source_file,
      tz = tz,
      encoding = encoding,
      strict = strict
    ),
    meta = appusage_parse_meta_context(
      x,
      input = input,
      participant_id = participant_id,
      source_file = source_file,
      tz = tz,
      encoding = encoding,
      strict = strict
    ),
    day = appusage_parse_day_context(
      x,
      input = input,
      participant_id = participant_id,
      source_file = source_file,
      tz = tz,
      encoding = encoding,
      strict = strict
    ),
    app = appusage_parse_app_context(
      x,
      input = input,
      participant_id = participant_id,
      source_file = source_file,
      tz = tz,
      encoding = encoding,
      strict = strict
    )
  )
}

normalize_uncoded_app_frames <- function(x) {
  if (is.data.frame(x)) {
    return(list(x))
  }
  if (!is.list(x)) {
    cli::cli_abort("`x` must be a data frame or second-level data list.")
  }
  grains <- intersect(c("event", "episode", "daily"), names(x))
  frames <- lapply(grains, function(grain) {
    frame <- x[[grain]]
    if (!is.data.frame(frame) || nrow(frame) == 0) {
      return(NULL)
    }
    if (!"source_export_type" %in% names(frame)) {
      frame$source_export_type <- grain
    }
    frame
  })
  Filter(Negate(is.null), frames)
}

empty_uncoded_apps_tibble <- function() {
  tibble::tibble(
    package_name = character(),
    app_name = character(),
    source_export_type = character(),
    n_rows = integer(),
    total_duration_ms = numeric()
  )
}
