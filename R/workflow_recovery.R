# Identity/configuration checks are shared by execution and planning. No source
# enumeration occurs here: callers supply their explicitly selected sources.
appusage_align_first_level_seed <- function(existing, x, id_plan, input, type,
                                            encoding, tz, provenance = NULL) {
  if (is.null(existing) || !nrow(existing) || !"source_file" %in% names(existing)) {
    return(NULL)
  }
  paths <- vapply(seq_along(x), function(i) source_file_label(x[[i]], input), character(1))
  old_paths <- as.character(existing$source_file)
  if (input == "file") {
    paths <- normalizePath(paths, winslash = "/", mustWork = FALSE)
    old_paths <- normalizePath(old_paths, winslash = "/", mustWork = FALSE)
  }
  requested <- appusage_parse_contract(type, input, encoding, tz,
    provenance$parser_strict %||% TRUE, provenance)
  rows <- vector("list", length(x))
  for (i in seq_along(x)) {
    candidates <- which(old_paths == paths[[i]])
    if (!length(candidates)) next
    candidate <- if ("participant_id" %in% names(existing)) candidates[
      as.character(existing$participant_id[candidates]) == as.character(id_plan$participant_id[[i]])] else candidates
    if (!length(candidate)) next
    row <- existing[candidate[[length(candidate)]], , drop = FALSE]
    row$index <- i
    if (!"participant_id" %in% names(row)) row$participant_id <- id_plan$participant_id[[i]]
    metadata_file <- appusage_get_col_value(row, "metadata_file", NA_character_)
    metadata <- if (is_present_string(metadata_file) && file.exists(metadata_file)) {
      tryCatch(jsonlite::read_json(metadata_file, simplifyVector = TRUE), error = function(e) NULL)
    } else NULL
    valid <- appusage_first_contract_valid(row, metadata, requested,
      x[[i]], input, provenance$source_verification %||% "content")
    if (!valid && !identical(appusage_get_col_value(row, "failure_family", NA_character_), "memory_allocation")) {
      row$status <- "incomplete"
      row$failure_family <- "stale_cache"
    }
    rows[[i]] <- row
  }
  rows <- Filter(Negate(is.null), rows)
  if (!length(rows)) return(NULL)
  do.call(bind_appusage_summary_rows, rows)
}

appusage_second_cache_for_options <- function(cache, options, provenance = NULL, verify = TRUE) {
  if (!identical(cache$status, "complete")) return(cache)
  requested <- appusage_research_contract(options, provenance)
  upstream_ok <- (is.null(cache$metadata$module_state$upstream_parse) && is.null(cache$upstream_parse)) ||
    appusage_contract_equal(cache$metadata$module_state$upstream_parse, cache$upstream_parse)
  if (!appusage_contract_equal(cache$metadata$module_state$research_data, requested) ||
      !upstream_ok ||
      !appusage_artifact_valid(cache$rda_file, cache$metadata$module_state$artifact, verify)) {
    cache$status <- "incomplete"
    cache$pair_state <- "stale_configuration"
    cache$reason <- "research_configuration_or_implementation_changed"
  }
  cache
}
