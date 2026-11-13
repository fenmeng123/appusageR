# Cheap byte integrity checks do not deserialize participant data. A missing
# signature is explicitly unverified, never equivalent to a matching signature.
appusage_artifact_signature <- function(path) {
  if (!is_present_string(path) || !file.exists(path)) return(NULL)
  list(size = unname(file.info(path)$size[[1]]),
    md5 = unname(as.character(appusage_file_md5(path))[[1]]))
}

appusage_artifact_valid <- function(path, recorded, verify = TRUE) {
  if (!is_present_string(path) || !file.exists(path) || file.info(path)$size <= 0) return(FALSE)
  if (!isTRUE(verify)) return(TRUE)
  !is.null(recorded) && appusage_contract_equal(recorded, appusage_artifact_signature(path))
}

appusage_source_stat <- function(path, input = "file") {
  if (input != "file" || length(path) != 1L || !file.exists(path)) return(NULL)
  info <- file.info(path)
  list(size = unname(info$size[[1]]), mtime = as.numeric(info$mtime[[1]]))
}

# The JSON receipt distinguishes an absent table from a present, empty table.
appusage_components_complete <- function(metadata) {
  if (!appusage_cache_metadata_shape(metadata)) return(FALSE)
  receipt <- metadata$module_state$components
  parse <- metadata$module_state$parse
  if (!is.list(parse) || !is.list(parse$configuration) ||
      !isTRUE(parse$configuration$component_contract == 1L) ||
      !is_present_string(parse$implementation) || !is_present_string(parse$configuration$type))
    return(FALSE)
  if (!is.list(receipt) || !is.list(receipt$tables) ||
      !identical(metadata$processing$first_level_status, "success") ||
      !identical(receipt$status, "complete") ||
      !is.logical(receipt$meta_summary_present) || length(receipt$meta_summary_present) != 1L ||
      is.na(receipt$meta_summary_present) ||
      !is_present_string(receipt$selection) ||
      !identical(receipt$selection, metadata$export$component_selection_rule)) return(FALSE)
  expected <- if (identical(receipt$selection, "bounded_line_meta")) {
    c("line", if (isTRUE(receipt$meta_summary_present)) "meta_summary", "meta_events")
  } else if (identical(metadata$export$detected_type, "meta")) {
    c("meta_summary", "meta_events")
  } else metadata$export$detected_type
  if (!length(expected) || !setequal(names(receipt$tables), expected) ||
      (identical(receipt$selection, "bounded_line_meta") && !isTRUE(receipt$event_source_lines)))
    return(FALSE)
  all(vapply(expected, function(key) {
    n <- receipt$tables[[key]]
    is.numeric(n) && length(n) == 1L && is.finite(n) && n >= 0 &&
      isTRUE(n == metadata$counts[[paste0("n_", key, "_rows")]])
  }, logical(1)))
}

appusage_first_contract_valid <- function(row, metadata, requested, x = NULL,
                                          input = "file", verify = "content") {
  appusage_parse_evidence(row, metadata, requested, x, input, verify)$valid
}

# A mixed export can finish parsing both components and then fail the line
# quality gate. The error receipt, component states and measured counts must
# corroborate the same terminal input diagnosis.
appusage_mixed_quality_failure <- function(row, metadata) {
  receipt <- metadata$module_state$components
  quality <- metadata$structural_quality
  errors <- metadata$errors
  if (!identical(appusage_get_col_value(row, "status"), "error") ||
      !identical(appusage_get_col_value(row, "failure_family"), "structural_quality") ||
      !identical(metadata$processing$first_level_status, "error") ||
      !identical(metadata$processing$first_level_failure_reason, "structural_quality_critical") ||
      !identical(metadata$export$component_selection_rule, "bounded_line_meta") ||
      !is.list(receipt) || !identical(receipt$status, "incomplete") ||
      !is.list(receipt$states) || !identical(receipt$states$line, "failed_quality") ||
      !identical(receipt$states$meta, "parsed") || !is.list(quality) ||
      !identical(quality$status, "critical") || !isTRUE(quality$critical) ||
      !is.data.frame(errors) || !any(startsWith(as.character(errors$class),
        "appusage_line_structural_quality,"), na.rm = TRUE)) return(FALSE)
  fields <- c("n_parsed_rows", "n_valid_intervals", "n_header_token_contamination", "n_exact_duplicates")
  if (!all(vapply(quality[fields], function(n)
    is.numeric(n) && length(n) == 1L && is.finite(n) && n >= 0 && n == floor(n), logical(1))))
    return(FALSE)
  n <- quality$n_parsed_rows
  if (any(unlist(quality[fields[-1L]]) > n)) return(FALSE)
  thresholds <- line_structural_quality_thresholds()
  if (!appusage_contract_equal(quality$thresholds, thresholds)) return(FALSE)
  reasons <- c(if (quality$n_valid_intervals == 0) "no_valid_intervals",
    if (quality$n_header_token_contamination > 0) "header_token_contamination",
    if (n > 0 && quality$n_valid_intervals / n < thresholds$valid_interval_critical_min)
      "valid_interval_ratio_below_critical",
    if (n > 0 && quality$n_exact_duplicates / n > thresholds$exact_duplicate_critical_ratio)
      "exact_duplicate_ratio_above_critical")
  length(reasons) > 0L && identical(quality$critical_reasons, reasons) &&
    identical(appusage_get_col_value(row, "structural_critical_reasons"), paste(reasons, collapse = ";"))
}

appusage_parse_evidence <- function(row, metadata, requested, x = NULL,
                                    input = "file", verify = "content", context = NULL) {
  source_verified <- FALSE
  result <- function(valid, reason) list(valid = valid, reason = reason, source_verified = source_verified)
  if (is.null(row)) return(result(FALSE, "parse_artifact_missing"))
  if (!appusage_cache_metadata_shape(metadata)) return(result(FALSE, if (file.exists(row$metadata_file[[1]]))
    "parse_metadata_corrupt" else "parse_metadata_missing"))
  recorded <- metadata$module_state$parse
  components <- metadata$module_state$components
  mixed_failure <- identical(row$status[[1L]], "error") &&
    (identical(metadata$export$component_selection_rule, "bounded_line_meta") ||
     identical(appusage_get_col_value(row, "component_selection_rule"), "bounded_line_meta"))
  if (((mixed_failure || (is.list(components) && identical(components$status, "incomplete"))) &&
       !appusage_mixed_quality_failure(row, metadata)) ||
      !identical(metadata$processing$first_level_status, row$status[[1]]))
    return(result(FALSE, "incomplete_parse"))
  if (is.null(recorded)) return(result(FALSE, "verification_insufficient"))
  if (identical(row$status[[1]], "success") && !appusage_components_complete(metadata))
    return(result(FALSE, "component_verification_required"))
  identity <- appusage_metadata_source_identity(metadata)
  if (any(vapply(c("source_fingerprint", "source_record_key"), function(key) {
    expected <- appusage_get_col_value(row, key, NA_character_)
    is_present_string(expected) && !identical(expected, identity[[key]])
  }, logical(1)))) return(result(FALSE, "source_identity_changed"))
  if (!appusage_contract_equal(recorded, requested)) return(result(FALSE,
    if (!identical(recorded$implementation, requested$implementation)) "implementation_changed" else "configuration_changed"))
  if (verify == "content") {
    if (input == "file" && !file.exists(x)) return(result(FALSE, "source_missing"))
    old <- appusage_get_col_value(row, "source_fingerprint", NA_character_)
    fresh <- appusage_runtime_measure(context, "source_hash", appusage_source_fingerprint(x, input),
      bytes = if (input == "file") unname(file.info(x)$size[[1]]) else 0)
    if (!identical(old, fresh)) return(result(FALSE, "source_changed"))
    source_verified <- TRUE
  } else if (verify == "metadata" && !is.null(x)) {
    if (!appusage_contract_equal(metadata$module_state$source_stat,
        appusage_source_stat(x, input))) return(result(FALSE, "source_stat_changed"))
  }
  if (identical(row$status[[1]], "success")) {
    valid <- appusage_runtime_measure(context, "parse_artifact_check",
      appusage_artifact_valid(row$data_file[[1]], metadata$module_state$artifact, verify != "preview"),
      bytes = if (verify != "preview" && file.exists(row$data_file[[1]])) unname(file.info(row$data_file[[1]])$size[[1]]) else 0)
    return(result(valid, if (valid) "up_to_date" else if (!file.exists(row$data_file[[1]]))
      "parse_artifact_missing" else if (is.null(metadata$module_state$artifact))
      "verification_insufficient" else "parse_artifact_corrupt"))
  }
  valid <- appusage_first_level_seed_row_complete(row)
  result(valid, if (valid) "up_to_date" else "incomplete_parse")
}

appusage_cache_metadata_shape <- function(metadata) {
  is.list(metadata) && !is.null(names(metadata)) &&
    all(vapply(metadata[intersect(c("processing", "export", "counts", "module_state", "source",
      "identity", "outputs"), names(metadata))], is.list, logical(1)))
}

appusage_read_cache_json <- function(path) {
  metadata <- appusage_read_json(path, simplifyVector = TRUE)
  if (!appusage_cache_metadata_shape(metadata)) stop("Invalid cache JSON structure.")
  metadata
}

appusage_read_json_safely <- function(path) {
  if (!is_present_string(path) || !file.exists(path)) return(NULL)
  tryCatch(appusage_read_json(path, simplifyVector = TRUE), error = function(e) NULL)
}
