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

appusage_first_contract_valid <- function(row, metadata, requested, x = NULL,
                                          input = "file", verify = "content") {
  appusage_parse_evidence(row, metadata, requested, x, input, verify)$valid
}

appusage_parse_evidence <- function(row, metadata, requested, x = NULL,
                                    input = "file", verify = "content", context = NULL) {
  source_verified <- FALSE
  result <- function(valid, reason) list(valid = valid, reason = reason, source_verified = source_verified)
  if (is.null(row)) return(result(FALSE, "parse_artifact_missing"))
  if (is.null(metadata)) return(result(FALSE, if (file.exists(row$metadata_file[[1]]))
    "parse_metadata_corrupt" else "parse_metadata_missing"))
  recorded <- metadata$module_state$parse
  if (is.null(recorded)) return(result(FALSE, "verification_insufficient"))
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

appusage_read_json_safely <- function(path) {
  if (!is_present_string(path) || !file.exists(path)) return(NULL)
  tryCatch(appusage_read_json(path, simplifyVector = TRUE), error = function(e) NULL)
}
