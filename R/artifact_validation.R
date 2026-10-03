# Cheap byte integrity checks do not deserialize participant data. A missing
# signature is explicitly unverified, never equivalent to a matching signature.
appusage_artifact_signature <- function(path) {
  if (!is_present_string(path) || !file.exists(path)) return(NULL)
  list(size = unname(file.info(path)$size[[1]]),
    md5 = unname(as.character(tools::md5sum(path))[[1]]))
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
  if (is.null(metadata) || !appusage_contract_equal(metadata$module_state$parse, requested)) return(FALSE)
  if (verify == "content") {
    if (input == "file" && !file.exists(x)) return(FALSE)
    old <- appusage_get_col_value(row, "source_fingerprint", NA_character_)
    if (!identical(old, appusage_source_fingerprint(x, input))) return(FALSE)
  } else if (verify == "metadata" && !is.null(x)) {
    if (!appusage_contract_equal(metadata$module_state$source_stat,
        appusage_source_stat(x, input))) return(FALSE)
  }
  if (identical(row$status[[1]], "success")) {
    return(appusage_artifact_valid(row$data_file[[1]],
      metadata$module_state$artifact, verify != "preview"))
  }
  appusage_first_level_seed_row_complete(row)
}

appusage_read_json_safely <- function(path) {
  if (!is_present_string(path) || !file.exists(path)) return(NULL)
  tryCatch(jsonlite::read_json(path, simplifyVector = TRUE), error = function(e) NULL)
}
