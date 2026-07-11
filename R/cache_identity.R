appusage_stable_text_md5 <- function(text) {
  path <- tempfile("appusage-source-identity-", fileext = ".bin")
  on.exit(unlink(path, force = TRUE), add = TRUE)
  writeBin(charToRaw(enc2utf8(paste(text, collapse = "\r\n"))), path)
  unname(as.character(tools::md5sum(path))[[1]])
}

appusage_source_fingerprint <- function(x, input = "file") {
  if (identical(input, "file") && length(x) == 1L && !is.na(x) && file.exists(x)) {
    content_md5 <- unname(as.character(tools::md5sum(x))[[1]])
    return(appusage_stable_text_md5(c(tolower(basename(x)), content_md5)))
  }
  appusage_stable_text_md5(as.character(x))
}

appusage_native_timestamp_token <- function(timestamp) {
  timestamp <- as.character(timestamp %||% NA_character_)[[1]]
  if (is.na(timestamp) || !nzchar(timestamp)) {
    return("notime")
  }
  token <- gsub("[^0-9]", "", timestamp)
  if (!nzchar(token)) "notime" else substr(token, 1L, 14L)
}

appusage_source_identity <- function(x, input, id_info, participant_id,
                                     export_type = "unknown",
                                     fingerprint = NULL) {
  fingerprint <- fingerprint %||% appusage_source_fingerprint(x, input)
  timestamp <- id_info$native_export_created_at[[1]] %||% NA_character_
  timestamp_token <- appusage_native_timestamp_token(timestamp)
  cache_key <- paste0(timestamp_token, "-", substr(fingerprint, 1L, 12L))
  record_key <- paste(
    "sub", sanitize_entity_value(participant_id),
    "type", sanitize_entity_value(export_type %||% "unknown"),
    "time", timestamp_token,
    "fp", fingerprint,
    sep = "="
  )
  list(
    source_record_key = record_key,
    source_fingerprint = fingerprint,
    source_cache_key = cache_key,
    native_export_created_at = timestamp
  )
}

appusage_metadata_source_identity <- function(metadata) {
  list(
    source_record_key = appusage_first_nonmissing(
      appusage_nested_value(metadata, c("identity", "source_record_key")),
      appusage_nested_value(metadata, c("source_record_key"))
    ),
    source_fingerprint = appusage_first_nonmissing(
      appusage_nested_value(metadata, c("source", "source_fingerprint")),
      appusage_nested_value(metadata, c("source_fingerprint"))
    ),
    source_cache_key = appusage_first_nonmissing(
      appusage_nested_value(metadata, c("identity", "source_cache_key")),
      appusage_nested_value(metadata, c("source", "source_cache_key"))
    )
  )
}

appusage_summary_cell <- function(summary, column, index,
                                  default = NA_character_) {
  if (is.null(summary) || !column %in% names(summary) ||
    length(summary[[column]]) < index) {
    return(default)
  }
  value <- summary[[column]][[index]]
  if (length(value) == 0L || is.null(value)) default else value
}
