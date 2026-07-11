#' Build BIDS-like APP Usage output filenames
#'
#' Uses key-value entities joined by underscores, with each key and value joined
#' by a hyphen.
#'
#' @param participant_id Participant identifier.
#' @param export_type APP Usage export type.
#' @param proc Processing step, using `1`, `2`, or `3`.
#' @param source_key Optional deterministic source cache key. New caches use
#'   this additive entity to distinguish multiple records with the same
#'   participant and export type; legacy names omit it.
#' @param extension File extension without leading dot.
#'
#' @return A file name.
#' @export
build_appusage_filename <- function(participant_id, export_type,
                                    proc = 1,
                                    extension = "rda",
                                    source_key = NULL) {
  entities <- c(
    sub = sanitize_entity_value(participant_id),
    type = sanitize_entity_value(export_type)
  )
  if (is_present_string(source_key)) {
    entities <- c(entities, src = sanitize_entity_value(source_key))
  }
  entities <- c(entities, proc = sanitize_entity_value(normalize_proc_value(proc)))
  paste0(
    paste(paste(names(entities), entities, sep = "-"), collapse = "_"),
    ".",
    sanitize_entity_value(extension)
  )
}

#' Parse BIDS-like APP Usage output filenames
#'
#' @param filename File name or path.
#'
#' @return A list of key-value entities plus extension.
#' @export
parse_appusage_filename <- function(filename) {
  base <- basename(filename)
  extension <- tools::file_ext(base)
  stem <- tools::file_path_sans_ext(base)
  parts <- strsplit(stem, "_", fixed = TRUE)[[1]]
  entities <- list()
  suffix <- NA_character_
  for (part in parts) {
    kv <- strsplit(part, "-", fixed = TRUE)[[1]]
    if (length(kv) >= 2) {
      key <- kv[[1]]
      value <- paste(kv[-1], collapse = "-")
      entities[[key]] <- value
    } else {
      suffix <- part
    }
  }
  if (!is.na(suffix)) {
    entities$suffix <- suffix
  }
  entities$extension <- extension
  entities
}

#' Parse a native APP Usage export filename
#'
#' Native APP Usage exports are expected to use names such as
#' `AppUsage_line_2023_12_15_19_36_5.txt`. Filename metadata is used for
#' provenance only; content detection remains the parser source of truth.
#'
#' @param filename File name or path.
#' @param tz Time zone used when converting the export timestamp.
#'
#' @return A list with parsed filename metadata.
#' @export
parse_native_appusage_filename <- function(filename, tz = "Asia/Shanghai") {
  base <- basename(filename)
  base <- appusage_native_filename_postfix(base) %||% base
  pattern <- paste0(
    "^AppUsage_",
    "([A-Za-z]+)_",
    "([0-9]{4})_([0-9]{1,2})_([0-9]{1,2})_",
    "([0-9]{1,2})_([0-9]{1,2})_([0-9]{1,2})",
    "\\.txt$"
  )
  match <- regexec(pattern, base, ignore.case = TRUE)
  parts <- regmatches(base, match)[[1]]

  if (length(parts) == 0) {
    return(list(
      native_export_file_name = ifelse(identical(base, ""), NA_character_, base),
      native_export_type_from_filename = NA_character_,
      native_export_type_raw = NA_character_,
      native_export_created_at = NA_character_,
      native_filename_parse_status = "failed",
      native_filename_parse_warning = "Filename does not match native APP Usage export pattern."
    ))
  }

  raw_export_type <- tolower(parts[[2]])
  supported <- raw_export_type %in% c("line", "meta", "day", "app")
  export_type <- if (supported) raw_export_type else "unknown"
  values <- as.integer(parts[3:8])
  created_text <- sprintf(
    "%04d-%02d-%02d %02d:%02d:%02d",
    values[[1]], values[[2]], values[[3]],
    values[[4]], values[[5]], values[[6]]
  )
  created_at <- as.POSIXct(created_text, tz = tz)
  warning <- if (!supported) {
    paste0("Native APP Usage export type '", raw_export_type, "' is not supported by appusageR.")
  } else if (is.na(created_at)) {
    "Native APP Usage export timestamp could not be parsed."
  } else {
    NA_character_
  }

  list(
    native_export_file_name = base,
    native_export_type_from_filename = export_type,
    native_export_type_raw = raw_export_type,
    native_export_created_at = ifelse(
      is.na(created_at),
      NA_character_,
      format(created_at, "%Y-%m-%dT%H:%M:%OS3%z")
    ),
    native_filename_parse_status = ifelse(!supported, "unsupported", ifelse(is.na(created_at), "partial", "success")),
    native_filename_parse_warning = warning
  )
}

appusage_native_filename_postfix <- function(filename) {
  base <- basename(filename)
  pattern <- paste0(
    "AppUsage_",
    "[A-Za-z]+_",
    "[0-9]{4}_[0-9]{1,2}_[0-9]{1,2}_",
    "[0-9]{1,2}_[0-9]{1,2}_[0-9]{1,2}",
    "\\.txt"
  )
  match <- regexpr(pattern, base, ignore.case = TRUE)
  if (identical(match[[1]], -1L)) {
    return(NULL)
  }
  regmatches(base, match)
}

#' Parse a Wenjuanxing-renamed APP Usage upload filename
#'
#' Wenjuanxing may prepend upload files with text similar to
#' `sequence3821_div style=tex_`. The Chinese prefix can appear as mojibake, so
#' this parser extracts the integer immediately before `_div style=tex_` and
#' then attempts to parse the remaining native APP Usage filename.
#'
#' @param filename File name or path.
#' @param tz Time zone used when converting any native export timestamp.
#'
#' @return A list with Wenjuanxing and native APP Usage filename metadata.
#' @export
parse_wenjuanxing_upload_filename <- function(filename, tz = "Asia/Shanghai") {
  base <- basename(filename)
  pattern <- "^(.+?)([0-9]+)_div style=tex_(.*)$"
  match <- regexec(pattern, base, ignore.case = TRUE)
  parts <- regmatches(base, match)[[1]]

  if (length(parts) == 0) {
    numeric_pattern <- paste0(
      "^([0-9]+)_",
      "(AppUsage_([A-Za-z]+)_",
      "[0-9]{4}_[0-9]{1,2}_[0-9]{1,2}_",
      "[0-9]{1,2}_[0-9]{1,2}_[0-9]{1,2}",
      "\\.txt)$"
    )
    numeric_match <- regexec(numeric_pattern, base, ignore.case = TRUE)
    numeric_parts <- regmatches(base, numeric_match)[[1]]
    if (length(numeric_parts) > 0) {
      sequence_id <- suppressWarnings(as.integer(numeric_parts[[2]]))
      uploaded <- numeric_parts[[3]]
      native <- parse_native_appusage_filename(uploaded, tz = tz)
      native_status <- native$native_filename_parse_status
      status <- if (identical(native_status, "success")) {
        "success"
      } else if (identical(native_status, "unsupported")) {
        "unsupported"
      } else {
        "partial"
      }
      warning <- if (identical(status, "success")) {
        NA_character_
      } else if (identical(status, "unsupported")) {
        native$native_filename_parse_warning
      } else {
        "Wenjuanxing numeric prefix was parsed, but the uploaded filename is not a complete native APP Usage filename."
      }
      return(c(list(
        file_name = base,
        wenjuanxing_sequence_id = sequence_id,
        wenjuanxing_filename_prefix = paste0(numeric_parts[[2]], "_"),
        uploaded_file_name = uploaded,
        filename_parse_status = status,
        filename_parse_warning = warning
      ), native))
    }
    native <- parse_native_appusage_filename(base, tz = tz)
    native_postfix <- appusage_native_filename_postfix(base)
    if (!is.null(native_postfix)) {
      sequence_match <- regexec("^\\D*([0-9]+)_", base, perl = TRUE)
      sequence_parts <- regmatches(base, sequence_match)[[1]]
      sequence_id <- if (length(sequence_parts) > 0) {
        suppressWarnings(as.integer(sequence_parts[[2]]))
      } else {
        NA_integer_
      }
      prefix <- if (length(sequence_parts) > 0) sequence_parts[[1]] else NA_character_
      native_status <- native$native_filename_parse_status
      status <- if (identical(native_status, "success")) {
        "success"
      } else if (identical(native_status, "unsupported")) {
        "unsupported"
      } else {
        "partial"
      }
      warning <- if (identical(status, "success")) {
        NA_character_
      } else if (identical(status, "unsupported")) {
        native$native_filename_parse_warning
      } else {
        "A native APP Usage filename postfix was parsed from a longer Wenjuanxing filename, but metadata are incomplete."
      }
      return(c(list(
        file_name = base,
        wenjuanxing_sequence_id = sequence_id,
        wenjuanxing_filename_prefix = prefix,
        uploaded_file_name = native_postfix,
        filename_parse_status = status,
        filename_parse_warning = warning
      ), native))
    }
    status <- if (identical(native$native_filename_parse_status, "success")) {
      "success"
    } else if (identical(native$native_filename_parse_status, "unsupported")) {
      "unsupported"
    } else {
      "failed"
    }
    warning <- if (identical(status, "success")) {
      NA_character_
    } else if (identical(status, "unsupported")) {
      native$native_filename_parse_warning
    } else {
      "Filename has no parseable Wenjuanxing upload prefix or native APP Usage pattern."
    }
    return(c(list(
      file_name = base,
      wenjuanxing_sequence_id = NA_integer_,
      wenjuanxing_filename_prefix = NA_character_,
      uploaded_file_name = base,
      filename_parse_status = status,
      filename_parse_warning = warning
    ), native))
  }

  sequence_id <- suppressWarnings(as.integer(parts[[3]]))
  prefix <- paste0(parts[[2]], parts[[3]], "_div style=tex_")
  uploaded <- parts[[4]]
  native <- parse_native_appusage_filename(uploaded, tz = tz)
  native_status <- native$native_filename_parse_status
  status <- if (identical(native_status, "success")) {
    "success"
  } else if (identical(native_status, "unsupported")) {
    "unsupported"
  } else {
    "partial"
  }
  warning <- if (identical(status, "success")) {
    NA_character_
  } else if (identical(status, "unsupported")) {
    native$native_filename_parse_warning
  } else {
    "Wenjuanxing sequence ID was parsed, but the uploaded filename is not a native APP Usage filename."
  }

  c(list(
    file_name = base,
    wenjuanxing_sequence_id = sequence_id,
    wenjuanxing_filename_prefix = prefix,
    uploaded_file_name = uploaded,
    filename_parse_status = status,
    filename_parse_warning = warning
  ), native)
}

#' Resolve participant IDs for APP Usage batch preprocessing
#'
#' Resolves one participant ID per input record from an explicit `ids` vector,
#' names on `x`, Wenjuanxing sequence IDs in filenames, or fallback record IDs.
#' The self-report matching interface is reserved here but not implemented until
#' a real example table is available.
#'
#' @param x Character vector of inputs.
#' @param ids Optional participant IDs parallel to `x`.
#' @param input Input mode, usually `"file"`.
#' @param self_report Optional self-report data frame. Reserved for a future
#'   Wenjuanxing matching implementation.
#' @param participant_id_col Reserved study participant ID column name.
#' @param wenjuanxing_sequence_col Reserved Wenjuanxing sequence column name.
#'
#' @return A tibble with `participant_id`, `participant_id_source`, and filename
#'   metadata columns.
#' @export
resolve_participant_ids <- function(x, ids = NULL, input = "file",
                                    self_report = NULL,
                                    participant_id_col = NULL,
                                    wenjuanxing_sequence_col = NULL) {
  if (!is.null(self_report)) {
    cli::cli_abort(
      "Self-report matching is reserved but not implemented yet; provide `ids` for now."
    )
  }
  if (!is.null(ids) && length(ids) != length(x)) {
    cli::cli_abort("`ids` must have the same length as `x`.")
  }

  filename_meta <- lapply(seq_along(x), function(i) {
    if (identical(input, "file")) {
      parse_wenjuanxing_upload_filename(x[[i]])
    } else {
      list(
        file_name = NA_character_,
        wenjuanxing_sequence_id = NA_integer_,
        wenjuanxing_filename_prefix = NA_character_,
        uploaded_file_name = NA_character_,
        filename_parse_status = "not_applicable",
        filename_parse_warning = NA_character_,
        native_export_file_name = NA_character_,
        native_export_type_from_filename = NA_character_,
        native_export_created_at = NA_character_,
        native_filename_parse_status = "not_applicable",
        native_filename_parse_warning = NA_character_
      )
    }
  })

  if (!is.null(ids)) {
    participant_id <- as.character(ids)
    id_source <- rep("ids_vector", length(x))
  } else if (!is.null(names(x)) && any(nzchar(names(x)))) {
    participant_id <- names(x)
    participant_id[!nzchar(participant_id)] <- NA_character_
    id_source <- ifelse(is.na(participant_id), "missing", "names")
  } else {
    sequence_ids <- vapply(
      filename_meta,
      function(z) z$wenjuanxing_sequence_id,
      integer(1)
    )
    participant_id <- ifelse(
      !is.na(sequence_ids),
      as.character(sequence_ids),
      default_batch_ids(x, input)
    )
    id_source <- ifelse(!is.na(sequence_ids), "filename_wenjuanxing", "fallback")
  }

  tibble::tibble(
    participant_id = participant_id,
    participant_id_source = id_source,
    wenjuanxing_sequence_id = vapply(filename_meta, function(z) z$wenjuanxing_sequence_id, integer(1)),
    wenjuanxing_filename_prefix = vapply(filename_meta, function(z) z$wenjuanxing_filename_prefix, character(1)),
    uploaded_file_name = vapply(filename_meta, function(z) z$uploaded_file_name, character(1)),
    filename_parse_status = vapply(filename_meta, function(z) z$filename_parse_status, character(1)),
    filename_parse_warning = vapply(filename_meta, function(z) z$filename_parse_warning, character(1)),
    native_export_file_name = vapply(filename_meta, function(z) z$native_export_file_name, character(1)),
    native_export_type_from_filename = vapply(filename_meta, function(z) z$native_export_type_from_filename, character(1)),
    native_export_created_at = vapply(filename_meta, function(z) z$native_export_created_at, character(1)),
    native_filename_parse_status = vapply(filename_meta, function(z) z$native_filename_parse_status, character(1)),
    native_filename_parse_warning = vapply(filename_meta, function(z) z$native_filename_parse_warning, character(1))
  )
}

normalize_proc_value <- function(proc) {
  proc <- as.character(proc)
  proc[proc %in% c("firstlevel", "first-level")] <- "1"
  proc[proc %in% c("secondlevel", "second-level")] <- "2"
  proc[proc %in% c("qc", "thirdlevel", "third-level")] <- "3"
  proc
}

sanitize_entity_value <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "unknown"
  x <- gsub("[^A-Za-z0-9]+", "-", x)
  x <- gsub("-+", "-", x)
  x <- gsub("^-|-$", "", x)
  ifelse(x == "", "unknown", x)
}
