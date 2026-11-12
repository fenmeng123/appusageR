# First-level source signature and content preflight helpers

appusage_binary_signature <- function(bytes) {
  values <- as.integer(utils::head(bytes, 8L))
  starts_with <- function(signature) {
    length(values) >= length(signature) &&
      identical(values[seq_along(signature)], as.integer(signature))
  }
  signatures <- list(
    jpeg = c(255, 216, 255),
    png = c(137, 80, 78, 71, 13, 10, 26, 10),
    pdf = stringi::stri_enc_toutf32("%PDF")[[1L]],
    zip = c(80, 75, 3, 4),
    zip_empty = c(80, 75, 5, 6),
    gif87a = stringi::stri_enc_toutf32("GIF87a")[[1L]],
    gif89a = stringi::stri_enc_toutf32("GIF89a")[[1L]],
    rar = c(82, 97, 114, 33, 26, 7),
    seven_zip = c(55, 122, 188, 175, 39, 28),
    gzip = c(31, 139),
    windows_executable = c(77, 90)
  )
  hit <- names(signatures)[vapply(signatures, starts_with, logical(1))]
  if (length(hit)) return(hit[[1]])
  # ISO base media stores its box type after a four-byte big-endian size.
  # Compressed media can stay below both byte-ratio gates despite embedded NULs.
  if (length(values) == 8L && identical(values[5:8], c(102L, 116L, 121L, 112L))) {
    box_size <- sum(values[1:4] * 256^(3:0))
    if (box_size >= 16 && box_size <= length(bytes)) return("iso_base_media")
  }
  NA_character_
}

appusage_byte_ratios <- function(bytes) {
  n <- length(bytes)
  if (!n) return(list(nul_byte_ratio = 0, control_byte_ratio = 0))
  nul <- control <- 0
  for (start in seq.int(1, n, by = 1048576)) {
    values <- as.integer(bytes[seq.int(start, min(n, start + 1048575))])
    nul <- nul + sum(values == 0L)
    control <- control + sum(values < 32L & !values %in% c(9L, 10L, 13L))
  }
  list(nul_byte_ratio = nul / n, control_byte_ratio = control / n)
}

appusage_record_candidate_rows <- function(lines) {
  appusage_context_records(appusage_parse_context(lines))
}

appusage_character_input_encoding_diagnostics <- function(encoding) {
  list(
    attempted_candidates = as.character(encoding),
    supported_candidates = as.character(encoding),
    unsupported_candidates = character(),
    selected_encoding = if (identical(encoding, "auto")) "character_input" else encoding,
    conversion_failures = list(),
    fallback_used = FALSE
  )
}

appusage_valid_meta_header <- function(header, pos, table2) {
  common <- c("\u5e94\u7528\u540d\u79f0", "\u5e94\u7528\u5305\u540d|\u5e94\u7528\u6807\u8bc6")
  fields <- if (table2) {
    c(common, "\u5177\u4f53\u9875\u9762", "\u65f6\u95f4", "\u65f6\u95f4\u6233", "\u7c7b\u578b", "\u914d\u7f6e")
  } else {
    c(common, "\u5f00\u59cb\u65f6\u95f4", "\u7ed3\u675f\u65f6\u95f4", "\u6700\u540e\u4e00\u6b21|\u6700\u540e\u65f6\u95f4", "\u603b\u65f6\u95f4|\u603b\u65f6\u957f",
      "\u5f00\u59cb\u65f6\u95f4\uff08ms\uff09", "\u7ed3\u675f\u65f6\u95f4\uff08ms\uff09", "\u6700\u540e\u4e00\u6b21\uff08ms\uff09|\u6700\u540e\u65f6\u95f4\uff08ms\uff09",
      "\u603b\u65f6\u95f4\uff08ms\uff09|\u603b\u65f6\u957f\uff08ms\uff09")
  }
  cells <- header[pos + seq_along(fields)]
  all(mapply(function(value, pattern)
    !is.na(value) && grepl(paste0("^(?:", pattern, ")$"), value, perl = TRUE),
    cells, fields, USE.NAMES = FALSE))
}

appusage_structural_boundaries <- function(mat) {
  empty <- function() data.frame(
    row = integer(), boundary_type = character(), component = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(dim(mat)) || nrow(mat) == 0L) return(empty())
  if (inherits(mat, "appusage_parse_context") && !is.null(mat$store$cache$boundaries)) return(mat$store$cache$boundaries)
  n_rows <- nrow(mat)
  all_matches <- function(patterns) {
    hits <- rep(TRUE, n_rows)
    for (pattern in patterns) hits <- hits & appusage_context_hits(mat, pattern)
    hits
  }
  line_fields <- unlist(line_required_fields(), use.names = FALSE)
  day_fields <- c(
    "\\u5e94\\u7528\\u540d\\u79f0", "\\u5e94\\u7528\\u6807\\u8bc6|\\u5e94\\u7528\\u5305\\u540d",
    "\\u683c\\u5f0f\\u5316\\u65f6\\u95f4", "\\u4f7f\\u7528\\u65f6\\u957f\\uff08ms\\uff09",
    "\\u542f\\u52a8\\u6b21\\u6570", "\\u901a\\u77e5\\u6b21\\u6570"
  )
  app_fields <- c(
    "\\u65e5\\u671f", "\\u683c\\u5f0f\\u5316\\u65f6\\u95f4",
    "\\u4f7f\\u7528\\u65f6\\u957f\\uff08ms\\uff09", "\\u542f\\u52a8\\u6b21\\u6570",
    "\\u901a\\u77e5\\u6b21\\u6570"
  )
  hits <- cbind(
    all_matches(line_fields), rep(FALSE, n_rows), rep(FALSE, n_rows),
    all_matches(day_fields), all_matches(app_fields)
  )

  # Original: trimws(as.character(na.omit(row)))[1]. In particular, do not
  # skip an empty first non-NA field or substitute Unicode trim semantics.
  if (inherits(mat, "appusage_parse_context")) {
    s <- mat$store
    row <- rep.int(seq_along(s$sizes), s$sizes)
    present <- which(!is.na(s$values))
    first_pos <- present[!duplicated(row[present])]
    first <- rep(NA_character_, length(s$sizes))
    first_col <- rep(NA_integer_, length(s$sizes))
    first[row[first_pos]] <- s$values[first_pos]
    first_col[row[first_pos]] <- first_pos - s$offsets[row[first_pos]] - s$leading
    first_col <- first_col[mat$rows]
    first <- first[mat$rows]
    widths <- s$sizes[mat$rows] - s$leading
  } else {
    first <- rep(NA_character_, n_rows)
    first_col <- rep(NA_integer_, n_rows)
    widths <- rep(ncol(mat), n_rows)
    for (j in seq_len(ncol(mat))) {
      selected <- is.na(first_col) & !is.na(mat[, j])
      first[selected] <- as.character(mat[selected, j])
      first_col[selected] <- j
      if (!anyNA(first_col)) break
    }
  }
  first <- appusage_text_trim(first)
  # A record's free-text configuration can contain a complete quoted header.
  # Only consider headings in their native leading fields.
  date_prefix <- "^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}"
  field_patterns <- paste0("^(?:", line_fields, ")$")
  line_rows <- which(hits[, 1L])
  for (i in line_rows) {
    header <- appusage_header_cells(mat, i)
    pos <- which(!is.na(header))[1L]
    if (grepl(date_prefix, first[i])) pos <- pos + 1L
    hits[i, 1L] <- any(appusage_text_detect(header[pos], field_patterns), na.rm = TRUE) &&
      all(vapply(field_patterns, function(field)
        any(appusage_text_detect(header, field), na.rm = TRUE), logical(1)))
  }
  for (i in which(hits[, 4L])) {
    header <- appusage_header_cells(mat, i)
    pos <- which(!is.na(header))[1L]
    hits[i, 4L] <- !is.na(safe_as_date(first[i])) &&
      any(appusage_text_detect(header[pos + 1L], paste0("^(?:", day_fields, ")$")), na.rm = TRUE)
  }
  for (i in which(hits[, 5L])) {
    header <- appusage_header_cells(mat, i)
    pos <- match("\u65e5\u671f", header)
    # The app and package identifiers precede the date column.
    hits[i, 5L] <- !is.na(pos) && pos >= 3L &&
      all(!is.na(header[seq_len(pos - 1L)]))
  }
  marker <- grepl("^(?:[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}\\s*)?\u8868[\u4e00\u4e8c]$", first)
  malformed <- logical(n_rows)
  for (i in which(marker)) {
    header <- appusage_header_cells(mat, i)
    pos <- which(!is.na(header))[1L]
    # A marker word followed by a package ID can itself be an app name.
    next_field <- header[pos + 1L]
    if (pos > 1L && !is.na(next_field) &&
        grepl("^[[:alnum:]_]+(?:\\.[[:alnum:]_-]+)+$", next_field, perl = TRUE)) next
    table2 <- endsWith(first[i], "\u8868\u4e8c")
    valid <- appusage_valid_meta_header(header, pos, table2)
    hits[i, if (table2) 3L else 2L] <- valid
    malformed[i] <- !valid
  }
  present <- !is.na(first) & appusage_text_nzchar(first)
  metadata_patterns <- c(
    step_metadata = "^(?:STEP|\u5f00\u673a\u81f3\u4eca\u6b65\u6570\u4fe1\u606f|\u6b65\u6570\u4fe1\u606f)",
    device_metadata = "^(?:DEVICE|\u8bbe\u5907\u4fe1\u606f|\u624b\u673a\u4fe1\u606f|\u8bbe\u5907\u578b\u53f7|\u5382\u5546|\u54c1\u724c)",
    system_metadata = "^(?:SYSTEM|BUILD|MODEL|BRAND|Android\\s*Version|\u7cfb\u7edf\u4fe1\u606f|\u7cfb\u7edf\u7248\u672c|Android\u7248\u672c)"
  )
  metadata_seen <- rep(FALSE, n_rows)
  for (pattern in metadata_patterns) {
    selected <- present & !metadata_seen &
      appusage_text_grepl(pattern, first, ignore.case = TRUE, perl = TRUE)
    hits <- cbind(hits, selected)
    metadata_seen <- metadata_seen | selected
  }
  hits <- cbind(hits, malformed)
  # A marker word in the application-name field of a Table 1/2 record is data,
  # including when its package, timestamp or event code is missing.
  candidates <- which(metadata_seen | malformed)
  if (length(candidates)) {
    headers <- which(rowSums(hits[, 1:5, drop = FALSE]) > 0L)
    preceding <- c(NA_integer_, headers)[findInterval(candidates, headers) + 1L]
    record <- !is.na(preceding) & (hits[preceding, 2L] | hits[preceding, 3L]) &
      first_col[candidates] == first_col[preceding] + 1L &
      widths[candidates] >= first_col[preceding] + 6L
    hits[candidates[which(record)], 6:ncol(hits)] <- FALSE
  }
  locations <- which(hits, arr.ind = TRUE)
  if (nrow(locations) == 0L) return(empty())
  locations <- locations[order(locations[, 1L], locations[, 2L]), , drop = FALSE]
  types <- c("line_header", "meta_table1", "meta_table2", "day_header", "app_header", names(metadata_patterns), "invalid_meta_header")
  components <- c("line", "meta", "meta", "day", "app", rep("metadata", 3L), "meta")
  out <- data.frame(
    row = as.integer(locations[, 1L]),
    boundary_type = unname(types[locations[, 2L]]),
    component = unname(components[locations[, 2L]]), stringsAsFactors = FALSE
  )
  if (inherits(mat, "appusage_parse_context")) mat$store$cache$boundaries <- out
  out
}

appusage_component_boundary_diagnostics <- function(lines, components) {
  mat <- appusage_parse_context(lines)
  all_candidates <- appusage_context_records(mat)
  boundaries <- appusage_structural_boundaries(mat)
  profiles <- lapply(components, function(component) {
    header_rows <- unique(boundaries$row[boundaries$component == component])
    blocks <- appusage_table_blocks(mat, header_rows)
    ends <- vapply(blocks, "[[", integer(1), "end")
    candidate_start <- findInterval(header_rows, all_candidates) + 1L
    candidate_end <- findInterval(ends, all_candidates)
    candidate_rows <- vector("list", length(header_rows))
    section_ranges <- vector("list", length(header_rows))
    for (j in seq_along(header_rows)) {
      start <- blocks[[j]]$start
      end <- ends[[j]]
      section_ranges[[j]] <- list(start = start, end = end)
      if (start <= end && candidate_start[[j]] <= candidate_end[[j]]) {
        candidate_rows[[j]] <- all_candidates[seq.int(candidate_start[[j]], candidate_end[[j]])]
      }
    }
    candidate_rows <- unlist(candidate_rows, use.names = FALSE)
    list(
      header_rows = as.integer(header_rows),
      section_ranges = section_ranges,
      candidate_rows = as.integer(unique(candidate_rows)),
      n_candidate_rows = length(unique(candidate_rows))
    )
  })
  names(profiles) <- components
  list(
    boundaries = lapply(seq_len(nrow(boundaries)), function(i) {
      list(
        row = boundaries$row[[i]],
        boundary_type = boundaries$boundary_type[[i]],
        component = boundaries$component[[i]]
      )
    }),
    component_profiles = profiles
  )
}

appusage_select_source_component <- function(components, filename_type,
                                             boundary_diagnostics, type = "auto") {
  filename_type <- as.character(filename_type %||% NA_character_)[[1]]
  types <- vapply(boundary_diagnostics$boundaries, "[[", character(1), "boundary_type")
  if ("invalid_meta_header" %in% types ||
      ("meta" %in% components && (!"meta_table2" %in% types ||
         (!"line" %in% components && !"meta_table1" %in% types)))) {
    return(list(selected_component = NA_character_, selection_rule = "incomplete_meta_blocks"))
  }
  if (!type %in% c("auto", "legacy") && length(components)) {
    return(if (type %in% components) list(selected_component = type,
      selection_rule = if (length(components) == 1L) "single_content_component" else "explicit_component")
    else list(selected_component = NA_character_, selection_rule = "requested_component_absent"))
  }
  if (type == "legacy" && "line" %in% components && !"meta_table1" %in% types) {
    components <- setdiff(components, "meta")
  }
  if (type == "auto" && setequal(components, c("line", "meta"))) {
    return(list(selected_component = "line", selection_rule = "bounded_line_meta"))
  }
  if (length(components) == 0L) {
    return(list(selected_component = NA_character_, selection_rule = "none"))
  }
  if (length(components) == 1L) {
    return(list(
      selected_component = components[[1]],
      selection_rule = "single_content_component"
    ))
  }
  if (is_present_string(filename_type) && filename_type %in% components) {
    return(list(
      selected_component = filename_type,
      selection_rule = "filename_within_detected_components"
    ))
  }
  profiles <- boundary_diagnostics$component_profiles %||% list()
  with_records <- components[vapply(components, function(component) {
    (profiles[[component]]$n_candidate_rows %||% 0L) > 0L
  }, logical(1))]
  if (length(with_records) == 1L) {
    return(list(
      selected_component = with_records[[1]],
      selection_rule = "single_bounded_component_with_records"
    ))
  }
  list(
    selected_component = NA_character_,
    selection_rule = "ambiguous_mixed_content"
  )
}

appusage_source_preflight <- function(x, input = c("file", "text", "lines"),
                                      encoding = "auto",
                                      filename_type = NA_character_,
                                      nul_ratio_threshold = 0.01,
                                      control_ratio_threshold = 0.20, .context_sink = NULL,
                                      type = "auto") {
  input <- match.arg(input)
  bytes <- NULL
  lines <- character()
  encoding_diagnostics <- appusage_character_input_encoding_diagnostics(encoding)

  if (identical(input, "file")) {
    if (length(x) != 1L || !file.exists(x)) {
      cli::cli_abort("APP Usage file does not exist: {.path {x}}")
    }
    size <- file.info(x)$size
    if (is.na(size) || size == 0) {
      return(appusage_source_preflight_result(
        status = "zero_byte",
        failure_family = "source_zero_byte",
        lines = character(),
        byte_count = if (is.na(size)) 0 else size,
        encoding_diagnostics = encoding_diagnostics
      ))
    }
    bytes <- readBin(x, what = "raw", n = size)
    signature <- appusage_binary_signature(bytes)
    ratios <- appusage_byte_ratios(bytes)
    if (!is.na(signature)) {
      return(appusage_source_preflight_result(
        status = "binary_signature",
        failure_family = "source_binary",
        lines = character(),
        byte_count = length(bytes),
        binary_signature = signature,
        nul_byte_ratio = ratios$nul_byte_ratio,
        control_byte_ratio = ratios$control_byte_ratio,
        encoding_diagnostics = encoding_diagnostics
      ))
    }
    if (ratios$nul_byte_ratio >= nul_ratio_threshold ||
      ratios$control_byte_ratio >= control_ratio_threshold) {
      return(appusage_source_preflight_result(
        status = "binary_control_bytes",
        failure_family = "source_binary",
        lines = character(),
        byte_count = length(bytes),
        nul_byte_ratio = ratios$nul_byte_ratio,
        control_byte_ratio = ratios$control_byte_ratio,
        encoding_diagnostics = encoding_diagnostics
      ))
    }
    # The file reader already removes NULs before decoding. Apply the same
    # contract after the binary gates, retaining diagnostics of original bytes.
    text_bytes <- if (ratios$nul_byte_ratio > 0) bytes[bytes != as.raw(0)] else bytes
    decoded <- decode_raw_text(text_bytes, encoding = encoding)
    encoding_diagnostics <- attr(decoded, "encoding_diagnostics") %||%
      encoding_diagnostics
    lines <- split_lines(decoded)
  } else {
    lines <- if (inherits(x, "appusage_parse_context")) {
      x$store$lines[x$rows]
    } else {
      read_appusage_lines(x, input = input, encoding = encoding)
    }
    encoding_diagnostics <- attr(lines, "encoding_diagnostics") %||%
      encoding_diagnostics
  }

  context <- if (inherits(x, "appusage_parse_context")) x else appusage_parse_context(lines)
  if (!is.null(.context_sink)) .context_sink$context <- context
  components <- appusage_detect_components_from_lines(context)
  candidate_rows <- appusage_record_candidate_rows(context)
  boundary_diagnostics <- appusage_component_boundary_diagnostics(context, components)
  selection <- appusage_select_source_component(
    components,
    filename_type,
    boundary_diagnostics, type
  )
  selected_component <- selection$selected_component
  selected_profile <- if (is_present_string(selected_component)) {
    boundary_diagnostics$component_profiles[[selected_component]] %||% list()
  } else {
    list()
  }
  selected_candidate_rows <- selected_profile$candidate_rows %||% integer()
  if (identical(selection$selection_rule, "bounded_line_meta")) {
    selected_candidate_rows <- sort(unique(c(selected_candidate_rows,
      boundary_diagnostics$component_profiles$meta$candidate_rows)))
  }
  has_records <- if (is_present_string(selected_component)) {
    length(selected_candidate_rows) > 0L
  } else {
    length(candidate_rows) > 0L
  }
  status <- if (identical(selection$selection_rule, "requested_component_absent")) {
    "requested_component_absent"
  } else if (identical(selection$selection_rule, "incomplete_meta_blocks")) {
    "incomplete_meta_blocks"
  } else if (length(components) == 0L) {
    "unknown_content"
  } else if (length(components) > 1L && !is_present_string(selected_component)) {
    "mixed_content_ambiguous"
  } else if (!has_records) {
    "header_only"
  } else {
    "ok"
  }
  family <- switch(status,
    unknown_content = "source_unknown_content",
    mixed_content_ambiguous = "source_mixed_content",
    incomplete_meta_blocks = "source_unknown_content",
    requested_component_absent = "source_unknown_content",
    header_only = "source_header_only",
    NA_character_
  )
  appusage_source_preflight_result(
    status = status,
    failure_family = family,
    lines = lines,
    byte_count = if (is.null(bytes)) NA_real_ else length(bytes),
    nul_byte_ratio = if (is.null(bytes)) 0 else ratios$nul_byte_ratio,
    control_byte_ratio = if (is.null(bytes)) 0 else ratios$control_byte_ratio,
    detected_components = components,
    selected_component = selected_component,
    mixed_content = length(components) > 1L,
    selection_rule = selection$selection_rule,
    filename_type = filename_type,
    filename_content_disagreement = is_present_string(filename_type) &&
      is_present_string(selected_component) &&
      !filename_type %in% components,
    boundary_diagnostics = boundary_diagnostics,
    has_record_rows = has_records,
    record_candidate_rows = selected_candidate_rows,
    encoding_diagnostics = encoding_diagnostics
  )
}

appusage_source_preflight_result <- function(status, failure_family, lines,
                                             byte_count = NA_real_,
                                             binary_signature = NA_character_,
                                             nul_byte_ratio = 0,
                                             control_byte_ratio = 0,
                                             detected_components = character(),
                                             selected_component = NA_character_,
                                             mixed_content = FALSE,
                                             selection_rule = NA_character_,
                                             filename_type = NA_character_,
                                             filename_content_disagreement = FALSE,
                                             boundary_diagnostics = list(),
                                             has_record_rows = FALSE,
                                             record_candidate_rows = integer(),
                                             encoding_diagnostics = list()) {
  structure(list(
    status = status,
    failure_family = failure_family,
    byte_count = as.numeric(byte_count),
    binary_signature = binary_signature,
    nul_byte_ratio = as.numeric(nul_byte_ratio),
    control_byte_ratio = as.numeric(control_byte_ratio),
    detected_components = detected_components,
    selected_component = selected_component,
    mixed_content = isTRUE(mixed_content),
    selection_rule = selection_rule,
    filename_type = filename_type,
    filename_content_disagreement = isTRUE(filename_content_disagreement),
    boundary_diagnostics = boundary_diagnostics,
    has_record_rows = isTRUE(has_record_rows),
    record_candidate_rows = as.integer(record_candidate_rows),
    encoding = encoding_diagnostics,
    lines = lines
  ), class = c("appusage_source_preflight", "list"))
}

appusage_compact_source_preflight <- function(preflight) {
  if (is.null(preflight)) return(list())
  preflight[setdiff(names(preflight), "lines")]
}

appusage_source_preflight_error <- function(preflight) {
  class_name <- switch(preflight$status,
    zero_byte = "appusage_zero_byte_source",
    binary_signature = "appusage_binary_source",
    binary_control_bytes = "appusage_binary_source",
    unknown_content = "appusage_unknown_content",
    incomplete_meta_blocks = "appusage_incomplete_meta_blocks",
    header_only = "appusage_header_only_source",
    mixed_content_ambiguous = "appusage_mixed_content_source",
    "appusage_source_preflight_error"
  )
  message <- switch(preflight$status,
    zero_byte = "Source file is zero-byte and contains no APP Usage data.",
    binary_signature = appusage_text_paste0(
      "Source file has a recognized binary signature: ",
      preflight$binary_signature, "."
    ),
    binary_control_bytes = "Source file has a high NUL/control-byte ratio and appears binary.",
    unknown_content = "Decoded text contains no recognized APP Usage markers.",
    incomplete_meta_blocks = "Incomplete APP Usage meta blocks: check the Table 1 and Table 2 headers.",
    requested_component_absent = "Requested APP Usage component is absent from this source.",
    header_only = "APP Usage headers were recognized, but no record rows were found.",
    mixed_content_ambiguous = appusage_text_paste0(
      "Multiple APP Usage components were detected, but no unambiguous bounded component could be selected: ",
      appusage_text_paste(preflight$detected_components, collapse = ";"), "."
    ),
    "APP Usage source preflight failed."
  )
  structure(
    list(
      message = message,
      call = NULL,
      source_preflight = appusage_compact_source_preflight(preflight)
    ),
    class = c(class_name, "appusage_source_preflight_error", "error", "condition")
  )
}

appusage_preflight_summary_fields <- function(preflight) {
  compact <- appusage_compact_source_preflight(preflight)
  encoding <- compact$encoding %||% list()
  failures <- encoding$conversion_failures %||% list()
  failure_text <- if (length(failures) == 0L) {
    NA_character_
  } else {
    appusage_text_paste(vapply(failures, function(x) {
      appusage_text_paste0(x$candidate %||% "unknown", ": ", x$message %||% "conversion failed")
    }, character(1)), collapse = "; ")
  }
  list(
    preflight_status = compact$status %||% NA_character_,
    detected_components = appusage_text_paste(compact$detected_components %||% character(), collapse = ";"),
    selected_component = compact$selected_component %||% NA_character_,
    mixed_content = compact$mixed_content %||% FALSE,
    component_selection_rule = compact$selection_rule %||% NA_character_,
    filename_content_disagreement = compact$filename_content_disagreement %||% FALSE,
    structural_boundary_count = length(compact$boundary_diagnostics$boundaries %||% list()),
    preflight_has_record_rows = compact$has_record_rows %||% FALSE,
    binary_signature = compact$binary_signature %||% NA_character_,
    nul_byte_ratio = compact$nul_byte_ratio %||% NA_real_,
    control_byte_ratio = compact$control_byte_ratio %||% NA_real_,
    encoding_attempted_candidates = appusage_text_paste(encoding$attempted_candidates %||% character(), collapse = ";"),
    encoding_supported_candidates = appusage_text_paste(encoding$supported_candidates %||% character(), collapse = ";"),
    encoding_selected = encoding$selected_encoding %||% NA_character_,
    encoding_conversion_failures = failure_text
  )
}
