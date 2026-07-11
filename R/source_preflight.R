# First-level source signature and content preflight helpers

appusage_binary_signature <- function(bytes) {
  values <- as.integer(bytes)
  starts_with <- function(signature) {
    length(values) >= length(signature) &&
      identical(values[seq_along(signature)], as.integer(signature))
  }
  signatures <- list(
    jpeg = c(255, 216, 255),
    png = c(137, 80, 78, 71, 13, 10, 26, 10),
    pdf = utf8ToInt("%PDF"),
    zip = c(80, 75, 3, 4),
    zip_empty = c(80, 75, 5, 6),
    gif87a = utf8ToInt("GIF87a"),
    gif89a = utf8ToInt("GIF89a"),
    rar = c(82, 97, 114, 33, 26, 7),
    seven_zip = c(55, 122, 188, 175, 39, 28),
    gzip = c(31, 139),
    windows_executable = c(77, 90)
  )
  hit <- names(signatures)[vapply(signatures, starts_with, logical(1))]
  if (length(hit) == 0L) NA_character_ else hit[[1]]
}

appusage_byte_ratios <- function(bytes) {
  if (length(bytes) == 0L) {
    return(list(nul_byte_ratio = 0, control_byte_ratio = 0))
  }
  values <- as.integer(bytes)
  controls <- values < 32L & !values %in% c(9L, 10L, 13L)
  list(
    nul_byte_ratio = mean(values == 0L),
    control_byte_ratio = mean(controls)
  )
}

appusage_record_candidate_rows <- function(lines) {
  mat <- as_text_matrix(lines)
  if (nrow(mat) == 0L) return(integer())
  which(vapply(seq_len(nrow(mat)), function(i) {
    cells <- trimws(as.character(stats::na.omit(mat[i, ])))
    cells <- cells[nzchar(cells)]
    if (length(cells) < 2L) return(FALSE)
    has_package <- any(grepl(
      "^(?:[A-Za-z][A-Za-z0-9_-]*[.])+[A-Za-z0-9_.-]+$|^ALL$",
      cells
    ))
    has_time_value <- any(grepl(
      "^T:[0-9]{6,}$|^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}|^[0-9]+(?:[.][0-9]+)?(?:[Ee][+-]?[0-9]+)?$",
      cells
    ))
    header_token_count <- sum(grepl(
      paste(c(
        "\u5f00\u59cb\u65f6\u95f4", "\u7ed3\u675f\u65f6\u95f4",
        "\u5e94\u7528\u540d\u79f0", "\u5e94\u7528\u6807\u8bc6",
        "\u683c\u5f0f\u5316\u65f6\u95f4", "\u4f7f\u7528\u65f6\u957f",
        "\u542f\u52a8\u6b21\u6570", "\u901a\u77e5\u6b21\u6570",
        "\u5177\u4f53\u9875\u9762", "\u65f6\u95f4\u6233", "\u914d\u7f6e"
      ), collapse = "|"),
      cells
    ))
    (has_package && has_time_value) ||
      (has_time_value && length(cells) >= 3L && header_token_count < 2L)
  }, logical(1)))
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

appusage_structural_boundaries <- function(mat) {
  if (is.null(dim(mat)) || nrow(mat) == 0L) {
    return(data.frame(
      row = integer(), boundary_type = character(), component = character(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- vector("list", nrow(mat))
  for (i in seq_len(nrow(mat))) {
    row <- mat[i, ]
    types <- character()
    components <- character()
    if (row_contains_all(row, unname(line_required_fields()))) {
      types <- c(types, "line_header")
      components <- c(components, "line")
    }
    if (row_contains_any(row, "\u8868\u4e00")) {
      types <- c(types, "meta_table1")
      components <- c(components, "meta")
    }
    if (row_contains_any(row, "\u8868\u4e8c")) {
      types <- c(types, "meta_table2")
      components <- c(components, "meta")
    }
    if (row_contains_all(row, unname(day_required_fields()))) {
      types <- c(types, "day_header")
      components <- c(components, "day")
    }
    if (row_contains_all(row, unname(app_required_fields()))) {
      types <- c(types, "app_header")
      components <- c(components, "app")
    }
    first <- trimws(as.character(stats::na.omit(row)))[1] %||% NA_character_
    if (is_present_string(first)) {
      metadata_patterns <- c(
        step_metadata = "^(?:STEP|\u5f00\u673a\u81f3\u4eca\u6b65\u6570\u4fe1\u606f|\u6b65\u6570\u4fe1\u606f)",
        device_metadata = "^(?:DEVICE|\u8bbe\u5907\u4fe1\u606f|\u624b\u673a\u4fe1\u606f|\u8bbe\u5907\u578b\u53f7|\u5382\u5546|\u54c1\u724c)",
        system_metadata = "^(?:SYSTEM|BUILD|MODEL|BRAND|Android\\s*Version|\u7cfb\u7edf\u4fe1\u606f|\u7cfb\u7edf\u7248\u672c|Android\u7248\u672c)"
      )
      hit <- names(metadata_patterns)[vapply(
        metadata_patterns,
        function(pattern) grepl(pattern, first, ignore.case = TRUE, perl = TRUE),
        logical(1)
      )]
      if (length(hit) > 0L) {
        types <- c(types, hit[[1]])
        components <- c(components, "metadata")
      }
    }
    if (length(types) > 0L) {
      rows[[i]] <- data.frame(
        row = rep.int(i, length(types)),
        boundary_type = types,
        component = components,
        stringsAsFactors = FALSE
      )
    }
  }
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0L) {
    return(data.frame(
      row = integer(), boundary_type = character(), component = character(),
      stringsAsFactors = FALSE
    ))
  }
  unique(do.call(rbind, rows))
}

appusage_component_boundary_diagnostics <- function(lines, components) {
  mat <- as_text_matrix(lines)
  boundaries <- appusage_structural_boundaries(mat)
  profiles <- lapply(components, function(component) {
    header_rows <- unique(boundaries$row[boundaries$component == component])
    candidate_rows <- integer()
    section_ranges <- vector("list", length(header_rows))
    for (j in seq_along(header_rows)) {
      start <- header_rows[[j]] + 1L
      next_boundary <- boundaries$row[boundaries$row > header_rows[[j]]]
      end <- if (length(next_boundary) == 0L) nrow(mat) else min(next_boundary) - 1L
      section_ranges[[j]] <- list(start = start, end = end)
      if (start <= end) {
        section <- apply(mat[start:end, , drop = FALSE], 1L, function(row) {
          paste(ifelse(is.na(row), "", row), collapse = ",")
        })
        local <- appusage_record_candidate_rows(section)
        if (length(local) > 0L) candidate_rows <- c(candidate_rows, start - 1L + local)
      }
    }
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
                                             boundary_diagnostics) {
  filename_type <- as.character(filename_type %||% NA_character_)[[1]]
  mixed <- length(components) > 1L
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
                                      control_ratio_threshold = 0.20) {
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
    decoded <- decode_raw_text(bytes, encoding = encoding)
    encoding_diagnostics <- attr(decoded, "encoding_diagnostics") %||%
      encoding_diagnostics
    lines <- split_lines(decoded)
  } else {
    lines <- read_appusage_lines(x, input = input, encoding = encoding)
    encoding_diagnostics <- attr(lines, "encoding_diagnostics") %||%
      encoding_diagnostics
  }

  components <- appusage_detect_components_from_lines(lines)
  candidate_rows <- appusage_record_candidate_rows(lines)
  boundary_diagnostics <- appusage_component_boundary_diagnostics(lines, components)
  selection <- appusage_select_source_component(
    components,
    filename_type,
    boundary_diagnostics
  )
  selected_component <- selection$selected_component
  selected_profile <- if (is_present_string(selected_component)) {
    boundary_diagnostics$component_profiles[[selected_component]] %||% list()
  } else {
    list()
  }
  selected_candidate_rows <- selected_profile$candidate_rows %||% integer()
  has_records <- if (is_present_string(selected_component)) {
    length(selected_candidate_rows) > 0L
  } else {
    length(candidate_rows) > 0L
  }
  status <- if (length(components) == 0L) {
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
    header_only = "source_header_only",
    NA_character_
  )
  appusage_source_preflight_result(
    status = status,
    failure_family = family,
    lines = lines,
    byte_count = if (is.null(bytes)) NA_real_ else length(bytes),
    detected_components = components,
    selected_component = selected_component,
    mixed_content = length(components) > 1L,
    selection_rule = selection$selection_rule,
    filename_type = filename_type,
    filename_content_disagreement = is_present_string(filename_type) &&
      is_present_string(selected_component) &&
      !identical(filename_type, selected_component),
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
    header_only = "appusage_header_only_source",
    mixed_content_ambiguous = "appusage_mixed_content_source",
    "appusage_source_preflight_error"
  )
  message <- switch(preflight$status,
    zero_byte = "Source file is zero-byte and contains no APP Usage data.",
    binary_signature = paste0(
      "Source file has a recognized binary signature: ",
      preflight$binary_signature, "."
    ),
    binary_control_bytes = "Source file has a high NUL/control-byte ratio and appears binary.",
    unknown_content = "Decoded text contains no recognized APP Usage markers.",
    header_only = "APP Usage headers were recognized, but no record rows were found.",
    mixed_content_ambiguous = paste0(
      "Multiple APP Usage components were detected, but no unambiguous bounded component could be selected: ",
      paste(preflight$detected_components, collapse = ";"), "."
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
    paste(vapply(failures, function(x) {
      paste0(x$candidate %||% "unknown", ": ", x$message %||% "conversion failed")
    }, character(1)), collapse = "; ")
  }
  list(
    preflight_status = compact$status %||% NA_character_,
    detected_components = paste(compact$detected_components %||% character(), collapse = ";"),
    selected_component = compact$selected_component %||% NA_character_,
    mixed_content = compact$mixed_content %||% FALSE,
    component_selection_rule = compact$selection_rule %||% NA_character_,
    filename_content_disagreement = compact$filename_content_disagreement %||% FALSE,
    structural_boundary_count = length(compact$boundary_diagnostics$boundaries %||% list()),
    preflight_has_record_rows = compact$has_record_rows %||% FALSE,
    binary_signature = compact$binary_signature %||% NA_character_,
    nul_byte_ratio = compact$nul_byte_ratio %||% NA_real_,
    control_byte_ratio = compact$control_byte_ratio %||% NA_real_,
    encoding_attempted_candidates = paste(encoding$attempted_candidates %||% character(), collapse = ";"),
    encoding_supported_candidates = paste(encoding$supported_candidates %||% character(), collapse = ";"),
    encoding_selected = encoding$selected_encoding %||% NA_character_,
    encoding_conversion_failures = failure_text
  )
}
