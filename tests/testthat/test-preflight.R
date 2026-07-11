test_that("encoding fallback filters unsupported candidates and continues after conversion failure", {
  converter <- function(x, from, to) {
    if (identical(from, "BROKEN")) stop("synthetic conversion failure")
    iconv(x, from = from, to = to)
  }
  decoded <- decode_raw_text(
    charToRaw("plain text"),
    detected_candidates = c("WINDOWS-UNSUPPORTED", "BROKEN", "UTF-8"),
    available_encodings = c("BROKEN", "UTF-8"),
    converter = converter
  )
  diagnostics <- attr(decoded, "encoding_diagnostics")

  expect_equal(as.vector(decoded), "plain text")
  expect_equal(
    diagnostics$attempted_candidates[seq_len(3)],
    c("WINDOWS-UNSUPPORTED", "BROKEN", "UTF-8")
  )
  expect_true("WINDOWS-UNSUPPORTED" %in% diagnostics$unsupported_candidates)
  expect_equal(diagnostics$supported_candidates[seq_len(2)], c("BROKEN", "UTF-8"))
  expect_equal(diagnostics$selected_encoding, "UTF-8")
  expect_equal(diagnostics$conversion_failures[[1]]$candidate, "BROKEN")
})

test_that("source preflight classifies empty and recognizable binary signatures", {
  signatures <- list(
    jpeg = as.raw(c(255, 216, 255, 224)),
    png = as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)),
    pdf = charToRaw("%PDF-1.7"),
    zip = as.raw(c(80, 75, 3, 4, 1, 2)),
    gif87a = charToRaw("GIF87a")
  )
  empty <- tempfile(fileext = ".txt")
  file.create(empty)
  expect_equal(appusage_source_preflight(empty)$status, "zero_byte")

  for (name in names(signatures)) {
    path <- tempfile(fileext = ".txt")
    writeBin(signatures[[name]], path)
    result <- appusage_source_preflight(path)
    expect_equal(result$status, "binary_signature", info = name)
    expect_equal(result$failure_family, "source_binary", info = name)
  }

  nul_path <- tempfile(fileext = ".txt")
  writeBin(as.raw(rep(c(0, 1, 2, 3), 32)), nul_path)
  nul <- appusage_source_preflight(nul_path)
  expect_equal(nul$status, "binary_control_bytes")
  expect_gt(nul$nul_byte_ratio, 0.01)
})

test_that("source preflight distinguishes unknown header-only valid and mixed content", {
  line_header <- paste(
    "2024-01-01", "\u5f00\u59cb\u65f6\u95f4\uff08ms\uff09", "\u5f00\u59cb\u65f6\u95f4",
    "\u5e94\u7528\u540d\u79f0", "\u5e94\u7528\u6807\u8bc6", "\u4f7f\u7528\u65f6\u957f",
    "\u7ed3\u675f\u65f6\u95f4\uff08ms\uff09", "\u7ed3\u675f\u65f6\u95f4",
    sep = ","
  )
  line_record <- paste(
    "", "T:1704067200000", "00:00:00", "Example", "org.example.app",
    "1 minute", "T:1704067260000", "00:01:00", sep = ","
  )
  meta_headers <- c(
    paste("2024-01-01\u8868\u4e00", "\u5e94\u7528\u540d\u79f0", "\u5e94\u7528\u5305\u540d", sep = ","),
    paste("2024-01-01\u8868\u4e8c", "\u5177\u4f53\u9875\u9762", "\u65f6\u95f4\u6233", "\u7c7b\u578b", "\u914d\u7f6e", sep = ",")
  )

  unknown <- appusage_source_preflight("ordinary notes", input = "text")
  header_only <- appusage_source_preflight(line_header, input = "text")
  valid <- appusage_source_preflight(c(line_header, line_record), input = "lines")
  mixed <- appusage_source_preflight(c(line_header, line_record, meta_headers), input = "lines")

  expect_equal(unknown$status, "unknown_content")
  expect_equal(header_only$status, "header_only")
  expect_equal(valid$status, "ok")
  expect_equal(valid$detected_components, "line")
  expect_equal(mixed$status, "ok")
  expect_setequal(mixed$detected_components, c("line", "meta"))
  expect_true(mixed$mixed_content)
  expect_equal(mixed$selected_component, "line")
  expect_equal(mixed$selection_rule, "single_bounded_component_with_records")
})

test_that("source preflight failures are terminal and are not memory retried", {
  retry_calls <- 0L
  families <- c(
    "source_zero_byte", "source_binary", "source_unknown_content",
    "source_header_only", "source_mixed_content"
  )
  for (family in families) {
    row <- data.frame(
      status = "error",
      failure_family = family,
      error_class = "appusage_source_preflight_error",
      error_message = family,
      stringsAsFactors = FALSE
    )
    result <- appusage_retry_memory_row(row, retry_fun = function() {
      retry_calls <<- retry_calls + 1L
      row
    })
    expect_equal(result$failure_family, family)
  }
  expect_equal(retry_calls, 0L)
})

test_that("batch preflight writes stable zero-byte diagnostics", {
  path <- tempfile(fileext = ".txt")
  file.create(path)
  output <- tempfile("appusage-preflight-batch-")
  summary <- read_appusage_batch(
    path,
    ids = "empty",
    output_dir = output,
    project_name = "Synthetic",
    project_id = "pf1",
    progress = FALSE,
    retry_memory_allocation = TRUE
  )

  expect_equal(summary$status, "error")
  expect_equal(summary$failure_family, "source_zero_byte")
  expect_equal(summary$preflight_status, "zero_byte")
  expect_equal(summary$retry_attempt, 0L)
  expect_true(file.exists(summary$metadata_file))
  metadata <- jsonlite::read_json(summary$metadata_file, simplifyVector = TRUE)
  expect_equal(metadata$source$preflight$status, "zero_byte")
})

test_that("memory variants and contextual NA control-flow classification stay narrow", {
  variants <- c(
    "cannot allocate vector of size 143 Kb",
    "could not allocate memory (0 Mb) in C function 'R_AllocStringBuffer'",
    "Failed to realloc working memory stack to 100000*4bytes",
    "memory exhausted",
    "vector memory exhausted",
    "not enough memory",
    "cannot reserve memory block"
  )
  expect_true(all(vapply(variants, appusage_is_memory_allocation_text, logical(1))))
  expect_true(all(vapply(variants, function(x) {
    identical(appusage_classify_failure_family("simpleError", x), "memory_allocation")
  }, logical(1))))

  na_message <- "missing value where TRUE/FALSE needed"
  expect_equal(
    appusage_classify_failure_family("simpleError", na_message),
    "parser_control_flow"
  )
  expect_equal(
    appusage_classify_failure_family(
      "simpleError", na_message,
      memory_risk_signal = TRUE
    ),
    "memory_allocation"
  )

  original <- data.frame(
    status = "error",
    error_class = "simpleError,error,condition",
    error_message = na_message,
    error_call = "if (flag) value",
    traceback = "parser_call()",
    index = 7L,
    worker_task_index = 7L,
    raw_line_number = 12L,
    raw_line_window = "11: header; 12: value",
    memory_risk_signal = TRUE,
    stringsAsFactors = FALSE
  )
  original <- appusage_annotate_first_level_row(original)
  final <- appusage_retry_memory_row(original, retry_fun = function() {
    data.frame(
      status = "error",
      error_class = "simpleError,error,condition",
      error_message = na_message,
      error_call = "if (flag) value",
      traceback = "parser_call()",
      index = 7L,
      memory_risk_signal = FALSE,
      stringsAsFactors = FALSE
    )
  })
  expect_equal(final$failure_family, "parser_control_flow")
  expect_equal(final$original_failure_family, "memory_allocation")
  expect_equal(final$original_error_call, "if (flag) value")
  expect_equal(final$original_traceback, "parser_call()")
  expect_equal(final$original_worker_task_index, 7L)
  expect_equal(final$original_raw_line_number, 12L)
  expect_match(final$original_raw_line_window, "12: value", fixed = TRUE)
})
