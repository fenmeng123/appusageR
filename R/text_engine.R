# Data-text operations use ICU. These adapters preserve the small set of base-R
# contracts used by appusageR (NA, recycling, terminal split fields and captures).
appusage_text_detect <- function(string, pattern) stringi::stri_detect_regex(string, pattern)
appusage_text_remove <- function(string, pattern) stringi::stri_replace_first_regex(string, pattern, "")
appusage_text_str_replace <- function(string, pattern, replacement) appusage_text_replace(pattern, replacement, string)
appusage_text_str_replace_all <- function(string, pattern, replacement) appusage_text_replace(pattern, replacement, string, all = TRUE)

appusage_text_paste <- function(..., sep = " ", collapse = NULL, recycle0 = FALSE) {
  values <- lapply(list(...), as.character)
  sizes <- lengths(values)
  n <- if (length(sizes)) max(sizes) else 0L
  if (isTRUE(recycle0) && any(sizes == 0L)) n <- 0L
  if (!n) return(if (is.null(collapse)) character() else "")
  values <- lapply(values, function(x) {
    if (!length(x)) x <- ""
    x[is.na(x)] <- "NA"
    if (length(x) != n) x <- rep_len(x, n)
    x
  })
  out <- do.call(stringi::stri_join, c(values, list(sep = sep)))
  if (is.null(collapse)) out else stringi::stri_join(out, collapse = collapse)
}

appusage_text_paste0 <- function(..., collapse = NULL, recycle0 = FALSE) {
  appusage_text_paste(..., sep = "", collapse = collapse, recycle0 = recycle0)
}

appusage_text_trim <- function(x, which = c("both", "left", "right"), whitespace = "[ \\t\\r\\n]") {
  which <- match.arg(which)
  pattern <- switch(which, both = appusage_text_paste0("^", whitespace, "+|", whitespace, "+$"),
    left = appusage_text_paste0("^", whitespace, "+"), right = appusage_text_paste0(whitespace, "+$"))
  out <- stringi::stri_replace_all_regex(as.character(x), pattern, "")
  names(out) <- names(x)
  out
}

appusage_text_split <- function(x, split, fixed = FALSE, perl = FALSE, useBytes = FALSE) {
  if (!is.character(x)) stop("non-character argument", call. = FALSE)
  if (length(split) != 1L) stop("Internal text split requires one pattern.")
  out <- if (fixed) stringi::stri_split_fixed(x, split, omit_empty = FALSE) else stringi::stri_split_regex(x, split, omit_empty = FALSE)
  for (i in seq_along(out)) {
    value <- out[[i]]
    if (!is.na(x[[i]]) && length(value) && identical(value[[length(value)]], "")) out[[i]] <- utils::head(value, -1L)
  }
  names(out) <- names(x)
  out
}

appusage_text_grepl <- function(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  x <- as.character(x)
  if (is.na(pattern[[1L]])) return(rep(NA, length(x)))
  out <- if (fixed) stringi::stri_detect_fixed(x, pattern, opts_fixed = list(case_insensitive = ignore.case)) else
    stringi::stri_detect_regex(x, pattern, opts_regex = list(case_insensitive = ignore.case))
  out[is.na(x)] <- FALSE
  out
}

appusage_text_grep <- function(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE, fixed = FALSE, useBytes = FALSE, invert = FALSE) {
  hit <- appusage_text_grepl(pattern, x, ignore.case, perl, fixed, useBytes)
  if (invert) hit <- !hit
  index <- which(hit)
  if (value) as.character(x)[index] else index
}

appusage_text_replace <- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE, all = FALSE) {
  replace <- if (fixed) {
    if (all) stringi::stri_replace_all_fixed else stringi::stri_replace_first_fixed
  } else {
    if (all) stringi::stri_replace_all_regex else stringi::stri_replace_first_regex
  }
  if (!fixed) replacement <- stringi::stri_replace_rstr(replacement)
  options <- if (fixed) list(opts_fixed = list(case_insensitive = ignore.case)) else list(opts_regex = list(case_insensitive = ignore.case))
  out <- do.call(replace, c(list(str = as.character(x), pattern = pattern, replacement = replacement), options))
  names(out) <- names(x)
  out
}

appusage_text_sub <- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  appusage_text_replace(pattern, replacement, x, ignore.case, perl, fixed, useBytes)
}
appusage_text_gsub <- function(pattern, replacement, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  appusage_text_replace(pattern, replacement, x, ignore.case, perl, fixed, useBytes, all = TRUE)
}

appusage_text_case <- function(x, upper = FALSE) {
  original_names <- names(x)
  x <- as.character(x)
  convert <- if (upper) stringi::stri_trans_toupper else stringi::stri_trans_tolower
  if (Sys.getlocale("LC_CTYPE") %in% c("C", "POSIX")) {
    out <- stringi::stri_trans_char(x, if (upper) "abcdefghijklmnopqrstuvwxyz" else "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
      if (upper) "ABCDEFGHIJKLMNOPQRSTUVWXYZ" else "abcdefghijklmnopqrstuvwxyz")
    names(out) <- original_names
    return(out)
  }
  out <- convert(x, locale = "en")
  bom <- vapply(x, function(value) !is.na(value) &&
    identical(utils::head(charToRaw(value), 3L), as.raw(c(239L, 187L, 191L))), logical(1))
  non_ascii <- which(stringi::stri_detect_regex(x, "[^\\x{00}-\\x{7F}]") | bom)
  if (length(non_ascii)) {
    points <- stringi::stri_enc_toutf32(x[non_ascii])
    for (i in which(bom[non_ascii])) points[[i]] <- c(65279L, points[[i]])
    flat <- unlist(points, use.names = FALSE)
    chars <- stringi::stri_enc_fromutf32(as.list(flat))
    changed <- convert(chars, locale = "en")
    delta <- appusage_text_case_delta(upper)
    key <- match(flat, delta$from)
    patch <- which(!is.na(key))
    changed[patch] <- stringi::stri_enc_fromutf32(delta$to[key[patch]])
    # Transcoding an isolated FEFF treats it as a BOM. Inside a string it is
    # data; the initial stri_enc_toutf32 call already handled a leading BOM.
    changed_points <- stringi::stri_enc_toutf32(changed)
    changed_points[flat == 65279L] <- list(65279L)
    rebuilt <- stringi::stri_enc_fromutf32(lapply(utils::relist(unlist(changed_points, use.names = FALSE), points),
      function(value) c(81L, value)))
    # Remove the protective ASCII prefix in raw bytes so ICU cannot consume a
    # leading FEFF as a byte-order marker on a second conversion.
    rebuilt <- vapply(rebuilt, function(value) rawToChar(charToRaw(value)[-1L]), character(1))
    Encoding(rebuilt) <- "UTF-8"
    out[non_ascii] <- rebuilt
  }
  names(out) <- original_names
  out
}
appusage_text_lower <- function(x) appusage_text_case(x)
appusage_text_upper <- function(x) appusage_text_case(x, TRUE)

appusage_text_nzchar <- function(x, keepNA = FALSE) {
  out <- stringi::stri_length(as.character(x)) > 0L
  if (!keepNA) out[is.na(out)] <- TRUE
  out
}

appusage_text_nchar <- function(x, type = "chars", allowNA = FALSE, keepNA = NA) {
  out <- switch(type, bytes = stringi::stri_numbytes(as.character(x)),
    width = stringi::stri_width(as.character(x)), stringi::stri_length(as.character(x)))
  if (identical(keepNA, FALSE)) out[is.na(x)] <- 2L
  names(out) <- names(x)
  out
}

appusage_text_substr <- function(x, start, stop) {
  start <- rep_len(pmax(1L, as.integer(start)), length(x))
  stop <- rep_len(as.integer(stop), length(x))
  out <- stringi::stri_sub(as.character(x), start, pmax(start, stop))
  out[!is.na(stop) & !is.na(start) & stop < start & !is.na(x)] <- ""
  names(out) <- names(x)
  out
}
appusage_text_substring <- function(text, first, last = 1000000L) appusage_text_substr(text, first, last)
appusage_text_starts <- function(x, prefix) stringi::stri_startswith_fixed(x, prefix)
appusage_text_ends <- function(x, suffix) stringi::stri_endswith_fixed(x, suffix)

# Internal match carriers are consumed only by appusage_text_regmatches. Capture
# extraction is performed by ICU; positions still support the existing -1 tests.
appusage_text_regexec <- function(pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  matches <- stringi::stri_match_first_regex(text, pattern, opts_regex = list(case_insensitive = ignore.case))
  where <- stringi::stri_locate_first_regex(text, pattern, opts_regex = list(case_insensitive = ignore.case))
  lapply(seq_len(nrow(matches)), function(i) {
    value <- matches[i, ]
    if (is.na(value[[1L]])) return(structure(-1L, text_values = character()))
    structure(as.integer(where[i, 1L]), text_values = unname(value))
  })
}
appusage_text_gregexpr <- function(pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  values <- stringi::stri_extract_all_regex(text, pattern, omit_no_match = TRUE, opts_regex = list(case_insensitive = ignore.case))
  positions <- stringi::stri_locate_all_regex(text, pattern, omit_no_match = TRUE, opts_regex = list(case_insensitive = ignore.case))
  lapply(seq_along(values), function(i) structure(if (!length(values[[i]])) -1L else as.integer(positions[[i]][, 1L]), text_values = values[[i]]))
}
appusage_text_regexpr <- function(pattern, text, ignore.case = FALSE, perl = FALSE, fixed = FALSE, useBytes = FALSE) {
  positions <- stringi::stri_locate_first_regex(text, pattern, opts_regex = list(case_insensitive = ignore.case))
  out <- positions[, 1L]
  out[is.na(out)] <- -1L
  structure(out, text_values = stringi::stri_extract_first_regex(text, pattern, opts_regex = list(case_insensitive = ignore.case)))
}
appusage_text_regmatches <- function(x, m, invert = FALSE) {
  if (invert) stop("Internal match extraction does not use invert.")
  if (is.list(m)) return(lapply(m, attr, which = "text_values", exact = TRUE))
  values <- attr(m, "text_values", exact = TRUE)
  values[m != -1L]
}

appusage_text_encoding_names <- function() {
  # Capability metadata only: preserve the prior platform's candidate filter.
  # All actual conversions below use ICU; no iconv conversion fallback exists.
  iconvlist()
}

# ICU locates percent escapes in a byte-preserving representation. Keep the
# legacy permissive byte arithmetic, rawToChar errors and literal '+' behavior.
appusage_text_url_decode <- function(URL) {
  vapply(URL, function(value) {
    bytes <- charToRaw(value)
    latin <- stringi::stri_encode(list(bytes), from = "ISO-8859-1", to = "UTF-8")
    at <- stringi::stri_locate_all_regex(latin, "%[\\s\\S]{0,2}", omit_no_match = TRUE)[[1L]]
    if (!nrow(at)) return(rawToChar(bytes))
    y <- cbind(as.integer(bytes[at[, 1L] + 1L]), as.integer(bytes[at[, 1L] + 2L]))
    y[y > 96L & !is.na(y)] <- y[y > 96L & !is.na(y)] - 32L
    y[y > 57L & !is.na(y)] <- y[y > 57L & !is.na(y)] - 7L
    decoded <- (y[, 1L] - 48L) * 16L + y[, 2L] - 48L
    bytes[at[, 1L]] <- as.raw(as.character(decoded))
    drop <- c(at[, 1L] + 1L, at[, 1L] + 2L)
    rawToChar(bytes[setdiff(seq_along(bytes), drop)])
  }, character(1), USE.NAMES = FALSE)
}

appusage_text_encode_strict <- function(x, from, to = "UTF-8") {
  encoding_key <- stringi::stri_trans_toupper(from)
  if (identical(to, "UTF-8") && encoding_key %in% c("SHIFT_JIS", "SHIFT-JIS", "SJIS", "CP932")) {
    return(appusage_text_sjis_compat(x))
  }
  if (identical(to, "UTF-8") && encoding_key %in% c("EUC-JP", "EUCJP")) {
    return(appusage_text_eucjp_compat(x))
  }
  icu_from <- if (encoding_key %in% c("EUC-KR", "EUCKR", "CP949")) "windows-949" else from
  substituted <- FALSE
  value <- withCallingHandlers(stringi::stri_encode(x, from = icu_from, to = to),
    warning = function(w) { substituted <<- TRUE; invokeRestart("muffleWarning") })
  delta <- if (identical(to, "UTF-8")) appusage_text_sbcs_delta(stringi::stri_trans_toupper(from)) else NULL
  if (!is.null(delta)) {
    inputs <- if (is.raw(x)) list(x) else as.list(x)
    for (i in seq_along(inputs)) {
      v <- inputs[[i]]
      if (is.character(v) && is.na(v)) next
      bytes <- as.integer(if (is.raw(v)) v else charToRaw(v))
      if (any(bytes %in% delta$error)) stop("embedded nul in converted string")
      keys <- match(bytes, delta$byte)
      at <- which(!is.na(keys))
      if (length(at)) {
        points <- delta$code[keys[at]]
        if (anyNA(unlist(points))) { value[[i]] <- NA_character_; next }
        value[[i]] <- stringi::stri_sub_replace_all(value[[i]], from = list(at), to = list(at),
          replacement = list(stringi::stri_enc_fromutf32(points)))
      }
    }
    return(value)
  }
  if (stringi::stri_trans_toupper(from) %in% c("GB18030", "GB18030-2022", "WINDOWS-54936") && identical(to, "UTF-8")) {
    inputs <- if (is.raw(x)) list(x) else as.list(x)
    affected <- substituted | stringi::stri_detect_regex(value,
      "[\u3000\uFE10-\uFE19\u1E3F\u9FB4-\u9FBB\uE7C7\u20AC]")
    for (i in which(affected & !is.na(value))) {
      v <- inputs[[i]]
      value[[i]] <- appusage_text_gb18030_compat(if (is.raw(v)) v else charToRaw(v))
    }
    return(value)
  }
  if (substituted) return(rep(NA_character_, length(value)))
  if (encoding_key %in% c("GBK", "CP936", "MS936", "WINDOWS-936", "WINDOWS-936-2000", "BIG5", "BIG-5", "CP950")) {
    # ICU's Windows-936 extension maps the invalid standalone FF byte to PUA.
    # It is not a valid trail byte either; the prior decoder rejected it.
    inputs <- if (is.raw(x)) list(x) else as.list(x)
    invalid <- vapply(inputs, function(v) {
      if (is.character(v) && is.na(v)) return(FALSE)
      bytes <- if (is.raw(v)) v else charToRaw(v)
      any(bytes == as.raw(255L))
    }, logical(1))
    value[invalid] <- NA_character_
  }
  value
}

# The Windows Shift-JIS converter preserves DOS control bytes and accepts 80.
# ICU tokenizes the byte representation and converts all character units in one
# vector call; four singleton mappings need the legacy compatibility correction.
appusage_text_sjis_compat <- function(x) {
  inputs <- if (is.raw(x)) list(x) else as.list(x)
  vapply(inputs, function(value) {
    if (is.character(value) && is.na(value)) return(NA_character_)
    bytes <- if (is.raw(value)) value else charToRaw(value)
    if (!length(bytes)) return("")
    latin <- stringi::stri_encode(list(bytes), from = "ISO-8859-1", to = "UTF-8")
    units <- stringi::stri_extract_all_regex(latin,
      "[\\x{81}-\\x{9F}\\x{E0}-\\x{FC}][\\x{40}-\\x{7E}\\x{80}-\\x{FC}]|[\\s\\S]")[[1L]]
    raw_units <- stringi::stri_encode(units, from = "UTF-8", to = "ISO-8859-1", to_raw = TRUE)
    out <- suppressWarnings(stringi::stri_encode(raw_units, from = "Shift_JIS", to = "UTF-8"))
    special <- match(units, c("\x1a", "\x1c", "\x7f", "\u0080"))
    invalid <- (out %in% c("\x1a", "\ufffd")) & is.na(special)
    if (any(invalid) || anyNA(out)) return(NA_character_)
    out[!is.na(special)] <- c("\x1a", "\x1c", "\x7f", "\u0080")[special[!is.na(special)]]
    stringi::stri_join(out, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

appusage_text_eucjp_compat <- function(x) {
  inputs <- if (is.raw(x)) list(x) else as.list(x)
  delta <- appusage_text_eucjp_delta()
  keys <- unlist(lapply(delta, `[[`, "key"), use.names = FALSE)
  codes <- unlist(lapply(delta, function(group) rep(group$code, length(group$key))), use.names = FALSE)
  vapply(inputs, function(value) {
    if (is.character(value) && is.na(value)) return(NA_character_)
    bytes <- if (is.raw(value)) value else charToRaw(value)
    if (!length(bytes)) return("")
    latin <- stringi::stri_encode(list(bytes), from = "ISO-8859-1", to = "UTF-8")
    units <- stringi::stri_extract_all_regex(latin,
      "\\x{8F}[\\x{A1}-\\x{FE}]{2}|\\x{8E}[\\x{A1}-\\x{DF}]|[\\x{A1}-\\x{FE}]{2}|[\\s\\S]")[[1L]]
    raw_units <- stringi::stri_encode(units, from = "UTF-8", to = "ISO-8859-1", to_raw = TRUE)
    key <- vapply(raw_units, function(b) sum(as.integer(b) * 256^(rev(seq_along(b)) - 1L)), numeric(1))
    index <- match(key, keys)
    patch <- which(!is.na(index))
    if (anyNA(codes[index[patch]])) return(NA_character_)
    out <- suppressWarnings(stringi::stri_encode(raw_units, from = "EUC-JP", to = "UTF-8"))
    out[patch] <- stringi::stri_enc_fromutf32(as.list(codes[index[patch]]))
    if (anyNA(out) || any(out == "\ufffd")) return(NA_character_)
    stringi::stri_join(out, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# Compatibility with the frozen Windows R 4.5.3 GB18030 converter. ICU 74 uses
# the newer mapping. This finite patch changes 20 two-byte mappings, one
# four-byte mapping and the legacy '?' reserved ranges; ICU still performs all
# actual transcoding. Byte positions are located by ICU, not a custom decoder.
appusage_text_gb18030_compat <- function(bytes) {
  if (!length(bytes)) return("")
  latin <- stringi::stri_encode(list(bytes), from = "ISO-8859-1", to = "UTF-8")
  locations <- stringi::stri_locate_all_regex(latin,
    "[\\x{81}-\\x{FE}][0-9][\\x{81}-\\x{FE}][0-9]|[\\x{81}-\\x{FE}][\\x{40}-\\x{7E}\\x{80}-\\x{FE}]|[\\x{01}-\\x{7F}]+|[\\s\\S]",
    omit_no_match = TRUE)[[1L]]
  width <- locations[, 2L] - locations[, 1L] + 1L
  first <- as.integer(bytes[locations[, 1L]])
  if (any(width == 1L & first >= 128L)) return(NA_character_)
  two <- which(width == 2L & first >= 129L)
  four <- which(width == 4L & first >= 129L)
  positions <- integer(); codepoints <- integer()
  if (length(two)) {
    code <- first[two] * 256L + as.integer(bytes[locations[two, 1L] + 1L])
    keys <- c(0xa3a0,0xa6d9,0xa6da,0xa6db,0xa6dc,0xa6dd,0xa6de,0xa6df,0xa6ec,0xa6ed,0xa6f3,0xa8bc,0xfe59,0xfe61,0xfe66,0xfe67,0xfe6d,0xfe7e,0xfe90,0xfea0)
    old <- c(0xe5e5,0xe78d,0xe78e,0xe78f,0xe790,0xe791,0xe792,0xe793,0xe794,0xe795,0xe796,0xe7c7,0xe81e,0xe826,0xe82b,0xe82c,0xe832,0xe843,0xe854,0xe864)
    key <- match(code, keys)
    positions <- two[!is.na(key)]; codepoints <- old[key[!is.na(key)]]
  }
  if (length(four)) {
    p <- locations[four, 1L]
    index <- (((first[four] - 129L) * 10 + as.integer(bytes[p + 1L]) - 48L) * 126 +
      as.integer(bytes[p + 2L]) - 129L) * 10 + as.integer(bytes[p + 3L]) - 48L
    remapped <- four[index == 7457]
    positions <- c(positions, remapped); codepoints <- c(codepoints, rep(0x1e3f, length(remapped)))
    reserved <- four[(index >= 39420 & index <= 188999) | index >= 1237576]
    if (length(reserved)) {
      latin <- stringi::stri_sub_replace_all(latin, from = list(locations[reserved, 1L]),
        to = list(locations[reserved, 2L]), replacement = list(rep("?", length(reserved))))
      bytes <- stringi::stri_encode(latin, from = "UTF-8", to = "ISO-8859-1", to_raw = TRUE)[[1L]]
    }
  }
  substituted <- FALSE
  result <- withCallingHandlers(stringi::stri_encode(list(bytes), from = "GB18030", to = "UTF-8"),
    warning = function(w) { substituted <<- TRUE; invokeRestart("muffleWarning") })
  if (substituted) return(NA_character_)
  if (length(positions)) {
    character_end <- cumsum(ifelse(first < 128L, width, 1L))
    order <- order(positions)
    at <- character_end[positions[order]]
    replacement <- stringi::stri_enc_fromutf32(as.list(as.integer(codepoints[order])))
    result <- stringi::stri_sub_replace_all(result, from = list(at), to = list(at), replacement = list(replacement))
  }
  result
}

# Only the explicit normalize_encoding(..., encoding=...) API skips invalid
# sequences. ICU validates every accepted prefix; no hand-written codec or
# deletion of genuine U+FFFD is involved. The normal valid-input path is bulk.
appusage_text_encode_skip <- function(x, from) {
  result <- appusage_text_encode_strict(x, from)
  bad <- which(is.na(result) & !is.na(x))
  for (i in bad) {
    whole <- appusage_text_encode_strict(x[[i]], from)
    if (!is.na(whole)) { result[[i]] <- whole; next }
    bytes <- charToRaw(x[[i]])
    accepted <- raw()
    position <- 1L
    output <- ""
    while (position <= length(bytes)) {
      consumed <- 0L
      for (width in seq_len(min(32L, length(bytes) - position + 1L))) {
        attempt <- c(accepted, bytes[seq.int(position, length.out = width)])
        value <- appusage_text_encode_strict(list(attempt), from)
        if (length(value) && !is.na(value)) {
          accepted <- attempt; output <- value; consumed <- width; break
        }
      }
      position <- position + max(1L, consumed)
    }
    result[[i]] <- output
  }
  result
}
