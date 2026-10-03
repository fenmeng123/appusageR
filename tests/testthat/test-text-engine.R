test_that("ICU splitting preserves the base terminal-field contract", {
  x <- c("", ",", "a,", "a,,", ",a", "a,b", NA_character_)
  expect_identical(appusage_text_split(x, ",", fixed = TRUE), strsplit(x, ",", fixed = TRUE))
  expect_identical(appusage_text_split(c("\n", "a\n\n"), "\n", fixed = TRUE), strsplit(c("\n", "a\n\n"), "\n", fixed = TRUE))
})

test_that("ICU base adapters preserve missing values, captures and recycling", {
  for (a in list(character(), c("a", NA), 1:3, as.Date("2024-01-01"))) {
    for (b in list(NULL, "x", c("", "b"))) {
      expect_identical(appusage_text_paste(a, b), paste(a, b))
      expect_identical(appusage_text_paste0(a, b, collapse = ";"), paste0(a, b, collapse = ";"))
    }
  }
  x <- c("a12", "b34", NA_character_, "a")
  expect_identical(appusage_text_sub("([a-z])([0-9]+)", "\\2-\\1", x), sub("([a-z])([0-9]+)", "\\2-\\1", x))
  expect_identical(appusage_text_grepl("a", x), grepl("a", x))
  expect_identical(appusage_text_nzchar(x), nzchar(x))
  expect_identical(appusage_text_nchar(x), nchar(x))
  x <- c(" \tfoo\r\n", "\u00a0foo\u00a0", NA_character_)
  expect_identical(appusage_text_trim(x), trimws(x))
  expect_identical(appusage_text_regmatches("ab12", appusage_text_regexec("([a-z]+)([0-9]+)", "ab12")), regmatches("ab12", regexec("([a-z]+)([0-9]+)", "ab12")))
})

test_that("ICU strict conversion does not accept substituted malformed input", {
  for (encoding in c("UTF-8", "GB18030", "GBK", "CP936")) {
    bytes <- stringi::stri_encode("\u4e2d\u6587", from = "UTF-8", to = encoding, to_raw = TRUE)[[1L]]
    expect_identical(appusage_text_encode_strict(list(bytes), encoding), iconv(rawToChar(bytes), from = encoding, to = "UTF-8"))
  }
  expect_true(is.na(appusage_text_encode_strict(list(as.raw(c(0xC3, 0x28))), "UTF-8")))
})

test_that("explicit invalid-sequence skipping preserves genuine U+FFFD", {
  for (encoding in c("UTF-8", "GB18030", "GBK", "CP936")) {
    bytes <- stringi::stri_encode("\u4e2d\u6587", from = "UTF-8", to = encoding, to_raw = TRUE)[[1L]]
    bad <- rawToChar(c(bytes, as.raw(0xFF), charToRaw("tail")))
    expect_identical(appusage_text_encode_skip(c(bad, "valid", NA_character_), encoding), iconv(c(bad, "valid", NA_character_), from = encoding, to = "UTF-8", sub = ""))
  }
  value <- rawToChar(c(charToRaw("\ufffd"), as.raw(0xFF), charToRaw("x")))
  expect_identical(appusage_text_encode_skip(value, "UTF-8"), iconv(value, from = "UTF-8", to = "UTF-8", sub = ""))
})

test_that("GB18030 preserves frozen mappings and legacy reserved-range behavior", {
  sequences <- list(c(0xa3,0xa0),c(0xa6,0xd9),c(0xa8,0xbc),c(0xfe,0xa0),
    c(0x81,0x35,0xf4,0x37),c(0x84,0x31,0xa5,0x30),c(0xfe,0x39,0xfe,0x39),0x80)
  for (seq in sequences) {
    bytes <- as.raw(seq)
    expect_identical(appusage_text_encode_strict(list(bytes), "GB18030"), iconv(rawToChar(bytes), from = "GB18030", to = "UTF-8"))
    combined <- c(charToRaw("before"), bytes, charToRaw("after"))
    expect_identical(appusage_text_encode_strict(list(combined), "GB18030"), iconv(rawToChar(combined), from = "GB18030", to = "UTF-8"))
  }
})

test_that("base case contracts remain distinct from full Unicode normalization", {
  x <- setNames(c("Straße", "İIıi", "ΟΣ", "\ufb03", "AppUsage_中文.TXT", "", NA_character_,
    "before\ufeffafter", "\ufeffstart"), letters[1:9])
  expect_identical(appusage_text_lower(x), tolower(x))
  expect_identical(appusage_text_upper(x), toupper(x))
})

test_that("auto detector legacy encodings reject undefined bytes and preserve values", {
  cases <- list(
    list('windows-1253', c(0x9f)), list('Shift_JIS',c(0x1a,0x1c,0x7f,0x80)),
    list('Big5',0xff),list('EUC-JP',c(0xa2,0xaf)),list('EUC-JP',c(0x8f,0xa1,0xa1)),
    list('EUC-KR',c(0x81,0x41)))
  for (case in cases) {
    bytes <- as.raw(case[[2L]])
    expect_identical(appusage_text_encode_strict(list(bytes),case[[1L]]),
      iconv(rawToChar(bytes),from=case[[1L]],to='UTF-8'))
  }
})

test_that("upload URL decoding and escaping retain filename bytes", {
  x <- c("plain.txt", "a+b.txt", "a%20b.txt", "%E4%B8%AD.txt", "a%2fb%5Cc", "%25%32%30", NA_character_)
  expect_identical(appusage_text_url_decode(x), utils::URLdecode(x))
  special <- c("a[b](c){d}+*^$|\\?.", "normal")
  old <- gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", special)
  new <- appusage_text_gsub("([\\[\\]{}()+*^$|\\\\?.])", "\\\\\\1", special)
  expect_identical(new, old)
  expect_true(all(stringi::stri_detect_regex(special, new)))
})
