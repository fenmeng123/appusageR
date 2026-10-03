bytes <- lapply(1:255, as.raw)
for (enc in c("GBK", "CP936", "GB18030")) {
  old <- iconv(vapply(bytes, rawToChar, character(1)), from = enc, to = "UTF-8")
  new <- suppressWarnings(stringi::stri_encode(bytes, from = enc, to = "UTF-8"))
  # Replacement character means a substitution, not a successful strict decode.
  new[stringi::stri_detect_fixed(new, "\ufffd")] <- NA_character_
  diff <- which((is.na(old) != is.na(new)) | (!is.na(old) & !is.na(new) & old != new))
  cat(enc, 'single-byte differences:', paste(sprintf('%02x', diff), collapse = ','), '\n')
}
bytes <- lapply(0:65535, function(i) as.raw(c(i %/% 256L, i %% 256L)))
bytes <- Filter(function(x) !any(x == as.raw(0)), bytes)
text <- vapply(bytes, rawToChar, character(1))
for (enc in c("GBK", "CP936", "GB18030")) {
  old <- iconv(text, from = enc, to = "UTF-8")
  new <- suppressWarnings(stringi::stri_encode(bytes, from = enc, to = "UTF-8"))
  new[stringi::stri_detect_fixed(new, "\ufffd")] <- NA_character_
  diff <- which((is.na(old) != is.na(new)) | (!is.na(old) & !is.na(new) & old != new))
  cat(enc, 'two-byte differences:',length(diff),'common-valid-differences:',sum(!is.na(old[diff]) & !is.na(new[diff])), '\n')
  print(utils::head(data.frame(hex=vapply(bytes[diff], function(x) paste(sprintf('%02x', as.integer(x)),collapse=''),character(1)),old=old[diff],new=new[diff]), 12))
}
