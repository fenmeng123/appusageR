# Synthetic exhaustive mapping audit; no participant/raw input is read.
out <- list()
for (base in seq.int(0, 1587599, by = 12600)) {
  index <- base + 0:12599
  bytes <- lapply(index, function(i) as.raw(c(129L + i %/% 12600L,
    48L + (i %/% 1260L) %% 10L, 129L + (i %/% 10L) %% 126L, 48L + i %% 10L)))
  old <- iconv(vapply(bytes, rawToChar, character(1)), from = "GB18030", to = "UTF-8")
  new <- suppressWarnings(stringi::stri_encode(bytes, from = "GB18030", to = "UTF-8"))
  substituted <- stringi::stri_detect_fixed(new, "\ufffd") & (is.na(old) | old != "\ufffd")
  new[substituted] <- NA_character_
  diff <- which((is.na(old) != is.na(new)) | (!is.na(old) & !is.na(new) & old != new))
  if (length(diff)) out[[length(out) + 1L]] <- data.frame(
    index = index[diff], hex = vapply(bytes[diff], function(x) paste(sprintf("%02x", as.integer(x)), collapse = ""), character(1)),
    old = vapply(old[diff], function(x) if (is.na(x)) NA_character_ else paste(sprintf("%04X", utf8ToInt(x)), collapse = ","), character(1)),
    new = vapply(new[diff], function(x) if (is.na(x)) NA_character_ else paste(sprintf("%04X", utf8ToInt(x)), collapse = ","), character(1)))
}
result <- do.call(rbind, out)
write.csv(result, "E:/mSens_AppUsage/reference/workflow_test/performance_035/gb18030_mapping_differences.csv", row.names = FALSE)
cat(nrow(result), "mapping differences saved; do not print exhaustive rows\n")
