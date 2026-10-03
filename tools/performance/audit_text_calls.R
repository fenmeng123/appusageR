forbidden <- c('paste','paste0','grep','grepl','sub','gsub','strsplit','substr','substring',
  'nchar','nzchar','trimws','tolower','toupper','chartr','startsWith','endsWith',
  'regexpr','gregexpr','regexec','regmatches','iconv','enc2utf8','enc2native','URLdecode')
rows <- list()
for(file in list.files('R',pattern='[.]R$',full.names=TRUE)) {
  parsed <- parse(file,keep.source=TRUE)
  tokens <- getParseData(parsed)
  calls <- tokens[tokens$token=='SYMBOL_FUNCTION_CALL',c('line1','text')]
  bad <- calls[calls$text %in% forbidden,,drop=FALSE]
  if(nrow(bad)) rows[[length(rows)+1L]] <- data.frame(file=file,bad)
  stopifnot(!any(tokens$text=='stringr' & tokens$token=='SYMBOL_PACKAGE'))
}
if(length(rows)) {print(do.call(rbind,rows));stop('Unmigrated production text calls')}
cat('Production AST audit: no old text-processing calls or stringr namespaces\n')
cat('iconvlist is retained solely as platform capability metadata, not conversion\n')
