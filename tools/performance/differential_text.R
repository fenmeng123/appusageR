pkgload::load_all('.', quiet = TRUE)
candidate <- asNamespace('appusageR')
baseline <- new.env(parent = globalenv())
root <- 'E:/mSens_AppUsage/reference/workflow_test/performance_035'
for (path in list.files(file.path(root, 'baseline-source/R'), full.names=TRUE, pattern='[.]R$')) sys.source(path, baseline)
capture <- function(f, args) {
  warnings <- character()
  value <- tryCatch(withCallingHandlers(do.call(f,args), warning=function(w) {
    warnings <<- c(warnings,conditionMessage(w)); invokeRestart('muffleWarning')
  }),error=function(e) list(error_class=class(e),error=TRUE))
  diagnostics <- attr(value, 'encoding_diagnostics')
  if (!is.null(diagnostics)) {
    diagnostics$conversion_failures <- lapply(diagnostics$conversion_failures,function(x) {x$message <- '<backend conversion diagnostic>';x})
    attr(value, 'encoding_diagnostics') <- diagnostics
  }
  list(value=value, warnings=warnings)
}
records <- list()
compare <- function(name,args,label) {
  old <- capture(get(name,baseline),args)
  new <- capture(get(name,candidate),args)
  equal <- identical(old,new)
  records[[length(records)+1L]] <<- data.frame(function_name=name,case=label,identical=equal)
  if (!equal) {
    saveRDS(list(old=old,new=new,args=args),file.path(root,'text_differential_failure.rds'))
    print(all.equal(old,new)); stop(name,': ',label)
  }
}
for (encoding in c('UTF-8','GB18030','GBK','CP936')) {
  bytes <- stringi::stri_encode('中文,AppUsage\r\n第二行',from='UTF-8',to=encoding,to_raw=TRUE)[[1L]]
  variants <- list(valid=bytes, bad=c(bytes,as.raw(255L)), leading_bad=c(as.raw(255L),bytes),
    mixed=c(bytes,charToRaw('Réponse')), bom=c(as.raw(c(239L,187L,191L)),bytes))
  for (label in names(variants)) {
    compare('decode_raw_text',list(bytes=variants[[label]],encoding=encoding),paste(encoding,label))
    text <- rawToChar(variants[[label]])
    compare('normalize_encoding',list(x=c(text,NA_character_,''),encoding=encoding),paste(encoding,label))
  }
}
set.seed(35035)
for (i in seq_len(100L)) {
  bytes <- as.raw(sample(1:255, sample(1:40,1L), TRUE))
  compare('decode_raw_text',list(bytes=bytes),paste('auto random',i))
}
for (kind in c('lower','upper')) {
  x <- c('before\ufeffafter', '\ufeffstart', 'ΟΣ', 'İIıi', 'ß', '\ufb03', NA_character_)
  old <- get(if(kind=='lower') 'tolower' else 'toupper',baseenv())(x)
  new <- get(paste0('appusage_text_',kind),candidate)(x)
  stopifnot(identical(old,new))
}
write.csv(do.call(rbind,records),file.path(root,'text_differential.csv'),row.names=FALSE)
cat(length(records),'exact text differential cases passed\n')
