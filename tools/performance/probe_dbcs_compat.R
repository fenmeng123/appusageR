Sys.setlocale('LC_ALL','Chinese_China.utf8')
raws <- c(lapply(1:255,as.raw),lapply(seq_len(255L*255L),function(i) as.raw(c((i-1L)%/%255L+1L,(i-1L)%%255L+1L))))
text <- vapply(raws,rawToChar,character(1))
for (encoding in c('Shift_JIS','Big5','EUC-JP','EUC-KR')) {
  old <- iconv(text,from=encoding,to='UTF-8')
  new <- suppressWarnings(stringi::stri_encode(raws,from=encoding,to='UTF-8'))
  no_new <- new == '\ufffd' | (encoding=='Shift_JIS' & new=='\x1a')
  one <- seq_along(text)<=255L | stringi::stri_length(old)==1L | (stringi::stri_length(new)==1L & !no_new)
  changed <- !is.na(old) & (is.na(new) | old!=new) | is.na(old) & !no_new & !is.na(new)
  keep <- which(one & changed)
  saveRDS(list(encoding=encoding,bytes=raws[keep],old=old[keep],new=new[keep]),file.path('E:/mSens_AppUsage/reference/workflow_test/performance_035',paste0('codec-',gsub('[^A-Za-z]','',encoding),'.rds')))
  cat(encoding,length(keep),'deltas; ',sum(is.na(old[keep])),'rejected\n')
}
