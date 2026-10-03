# Strict artifact comparator. Use fresh processes and private output roots.
args <- commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==3L)
old_root <- normalizePath(args[[1]],winslash='/',mustWork=TRUE)
new_root <- normalizePath(args[[2]],winslash='/',mustWork=TRUE)
report <- args[[3]]
ignored <- character()
runtime_columns <- c('package_version','parser_version','provenance_package_version',
  'workflow_run_id','git_commit_sha','git_build_marker','provenance_git_commit_sha',
  'provenance_git_build_marker','parser_implementation_fingerprint','second_level_implementation_fingerprint',
  'started_at','finished_at','elapsed_sec','worker_pid','second_level_worker_pid',
  'second_level_total_elapsed_sec','second_level_load_elapsed_sec','second_level_convert_elapsed_sec',
  'second_level_save_elapsed_sec','second_level_inline_qc_elapsed_sec',
  'first_level_rda_size_bytes','second_level_rda_size_bytes')
timing_keys <- c('started_at','finished_at','elapsed_sec','second_level_started_at','second_level_finished_at',
  'second_level_elapsed_sec','qc_started_at','qc_finished_at','qc_elapsed_sec',
  'total_elapsed_sec','load_elapsed_sec','convert_elapsed_sec','save_elapsed_sec',
  'inline_qc_elapsed_sec','metadata_json_write_elapsed_sec','worker_pid',
  'first_level_rda_size_bytes','second_level_rda_size_bytes')
provenance_keys <- c('package_version','workflow_run_id','git_commit_sha','git_build_marker',
  'parser_implementation_fingerprint','second_level_implementation_fingerprint','created_at')
path_keys <- c('metadata_file','data_file','project_root','proclevel_1_dir','proclevel_2_dir',
  'metadata_json','first_level_rda','second_level_rda','first_level_data_file','second_level_data_file',
  'first_level_metadata_json','second_level_metadata_json','second_level_metadata_file',
  'latest_summary_file','summary_file','output_dir','qc_metadata_source')
root_path <- function(x,root) {
  if (!is.character(x)) return(x)
  slash <- stringi::stri_replace_all_fixed(x,'\\','/')
  hit <- !is.na(slash) & (slash==root | stringi::stri_startswith_fixed(slash,paste0(root,'/')))
  x[hit] <- paste0('<OUTPUT_ROOT>',stringi::stri_sub(slash[hit],nchar(root)+1L))
  x
}
normalize <- function(x,root,kind,path='',key='',parent='') {
  ignore <- (kind=='summary' && path %in% paste0('/',runtime_columns)) ||
    (kind=='json' && path %in% c('/package_version','/parser_version','/created_at','/updated_at')) ||
    (parent %in% c('implementation_provenance','upstream_implementation_provenance') && key %in% provenance_keys) ||
    (parent %in% c('processing','profiling') && key %in% timing_keys) ||
    (parent %in% c('source_anomaly_qc','anomaly_qc') && key=='created_at') ||
    (parent=='qc' && key=='qc_created_at') ||
    (parent=='first_level_worker_decision' && key %in% c('detected_free_memory_bytes'))
  if (ignore) {
    ignored <<- unique(c(ignored,paste(kind,path)))
    return('<ALLOWED_RUNTIME_FIELD>')
  }
  # Only backend conversion wording is normalized; candidates, failure classes,
  # fallback status, counts and selected encodings remain strict.
  if (grepl('/conversion_failures/[0-9]+/message$',path)) {
    ignored <<- unique(c(ignored,paste(kind,path)))
    return('<BACKEND_CONVERSION_MESSAGE>')
  }
  if (is.list(x)) {
    nm <- if (is.null(names(x))) as.character(seq_along(x)) else names(x)
    for (i in seq_along(x)) x[i] <- list(normalize(x[[i]],root,kind,paste0(path,'/',nm[[i]]),nm[[i]],key))
  } else if (key %in% path_keys || parent %in% c('directories','outputs')) {
    x <- root_path(x,root)
  }
  a <- attributes(x)
  if (!is.null(a)) {
    for (name in setdiff(names(a),c('class','names','row.names','dim','dimnames','tzone','levels'))) {
      a[[name]] <- normalize(a[[name]],root,kind,paste0(path,'/@',name),name,'@attributes')
    }
    attributes(x) <- a
  }
  x
}
# Comparator sensitivity checks: these fields must never be normalized away.
probe <- list(duration_ms=c(1,2),source_fingerprint='source-a',
  source_qc_config_fingerprint='config-a',schema_version='0.3.4',reason='original')
for (key in names(probe)) {
  changed <- probe
  changed[[key]] <- if(key=='duration_ms') rev(changed[[key]]) else 'different'
  stopifnot(!identical(normalize(probe,old_root,'json'),normalize(changed,new_root,'json')))
}
typed <- data.frame(duration_ms=c(1,2))
changed <- typed;changed$duration_ms<-as.integer(changed$duration_ms)
stopifnot(!identical(normalize(typed,old_root,'rda'),normalize(changed,new_root,'rda')))
rows <- list()
check <- function(a,b,label,kind) {
  a <- normalize(a,old_root,kind)
  b <- normalize(b,new_root,kind)
  equal <- identical(a,b)
  detail <- if(equal) '' else paste(all.equal(a,b,tolerance=0,check.attributes=TRUE),collapse='; ')
  rows[[length(rows)+1L]] <<- data.frame(artifact=label,identical=equal,detail=detail)
}
load_data <- function(path) {
  env <- new.env(parent=emptyenv())
  stopifnot(identical(load(path,env),'data'))
  env$data
}
result_old <- jsonlite::read_json(file.path(old_root,'result.json'),simplifyVector=TRUE)
result_new <- jsonlite::read_json(file.path(new_root,'result.json'),simplifyVector=TRUE)
stopifnot(result_old$status=='success',result_new$status=='success',
  identical(result_old$id,result_new$id),identical(result_old$source_md5,result_new$source_md5),
  identical(result_old$source_bytes,result_new$source_bytes),
  identical(result_old$locale,result_new$locale),identical(result_old$timezone,result_new$timezone))
first_old <- readRDS(file.path(old_root,'private_first_summary.rds'))
first_new <- readRDS(file.path(new_root,'private_first_summary.rds'))
second_old <- readRDS(file.path(old_root,'private_second_summary.rds'))
second_new <- readRDS(file.path(new_root,'private_second_summary.rds'))
for (pair in list(list(first_old,first_new),list(second_old,second_new))) {
  stopifnot(nrow(pair[[1]])==1L,nrow(pair[[2]])==1L,
    identical(pair[[1]]$source_record_key,pair[[2]]$source_record_key),
    identical(pair[[1]]$source_fingerprint,pair[[2]]$source_fingerprint),
    identical(pair[[1]]$source_qc_config_fingerprint,pair[[2]]$source_qc_config_fingerprint))
}
stopifnot(first_old$status=='success',first_new$status=='success',second_old$status=='success',second_new$status=='success')
check(first_old,first_new,'proc-1 summary RDS','summary')
check(second_old,second_new,'proc-2 summary RDS','summary')
check(load_data(first_old$data_file),load_data(first_new$data_file),'complete proc-1 data','rda')
check(load_data(second_old$second_level_data_file),load_data(second_new$second_level_data_file),'complete proc-2 data','rda')
for (ext in c('json','csv')) {
  old_files <- list.files(old_root,pattern=paste0('[.]',ext,'$'),recursive=TRUE)
  new_files <- list.files(new_root,pattern=paste0('[.]',ext,'$'),recursive=TRUE)
  old_files <- setdiff(old_files,'result.json');new_files <- setdiff(new_files,'result.json')
  stopifnot(identical(old_files,new_files))
  for (i in seq_along(old_files)) {
    read <- if(ext=='json') jsonlite::read_json else function(p) read.csv(p,check.names=FALSE,stringsAsFactors=FALSE,fileEncoding='UTF-8')
    check(read(file.path(old_root,old_files[i])),read(file.path(new_root,new_files[i])),paste(ext,i),if(ext=='json')'json' else 'summary')
  }
}
dir.create(dirname(report),recursive=TRUE,showWarnings=FALSE)
write.csv(do.call(rbind,rows),report,row.names=FALSE,fileEncoding='UTF-8')
writeLines(sort(ignored),paste0(report,'.normalization.txt'))
cat(sum(vapply(rows,function(x)x$identical,logical(1))),'/',length(rows),'artifacts exactly equal\n')
if (!all(vapply(rows,function(x)x$identical,logical(1)))) quit(status=1L)
