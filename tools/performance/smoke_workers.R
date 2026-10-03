args<-commandArgs(trailingOnly=TRUE)
stopifnot(length(args)==2L)
lib<-normalizePath(args[[1]],winslash='/');root<-args[[2]]
stopifnot(!dir.exists(root));dir.create(root,recursive=TRUE)
Sys.setenv(R_LIBS_USER=lib,LC_ALL='Chinese_China.utf8',LANG='Chinese_China.utf8')
.libPaths(c(lib,.libPaths()));library(appusageR)
expected_path<-normalizePath(find.package('appusageR'),winslash='/')
stopifnot(expected_path==paste0(lib,'/appusageR'),as.character(packageVersion('appusageR'))=='0.3.5')
expected_parser<-appusageR:::appusage_parser_implementation_fingerprint()
expected_second<-appusageR:::appusage_second_level_implementation_fingerprint()
worker_evidence<-list()
trace('makeCluster',where=asNamespace('parallel'),print=FALSE,exit=quote({
  cluster<-returnValue()
  info<-parallel::clusterCall(cluster,function() {
    library(appusageR)
    list(pid=Sys.getpid(),path=normalizePath(find.package('appusageR'),winslash='/'),
      version=as.character(packageVersion('appusageR')),
      parser=appusageR:::appusage_parser_implementation_fingerprint(),
      second=appusageR:::appusage_second_level_implementation_fingerprint())
  })
  stopifnot(length(info)==2L)
  for (record in info) stopifnot(record$path==get('expected_path',.GlobalEnv),
    record$parser==get('expected_parser',.GlobalEnv),record$second==get('expected_second',.GlobalEnv))
  evidence<-get('worker_evidence',.GlobalEnv)
  assign('worker_evidence',c(evidence,list(info)),.GlobalEnv)
}))
files<-normalizePath(file.path('tests/testthat/fixtures',paste0(c('line','meta','day','app'),'_sample.txt')),winslash='/')
ids<-paste0('synthetic-',1:4)
run<-function(parallel) {
  out<-file.path(root,if(parallel)'parallel' else 'serial')
  first<-read_appusage_batch(files,ids=ids,output_dir=out,project_name='worker-smoke',project_id='synthetic',
    progress=FALSE,parallel=parallel,n_cores=2L,retry_memory_allocation=FALSE)
  stopifnot(all(first$status=='success'))
  second<-write_second_level_batch(first,output_dir=file.path(out,'proclevel-2'),
    progress=FALSE,parallel=parallel,n_cores=2L,inline_qc=TRUE)
  stopifnot(all(second$status=='success'))
  list(first=first,second=second)
}
parallel_output<-run(TRUE)
untrace('makeCluster',where=asNamespace('parallel'))
stopifnot(length(worker_evidence)==2L,
  length(unique(parallel_output$first$worker_pid))==2L,
  length(unique(parallel_output$second$second_level_worker_pid))==2L)
serial_output<-run(FALSE)
load_data<-function(path){e<-new.env(parent=emptyenv());stopifnot(identical(load(path,e),'data'));e$data}
for (stage in c('first','second')) {
  a<-parallel_output[[stage]];b<-serial_output[[stage]]
  index<-match(a$source_record_key,b$source_record_key)
  stopifnot(!anyNA(index),!anyDuplicated(index))
  col<-if(stage=='first') 'data_file' else 'second_level_data_file'
  for(i in seq_len(nrow(a)))stopifnot(identical(load_data(a[[col]][i]),load_data(b[[col]][index[i]])))
}
jsonlite::write_json(list(status='pass',workers=worker_evidence,exact_rda_comparisons=8L),
  file.path(root,'result.json'),auto_unbox=TRUE,pretty=TRUE)
cat('Two real PSOCK workers per stage loaded the expected candidate; 8 RDA comparisons passed\n')
