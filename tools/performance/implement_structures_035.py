from pathlib import Path
import re
ROOT=Path(r'E:\mSens_AppUsage\sourcecode')

def read(name): return (ROOT/'R'/name).read_text(encoding='utf-8')
def write(name,text): (ROOT/'R'/name).write_text(text,encoding='utf-8')
def span(text,name):
    start=text.index(name+' <- function(')
    opening=text.index('{',start)
    level=0; quote=None; escape=False; comment=False
    for i in range(opening,len(text)):
        c=text[i]
        if comment:
            if c=='\n': comment=False
            continue
        if quote:
            if escape: escape=False
            elif c=='\\': escape=True
            elif c==quote: quote=None
            continue
        if c=='#': comment=True
        elif c in '\"\'`': quote=c
        elif c=='{': level+=1
        elif c=='}':
            level-=1
            if level==0: return start,i+1
    raise ValueError(name)
def get(text,name):
    a,b=span(text,name); return text[a:b]
def replace(text,name,new):
    a,b=span(text,name); return text[:a]+new+text[b:]

def main():
    text=read('internal_parse.R')
    f=get(text,'find_header_rows')
    text=replace(text,'find_header_rows','find_header_rows <- function(mat, required_patterns) {\n  which(appusage_context_match(mat, required_patterns))\n}')
    text=text.replace('  if (row_index <= 1) {','  if (inherits(mat, "appusage_parse_context")) return(appusage_context_previous_date(mat, row_index))\n  if (row_index <= 1) {',1)
    text=text.replace('  text <- paste(mat, collapse = "\\n")','  text <- if (inherits(mat, "appusage_parse_context")) paste(mat$store$values, collapse = "\\n") else paste(mat, collapse = "\\n")',1)
    write('internal_parse.R',text)
    for file,names in [('parse_line_meta.R',['line','meta']),('parse_day_app.R',['day','app'])]:
        text=read(file)
        for name in names:
            fn='parse_'+name
            old=get(text,fn)
            signature=old[:old.index('{')]
            body=old.replace(fn+' <- function','appusage_'+fn+'_context <- function',1)
            body=body.replace('  lines <- read_appusage_lines(x, input = input, encoding = encoding)\n  mat <- as_text_matrix(lines)','  mat <- appusage_context_input(x, input, encoding)\n  lines <- mat$store$lines')
            body=body.replace('  marker_text <- apply(mat, 1, paste, collapse = " ")','  marker_text <- appusage_context_row_text(mat)')
            wrapper=signature+'{\n  input <- match.arg(input)\n  source_file <- source_file %||% source_file_label(x, input)\n  appusage_'+fn+'_context(\n    appusage_context_input(x, input, encoding), input = "lines",\n    participant_id = participant_id, source_file = source_file, tz = tz,\n    encoding = encoding, strict = strict\n  )\n}\n\n'+body
            text=replace(text,fn,wrapper)
        for fn in (['parse_line_block','parse_meta_summary_pair','parse_meta_events_pair'] if file=='parse_line_meta.R' else ['parse_day_block','parse_app_block']):
            old=get(text,fn)
            prefix=old[:old.index('  rows <- vector("list", nrow(data))')]
            pattern=re.search(r'row_contains_any\(row, (c\(.*?\))\)',old,re.S)[1]
            prefix+='  keep <- appusage_context_valid(data) & !appusage_context_match(data, '+pattern+', all = FALSE)\n  data <- data[which(keep), , drop = FALSE]\n  if (!nrow(data)) return(NULL)\n'
            if fn=='parse_line_block':
                frame=old[old.index('    values <- list('):old.index('    if (all(is.na')]
                frame=frame.replace('    values <- list(', '  values <- list(')
                frame=frame.replace('extract_row_date(row, date)','appusage_context_dates(data, date)')
                frame=frame.replace('first_present(row,','appusage_context_column(data,')
                frame+='  keep <- Reduce(`|`, lapply(values[c("app_name", "package_name", "start_ts_ms", "end_ts_ms")], function(x) !is.na(x)))\n  if (!any(keep)) return(NULL)\n  values <- lapply(values, `[`, keep)\n  values$parse_warning <- rep(NA_character_, sum(keep))\n  as.data.frame(values, stringsAsFactors = FALSE)\n}'
            else:
                start=old.index('rows[[i]] <- data.frame(')+len('rows[[i]] <- ')
                end=old.index('\n    )',start)+len('\n    )')
                frame='  '+old[start:end].replace('first_present(row,','appusage_context_column(data,').replace('extract_row_date(row, date)','appusage_context_dates(data, date)')+'\n}'
            text=replace(text,fn,prefix+frame)
        write(file,text)
    text=read('source_preflight.R')
    text=text.replace('  values <- as.integer(bytes)','  values <- as.integer(utils::head(bytes, 8L))',1)
    text=replace(text,'appusage_byte_ratios','''appusage_byte_ratios <- function(bytes) {
  n <- length(bytes)
  if (!n) return(list(nul_byte_ratio = 0, control_byte_ratio = 0))
  nul <- control <- 0
  for (start in seq.int(1, n, by = 1048576)) {
    values <- as.integer(bytes[seq.int(start, min(n, start + 1048575))])
    nul <- nul + sum(values == 0L)
    control <- control + sum(values < 32L & !values %in% c(9L, 10L, 13L))
  }
  list(nul_byte_ratio = nul / n, control_byte_ratio = control / n)
}''')
    text=replace(text,'appusage_record_candidate_rows','''appusage_record_candidate_rows <- function(lines) {
  appusage_context_records(appusage_parse_context(lines))
}''')
    candidate=Path(r'E:\mSens_AppUsage\reference\workflow_test\performance_audit_20260929\parser_candidates.R').read_text(encoding='utf-8')
    f=get(candidate,'perf_structural_boundaries_vectorized').replace('perf_structural_boundaries_vectorized','appusage_structural_boundaries',1)
    f=f.replace('  n_rows <- nrow(mat)','  if (inherits(mat, "appusage_parse_context") && !is.null(mat$store$cache$boundaries)) return(mat$store$cache$boundaries)\n  n_rows <- nrow(mat)',1)
    f=f.replace('  text <- perf_matrix_row_text(mat)','  text <- appusage_context_row_text(mat)')
    f=f.replace('hits <- hits & stringr::str_detect(text, pattern)','hits <- hits & appusage_context_hits(mat, pattern)')
    start=f.index('  first <- rep('); end=f.index('  first <- trimws(first)',start)
    old=f[start:end]
    f=f[:start]+'''  if (inherits(mat, "appusage_parse_context")) {
    s <- mat$store
    row <- rep.int(seq_along(s$sizes), s$sizes)
    present <- which(!is.na(s$values))
    first_pos <- present[!duplicated(row[present])]
    first <- rep(NA_character_, length(s$sizes))
    first[row[first_pos]] <- s$values[first_pos]
    first <- first[mat$rows]
  } else {
'''+old+'  }\n'+f[end:]
    f=f.replace('  data.frame(\n    row = as.integer(locations', '  out <- data.frame(\n    row = as.integer(locations')
    f=f[:-1]+'  if (inherits(mat, "appusage_parse_context")) mat$store$cache$boundaries <- out\n  out\n}'
    text=replace(text,'appusage_structural_boundaries',f)
    f=get(text,'appusage_component_boundary_diagnostics')
    f=f.replace('  mat <- as_text_matrix(lines)','  mat <- appusage_parse_context(lines)\n  all_candidates <- appusage_context_records(mat)')
    start=f.index('        section <- apply('); end=f.index('        if (length(local)',start)
    f=f[:start]+'        local <- all_candidates[all_candidates >= start & all_candidates <= end] - start + 1L\n'+f[end:]
    text=replace(text,'appusage_component_boundary_diagnostics',f)
    text=text.replace('control_ratio_threshold = 0.20) {','control_ratio_threshold = 0.20, .context_sink = NULL) {',1)
    text=text.replace('  components <- appusage_detect_components_from_lines(lines)\n  candidate_rows <- appusage_record_candidate_rows(lines)\n  boundary_diagnostics <- appusage_component_boundary_diagnostics(lines, components)','  context <- appusage_parse_context(lines)\n  if (!is.null(.context_sink)) .context_sink$context <- context\n  components <- appusage_detect_components_from_lines(context)\n  candidate_rows <- appusage_record_candidate_rows(context)\n  boundary_diagnostics <- appusage_component_boundary_diagnostics(context, components)')
    write('source_preflight.R',text)
    text=read('detect.R').replace('  mat <- as_text_matrix(lines)\n  text <- paste(mat, collapse = "\\n")','  mat <- appusage_parse_context(lines)\n  text <- paste(mat$store$values, collapse = "\\n")')
    start=text.index('  is_day <- any(vapply('); end=text.index('  if (is_day)',start)
    text=text[:start]+'''  first <- appusage_context_column(mat, 1L)
  is_day <- any(!is.na(first) & nzchar(first) & !is.na(safe_as_date(first)) &
    appusage_context_match(mat, day_tokens), na.rm = TRUE)
'''+text[end:]
    write('detect.R',text)
    for file in ['batch.R','api_wrappers.R']:
        text=read(file)
        # Only actual workflow preflight sites; standalone preflight stays pure.
        text=text.replace('        preflight <- appusage_source_preflight(', '        prepared <- appusage_prepare_source(')
        text=text.replace('        if (!identical(preflight$status, "ok")) {','        preflight <- prepared$preflight\n        if (!identical(preflight$status, "ok")) {')
        text=text.replace('        parse_x <- preflight$lines','        parse_x <- prepared$context')
        for name in ['line','meta','day','app']:
            if file=='batch.R': text=text.replace(name+' = parse_'+name+'(',name+' = appusage_parse_'+name+'_context(')
        write(file,text)
    text=read('api_wrappers.R')
    # Dispatch helper can receive private context; use context parsers directly.
    start=text.index('parse_first_level_by_type <- function')
    a,b=span(text,'parse_first_level_by_type')
    f=text[a:b]
    for name in ['line','meta','day','app']: f=f.replace('parse_'+name+'(', 'appusage_parse_'+name+'_context(')
    text=text[:a]+f+text[b:]
    write('api_wrappers.R',text)

if __name__=='__main__': main()
