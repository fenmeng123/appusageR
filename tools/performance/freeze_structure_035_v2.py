"""Refresh structural attribution without migrating the original text engines."""
from pathlib import Path
import hashlib,json,re,shutil
root=Path(r'E:\mSens_AppUsage');package=root/'sourcecode'
work=root/'reference/workflow_test/performance_035'
target=work/'structure-source-v3'
if target.exists(): raise SystemExit('Refusing to overwrite a frozen structure snapshot')
shutil.copytree(work/'structure-source',target)
def read(path): return path.read_text(encoding='utf-8-sig')
def save(name,text): (target/'R'/name).write_text(text,encoding='utf-8')
def block(text,name):
    start=text.index(name+' <- function')
    next_function=re.search(r'^\w+ <- function',text[start+len(name)+1:],re.M)
    end=start+len(name)+1+next_function.start() if next_function else len(text)
    return text[start:end]
def restore_text(text):
    for a,b in {'appusage_text_paste0':'paste0','appusage_text_paste':'paste',
        'appusage_text_nzchar':'nzchar','appusage_text_trim':'trimws',
        'appusage_text_detect':'stringr::str_detect','stringi::stri_extract_first_regex':'stringr::str_extract'}.items():
        text=text.replace(a,b)
    return text
save('timezone.R',restore_text(read(package/'R/timezone.R')))
name='utils.R';text=read(target/'R'/name);at=text.index('ms_to_datetime <- function')
save(name,text[:at]+read(package/'R'/name)[read(package/'R'/name).index('ms_to_datetime <- function'):])
name='source_anomaly_qc.R';text=read(target/'R'/name)
text=text.replace(block(text,'appusage_source_qc_interval_segments'),block(read(package/'R'/name),'appusage_source_qc_interval_segments'))
save(name,text)
name='second_level.R';text=read(target/'R'/name);new=read(package/'R'/name)
for before,after in [('  needed <- intersect(c("start_ts_ms"','  segmentation <- attr(x'),('  needed <- c("start_ts_ms"','  segmentation <- attr(meta')]:
    piece=new[new.index(before):new.index(after,new.index(before))]
    old='  x <- appusage_interval_segments(x, tz = tz)\n' if 'intersect' in before else '  meta <- appusage_interval_segments(meta, tz = tz)\n'
    assert text.count(old)==1
    text=text.replace(old,piece)
save(name,text)
name='parse_context.R';text=read(target/'R'/name);new=read(package/'R'/name)
text=text.replace('cells <- trimws(s$values)','cells <- s$values')
text=text.replace(block(text,'appusage_context_dates'),restore_text(block(new,'appusage_context_dates')))
text+='\n'+restore_text(block(new,'appusage_context_all_text'))
save(name,text)
name='internal_parse.R';text=read(target/'R'/name);new=read(package/'R'/name)
text=text.replace(block(text,'header_position'),restore_text(block(new,'header_position')))
text=re.sub(r'  text <- if \(inherits\(mat, "appusage_parse_context"\)\).*\n','  text <- appusage_context_all_text(mat)\n',text)
save(name,text)
name='parse_line_meta.R';text=read(target/'R'/name);new=read(package/'R'/name)
start=text.index('  marker_text <-');end=text.index('  diagnostics <-',start)
ns=new.index('  boundaries <- appusage_structural_boundaries(mat)',new.index('appusage_parse_meta_context <-'))
ne=new.index('  diagnostics <-',ns)
save(name,text[:start]+new[ns:ne]+text[end:])
name='provenance.R';text=read(target/'R'/name)
text=text.replace('"appusage_parse_day_context", "appusage_parse_app_context"',
  '"appusage_parse_day_context", "appusage_parse_app_context", "appusage_context_all_text", "appusage_context_dates", "header_position"')
text=text.replace('"appusage_timezone_names"','"appusage_timezone_names", "appusage_daily_segment_index", "appusage_interval_calendar", "appusage_date_from_datetime_validated", "ms_to_datetime_validated"')
save(name,text)
entries={p.relative_to(target).as_posix():hashlib.sha256(p.read_bytes()).hexdigest() for p in target.rglob('*') if p.is_file()}
(work/'structure-source-v3-manifest.json').write_text(json.dumps(entries,indent=2),encoding='utf-8')
print('Frozen updated structural attribution snapshot:',len(entries),'files; original text engines retained')
