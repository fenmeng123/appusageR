"""Preserve the original comparator; specialize latest comparisons transparently."""
from pathlib import Path
import datetime
import json
import psutil

tools=Path(__file__).resolve().parent
work=tools.parents[2]/'reference/workflow_test/performance_035'
owned=[]
for p in psutil.process_iter(['cmdline','name']):
    cmd=p.info['cmdline'] or []
    if (p.info['name'] or '').lower()=='python.exe' and any(
        Path(c).name=='compare_latest_035.py' for c in cmd):
        owned=[p]+p.children(recursive=True)
        break
record=dict(reason='Add explicitly enumerated host measurement fields and avoid scalar recursion on JSON arrays',
            stopped_at=datetime.datetime.now().astimezone().isoformat(),pids=[p.pid for p in owned])
(work/'latest-only/comparison-harness-revision.json').write_text(json.dumps(record,indent=2),encoding='utf-8')
for p in owned:
    try: p.terminate()
    except psutil.NoSuchProcess: pass
psutil.wait_procs(owned,timeout=5)
s=(tools/'compare_runs.R').read_text(encoding='utf-8-sig')
old="(parent=='first_level_worker_decision' && key %in% c('detected_free_memory_bytes'))"
new="(parent=='first_level_worker_decision' && key %in% c('detected_free_memory_bytes',\n      'detected_total_memory_bytes','memory_detected','memory_source'))"
assert s.count(old)==1
s=s.replace(old,new)
old="""    for (i in seq_along(x)) x[i] <- list(normalize(x[[i]],root,kind,paste0(path,'/',nm[[i]]),nm[[i]],key))"""
new="""    # An unnamed JSON array of plain scalars has no named runtime fields or
    # attributes to normalize. Keep it intact, preserving types and order.
    plain <- is.null(names(x)) && is.null(attributes(x)) &&
      all(vapply(x, function(z) is.atomic(z) && is.null(attributes(z)), logical(1)))
    if (plain) return(x)
    old_attributes <- attributes(x)
    values <- lapply(seq_along(x), function(i) {
      normalize(x[[i]],root,kind,paste0(path,'/',nm[[i]]),nm[[i]],key)
    })
    attributes(values) <- old_attributes
    x <- values"""
assert s.count(old)==1
s=s.replace(old,new)
# Prove that array identity, including scalar type and order, remains strict.
marker="rows <- list()"
assert s.count(marker)==1
s=s.replace(marker,"""array <- list(1L, 2L, NA_integer_)
stopifnot(identical(normalize(array,old_root,'json'),array),
  !identical(normalize(array,old_root,'json'),normalize(rev(array),new_root,'json')),
  !identical(normalize(array,old_root,'json'),normalize(list(1,2,NA_real_),new_root,'json')))
"""+marker)
(tools/'compare_latest_runs.R').write_text(s,encoding='utf-8')
p=tools/'compare_latest_035.py'
s=p.read_text(encoding='utf-8').replace("out = latest / 'comparisons'", "out = latest / 'comparisons-final'").replace("with_name('compare_runs.R')", "with_name('compare_latest_runs.R')")
p.write_text(s,encoding='utf-8')
p=tools/'publish_latest_report_035.py'
s=p.read_text(encoding='utf-8').replace("'comparisons/status.json'", "'comparisons-final/status.json'").replace("root/'comparisons'/", "root/'comparisons-final'/")
p.write_text(s,encoding='utf-8')
print('Preserved original comparisons; prepared scoped final comparator')
