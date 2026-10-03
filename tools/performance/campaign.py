"""Serial campaign; consumes reviewed, frozen library and harness paths.

No package scheduling changes. No wall-clock limit. All real-source paths stay
in the private manifest. Run --dry-run first to freeze/inspect the schedule.
"""
import argparse, csv, hashlib, json, os, pathlib, statistics, subprocess, sys, time
import psutil

R = r'D:\Program Files\R\R-4.5.3\bin\Rscript.exe'
IDS = ['raw01', 'raw02', 'raw03', 'raw04', 'raw05']
p = argparse.ArgumentParser()
p.add_argument('--config', required=True)
p.add_argument('--dry-run', action='store_true')
a = p.parse_args()
config = json.loads(pathlib.Path(a.config).read_text(encoding='utf-8'))
assert len({str(pathlib.Path(v).resolve()) for v in config['libraries'].values()}) == 3
root = pathlib.Path(config['root'])
root.mkdir(parents=True, exist_ok=True)
harness = pathlib.Path(__file__).parent
schedule = []
for repeat in range(1, 4):
    for index, bid in enumerate(IDS):
        roles = ['baseline', 'candidate'] if (repeat + index) % 2 else ['candidate', 'baseline']
        for role in roles:
            schedule.append(dict(repeat=repeat, id=bid, role=role,
                output=str(root/'formal'/f'repeat-{repeat}'/role)))
assert len(schedule) == 30
assert all(sum(x['id']==bid and x['role']==role for x in schedule)==3
           for bid in IDS for role in ['baseline','candidate'])
schedule_file = root/'formal-schedule.json'
if schedule_file.exists():
    assert json.loads(schedule_file.read_text(encoding='utf-8')) == schedule
else:
    schedule_file.write_text(json.dumps(schedule,indent=2),encoding='utf-8')
if a.dry_run:
    print('Frozen serial schedule: 30 runs; 3 repeats per file/version; alternating pair order')
    raise SystemExit(0)

env = os.environ.copy()
env.update(LC_ALL='Chinese_China.utf8', LANG='Chinese_China.utf8',
           R_USER_CACHE_DIR=r'E:\mSens_AppUsage\.r-cache', XDG_CACHE_HOME=r'E:\mSens_AppUsage\.cache')
records = []
comparisons = []
def status(phase, **details):
    payload = dict(phase=phase, pid=os.getpid(), updated_at=time.strftime('%Y-%m-%dT%H:%M:%S%z'), **details)
    (root/'campaign-status.json').write_text(json.dumps(payload,indent=2),encoding='utf-8')
    print(phase, details.get('id',''), flush=True)

def successful(path):
    try:
        return json.loads((path/'result.json').read_text(encoding='utf-8'))['status']=='success'
    except (OSError, KeyError, ValueError):
        return False

def read_state(path):
    # The independent supervisor may be between truncation and flush.
    try:
        return json.loads(path.read_text(encoding='utf-8'))
    except (OSError, ValueError):
        return {}

def run(role, bid, output, repeat=0, phase='formal'):
    verify_library(role)
    output = pathlib.Path(output)
    path = output/bid
    status(phase, id=bid, role=role, repeat=repeat)
    if path.exists():
        # Never convert an interrupted partial elapsed time into a valid result.
        if not successful(path):
            records.append(dict(phase=phase,id=bid,role=role,repeat=repeat,status='incomplete_existing',output=str(output)))
            return False
    else:
        command = [sys.executable,str(harness/'supervise.py'),'--library',config['libraries'][role],
                   '--manifest',config['manifest'],'--output',str(output),'--role',role,'--ids',bid]
        subprocess.run(command,env=env,creationflags=subprocess.CREATE_NO_WINDOW,check=False)
    ok = successful(path)
    record = dict(phase=phase,id=bid,role=role,repeat=repeat,status='success' if ok else 'incomplete',output=str(output))
    if ok:
        result = json.loads((path/'result.json').read_text(encoding='utf-8'))
        metrics = json.loads((output/(bid+'.metrics.json')).read_text(encoding='utf-8'))
        if metrics['exit_code'] != 0 or metrics['stop_reason'] is not None:
            raise RuntimeError('Successful output cannot override a failed process/resource observation')
        record.update({k:result[k] for k in ['first_wall_sec','second_wall_sec','total_wall_sec','total_cpu_sec']})
        record['first_wall_fraction'] = result['first_wall_sec']/result['total_wall_sec']
        record['second_wall_fraction'] = result['second_wall_sec']/result['total_wall_sec']
        record['process_wall_sec'] = metrics['process_wall_sec']
        actual = [v for v in metrics['processes'].values() if '/bin/x64/' in v['exe'].replace('\\','/').lower()
                  and v['exe'].lower().endswith(('rscript.exe','rterm.exe'))]
        if len(actual)!=1:
            raise RuntimeError('Actual x64 R process identity must be unique')
        record['peak_working_set_bytes'] = actual[0]['peak_wset']
        record['max_sampled_private_bytes'] = actual[0]['private']
        record['minimum_observed_available_bytes'] = metrics['minimum_available_bytes']
        proc2 = list(path.rglob('*_proc-2.json'))
        if len(proc2)!=1:
            raise RuntimeError('Expected one completed proc-2 JSON')
        profile = json.loads(proc2[0].read_text(encoding='utf-8'))['second_level']['profiling']
        for key in ['load_elapsed_sec','convert_elapsed_sec','save_elapsed_sec','inline_qc_elapsed_sec']:
            record['second_'+key] = profile.get(key)
    records.append(record)
    (root/'campaign-runs.json').write_text(json.dumps(records,indent=2),encoding='utf-8')
    publish()
    return ok

def compare(old, new, bid, label):
    report = root/'comparisons'/f'{label}-{bid}.csv'
    if not successful(old) or not successful(new):
        comparisons.append(dict(id=bid,comparison=label,status='missing_complete_pair'))
        return False
    command = [R,'--vanilla',str(harness/'compare_runs.R'),str(old),str(new),str(report)]
    result = subprocess.run(command,env=env,creationflags=subprocess.CREATE_NO_WINDOW,check=False)
    comparisons.append(dict(id=bid,comparison=label,status='pass' if result.returncode==0 else 'different'))
    (root/'campaign-comparisons.json').write_text(json.dumps(comparisons,indent=2),encoding='utf-8')
    publish()
    return result.returncode==0

def publish():
    public = pathlib.Path(config['public_report_dir'])
    public.mkdir(parents=True,exist_ok=True)
    safe = [{k:v for k,v in row.items() if k!='output'} for row in records]
    if safe:
        fields = list(dict.fromkeys(k for row in safe for k in row))
        with (public/'PERFORMANCE_0.3.5_RUNS.csv').open('w',newline='',encoding='utf-8') as f:
            writer=csv.DictWriter(f,fieldnames=fields);writer.writeheader();writer.writerows(safe)
    lines = ['# appusageR 0.3.5 performance evidence','',
      'Status: measurements in progress; no performance acceptance or release.','',
      'R 4.5.3 / stringi 1.8.7 / ICU 74.1 / Chinese_China.utf8; serial, one worker.',
      'Each run uses an empty output root and a fresh process. Sources are hashed',
      'before the timed API calls; OS cache is not flushed and these are not cold-cache claims.','',
      'API wall time covers first level through completed second level, inline QC,',
      'cache pairs and summaries. Process wall includes startup/validation overhead.',
      'Working set is the OS-recorded peak for the identified x64 R process.',
      'Private memory maximum and host available-memory minimum are sampled at 1 s.','',
      '| File | Old complete / 3 | New complete / 3 | Old median [range] s | New median [range] s | Ratio |',
      '|---|---:|---:|---:|---:|---:|']
    for bid in IDS:
        values = {}
        for role in ['baseline','candidate']:
            values[role] = [row['total_wall_sec'] for row in records if row['phase']=='formal' and row['id']==bid
                            and row['role']==role and row['status']=='success']
        def cell(v):
            return f'{statistics.median(v):.3f} [{min(v):.3f}, {max(v):.3f}]' if len(v)==3 else 'incomplete'
        old,new=values['baseline'],values['candidate']
        equivalent = all(any(c['id']==bid and c['comparison']==f'formal-{r}' and c['status']=='pass'
                             for c in comparisons) for r in range(1,4))
        ratio=(f'{statistics.median(old)/statistics.median(new):.3f}'
               if len(old)==len(new)==3 and equivalent else 'not calculated')
        lines.append(f'| {bid} | {len(old)} | {len(new)} | {cell(old)} | {cell(new)} | {ratio} |')
    summary=[]
    for bid in IDS:
        for role in ['baseline','candidate']:
            group=[row for row in safe if row['phase']=='formal' and row['id']==bid and row['role']==role and row['status']=='success']
            item=dict(id=bid,role=role,complete_repetitions=len(group))
            if len(group)==3:
                for key in group[0]:
                    if key not in ['repeat'] and all(isinstance(row.get(key),(int,float)) for row in group):
                        values=[row[key] for row in group]
                        item[key+'_median']=statistics.median(values)
                        item[key+'_min']=min(values);item[key+'_max']=max(values)
            summary.append(item)
    fields=list(dict.fromkeys(k for row in summary for k in row))
    with (public/'PERFORMANCE_0.3.5_SUMMARY.csv').open('w',newline='',encoding='utf-8') as f:
        writer=csv.DictWriter(f,fieldnames=fields);writer.writeheader();writer.writerows(summary)
    lines += ['', 'The five largest files are a long-tail stress set, not a corpus-wide speed estimate.',
      'No ratio is computed from incomplete repetitions. The structural attribution',
      'runs are single observations, separate from the three-repeat formal comparison.','',
      '[Raw runs and stage/memory measurements](PERFORMANCE_0.3.5_RUNS.csv);',
      '[medians and ranges for wall/CPU/stages/memory](PERFORMANCE_0.3.5_SUMMARY.csv).',
      'Full private artifact comparisons and source/library manifests remain local.']
    (public/'PERFORMANCE_0.3.5.md').write_text('\n'.join(lines)+'\n',encoding='utf-8')
    equivalence=['# appusageR 0.3.5 complete-artifact equivalence','',
      'Comparisons require exact values, types, ordering, attributes, dates,',
      'millisecond fields, pairing, daily summaries, diagnostics and QC. Identity',
      'uses source record keys plus strict source and configuration fingerprints.',
      'Version/implementation, run/time/measurement, output-root and explicitly',
      'approved backend wording fields are the only normalization whitelist.','',
      'The user approved the separately reproduced explicit-encoding duplicate',
      'transcoding bug fix on 2026-09-29. It does not relax these automatic-encoding',
      'real-source comparisons. Full differences and normalization logs remain local.','',
      '| File | Structure | Final candidate | Formal pairs passed / 3 |',
      '|---|---|---|---:|']
    for bid in IDS:
        def verdict(label):
            return next((c['status'] for c in comparisons if c['id']==bid and c['comparison']==label),'pending')
        count=sum(verdict(f'formal-{r}')=='pass' for r in range(1,4))
        equivalence.append(f"| {bid} | {verdict('structure')} | {verdict('candidate')} | {count} |")
    (public/'EQUIVALENCE_0.3.5.md').write_text('\n'.join(equivalence)+'\n',encoding='utf-8')

status('waiting_for_baseline_and_candidate_readiness')
while True:
    golden_state_path = pathlib.Path(config['golden'])/'active.json'
    golden = read_state(golden_state_path)
    ready_path = pathlib.Path(config['ready_file'])
    ready = read_state(ready_path)
    if golden.get('status')=='complete' and ready.get('status')=='ready_for_measurement':
        break
    if golden.get('pid') and not psutil.pid_exists(golden['pid']) and golden.get('status')!='complete':
        # Allow the supervisor a brief interval to write the next active record.
        time.sleep(2)
        again=read_state(golden_state_path)
        if again==golden:
            status('blocked_incomplete_baseline',id=golden.get('id'))
            raise SystemExit(2)
    time.sleep(5)

assert ready['package_check']=='0 errors, 0 warnings, 0 notes'
assert ready['test_failures']==0 and ready['test_warnings']==0
assert ready['explicit_encoding_decision'] in ['preserve_legacy_exception','approved_specific_bugfix']
env['APPUSAGER_BENCHMARK_DEPENDENCY_LOCK'] = ready['dependency_lock']
assert all(successful(pathlib.Path(config['golden'])/bid) for bid in IDS)
for relative, digest in ready['harness_sha256'].items():
    assert hashlib.sha256((harness/relative).read_bytes()).hexdigest() == digest
def verify_library(role):
    package=pathlib.Path(config['libraries'][role])/'appusageR'
    expected=ready['library_sha256'][role]
    assert expected
    assert {p.relative_to(package).as_posix() for p in package.rglob('*') if p.is_file()}==set(expected)
    for relative,digest in expected.items():
        if hashlib.sha256((package/relative).read_bytes()).hexdigest()!=digest:
            raise RuntimeError('Frozen installed library changed: '+role)

for bid in IDS:
    if not run('structure',bid,root/'intermediate',phase='structure_single_observation') or not compare(
        pathlib.Path(config['golden'])/bid,root/'intermediate'/bid,bid,'structure'):
        status('blocked_structure_equivalence',id=bid);raise SystemExit(3)
for bid in IDS:
    if not run('candidate',bid,root/'candidate-equivalence',phase='candidate_equivalence') or not compare(
        pathlib.Path(config['golden'])/bid,root/'candidate-equivalence'/bid,bid,'candidate'):
        status('blocked_candidate_equivalence',id=bid);raise SystemExit(4)
for item in schedule:
    run(item['role'],item['id'],item['output'],repeat=item['repeat'])
    old=root/'formal'/f"repeat-{item['repeat']}"/'baseline'/item['id']
    new=root/'formal'/f"repeat-{item['repeat']}"/'candidate'/item['id']
    label=f"formal-{item['repeat']}"
    if successful(old) and successful(new) and not any(x['id']==item['id'] and x['comparison']==label for x in comparisons):
        if not compare(old,new,item['id'],label):
            status('blocked_formal_equivalence',id=item['id']);raise SystemExit(5)
complete = (sum(x['phase']=='formal' and x['status']=='success' for x in records)==30
            and len(comparisons)==25 and all(x['status']=='pass' for x in comparisons))
status('awaiting_user_performance_acceptance' if complete else 'incomplete_measurements')
publish()
if complete:
    report=pathlib.Path(config['public_report_dir'])/'PERFORMANCE_0.3.5.md'
    text=report.read_text(encoding='utf-8').replace('Status: measurements in progress; no performance acceptance or release.',
        'Status: measurements and exact comparisons complete; awaiting user performance acceptance. No release or migration has occurred.')
    report.write_text(text,encoding='utf-8')
