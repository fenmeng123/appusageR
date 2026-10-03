"""Three serial latest-only repetitions; never dispatch an old-version run."""
from pathlib import Path
import argparse
import hashlib
import json
import os
import subprocess
import sys
import time
import psutil

p = argparse.ArgumentParser()
p.add_argument('--work', required=True)
p.add_argument('--candidate', default='candidate-plus-v2')
a = p.parse_args()
work = Path(a.work).resolve()
out = work / 'latest-only'
out.mkdir(exist_ok=False)
tools = Path(__file__).resolve().parent
library = work / (a.candidate + '-lib')
snapshot = work / (a.candidate + '-source')
source_manifest_name = a.candidate + '-source-manifest.json'
source_manifest = json.loads((work / source_manifest_name).read_text(encoding='utf-8'))
def verify_sources():
    for relative, digest in source_manifest.items():
        if hashlib.sha256((snapshot / relative).read_bytes()).hexdigest() != digest:
            raise RuntimeError('Frozen source changed: ' + relative)
verify_sources()
installed = {p.relative_to(library).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
             for p in sorted(library.rglob('*')) if p.is_file()}
env = os.environ.copy()
env['APPUSAGER_BENCHMARK_DEPENDENCY_LOCK'] = str(work / 'dependency-lock.json')
assert Path(env['APPUSAGER_BENCHMARK_DEPENDENCY_LOCK']).is_file()
state = dict(status='running', started_at=time.strftime('%Y-%m-%dT%H:%M:%S%z'),
             repetitions=3, workers=1, role='candidate-plus',
             installed_sha256=installed, source_manifest=source_manifest_name,
             harness_sha256={n:hashlib.sha256((tools/n).read_bytes()).hexdigest()
                for n in ['latest_campaign_035.py','supervise.py','benchmark_one.R']})
state['host_context'] = dict(logical_cpus=psutil.cpu_count(),
    available_memory_bytes=psutil.virtual_memory().available,
    existing_r_processes=sum(1 for p in psutil.process_iter(['name'])
        if (p.info['name'] or '').lower() in ['rscript.exe','rterm.exe','r.exe']),
    note='Other user research processes are preserved; this is not an idle-host claim')
def persist():
    (out / 'campaign.json').write_text(json.dumps(state, indent=2), encoding='utf-8')
persist()
try:
    for repeat in range(1, 4):
        state['active_repeat'] = repeat
        persist()
        subprocess.run([sys.executable, str(tools / 'supervise.py'), '--library', str(library),
            '--manifest', str(work / 'private_frozen_manifest.csv'), '--output', str(out / f'repeat-{repeat}'),
            '--role', 'candidate-plus'], env=env, check=True, creationflags=subprocess.CREATE_NO_WINDOW)
    verify_sources()
    for relative, digest in installed.items():
        if hashlib.sha256((library / relative).read_bytes()).hexdigest() != digest:
            raise RuntimeError('Installed candidate changed')
    state['status'] = 'success'
except Exception as exc:
    state['status'] = 'failed'
    state['error'] = str(exc)
    raise
finally:
    state['finished_at'] = time.strftime('%Y-%m-%dT%H:%M:%S%z')
    persist()
