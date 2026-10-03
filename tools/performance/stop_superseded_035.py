"""Stop only the verified, superseded performance campaign and its children."""
import datetime
import json
import pathlib
import psutil

root = pathlib.Path(r'E:\mSens_AppUsage\reference\workflow_test\performance_035')
specs = {47028: (1790678046.6487017, 'campaign.py'),
         35588: (1790671460.2303307, 'supervise.py')}
parents, children, records = [], [], []
for pid, (created, script) in specs.items():
    try:
        process = psutil.Process(pid)
        if abs(process.create_time() - created) > .1 or not any(script in c for c in process.cmdline()):
            raise RuntimeError('Process identity changed: ' + str(pid))
        parents.append(process)
        children.extend(process.children(recursive=True))
        records.append(dict(pid=pid, created=process.create_time(), cmd=process.cmdline()))
    except psutil.NoSuchProcess:
        pass
targets = list({p.pid: p for p in parents + children}.values())
record = dict(status='stopped_by_user', at=datetime.datetime.now().astimezone().isoformat(),
              reason='User requested QC optimization and latest-only measurements',
              parents=records, owned_pids=[p.pid for p in targets], raw05_complete=False)
(root / 'cancellation-2026-09-30.json').write_text(json.dumps(record, indent=2), encoding='utf-8')
for process in targets:
    try:
        process.terminate()
    except psutil.NoSuchProcess:
        pass
gone, alive = psutil.wait_procs(targets, timeout=5)
for process in alive:
    try:
        process.kill()
    except psutil.NoSuchProcess:
        pass
(root / 'campaign' / 'cancelled-by-user.json').write_text(json.dumps(record, indent=2), encoding='utf-8')
(root / 'campaign' / 'campaign-status.json').write_text(json.dumps(dict(phase='cancelled_by_user', evidence='../cancellation-2026-09-30.json')), encoding='utf-8')
(root / 'golden' / 'active.json').write_text(json.dumps(dict(status='stopped_by_user', id='raw05', partial=True)), encoding='utf-8')
print(json.dumps(dict(stopped=[p.pid for p in targets], survivors=[p.pid for p in targets if p.is_running()])))
