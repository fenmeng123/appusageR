"""Summarize complete latest-only measurements without source identifiers."""
from pathlib import Path
import argparse
import csv
import json
import statistics
import sys

sys.stdout.reconfigure(encoding='utf-8')

p = argparse.ArgumentParser()
p.add_argument('--work', required=True)
a = p.parse_args()
work = Path(a.work)
root = work / 'latest-only'
state = json.loads((root / 'campaign.json').read_text(encoding='utf-8'))
assert state['status'] == 'success'
rows = []
for repeat in range(1, 4):
    for number in range(1, 6):
        bid = f'raw{number:02d}'
        run = root / f'repeat-{repeat}' / bid
        result = json.loads((run / 'result.json').read_text(encoding='utf-8'))
        metrics = json.loads((run.parent / (bid + '.metrics.json')).read_text(encoding='utf-8'))
        assert result['status'] == 'success' and metrics['exit_code'] == 0
        actual = [v for v in metrics['processes'].values() if '\\bin\\x64\\' in v['exe'].lower() and v['exe'].lower().endswith('rscript.exe')]
        assert len(actual) == 1, 'Must identify actual x64 R child'
        proc = actual[0]
        paths = list(run.rglob('*_proc-2.json'))
        assert len(paths) == 1
        metadata = json.loads(paths[0].read_text(encoding='utf-8'))
        profile = metadata['second_level']['profiling']
        rows.append(dict(id=bid, type='meta' if number == 2 else 'line', repeat=repeat,
            size_mib=result['source_bytes']/1024**2, source_md5=result['source_md5'],
            first_sec=result['first_wall_sec'], second_sec=result['second_wall_sec'],
            convert_sec=profile['convert_elapsed_sec'], qc_sec=profile['inline_qc_elapsed_sec'],
            qc_total_pct=100*profile['inline_qc_elapsed_sec']/result['total_wall_sec'],
            first_total_pct=100*result['first_wall_sec']/result['total_wall_sec'],
            convert_total_pct=100*profile['convert_elapsed_sec']/result['total_wall_sec'],
            total_sec=result['total_wall_sec'], cpu_sec=result['total_cpu_sec'],
            process_wall_sec=metrics['process_wall_sec'], process_cpu_sec=proc['cpu_sec'],
            peak_working_set_gib=proc['peak_wset']/1024**3,
            sampled_private_gib=proc['private']/1024**3,
            host_min_available_gib=metrics['minimum_available_bytes']/1024**3,
            parser_fingerprint=result['provenance']['parser_implementation_fingerprint'],
            second_fingerprint=result['provenance']['second_level_implementation_fingerprint'],
            event_rows=profile['n_event_rows'], episode_rows=profile['n_episode_rows'],
            daily_rows=profile['n_daily_rows'], status='success'))
assert len({r['parser_fingerprint'] for r in rows}) == 1
assert len({r['second_fingerprint'] for r in rows}) == 1
with (root / 'measurements.csv').open('w', newline='', encoding='utf-8') as f:
    w=csv.DictWriter(f, fieldnames=list(rows[0])); w.writeheader(); w.writerows(rows)
groups=[]
lines=['| 文件 | 类型 | 大小 MiB | 一级 s | 二级转换 s | inline QC s | 总耗时中位数 [范围] s | CPU s | 峰值工作集 GiB | 私有内存 GiB |',
       '|---|---|---:|---:|---:|---:|---:|---:|---:|---:|']
for number in range(1, 6):
    values=[r for r in rows if r['id']==f'raw{number:02d}']
    assert len({r['source_md5'] for r in values})==1
    med={k:statistics.median(r[k] for r in values) for k in ['first_sec','second_sec','convert_sec','qc_sec','total_sec','cpu_sec','qc_total_pct','first_total_pct','convert_total_pct']}
    low=min(r['total_sec'] for r in values); high=max(r['total_sec'] for r in values)
    peak=max(r['peak_working_set_gib'] for r in values)
    private=max(r['sampled_private_gib'] for r in values)
    v=values[0]
    lines.append(f"| {v['id']} | {v['type']} | {v['size_mib']:.3f} | {med['first_sec']:.2f} | {med['convert_sec']:.2f} | {med['qc_sec']:.2f} | {med['total_sec']:.2f} [{low:.2f}–{high:.2f}] | {med['cpu_sec']:.2f} | {peak:.3f} | {private:.3f} |")
    groups.append(dict(id=v['id'], medians=med, total_range=[low,high],
        max_working_set_gib=peak, max_sampled_private_gib=private,
        host_min_available_gib=min(r['host_min_available_gib'] for r in values)))
summary=dict(status='success', runs=len(rows), files=groups,
    host_min_available_gib=min(r['host_min_available_gib'] for r in rows),
    parser_fingerprint=rows[0]['parser_fingerprint'], second_fingerprint=rows[0]['second_fingerprint'])
(root / 'summary.json').write_text(json.dumps(summary, indent=2), encoding='utf-8')
(root / 'table.md').write_text('\n'.join(lines)+'\n', encoding='utf-8')
print('\n'.join(lines))
