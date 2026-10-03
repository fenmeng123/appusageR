"""Publish de-identified latest-only timing and artifact-comparison evidence."""
from pathlib import Path
import argparse
import csv
import json

p=argparse.ArgumentParser()
p.add_argument('--work',required=True)
a=p.parse_args()
work=Path(a.work)
root=work/'latest-only'
project=Path(__file__).resolve().parents[3]
docs=project/'docs'
summary=json.loads((root/'summary.json').read_text(encoding='utf-8'))
campaign=json.loads((root/'campaign.json').read_text(encoding='utf-8'))
comparisons=json.loads((root/'comparisons-final/status.json').read_text(encoding='utf-8'))
assert len(comparisons)==14 and all(x['exit_code']==0 for x in comparisons)
rows=list(csv.DictReader((root/'measurements.csv').open(encoding='utf-8')))
table=(root/'table.md').read_text(encoding='utf-8')
raw=['| File | Repeat | First s | Conversion s | QC s | API wall s | API CPU s | Process wall s | Process CPU s | Working set GiB | Private GiB | Min host available GiB |',
     '|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|']
for row in rows:
    raw.append('| '+row['id']+' | '+row['repeat']+' | '+' | '.join(f'{float(row[k]):.4f}' for k in
      ['first_sec','convert_sec','qc_sec','total_sec','cpu_sec','process_wall_sec','process_cpu_sec',
       'peak_working_set_gib','sampled_private_gib','host_min_available_gib'])+' |')
phase=['| File | First / total % | Conversion / total % | QC / total % |',
       '|---|---:|---:|---:|']
for item in summary['files']:
    m=item['medians']
    phase.append(f"| {item['id']} | {m['first_total_pct']:.2f} | {m['convert_total_pct']:.2f} | {m['qc_total_pct']:.2f} |")
report=f'''# appusageR 0.3.5+ latest-only performance evidence

Date: 2026-09-30. Status: **awaiting_user_performance_acceptance**.
The authorized QC context/date reuse is implemented. All 15 latest-only runs
completed successfully; no resource failure or partial latest run is counted.
The user cancelled old-version validation and the A/B queue. Existing old
raw01-raw04 artifacts were retained and compared without rerunning old processing;
raw05 has no complete old artifact. No release or project migration is implied.

## Fixed implementation and measurement conditions

- Package version 0.3.5, schema 0.3.4. “0.3.5+” denotes the additional QC work.
- Frozen source manifest SHA-256: `97db37ce02eb624a56e63c66c2b0cbf949e5b1530378621385fa84104c655bac`.
- Parser fingerprint: `{summary['parser_fingerprint']}`.
- Second-level fingerprint: `{summary['second_fingerprint']}`.
- R 4.5.3 x64, stringi 1.8.7, ICU 74.1, `Chinese_China.utf8`, Asia/Shanghai.
- Three independent R processes per file, raw01-raw05 in fixed order per round,
  one worker, fresh outputs, unchanged source/library hashes and dependency lock.
- Source hashing warms the OS cache; cache was not flushed. This is not a cold
  cache experiment. Existing user research computation remained active; this
  is not an idle-host or isolated-machine claim. No other task performance
  experiment overlapped these measurements.
- API wall/CPU cover first-level entry through completed second-level processing,
  inline QC and RDA/JSON/summary writes. External process wall/CPU also include
  startup and verification. Stage times are nested and cannot be added to totals.
- The external monitor sampled once per second, identifying the actual x64 R
  child. Working set is the maximum observed OS peak-working-set counter;
  private memory is the maximum sampled private usage. Memory maxima below
  are across repetitions. Minimum host available memory was
  **{summary['host_min_available_gib']:.3f} GiB**.

## Three-run summaries

Times are medians; the total column also shows minimum–maximum. Memory is the
maximum across three runs, not a median. raw02 is meta; the others are line.

{table}
## Stage shares

Shares are medians of the three within-run ratios, not ratios of separately
rounded medians. Remaining time includes load/save, metadata and orchestration.

{chr(10).join(phase)}

## All individual observations

{chr(10).join(raw)}

## Correctness and scope

- Full package check: 0 errors, 0 warnings, 0 notes, including package tests.
- 34 new QC-context assertions; 39 exact synthetic pre-addition QC comparisons.
- Exact full pre-serialization QC equality on four existing real-source objects,
  ignoring only the two generated QC timestamps.
- Four existing-old versus latest complete-artifact comparisons passed; all ten
  comparisons between latest repetitions passed. Source/configuration hashes,
  values, types, ordering, attributes and QC remain strict.
- The original raw01 comparison was 9/10 exact: only host memory detection in
  the first-summary attributes differed. The final comparator explicitly
  normalizes `first_level_worker_decision/detected_total_memory_bytes`,
  `memory_detected`, and `memory_source`, alongside already allowed free-memory
  measurements. Worker choice, source/configuration fingerprints and all
  scientific fields remain strict. The original difference is preserved.
- Two actual PSOCK workers per stage loaded the exact candidate; eight serial/
  parallel RDA comparisons passed. Production stringi/text audit passed.
- Old raw05 was stopped in first level by the user's instruction. Its full old
  equivalence evidence is unavailable; new three-run repeatability does not
  substitute for an old-versus-new comparison.

Only the five frozen sources were read. No additional corpus files were listed
or processed. The long-tail stress set does not estimate whole-corpus performance.
No old performance ratio is calculated, and this report does not isolate the
incremental speed effect of just the two QC changes.

## Reproduction and retained evidence

From `sourcecode`, use the configured absolute Python executable with
`tools/performance/latest_campaign_035.py --work PRIVATE_WORK_ROOT`, then
`report_latest_035.py`, `compare_latest_035.py`, and `publish_latest_report_035.py`
with the same work argument. A fresh output root is required. See the
[tool protocol](../../sourcecode/tools/performance/README.md).
Private manifests, full outputs, source/library hashes, all memory samples,
15 timing rows and comparison details remain in local workflow_test. No real
source paths or participant data are included here.

Instructions used: root AGENTS.md, docs/NORTH_STAR.md, the active execution plan,
and the user's 2026-09-30 revised direction. Code/tests/tools changed only under
sourcecode; planning/report changes are in AGENTS.md and docs. See
[implementation](IMPLEMENTATION_0.3.5.md), [validation](VALIDATION_0.3.5.md), and
[equivalence](EQUIVALENCE_0.3.5.md). Final performance acceptance remains the user's decision.
'''
(docs/'reviews/PERFORMANCE_0.3.5.md').write_text(report,encoding='utf-8')
eq=['# appusageR 0.3.5+ complete-artifact equivalence', '',
    'Date: 2026-09-30. The old validation queue was cancelled by user request.',
    'Existing outputs were compared without running old preprocessing again.', '',
    '| File | Existing old vs latest | Latest repeat 1 vs 2 | Latest repeat 1 vs 3 |',
    '|---|---|---|---|']
for n in range(1,6):
    bid=f'raw{n:02d}'
    def count(label):
        records=list(csv.DictReader((root/'comparisons-final'/(label+'.csv')).open(encoding='utf-8-sig')))
        assert all(x['identical']=='TRUE' for x in records)
        return f'{len(records)}/{len(records)} exact'
    eq.append(f"| {bid} | {count(bid+'-existing-old') if n<5 else 'unavailable: stopped in first level'} | {count(bid+'-repeat-2')} | {count(bid+'-repeat-3')} |")
eq += ['', 'Each comparison checks full proc-1/proc-2 RDA objects, summary RDS, JSON',
    'and CSV artifacts using deterministic source identity. Values, types, order,',
    'attributes, source/configuration fingerprints, schema and QC remain strict.',
    'The explicit whitelist is limited to runtime/version/implementation/output-root',
    'and already approved backend diagnostic fields; every normalization is logged.', '',
    'Host measurement fields explicitly added under first_level_worker_decision:',
    'detected_total_memory_bytes, memory_detected and memory_source. The original',
    'raw01 comparison (9/10 exact, only these runtime attributes different) is',
    'preserved. Worker decisions and scientific/configuration fields stay strict.', '',
    'There is no claim of complete old/new equivalence for raw05. Repeated latest',
    'outputs establish repeatability only. The separately approved explicit-encoding',
    'duplicate-transcoding fix does not broaden these automatic-encoding comparisons.', '',
    'Additional QC evidence: 39 exact synthetic cases and four full existing',
    'real-source QC objects matched the pre-addition 0.3.5 implementation, ignoring',
    'only two generated timestamps. See [validation](VALIDATION_0.3.5.md) and',
    '[latest-only performance](PERFORMANCE_0.3.5.md).']
(docs/'reviews/EQUIVALENCE_0.3.5.md').write_text('\n'.join(eq)+'\n',encoding='utf-8')
plan=docs/'exec-plans/EXEC_PLAN_0.3.5_PREPROCESSING_PERFORMANCE_STRINGI.md'
text=plan.read_text(encoding='utf-8').replace('Status: in progress.', 'Status: awaiting_user_performance_acceptance.')
text+='''
## Latest-only delivery, 2026-09-30

The QC addition, full package check (0/0/0), exact QC regression and two-worker
smoke are complete. All 15 latest-only measurements succeeded on the five
frozen files; source/library hashes remained unchanged. Four existing-old full
artifact comparisons and ten repeated-latest comparisons passed. Old raw05
equivalence is unavailable after user cancellation and remains disclosed.
The performance report contains all observations, medians/ranges, stage shares,
CPU and memory. Status is awaiting_user_performance_acceptance; no release or
historical-project migration has been performed.
'''
plan.write_text(text,encoding='utf-8')
print('Published latest-only performance, equivalence and plan status')
