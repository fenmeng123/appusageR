"""Verify evidence and freeze installed libraries/harness before the campaign."""
from pathlib import Path
import csv, hashlib, json

harness=Path(__file__).resolve().parent
package=harness.parent.parent
work=package.parent/'reference/workflow_test/performance_035'
def read(name): return json.loads((work/name).read_text(encoding='utf-8'))
def hashes(root):
    return {p.relative_to(root).as_posix():hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(root.rglob('*')) if p.is_file()}
counts=read('validation-counts.json')
assert counts['failed']==counts['warning']==counts['error']==0
assert read('workers-smoke-final/result.json')['status']=='pass'
assert read('workers-smoke-final/result.json')['exact_rda_comparisons']==8
assert 'Status: OK' in (work/'check-final/appusageR.Rcheck/00check.log').read_text()
assert read('baseline.json')['commit']=='26c4d7e8b7f2dfa4b8b65c5bb0d61554299633f1'
assert hashlib.sha256((work/'baseline-source.zip').read_bytes()).hexdigest()==read('baseline.json')['source_archive_sha256']
for source in ['candidate-source','structure-source-final']:
    assert hashes(work/source)==read(source+'-manifest.json')
for rel,digest in read('candidate-source-manifest.json').items():
    assert hashlib.sha256((package/rel).read_bytes()).hexdigest()==digest
for name,column,count in [('structure_differential.csv','identical',59),
    ('structure_final_differential.csv','identical',59),('text_differential.csv','identical',140),
    ('comparisons/synthetic-final.csv','identical',10),('comparisons/synthetic-structure-final.csv','identical',10),
    ('approved-encoding-exception.csv','exact_data_equals_old_auto',12)]:
    with (work/name).open(encoding='utf-8-sig',newline='') as f: rows=list(csv.DictReader(f))
    assert len(rows)==count and all(r[column]=='TRUE' for r in rows),name
config=read('campaign-config.json')
evidence=['validation-counts.json','workers-smoke-final/result.json',
          'check-final/appusageR.Rcheck/00check.log','dependency-lock.json',
          'candidate-source-manifest.json','structure-source-final-manifest.json',
          'approved-encoding-exception.csv']
ready=dict(status='ready_for_measurement',package_check='0 errors, 0 warnings, 0 notes',
    test_failures=counts['failed'],test_warnings=counts['warning'],test_passed=counts['passed'],
    explicit_encoding_decision='approved_specific_bugfix',
    dependency_lock=str(work/'dependency-lock.json'),
    library_sha256={role:hashes(Path(lib)/'appusageR') for role,lib in config['libraries'].items()},
    source_manifests={source:read(source+'-manifest.json') for source in ['candidate-source','structure-source-final']},
    harness_sha256={p.name:hashlib.sha256(p.read_bytes()).hexdigest() for p in sorted(harness.iterdir())
                    if p.is_file() and p.suffix in ['.R','.py','.md']},
    evidence_sha256={name:hashlib.sha256((work/name).read_bytes()).hexdigest() for name in evidence})
target=Path(config['ready_file'])
assert not target.exists(),'Refusing to overwrite measurement readiness'
target.write_text(json.dumps(ready,indent=2),encoding='utf-8')
print('Measurement gate ready:',counts['passed'],'tests, three isolated libraries, frozen harness')
