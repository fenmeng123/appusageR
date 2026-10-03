"""Freeze a separate latest-only candidate, preserving earlier evidence."""
from pathlib import Path
import hashlib
import json
import shutil
import argparse

parser = argparse.ArgumentParser()
parser.add_argument('--candidate', default='candidate-plus-v2')
args = parser.parse_args()

root = Path(__file__).resolve().parents[2]
work = root.parent / 'reference/workflow_test/performance_035'
target = work / (args.candidate + '-source')
if target.exists():
    raise SystemExit('Refusing to overwrite frozen candidate')
target.mkdir()
for name in ['R', 'man', 'tests', 'vignettes']:
    shutil.copytree(root / name, target / name)
for name in ['DESCRIPTION', 'NAMESPACE', 'LICENSE', '.Rbuildignore', 'NEWS.md', 'README.md', 'README.Rmd']:
    shutil.copy2(root / name, target / name)
entries = {p.relative_to(target).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
           for p in sorted(target.rglob('*')) if p.is_file()}
(work / (args.candidate + '-source-manifest.json')).write_text(json.dumps(entries, indent=2), encoding='utf-8')
print('Frozen', len(entries), 'files; SHA256 manifest', hashlib.sha256(
    (work / (args.candidate + '-source-manifest.json')).read_bytes()).hexdigest())
