"""Freeze the final candidate and structural attribution sources, without raw data."""
from pathlib import Path
import hashlib, json, re, shutil

root = Path(__file__).resolve().parents[2]
work = root.parent/'reference/workflow_test/performance_035'
candidate = work/'candidate-source'
structure = work/'structure-source-final'
if candidate.exists() or structure.exists():
    raise SystemExit('Refusing to overwrite frozen sources')
candidate.mkdir()
for name in ['R', 'man', 'tests', 'vignettes']:
    shutil.copytree(root/name, candidate/name)
for name in ['DESCRIPTION','NAMESPACE','LICENSE','.Rbuildignore','NEWS.md','README.md','README.Rmd']:
    shutil.copy2(root/name, candidate/name)
shutil.copytree(work/'structure-source-v3', structure)

def read(path): return path.read_text(encoding='utf-8-sig')
def block(text, name):
    start=text.index(name+' <- function')
    following=re.search(r'^\w+ <- function',text[start+len(name)+1:],re.M)
    end=start+len(name)+1+following.start() if following else len(text)
    return text[start:end]

# Carry the final structural fixes into the attribution snapshot. Existing
# base/stringr conversion functions in that snapshot remain unchanged.
for filename, names in {
    'source_preflight.R':['appusage_component_boundary_diagnostics','appusage_source_preflight'],
    'api_wrappers.R':['normalize_first_level_input']
}.items():
    old=read(structure/'R'/filename);new=read(candidate/'R'/filename)
    for name in names:
        replacement=block(new,name)
        assert 'appusage_text_' not in replacement and 'stringi::' not in replacement
        old=old.replace(block(old,name),replacement)
    (structure/'R'/filename).write_text(old,encoding='utf-8')
p=structure/'R/provenance.R'
p.write_text(read(p).replace('"appusage_source_preflight", "first_level_detect_type",',
    '"appusage_source_preflight", "first_level_detect_type", "normalize_first_level_input", "appusage_component_boundary_diagnostics",'),encoding='utf-8')
shutil.copy2(candidate/'tests/testthat/test-parse-context.R',structure/'tests/testthat/test-parse-context.R')
for source in [candidate,structure]:
    entries={p.relative_to(source).as_posix():hashlib.sha256(p.read_bytes()).hexdigest()
             for p in sorted(source.rglob('*')) if p.is_file()}
    (work/(source.name+'-manifest.json')).write_text(json.dumps(entries,indent=2),encoding='utf-8')
    print('Frozen',source.name,len(entries),'files')
