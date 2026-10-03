from pathlib import Path
import hashlib,json,shutil
root=Path(r'E:\mSens_AppUsage\sourcecode')
target=Path(r'E:\mSens_AppUsage\reference\workflow_test\performance_035\structure-source')
if target.exists(): raise SystemExit('Refusing to replace frozen structure snapshot')
target.mkdir()
for name in ['R','man','tests','vignettes']:
    if (root/name).exists(): shutil.copytree(root/name,target/name)
for name in ['DESCRIPTION','NAMESPACE','LICENSE','LICENSE.md','.Rbuildignore','NEWS.md','README.md']:
    if (root/name).exists(): shutil.copy2(root/name,target/name)
entries={p.relative_to(target).as_posix():hashlib.sha256(p.read_bytes()).hexdigest() for p in target.rglob('*') if p.is_file()}
(target.parent/'structure-source-manifest.json').write_text(json.dumps(entries,indent=2),encoding='utf-8')
print('Frozen structure snapshot:',len(entries),'files')
