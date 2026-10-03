"""Compare existing artifacts only; never rerun old preprocessing."""
from pathlib import Path
import argparse
import json
import os
import subprocess

p = argparse.ArgumentParser()
p.add_argument('--work', required=True)
a = p.parse_args()
work = Path(a.work)
latest = work / 'latest-only'
out = latest / 'comparisons-final'
out.mkdir(exist_ok=False)
tool = Path(__file__).with_name('compare_latest_runs.R')
env = os.environ.copy()
env.update(LC_ALL='Chinese_China.utf8', LANG='Chinese_China.utf8',
           R_LIBS_USER=str(work / 'candidate-plus-v2-lib'))
records=[]
pairs=[]
for n in range(1,5):
    bid=f'raw{n:02d}'
    pairs.append((f'{bid}-existing-old', work/'golden'/bid, latest/'repeat-1'/bid))
for rep in [2,3]:
    for n in range(1,6):
        bid=f'raw{n:02d}'
        pairs.append((f'{bid}-repeat-{rep}',latest/'repeat-1'/bid,latest/f'repeat-{rep}'/bid))
for label, old, new in pairs:
    with (out/(label+'.log')).open('w',encoding='utf-8') as log:
        result=subprocess.run([r'D:\Program Files\R\R-4.5.3\bin\Rscript.exe','--vanilla',str(tool),
            str(old),str(new),str(out/(label+'.csv'))],env=env,stdout=log,stderr=subprocess.STDOUT,
            creationflags=subprocess.CREATE_NO_WINDOW)
    records.append(dict(comparison=label,exit_code=result.returncode))
    (out/'status.json').write_text(json.dumps(records,indent=2),encoding='utf-8')
    print(label, 'exact' if result.returncode==0 else 'DIFFERENT',flush=True)
if any(r['exit_code'] for r in records): raise SystemExit(1)
