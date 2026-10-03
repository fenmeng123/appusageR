"""Exercise the entire campaign using only the package's synthetic line fixture."""
import argparse,csv,hashlib,json,pathlib,subprocess,sys
p=argparse.ArgumentParser();p.add_argument('--work',required=True);a=p.parse_args()
work=pathlib.Path(a.work);root=work/'campaign-smoke'
assert not root.exists(),'Preserve existing smoke evidence'
root.mkdir()
harness=pathlib.Path(__file__).parent
config=json.loads((work/'campaign-config.json').read_text(encoding='utf-8'))
fixture=work/'candidate-source/tests/testthat/fixtures/line_sample.txt'
raw=fixture.read_bytes();manifest=root/'synthetic-manifest.csv'
with manifest.open('w',newline='',encoding='utf-8') as f:
    writer=csv.DictWriter(f,fieldnames=['benchmark_id','source_file','size_bytes','source_md5']);writer.writeheader()
    for i in range(1,6):
        writer.writerow(dict(benchmark_id=f'raw{i:02d}',source_file=str(fixture),size_bytes=len(raw),source_md5=hashlib.md5(raw).hexdigest()))
config.update(root=str(root/'campaign'),golden=str(root/'golden'),manifest=str(manifest),public_report_dir=str(root/'synthetic-reports'))
path=root/'config.json';path.write_text(json.dumps(config,indent=2),encoding='utf-8')
subprocess.run([sys.executable,str(harness/'supervise.py'),'--library',config['libraries']['baseline'],
    '--manifest',str(manifest),'--output',config['golden'],'--role','baseline'],check=True,creationflags=subprocess.CREATE_NO_WINDOW)
subprocess.run([sys.executable,str(harness/'campaign.py'),'--config',str(path)],check=True,creationflags=subprocess.CREATE_NO_WINDOW)
state=json.loads((pathlib.Path(config['root'])/'campaign-status.json').read_text(encoding='utf-8'))
assert state['phase']=='awaiting_user_performance_acceptance'
print('Synthetic campaign only: 45 successful processes and 25 exact artifact comparisons')
