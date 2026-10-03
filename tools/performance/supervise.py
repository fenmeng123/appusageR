"""Serial, unlimited-wall-clock benchmark supervisor; arguments contain local paths."""
import argparse, csv, hashlib, json, os, pathlib, subprocess, time
import psutil

p = argparse.ArgumentParser()
p.add_argument('--library', required=True)
p.add_argument('--manifest', required=True)
p.add_argument('--output', required=True)
p.add_argument('--role', required=True)
p.add_argument('--ids', nargs='+', default=['raw01','raw02','raw03','raw04','raw05'])
a = p.parse_args()
root = pathlib.Path(a.output)
root.mkdir(parents=True, exist_ok=True)
harness = {name: hashlib.sha256(pathlib.Path(__file__).with_name(name).read_bytes()).hexdigest()
           for name in ['supervise.py', 'benchmark_one.R']}
env = os.environ.copy()
env.update(LC_ALL='Chinese_China.utf8', LANG='Chinese_China.utf8',
    R_USER_CACHE_DIR=r'E:\mSens_AppUsage\.r-cache', XDG_CACHE_HOME=r'E:\mSens_AppUsage\.cache',
    R_LIBS_USER=a.library, APPUSAGER_GIT_COMMIT='26c4d7e8b7f2dfa4b8b65c5bb0d61554299633f1' if a.role=='baseline' else '',
    APPUSAGER_GIT_DIRTY='false' if a.role=='baseline' else 'true')
for bid in a.ids:
    run = root / bid
    if run.exists():
        raise RuntimeError('Refusing to overwrite a measured or incomplete run: '+bid)
    with (root/(bid+'.stdout.log')).open('w', encoding='utf-8') as out, (root/(bid+'.stderr.log')).open('w', encoding='utf-8') as err:
        started = time.perf_counter()
        proc = subprocess.Popen([r'D:\Program Files\R\R-4.5.3\bin\Rscript.exe','--vanilla',str(pathlib.Path(__file__).with_name('benchmark_one.R')),a.library,a.manifest,bid,str(run),a.role], env=env, stdout=out, stderr=err,creationflags=subprocess.CREATE_NO_WINDOW)
        (root/'active.json').write_text(json.dumps(dict(id=bid,pid=proc.pid,role=a.role)),encoding='utf-8')
        seen = {}; min_free = psutil.virtual_memory().available; guard_count = 0; reason = None
        with (root/(bid+'.memory.csv')).open('w',newline='') as mem:
            writer=csv.writer(mem); writer.writerow(['elapsed_sec','pid','rss','private','peak_wset','cpu_sec','available'])
            while proc.poll() is None:
                try:
                    parent=psutil.Process(proc.pid)
                    available=psutil.virtual_memory().available
                    min_free=min(min_free,available)
                    children=[parent]+parent.children(recursive=True)
                    for child in children:
                        try:
                            m=child.memory_info(); cpu=sum(child.cpu_times()[:2])
                            v=dict(rss=m.rss,private=getattr(m,'private',0),peak_wset=getattr(m,'peak_wset',m.rss),cpu_sec=cpu,exe=child.exe())
                            prior=seen.get(child.pid,{})
                            seen[child.pid]={k:max(prior.get(k,0),x) if isinstance(x,(int,float)) else x for k,x in v.items()}
                            writer.writerow([round(time.perf_counter()-started,3),child.pid,v['rss'],v['private'],v['peak_wset'],cpu,available])
                        except (psutil.NoSuchProcess,psutil.AccessDenied): pass
                    mem.flush()
                    guard_count = guard_count+1 if available < 4*1024**3 else 0
                    if guard_count >= 30:
                        reason='memory_guard_host_below_4GiB_for_30_seconds'
                        for child in reversed(children):
                            try: child.terminate()
                            except psutil.NoSuchProcess: pass
                except psutil.NoSuchProcess: pass
                time.sleep(1)
        code=proc.wait()
        (root/(bid+'.metrics.json')).write_text(json.dumps(dict(id=bid,role=a.role,exit_code=code,stop_reason=reason,process_wall_sec=time.perf_counter()-started,minimum_available_bytes=min_free,processes=seen,harness_sha256=harness),indent=2),encoding='utf-8')
        print(bid, 'complete' if code==0 else 'failed', flush=True)
        if code != 0: raise SystemExit(code)
(root/'active.json').write_text(json.dumps(dict(status='complete')),encoding='utf-8')
