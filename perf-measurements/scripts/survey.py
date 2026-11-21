"""One pass of check / build --opt=dev / test over every sweep root, to see which work."""
import concurrent.futures
import json
import os
from pathlib import Path
import resource
import subprocess
import sys
import time

P = Path.home() / ".cache/perf"
ROC = Path.home() / ".cache/wt-perf/zig-out/bin/roc"
sys.path.insert(0, str(P))
from targets import targets  # noqa: E402

tasks = []
for repo, (apps, tests) in targets.items():
    for app in apps.split():
        tasks.append((repo, "check", app))
        tasks.append((repo, "build", app))
    for t in tests.split():
        tasks.append((repo, "test", t))


def run(item):
    n, (repo, cmd, root) = item
    argv = [str(ROC), cmd]
    if cmd == "build":
        argv += ["--opt=dev", f"--output={P}/out/{n:03}"]
    argv.append(root)
    env = dict(os.environ, XDG_CACHE_HOME=str(P / "xdg-survey"), NO_COLOR="1")
    log = P / "survey-logs" / f"{n:03}.log"
    start = time.monotonic()
    with log.open("wb") as out:
        proc = subprocess.Popen(argv, cwd=P / "src" / repo, env=env, stdout=out, stderr=subprocess.STDOUT, start_new_session=True)
        try:
            rc = proc.wait(timeout=1200)
        except subprocess.TimeoutExpired:
            os.killpg(proc.pid, 9)
            proc.wait()
            rc = "timeout"
    elapsed = time.monotonic() - start
    rec = dict(n=n, repo=repo, cmd=cmd, root=root, rc=rc, seconds=round(elapsed, 2), log=str(log))
    print(json.dumps(rec), flush=True)
    return rec


if __name__ == "__main__":
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    (P / "survey-logs").mkdir(exist_ok=True)
    (P / "out").mkdir(exist_ok=True)
    with concurrent.futures.ThreadPoolExecutor(3) as pool:
        results = list(pool.map(run, enumerate(tasks)))
    (P / "survey.json").write_text(json.dumps(results, indent=1))
