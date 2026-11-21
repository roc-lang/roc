"""Cold / warm / root-changed / import-changed timings of roc check, build --opt=dev and test.

Usage: measure.py PLAN.json OUT.jsonl [REPS]

Each plan entry is {"repo", "cmd", "root"}. For every repetition the project is
restored from its pristine copy, the compiled-module cache is cleared (downloaded
packages are kept), the builtins are cached by checking and testing a
platform-free hello module, and then the four scenarios run in order.
"""
import json
import os
from pathlib import Path
import re
import resource
import shutil
import subprocess
import sys
import time

P = Path.home() / ".cache/perf"
ROC = Path.home() / ".cache/wt-perf/zig-out/bin/roc"
HEAVY = Path.home() / ".local/bin/heavy"
XDG = P / "xdg-measure"
WORK = P / "work"
TIMEOUT = 1800
sys.path.insert(0, str(P))
from imports import most_imported  # noqa: E402


def env():
    return dict(os.environ, XDG_CACHE_HOME=str(XDG), NO_COLOR="1")


def clear_compiled_cache():
    for name in ("files-v2", "tmp"):
        path = XDG / "roc" / name
        if path.exists():
            shutil.rmtree(path)


def warm_builtins():
    for cmd in ("check", "test"):
        subprocess.run([str(ROC), cmd, "Hello.roc"], cwd=P / "hello", env=env(), stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=True)


def argv_for(cmd, root, n):
    argv = [str(ROC), cmd]
    if cmd == "build":
        argv += ["--opt=dev", f"--output={P}/out/measure-{n}"]
    return argv + [root]


def timed(argv, cwd, log):
    """Wall, user and system seconds and peak RSS of one command, timed by the runner."""
    stats = P / "time.json"
    if stats.exists():
        stats.unlink()
    wrapped = [str(HEAVY), "measure", "--", sys.executable, "-I", str(P / "runner.py"), str(stats), "timeout", "-k", "5", str(TIMEOUT)] + argv
    start = time.monotonic()
    with open(log, "wb") as out:
        subprocess.run(wrapped, cwd=cwd, env=env(), stdout=out, stderr=subprocess.STDOUT)
    outer = time.monotonic() - start
    result = json.loads(stats.read_text())
    result["outer"] = round(outer, 3)
    return result


def append_comment(path: Path, tag: str):
    with path.open("a") as f:
        f.write(f"\n# perf measurement edit {tag}\n")


def main():
    plan = json.loads(Path(sys.argv[1]).read_text())
    out_path = Path(sys.argv[2])
    reps = int(sys.argv[3]) if len(sys.argv) > 3 else 3
    (P / "out").mkdir(exist_ok=True)
    (P / "measure-logs").mkdir(exist_ok=True)
    done = set()
    if out_path.exists():
        for line in out_path.read_text().splitlines():
            r = json.loads(line)
            done.add((r["repo"], r["cmd"], r["root"], r["rep"], r["scenario"]))
    for index, item in enumerate(plan):
        repo, cmd, root = item["repo"], item["cmd"], item["root"]
        src = P / "src" / repo
        import_file, importers = most_imported(src / root, src)
        import_rel = str(import_file.relative_to(src.resolve())) if import_file else None
        for rep in range(1, reps + 1):
            scenarios = ["cold", "warm", "root_changed"] + (["import_changed"] if import_rel else [])
            if all((repo, cmd, root, rep, s) in done for s in scenarios):
                continue
            work = WORK / repo
            if work.exists():
                shutil.rmtree(work)
            shutil.copytree(src, work, symlinks=True)
            clear_compiled_cache()
            warm_builtins()
            for scenario in scenarios:
                if scenario == "root_changed":
                    append_comment(work / root, f"root {rep}")
                elif scenario == "import_changed":
                    append_comment(work / import_rel, f"import {rep}")
                safe = re.sub(r"[^A-Za-z0-9]+", "_", f"{repo}_{cmd}_{root}_{rep}_{scenario}")
                log = P / "measure-logs" / f"{safe}.log"
                result = timed(argv_for(cmd, root, index), work, log)
                record = dict(repo=repo, cmd=cmd, root=root, rep=rep, scenario=scenario,
                              import_file=import_rel, import_importers=importers, log=str(log), **result)
                with out_path.open("a") as f:
                    f.write(json.dumps(record) + "\n")
                print(json.dumps(record), flush=True)


if __name__ == "__main__":
    resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
    main()
