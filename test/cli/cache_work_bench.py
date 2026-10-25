#!/usr/bin/env python3
"""Compare ReleaseFast compilers on synthetic dev-cache workloads (Linux).

Pass --platform pointing to a built fx-open platform, and --work under TMPDIR.
The URL platform is seeded locally; no download or external project is used.
Each measured command is roc build --opt=dev. Logs retain phase timings and
cache counters. Optional strace runs are separate from wall-time measurements.
Use --sample-memory in separate profiling runs to sample anonymous versus total
resident memory; its wall times include sampling overhead. RSS includes resident
executable pages and can differ even for byte-identical binaries depending on
file-cache state. Stage comparison binaries identically and inspect anonymous
memory before attributing an RSS difference to compiler allocations.
"""
import argparse
import os
from pathlib import Path
import re
import shutil
import statistics
import subprocess
import tempfile
import time


def run_memory_profile(cmd, env):
    """Sample the compiler child of /usr/bin/time; retain its exact max RSS too.

    Component peaks are sampled independently, so they must not be added
    together. Short-lived peaks can be missed between samples.
    """
    peak_rss = peak_anon = nonanon_at_peak = samples = 0
    with tempfile.TemporaryFile(mode="w+t") as stdout, tempfile.TemporaryFile(mode="w+t") as stderr:
        with subprocess.Popen(cmd, env=env, stdout=stdout, stderr=stderr, text=True) as process:
            children = Path(f"/proc/{process.pid}/task/{process.pid}/children")
            compiler = None
            while process.poll() is None:
                try:
                    if compiler is None:
                        pids = children.read_text().split()
                        if pids:
                            compiler = Path(f"/proc/{pids[0]}/smaps_rollup")
                    if compiler is not None:
                        fields = dict(re.findall(r"^(\w+):\s+(\d+) kB$", compiler.read_text(), re.MULTILINE))
                        rss, anon = int(fields["Rss"]), int(fields["Anonymous"])
                        samples += 1
                        peak_anon = max(peak_anon, anon)
                        if rss > peak_rss:
                            peak_rss, nonanon_at_peak = rss, rss - anon
                except (FileNotFoundError, ProcessLookupError):
                    pass  # The child can exit between discovering its PID and reading it.
                time.sleep(0.001)
        stdout.seek(0)
        stderr.seek(0)
        result = subprocess.CompletedProcess(cmd, process.returncode, stdout.read(), stderr.read())
    if not samples:
        raise RuntimeError("No compiler memory samples collected")
    return result, {
        "memory_samples": samples,
        "sampled_peak_rss_kib": peak_rss,
        "sampled_peak_anon_kib": peak_anon,
        "nonanon_at_sampled_peak_kib": nonanon_at_peak,
    }


def main():
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument("roc", type=Path)
    p.add_argument("--platform", type=Path, required=True)
    p.add_argument("--work", type=Path, required=True)
    p.add_argument("--label", required=True)
    p.add_argument("--modules", type=int, default=96)
    p.add_argument("--functions", type=int, default=4)
    p.add_argument("--repeats", type=int, default=3)
    p.add_argument("--trace", action="store_true")
    p.add_argument("--sample-memory", action="store_true", help="Profile RSS components separately; wall times include sampling overhead")
    a = p.parse_args()
    roc = a.roc.resolve()
    work = a.work.resolve()
    work.mkdir(parents=True, exist_ok=True)
    source = work / "source"
    source.mkdir(exist_ok=True)
    package_hash = "FakeHashAbcDefGhiJkLmNoPqRsTuVwXyZ123456789o"
    imports = "\n".join(f"import M{i}" for i in range(a.modules))
    calls = " +\n".join(f"M{i}.f{j}(n)" for i in range(a.modules) for j in range(a.functions))
    app = f'''app [main!] {{ pf: platform "https://example.com/bench/{package_hash}.tar.zst" }}
import pf.Stdout
{imports}
main! = |args| {{
    n = args.len().to_u64() + 10
    total = {calls}
    Stdout.line!(total.to_str())
    Ok({{}})
}}
'''
    module_sources = {}
    for i in range(a.modules):
        functions = []
        for j in range(a.functions):
            functions.append(f'''f{j} : U64 -> U64
    f{j} = |n| {{
        var $sum = 0
        var $index = 0
        while $index < n {{
            $sum = $sum + $index + {i * a.functions + j}
            $index = $index + 1
        }}
        $sum
    }}''')
        module_sources[source / f"M{i}.roc"] = f"M{i} := [].{{\n" + "\n".join(functions) + "\n}\n"
    results = {}
    for repeat in range(a.repeats):
        run_dir = work / f"{a.label}-{repeat}"
        run_dir.mkdir()
        cache = run_dir / "cache"
        shutil.copytree(a.platform, cache / "roc/packages" / package_hash)
        env = dict(os.environ, ROC_CACHE_DIR=str(cache), XDG_CACHE_HOME=str(cache), ROC_PACK_STATS="1")
        for phase in ("cold", "completed", "lookup", "edit", "wide_edit", "revert"):
            for path, contents in module_sources.items():
                path.write_text(contents.replace("$sum + $index +", "$sum + $index + 1 +") if phase == "wide_edit" else contents)
            (source / "main.roc").write_text(app.replace("+ 10", "+ 11") if phase == "edit" else app)
            if phase in ("lookup", "revert"):
                for f in cache.rglob("*.rpo"):
                    f.unlink()
            cmd = [str(roc), "build", "--opt=dev", "--jobs=1", "--timings", str(source / "main.roc"), f"--output={run_dir / 'app'}"]
            start = time.perf_counter_ns()
            measured_cmd = ["/usr/bin/time", "-f", "%M", "-o", str(run_dir / "rss"), *cmd]
            memory = {}
            if a.sample_memory:
                r, memory = run_memory_profile(measured_cmd, env)
            else:
                r = subprocess.run(measured_cmd, env=env, capture_output=True, text=True)
            elapsed = (time.perf_counter_ns() - start) / 1e6
            (run_dir / f"{phase}.log").write_text(r.stdout + r.stderr)
            if r.returncode:
                raise RuntimeError(r.stdout + r.stderr)
            n = 12 if phase == "edit" else 11
            count = a.modules * a.functions
            expected = count * n * (n - 1) // 2 + n * count * (count - 1) // 2
            if phase == "wide_edit":
                expected += n * count
            output = subprocess.check_output([str(run_dir / "app")], text=True)
            assert output == f"{expected}\n", output
            stats = {}
            publication = 0
            for line in r.stderr.splitlines():
                if line.startswith("pack index publication:"):
                    publication += int(re.search(r"wall_ns=(\d+)", line)[1])
                if line.startswith("pack index:"):
                    for k, v in re.findall(r"(\w+)=(\d+)", line):
                        stats[k] = stats.get(k, 0) + int(v)
            rss = int((run_dir / "rss").read_text())
            results.setdefault(phase, []).append((elapsed, rss, publication / 1e6))
            files = [f for f in cache.rglob("*") if f.suffix in (".rpi", ".rpa", ".rpm", ".ris")]
            print(a.label, repeat, phase, f"wall_ms={elapsed:.3f} rss_kib={rss} publication_ms={publication / 1e6:.3f} records={len(files)} bytes={sum(f.stat().st_size for f in files)}", stats, flush=True)
            if a.sample_memory:
                print(a.label, repeat, phase, "memory_profile", memory, flush=True)
        if a.trace and repeat == 0:
            (source / "main.roc").write_text(app.replace("+ 10", "+ 12"))
            for path, contents in module_sources.items():
                path.write_text(contents.replace("$sum + $index +", "$sum + $index + 2 +"))
            trace = subprocess.run(["strace", "-f", "-c", "-e", "trace=openat,mkdir,rename,renameat,renameat2,write", "-o", str(run_dir / "syscalls.txt"), *cmd], env=env, capture_output=True, text=True)
            if trace.returncode:
                raise RuntimeError(trace.stderr)
            count = a.modules * a.functions
            n = 13
            expected = count * n * (n - 1) // 2 + n * count * (count - 1) // 2 + 2 * n * count
            assert subprocess.check_output([str(run_dir / "app")], text=True) == f"{expected}\n"
            print((run_dir / "syscalls.txt").read_text(), flush=True)
    for phase, rows in results.items():
        print(a.label, phase, "medians", *(f"{statistics.median(row[i] for row in rows):.3f}" for i in range(3)), flush=True)


if __name__ == "__main__":
    main()
