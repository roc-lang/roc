#!/usr/bin/env python3
"""Exercise and measure persistent interface replay on a synthetic application.

Requires a compiler built with -Ddev-diagnostics and a built fx-open platform.
Uses the diagnostic comparison switch to isolate this optimization in the same
binary. Work directories must be new and should live under TMPDIR. No external
project is downloaded or benchmarked. Logs retain timings and work counters.
"""
import argparse
import os
from pathlib import Path
import re
import shutil
import statistics
import subprocess
import time


def counter(log, label):
    return sum(int(n.replace(",", "")) for n in re.findall(rf"^\s+{re.escape(label)}\s+([\d,]+)\s*$", log, re.M))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("roc", type=Path)
    parser.add_argument("--platform", type=Path, required=True)
    parser.add_argument("--work", type=Path, required=True)
    parser.add_argument("--depth", type=int, default=32)
    parser.add_argument("--repeats", type=int, default=5)
    args = parser.parse_args()
    roc = args.roc.resolve()
    work = args.work.resolve()
    work.mkdir(parents=True, exist_ok=False)
    package_hash = "FakeHashAbcDefGhiJkLmNoPqRsTuVwXyZ123456789o"
    samples = {}
    for repeat in range(args.repeats):
        # Alternate the first mode to avoid a systematic ordering advantage.
        modes = ("before", "after") if repeat % 2 == 0 else ("after", "before")
        for mode in modes:
            run = work / f"{mode}-{repeat}"
            source = run / "source"
            source.mkdir(parents=True)
            cache = run / "cache"
            shutil.copytree(args.platform, cache / "roc/packages" / package_hash)
            env = dict(os.environ, ROC_CACHE_DIR=str(cache), XDG_CACHE_HOME=str(cache))
            env.pop("ROC_DISABLE_PERSISTENT_INTERFACES", None)
            if mode == "before":
                env["ROC_DISABLE_PERSISTENT_INTERFACES"] = "1"
            for phase in ("cold", "app_edit", "callee_edit", "dependency_edit", "ctfe_edit", "corrupt"):
                delta = 1 if phase in ("cold", "app_edit") else 2
                dependency_delta = 1 if phase in ("dependency_edit", "ctfe_edit", "corrupt") else 0
                offset_arg = 3 if phase in ("ctfe_edit", "corrupt") else 2
                main_arg = 10 if phase == "cold" else 11
                (source / "Leaf.roc").write_text(f"Leaf := [].{{\nadd = |x| x + {dependency_delta}\n}}\n")
                helpers = [f"f0 = |x| Leaf.add(x) + {delta}"]
                helpers += [f"f{i} = |x| f{i - 1}(x) + 1" for i in range(1, args.depth)]
                (source / "Generic.roc").write_text("import Leaf\nGeneric := [].{\n" + "\n".join(helpers) + "\n}\n")
                app = f'''app [main!] {{ pf: platform "https://example.com/interface/{package_hash}.tar.zst" }}
import pf.Stdout
import Generic

offset : U64
offset = {{
    dbg "interface cache observation"
    Generic.f{args.depth - 1}({offset_arg})
}}

unused : U64
unused = crash "unused failing interface root"

main! = |args| {{
    total = Generic.f{args.depth - 1}(args.len().to_u64() + {main_arg}) + offset
    Stdout.line!(total.to_str())
    Ok({{}})
}}
'''
                (source / "main.roc").write_text(app)
                if phase == "corrupt":
                    for record in cache.rglob("*.ris"):
                        record.write_bytes(b"RIFACE02truncated")
                    for output in cache.rglob("*.rpo"):
                        output.unlink()
                exe = run / f"{phase}-app"
                command = [str(roc), "build", "--opt=dev", "--jobs=2", "--timings", str(source / "main.roc"), f"--output={exe}"]
                start = time.perf_counter_ns()
                result = subprocess.run(command, env=env, capture_output=True, text=True)
                elapsed = (time.perf_counter_ns() - start) / 1e6
                log = result.stdout + result.stderr
                (run / f"{phase}.log").write_text(log)
                if result.returncode:
                    raise RuntimeError(log)
                assert "unused failing interface root" not in log, log
                assert log.count("interface cache observation") == 1, log
                expected = 1 + main_arg + offset_arg + 2 * (args.depth - 1 + delta + dependency_delta)
                actual = subprocess.check_output([str(exe)], text=True)
                assert actual == f"{expected}\n", (phase, expected, actual)
                hits = counter(log, "Persistent interface hits")
                expansions = counter(log, "Interface summary expansions")
                if mode == "before":
                    assert hits == 0, log
                if mode == "after" and phase == "app_edit":
                    assert hits > 0, "application edit reused no persistent interface"
                key = (mode, phase)
                samples.setdefault(key, []).append((elapsed, hits, expansions))
                records = list(cache.rglob("*.ris"))
                print(mode, repeat, phase, f"wall_ms={elapsed:.3f} hits={hits} expansions={expansions} files={len(records)} bytes={sum(p.stat().st_size for p in records)}", flush=True)
    for (mode, phase), rows in samples.items():
        print(mode, phase, "median", *(f"{statistics.median(row[i] for row in rows):.3f}" for i in range(3)), flush=True)
    before = statistics.median(row[2] for row in samples["before", "app_edit"])
    after = statistics.median(row[2] for row in samples["after", "app_edit"])
    assert after < before, ("persistent replay did not reduce interface expansion", before, after)


if __name__ == "__main__":
    main()
