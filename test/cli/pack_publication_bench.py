#!/usr/bin/env python3
"""Measure native publication with a ReleaseFast diagnostic compiler on Linux.

    zig build -j2 roc -Doptimize=ReleaseFast -Ddev-diagnostics
    python3 test/cli/pack_publication_bench.py zig-out/bin/roc

Uses a built test/fx-open platform (or --platform). GNU time measures the whole
process, including maintenance; operation counts measure publication separately.
Only this disposable fixture's completed-command records are removed, forcing
native compilation/publication while preserving checked and procedure caches.
The test command runs independently; this fixture's runtime tests currently
publish no native offers, so its empty publication counters are reported
explicitly, not credited as a publication improvement. No timing thresholds
are asserted on the shared machine.
"""
import argparse
import os
from pathlib import Path
import re
import subprocess
import tempfile


def counters(stderr, prefix):
    result = {}
    for line in stderr.splitlines():
        if line.startswith(prefix):
            for key, value in re.findall(r"(\w+)\s*=\s*(\d+)", line):
                result[key] = result.get(key, 0) + int(value)
    return result


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("roc", type=Path)
    parser.add_argument("--platform", type=Path, default=Path(__file__).resolve().parents[1] / "fx-open/platform/main.roc")
    parser.add_argument("--callers", type=int, default=24)
    args = parser.parse_args()
    assert 1 <= args.callers <= 128
    roc = args.roc.resolve()
    with tempfile.TemporaryDirectory(prefix="roc-native-publication-") as tmp:
        work = Path(tmp)
        (work / "Shared.roc").write_text('''Shared := [].{
    leaf : U64 -> U64
    leaf = |limit| {
        var $sum = 0
        var $i = 0
        while $i < limit {
            $sum = $sum + $i
            $i = $i + 1
        }
        $sum
    }
}
''')
        def callers(edited):
            for i in range(args.callers):
                increment = i + int(edited and i == 0)
                (work / f"Caller{i}.roc").write_text(f'''import Shared
Caller{i} := [].{{
    run : U64 -> U64
    run = |n| {{
        var $sum = {increment}
        var $i = 0
        while $i < n {{
            $sum = $sum + Shared.leaf($i)
            $i = $i + 1
        }}
        $sum
    }}
}}
''')
        imports = "".join(f"import Caller{i}\n" for i in range(args.callers))
        expr = " + ".join(f"Caller{i}.run(n)" for i in range(args.callers))
        platform = os.path.relpath(args.platform.resolve(), work)
        app = work / "main.roc"
        app.write_text(f'''app [main!] {{ pf: platform "{platform}" }}
import pf.Stdout
{imports}
main! = |args| {{
    n = args.len().to_u64() + 3
    Stdout.line!(({expr}).to_str())
    Ok({{}})
}}
''')
        test = work / "Tests.roc"
        def tests(edited):
            test.write_text(f"Tests := []\n{imports}" + "".join(
                f"expect Caller{i}.run(4) == {4 + i + int(edited and i == 0)}\n"
                for i in range(args.callers)))
        for command in ("build", "test"):
            cache = work / (command + "-cache")
            env = dict(os.environ, ROC_CACHE_DIR=str(cache), ROC_PACK_STATS="1")
            cold = None
            for phase, edited in (("cold", False), ("unchanged", False), ("one-caller edit", True), ("unchanged after edit", True)):
                callers(edited)
                tests(edited)
                # Do not count completed-result hits as native publication.
                for path in cache.rglob("*"):
                    if path.is_file() and (path.suffix == ".rpo" or "test" in path.relative_to(cache).parts):
                        path.unlink()
                cmd = [str(roc), command, "--opt=dev", "--jobs=1", str(app if command == "build" else test)]
                if command == "build":
                    cmd.append(f"--output={work / 'app'}")
                run = subprocess.run(["/usr/bin/time", "-f", "%e %M", "-o", str(work / "time"), *cmd], env=env, capture_output=True, text=True)
                if run.returncode:
                    raise RuntimeError(run.stdout + run.stderr)
                publication = counters(run.stderr, "pack publication:")
                acquired = counters(run.stderr, "pack acquisitions:")
                if command == "build" and not publication:
                    raise RuntimeError("No native publication measured. Use a ReleaseFast compiler built with -Ddev-diagnostics.\n" + run.stdout + run.stderr)
                if command == "build":
                    # argv contains the executable name, hence n == 4.
                    expected = 4 * args.callers + args.callers * (args.callers - 1) // 2 + int(edited)
                    assert subprocess.check_output([str(work / "app")], text=True) == f"{expected}\n"
                if cold is None:
                    cold = publication
                elif command == "build":
                    assert publication["validated_references"] > 0, publication
                    assert publication["payloads_encoded"] < cold["payloads_encoded"], (cold, publication)
                wall, rss = (work / "time").read_text().split()
                print(command, phase, f"wall={wall}s peak_rss={rss}KiB", f"acquired={acquired}", f"publication={publication}", flush=True)


if __name__ == "__main__":
    main()
