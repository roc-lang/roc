#!/usr/bin/env python3
"""Check actual Zig test reports across two producers and runtime filters."""

import argparse
import json
import re
import subprocess
import tempfile
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("zig")
    parser.add_argument("--work-dir", type=Path)
    args, options = parser.parse_known_args()
    if options[:1] == ["--"]:
        options = options[1:]
    work = (args.work_dir or Path(tempfile.mkdtemp(prefix="roc-test-reports-"))).resolve()
    work.mkdir(parents=True, exist_ok=True)
    cache = work / "cache"
    command = [args.zig, "build", "run-check-zig-test-reports", *options,
               "--cache-dir", str(cache), "--prefix", str(work / "out"),
               "--summary", "all"]
    records = []
    baseline_names = None
    for label, filters in (("baseline", []), ("unchanged", []),
                           ("runtime-filter", ["--", "--test-filter", "parseMiniArgs"]),
                           ("restored", [])):
        run = subprocess.run(command + filters, cwd=ROOT, capture_output=True, text=True)
        output = run.stdout + run.stderr
        (work / f"{label}.log").write_text(output)
        assert run.returncode == 0, output
        reports = {}
        for filename in ("minici_test.tsv", "build_helpers.tsv"):
            paths = list((cache / "o").glob(f"*/{filename}"))
            assert paths, f"Missing declared report: {filename}"
            path = max(paths, key=lambda value: value.stat().st_mtime_ns)
            rows = [line.split("\t", 1) for line in path.read_text().splitlines()]
            assert all(status == "1" for status, _ in rows), rows
            reports[filename] = {"path": str(path), "names": [name for _, name in rows]}
        names = {filename: report["names"] for filename, report in reports.items()}
        if filters:
            assert names["minici_test.tsv"], "Filter selected no MiniCI tests"
            assert all("parseMiniArgs" in name for group in names.values() for name in group)
        else:
            assert all(names.values()), "A producer ran no tests"
            assert set(names["minici_test.tsv"]).isdisjoint(names["build_helpers.tsv"])
            if baseline_names is None:
                baseline_names = names
            else:
                assert names == baseline_names
        counts = re.findall(r"All (\d+) tests passed\.", output)
        assert counts, "Missing aggregate test summary"
        passed = int(counts[-1])
        assert passed == sum(map(len, names.values()))
        records.append({"label": label, "argv": command + filters, "reports": reports,
                        "passed": passed, "skipped": 0, "failed": 0})
        print(f"{label}: {passed} tests, two distinct reports")
    (work / "results.json").write_text(json.dumps(records, indent=2) + "\n")


if __name__ == "__main__":
    main()
