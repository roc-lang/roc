#!/usr/bin/env python3
"""Check actual unit reports across producers, modes, edits, and concurrent runs."""

import argparse
from concurrent.futures import ThreadPoolExecutor
import hashlib
import json
import re
import shlex
import shutil
import subprocess
import tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
REPORT_NAMES = ("minici_test.tsv", "build_helpers.tsv")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("zig")
    parser.add_argument("--work-dir", type=Path)
    args, options = parser.parse_known_args()
    if options[:1] == ["--"]:
        options = options[1:]
    work = (args.work_dir or Path(tempfile.mkdtemp(prefix="roc-test-reports-"))).resolve()
    work.mkdir(parents=True, exist_ok=True)
    source = work / "source"
    source.mkdir()
    for directory in ("src", "vendor", "test", "ci"):
        shutil.copytree(ROOT / directory, source / directory,
                        ignore=shutil.ignore_patterns("__pycache__", "*.pyc", "*.pyo"))
    for name in ("build.zig", "build.zig.zon", "design.md", "legal_details", "README.md"):
        shutil.copyfile(ROOT / name, source / name)
    # Declared display metadata is independent of the report inputs and has
    # no Git history in this private source snapshot.
    (source / ".git").mkdir()
    (source / ".git/HEAD").write_text("a" * 40 + "\n")
    cache = work / "cache"
    command = [args.zig, "build", "run-check-zig-test-reports", *options,
               "--cache-dir", str(cache), "--prefix", str(work / "out"),
               "--summary", "all", "--verbose"]
    records = []
    retained = {}
    invocation_dirs = set()

    def run(label, extra=(), failed=False):
        result = subprocess.run(command + list(extra), cwd=source, capture_output=True, text=True)
        output = result.stdout + result.stderr
        (work / f"{label}.log").write_text(output)
        if failed:
            assert result.returncode != 0 and "helpers_test_root.test.report failure control" in output, output
            assert not re.search(r"All \d+ tests passed\.", output), output
            return {"label": label, "argv": command + list(extra), "exitCode": result.returncode,
                    "expectedFailure": True, "namedFailure": "helpers_test_root.test.report failure control"}
        assert result.returncode == 0, output
        reports = {}
        directories = set()
        for line in output.splitlines():
            argv = shlex.split(line.removeprefix("info(verbose): "))
            for value in argv:
                if value.startswith("--roc-test-report-dir="):
                    directories.add(value.split("=", 1)[1])
            if "tests-summary" not in argv or not any(Path(value).name == "roc-build-checks" for value in argv):
                continue
            index = argv.index("tests-summary")
            report_args = argv[index + 3 + int(argv[index + 2]):]
            if "--" in report_args:
                report_args = report_args[:report_args.index("--")]
            for producer, filename in zip(report_args[::2], report_args[1::2]):
                path = Path(filename)
                if not path.is_absolute():
                    path = source / path
                contents = path.read_bytes()
                rows = [row.split("\t", 1) for row in contents.decode().splitlines()]
                assert all(status == "1" for status, _ in rows), rows
                assert path.name == f"{producer}.tsv"
                reports[path.name] = {"path": str(path), "names": [name for _, name in rows],
                                      "sha256": hashlib.sha256(contents).hexdigest()}
        assert set(reports) == set(REPORT_NAMES), output
        assert len(directories) == 1, "Both producers must write into this summary's own temporary directory"
        names = {name: report["names"] for name, report in reports.items()}
        assert set(names[REPORT_NAMES[0]]).isdisjoint(names[REPORT_NAMES[1]])
        counts = re.findall(r"All (\d+) tests passed\.", output)
        assert counts and int(counts[-1]) == sum(map(len, names.values())), output
        return {"label": label, "argv": command + list(extra), "reports": reports,
                "mutableReportDirectory": directories.pop(), "passed": int(counts[-1]),
                "skipped": 0, "failed": 0}

    def check(record, baseline=None, filtered=False):
        directory = record["mutableReportDirectory"]
        assert directory not in invocation_dirs, "Concurrent/repeated summaries shared their mutable destination"
        invocation_dirs.add(directory)
        for name, report in record["reports"].items():
            if filtered:
                assert all("parseMiniArgs" in value for value in report["names"])
            elif baseline is not None:
                assert report["names"] == baseline["reports"][name]["names"]
            retained[Path(report["path"])] = bytes.fromhex(report["sha256"])
        assert record["passed"] > 0, "A filter selected no tests"
        for path, digest in retained.items():
            assert hashlib.sha256(path.read_bytes()).digest() == digest, f"Previously retained report was modified: {path}"
        records.append(record)
        print(f"{record['label']}: {record['passed']} tests, private mutable reports and retained copies")

    baseline = run("baseline")
    check(baseline)
    check(run("unchanged"), baseline)
    filters = ["--", "--test-filter", "parseMiniArgs"]
    check(run("runtime-filter", filters), filtered=True)
    check(run("restored"), baseline)
    mode_options = [value for value in options if value.startswith("-Doptimize=")]
    mode = mode_options[-1].split("=", 1)[1] if mode_options else "Debug"
    alternate = "ReleaseSafe" if mode == "Debug" else "Debug"
    check(run("alternate-mode", [f"-Doptimize={alternate}"]), baseline)
    helper = source / "src/build/helpers_test_root.zig"
    original = helper.read_bytes()
    try:
        helper.write_bytes(original + b'\ntest "report producer source variant" {}\n')
        variant = run("source-variant")
        assert variant["passed"] == baseline["passed"] + 1
        assert "helpers_test_root.test.report producer source variant" in variant["reports"]["build_helpers.tsv"]["names"]
        check(variant)
    finally:
        helper.write_bytes(original)
    check(run("source-restored"), baseline)
    with ThreadPoolExecutor(max_workers=2) as pool:
        full = pool.submit(run, "concurrent-full")
        filtered = pool.submit(run, "concurrent-filtered", filters)
        check(full.result(), baseline)
        check(filtered.result(), filtered=True)
    with ThreadPoolExecutor(max_workers=2) as pool:
        first = pool.submit(run, "concurrent-identical-first")
        second = pool.submit(run, "concurrent-identical-second")
        check(first.result(), baseline)
        check(second.result(), baseline)
    try:
        helper.write_bytes(original + b'\ntest "report failure control" { return error.ReportFailureControl; }\n')
        records.append(run("named-failure", ["--", "--test-filter", "report failure control"], failed=True))
    finally:
        helper.write_bytes(original)
    build = source / "build.zig"
    original_build = build.read_bytes()
    unique = b'.description = "Build helper report isolation fixture",\n        .compile = build_helpers_test,'
    assert original_build.count(unique) == 1
    try:
        build.write_bytes(original_build.replace(unique, unique.replace(b"build_helpers_test", b"minici_compile")))
        negative = subprocess.run([args.zig, "build", "--help", *options, "--cache-dir", str(cache)],
                                  cwd=source, capture_output=True, text=True)
        output = negative.stdout + negative.stderr
        (work / "duplicate-producer.log").write_text(output)
        assert negative.returncode != 0 and "duplicate test report producer: minici_test" in output, output
        records.append({"label": "duplicate-producer", "exitCode": negative.returncode,
                        "expectedFailure": True, "producer": "minici_test"})
    finally:
        build.write_bytes(original_build)
    for path, digest in retained.items():
        assert hashlib.sha256(path.read_bytes()).digest() == digest
    (work / "results.json").write_text(json.dumps(records, indent=2) + "\n")


if __name__ == "__main__":
    main()
