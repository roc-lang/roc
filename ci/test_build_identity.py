#!/usr/bin/env python3
"""Check declared build inputs in a mutable checkout, without compiling Roc.

Usage: python3 ci/test_build_identity.py /path/to/zig [additional build options]
Do not edit compiler sources concurrently with this check.
"""

import re
import shlex
import subprocess
import sys
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
ZIG = sys.argv[1]
BUILD_OPTIONS = sys.argv[2:]
PRODUCTION = ROOT / "src/compile/roc_build_identity_probe.zig"
TEST_ONLY = ROOT / "src/compile/test/roc_build_identity_probe.zig"
TEST_RUNNER = ROOT / "vendor/zig_test_runner.zig"


def git_head():
    return subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=ROOT, text=True).strip()


def identity(label):
    result = subprocess.run(
        [ZIG, "build", "run-test-zig-minici", "--verbose", "--summary", "all",
         "--cache-poison=disallowed", *BUILD_OPTIONS, "--",
         "--test-filter=__build_identity_probe_no_tests__"],
        cwd=ROOT, capture_output=True, text=True,
    )
    if result.returncode:
        raise RuntimeError(result.stdout + result.stderr)
    # Query the actual module input, including when the Run output was cached.
    # Output mtimes cannot identify a reused artifact after a source revert.
    for line in result.stderr.splitlines():
        if "-Mcompiler_identity=" not in line:
            continue
        argument = next(arg for arg in shlex.split(line) if arg.startswith("-Mcompiler_identity="))
        path = Path(argument.split("=", 1)[1])
        if not path.is_absolute():
            path = ROOT / path
        match = re.search(r'compiler_compatibility_id = "([0-9a-f]{64})"', path.read_text())
        if match:
            print(f"{label}: {match.group(1)}")
            return match.group(1)
    raise RuntimeError("compiler identity module was absent from verbose build output")


def main():
    assert not PRODUCTION.exists() and not TEST_ONLY.exists(), "probe paths must be absent"
    head = git_head()
    try:
        baseline = identity("baseline")
        assert identity("unchanged") == baseline
        TEST_ONLY.write_text("// dedicated test-only identity probe\n")
        assert identity("test added") == baseline
        TEST_ONLY.write_text("// dirty test-only identity probe\n")
        assert identity("test edited") == baseline
        TEST_ONLY.unlink()
        assert identity("test deleted") == baseline
        original_runner = TEST_RUNNER.read_bytes()
        try:
            TEST_RUNNER.write_bytes(original_runner + b"\n// Test-only runner identity probe.\n")
            assert identity("vendored test runner edited") == baseline
        finally:
            TEST_RUNNER.write_bytes(original_runner)
        assert identity("vendored test runner restored") == baseline
        PRODUCTION.write_text("// declared production identity probe\n")
        added = identity("production added")
        assert added != baseline
        PRODUCTION.write_text("// dirty production identity probe\n")
        edited = identity("production edited")
        assert edited != added and edited != baseline
        PRODUCTION.unlink()
        assert identity("production deleted") == baseline
        assert git_head() == head, "HEAD must remain unchanged during the check"
    finally:
        PRODUCTION.unlink(missing_ok=True)
        TEST_ONLY.unlink(missing_ok=True)
    print("Build input identity checks passed.")


if __name__ == "__main__":
    main()
