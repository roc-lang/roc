#!/usr/bin/env python3
"""Forbid reintroducing deleted post-check lowering machinery.

This check is intentionally structural. These APIs represent the old
publication/workspace-remapping architecture and must not exist anywhere in
compiler code once the cor-style pipeline is in place.
"""

from __future__ import annotations

import pathlib
import re


ROOT = pathlib.Path(__file__).resolve().parent.parent

RULES = (
    ("publication", re.compile(r"\bcanonicalizePublished(?:Inner)?\b")),
    ("resolved-canonicalization", re.compile(r"\bcanonicalizeResolved(?:Inner)?\b")),
    ("graph-clone", re.compile(r"\bcloneTypeGraph(?:Inner)?\b")),
    ("workspace-root", re.compile(r"\bprepareScopedFunctionRoot\b")),
    ("workspace-bind", re.compile(r"\bbindSourceVarToExistingWorkspace\b")),
    ("workspace-ret", re.compile(r"\blookupFunctionNodeRetVar\b")),
    ("workspace-curried-ret", re.compile(r"\blookupCurriedFunctionFinalRetVar\b")),
    ("workspace-call-result", re.compile(r"\bmaterializeAppliedFunctionResultVar\b")),
    ("canonical-source-lookup", re.compile(r"\blookupFnByCanonicalSource\b")),
)


def iter_zig_files() -> list[pathlib.Path]:
    files: list[pathlib.Path] = []
    for base in ("src", "test"):
        for path in sorted((ROOT / base).rglob("*.zig")):
            if ".zig-cache" in path.parts:
                continue
            files.append(path)
    return files


def main() -> int:
    violations: list[str] = []
    for path in iter_zig_files():
        rel = path.relative_to(ROOT).as_posix()
        for line_no, line in enumerate(path.read_text().splitlines(), start=1):
            for category, regex in RULES:
                if regex.search(line):
                    violations.append(f"{rel}:{line_no}: {category}: {line.strip()}")

    if violations:
        print("Post-check architecture violations found:")
        for violation in violations:
            print(violation)
        return 1

    print("Post-check architecture check passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
