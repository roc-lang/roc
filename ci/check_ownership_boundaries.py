#!/usr/bin/env python3
"""Scan Zig sources for ownership-boundary violations outside allowlisted sites.

This check is intentionally structural. It does not attempt to infer intent;
it flags ownership-sensitive operations by pattern and only suppresses matches
through explicit checked-in allowlist entries.
"""

from __future__ import annotations

import pathlib
import re
from dataclasses import dataclass


ROOT = pathlib.Path(__file__).resolve().parent.parent
ALLOWLIST_PATH = ROOT / "ci" / "ownership_boundary_allowlist.txt"


@dataclass(frozen=True)
class Rule:
    category: str
    path_prefixes: tuple[str, ...]
    regex: re.Pattern[str]
    description: str


@dataclass(frozen=True)
class AllowEntry:
    category: str
    path: str
    function: str
    reason: str


RULES = (
    Rule(
        category="interpreter-perform-rc",
        path_prefixes=("src/eval/interpreter.zig",),
        regex=re.compile(r"\bperform(?:Raw)?Rc(?:Plan)?\("),
        description="interpreter RC outside explicit RC handlers or builtin/runtime internals",
    ),
    Rule(
        category="dev-emit-rc-helper",
        path_prefixes=("src/backend/dev/LirCodeGen.zig",),
        regex=re.compile(r"\b(?:emit(?:Raw)?Rc(?:HelperCall(?:AtStackOffset|ForValue|FromPtrReg)?|IncrefAtStackOffset|DecrefAtStackOffset)|emitBuiltinInternalOptionalRcHelperAddress|generateBuiltinInternalRcHelperBody|compileBuiltinInternalRcHelper)\("),
        description="dev backend RC emission outside explicit RC lowering or builtin lowering",
    ),
    Rule(
        category="wasm-emit-rc-helper",
        path_prefixes=("src/backend/wasm/WasmCodeGen.zig",),
        regex=re.compile(r"\b(?:emit(?:Raw)?Rc(?:ForValueLocal|HelperCallForValuePtr|AtPtr|HelperCallByKey)|generateBuiltinInternalRcHelperBody|compileBuiltinInternalRcHelper)\("),
        description="wasm backend RC emission outside explicit RC lowering or builtin lowering",
    ),
    Rule(
        category="ordinary-layout-refcount-branch",
        path_prefixes=(
            "src/eval/interpreter.zig",
            "src/backend/dev/LirCodeGen.zig",
            "src/backend/wasm/WasmCodeGen.zig",
        ),
        regex=re.compile(r"\b(?:layoutContainsRefcounted|containsRefcounted)\("),
        description="ordinary interpreter/backend ownership reasoning via refcounted-layout checks",
    ),
    Rule(
        category="wasm-raw-rc-helper-call",
        path_prefixes=("src/backend/wasm/WasmCodeGen.zig",),
        regex=re.compile(r"\bemitRawRcHelperCallForValuePtr\("),
        description="wasm raw RC helper emission outside wrapper/builtin/helper infrastructure",
    ),
)


FN_RE = re.compile(r"^\s*fn\s+([A-Za-z0-9_]+)\s*\(")


def load_allowlist() -> list[AllowEntry]:
    entries: list[AllowEntry] = []
    for raw in ALLOWLIST_PATH.read_text().splitlines():
        line = raw.strip()
        if not line or line.startswith("#"):
            continue
        parts = [part.strip() for part in line.split("|", 3)]
        if len(parts) != 4:
            raise SystemExit(f"invalid ownership allowlist entry: {raw}")
        entries.append(AllowEntry(*parts))
    return entries


def iter_zig_files() -> list[pathlib.Path]:
    files: list[pathlib.Path] = []
    for base in ("src", "test"):
        for path in sorted((ROOT / base).rglob("*.zig")):
            if ".zig-cache" in path.parts:
                continue
            files.append(path)
    return files


def current_function(lines: list[str], line_index: int) -> str:
    idx = line_index
    while idx >= 0:
        match = FN_RE.match(lines[idx])
        if match:
            return match.group(1)
        idx -= 1
    return "<top-level>"


def path_matches(path: str, prefixes: tuple[str, ...]) -> bool:
    return any(path == prefix or path.startswith(prefix + "/") for prefix in prefixes)


def is_allowlisted(entries: list[AllowEntry], category: str, path: str, function: str) -> bool:
    for entry in entries:
        if entry.category == category and entry.path == path and entry.function == function:
            return True
    return False


def main() -> int:
    allowlist = load_allowlist()
    violations: list[str] = []

    for path in iter_zig_files():
        rel = path.relative_to(ROOT).as_posix()
        source = path.read_text()
        lines = source.splitlines()
        for rule in RULES:
            if not path_matches(rel, rule.path_prefixes):
                continue
            for line_index, line in enumerate(lines):
                if not rule.regex.search(line):
                    continue
                function = current_function(lines, line_index)
                if is_allowlisted(allowlist, rule.category, rel, function):
                    continue
                violations.append(
                    f"{rel}:{line_index + 1}: {rule.category}: {rule.description} "
                    f"(function={function})"
                )

    if violations:
        print("Ownership boundary violations found:")
        for violation in violations:
            print(violation)
        return 1

    print("Ownership boundary check passed.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
