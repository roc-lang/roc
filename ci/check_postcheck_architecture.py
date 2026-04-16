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
    ("layout-facts-file", re.compile(r"\blayout_facts\b")),
    ("layout-facts-export", re.compile(r"\bLayoutFacts\b")),
    ("published-layout-finalizer", re.compile(r"\bfinalizePublishedTypes\b")),
    ("old-clone-inst-file", re.compile(r"\btype_clone_source\b")),
    ("workspace-root", re.compile(r"\bprepareScopedFunctionRoot\b")),
    ("workspace-bind", re.compile(r"\bbindSourceVarToExistingWorkspace\b")),
    ("workspace-ret", re.compile(r"\blookupFunctionNodeRetVar\b")),
    ("workspace-curried-ret", re.compile(r"\blookupCurriedFunctionFinalRetVar\b")),
    ("workspace-call-result", re.compile(r"\bmaterializeAppliedFunctionResultVar\b")),
    ("workspace-align", re.compile(r"\balignSourceVarWithWorkspaceVar\b")),
    ("workspace-bind-content", re.compile(r"\bbindSourceContentToExistingWorkspace\b")),
    ("workspace-materialize-content", re.compile(r"\bmaterializeSourceContentIntoWorkspaceVar\b")),
    ("workspace-merge-flex", re.compile(r"\bmergeSourceFlexLikeIntoWorkspaceVar\b")),
    ("workspace-compute-call-result", re.compile(r"\bcomputeAppliedFunctionResultVar\b")),
    ("module-name-scan", re.compile(r"\bfindModuleIdxByName\b")),
    ("nominal-identity-wrapper", re.compile(r"\bresolveNominalDefiningIdentity\b")),
    ("canonical-source-lookup", re.compile(r"\blookupFnByCanonicalSource\b")),
    ("nullable-recorded-dispatch-lowering", re.compile(r"\)\s*std\.mem\.Allocator\.Error!\?LoweredCall\s*\{")),
    ("nullable-attached-method-target", re.compile(r"\)\s*std\.mem\.Allocator\.Error!\?ResolvedTarget\s*\{")),
    ("runtime-error-wrapper", re.compile(r"\bmakeRuntimeErrorExprAt\b")),
    ("monotype-source-fn-arg-walk", re.compile(r"\blookupCurriedFunctionArgVarInStore\(typed_cir_module\.typeStoreConst\(\),")),
    ("monotype-source-fn-ret-walk", re.compile(r"\blookupFunctionRetVarInStore\(typed_cir_module\.typeStoreConst\(\),")),
    ("monotype-source-fn-arity-walk", re.compile(r"\bfunctionArgCountInStore\(typed_cir_module\.typeStoreConst\(\),")),
    ("monotype-source-curried-result-walk", re.compile(r"\blookupCurriedFunctionResultVarInStore\(typed_cir_module\.typeStoreConst\(\),")),
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
