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
    ("publication", re.compile(r"\bcanonicalizePublished(?:Inner)?\b"), frozenset()),
    ("resolved-canonicalization", re.compile(r"\bcanonicalizeResolved(?:Inner)?\b"), frozenset()),
    ("graph-clone", re.compile(r"\bcloneTypeGraph(?:Inner)?\b"), frozenset()),
    ("layout-facts-file", re.compile(r"\blayout_facts\b"), frozenset()),
    ("layout-facts-export", re.compile(r"\bLayoutFacts\b"), frozenset()),
    ("published-layout-finalizer", re.compile(r"\bfinalizePublishedTypes\b"), frozenset()),
    ("old-clone-inst-file", re.compile(r"\btype_clone_source\b"), frozenset()),
    ("workspace-root", re.compile(r"\bprepareScopedFunctionRoot\b"), frozenset()),
    ("workspace-bind", re.compile(r"\bbindSourceVarToExistingWorkspace\b"), frozenset()),
    ("workspace-ret", re.compile(r"\blookupFunctionNodeRetVar\b"), frozenset()),
    ("workspace-curried-ret", re.compile(r"\blookupCurriedFunctionFinalRetVar\b"), frozenset()),
    ("workspace-call-result", re.compile(r"\bmaterializeAppliedFunctionResultVar\b"), frozenset()),
    ("workspace-align", re.compile(r"\balignSourceVarWithWorkspaceVar\b"), frozenset()),
    ("workspace-bind-content", re.compile(r"\bbindSourceContentToExistingWorkspace\b"), frozenset()),
    ("workspace-materialize-content", re.compile(r"\bmaterializeSourceContentIntoWorkspaceVar\b"), frozenset()),
    ("workspace-merge-flex", re.compile(r"\bmergeSourceFlexLikeIntoWorkspaceVar\b"), frozenset()),
    ("workspace-compute-call-result", re.compile(r"\bcomputeAppliedFunctionResultVar\b"), frozenset()),
    ("module-name-scan", re.compile(r"\bfindModuleIdxByName\b"), frozenset()),
    ("nominal-identity-wrapper", re.compile(r"\bresolveNominalDefiningIdentity\b"), frozenset()),
    ("canonical-source-lookup", re.compile(r"\blookupFnByCanonicalSource\b"), frozenset()),
    ("text-def-lookup-outside-typed-cir", re.compile(r"\btopLevelDefByText\b"), frozenset({"src/check/typed_cir.zig"})),
    ("text-method-lookup-outside-typed-cir", re.compile(r"\bresolveAttachedMethodTargetByText\b"), frozenset({"src/check/typed_cir.zig"})),
    ("text-ident-lookup-outside-typed-cir", re.compile(r"\bfindCommonIdent\b"), frozenset({"src/check/typed_cir.zig"})),
    ("nullable-recorded-dispatch-lowering", re.compile(r"\)\s*std\.mem\.Allocator\.Error!\?LoweredCall\s*\{"), frozenset()),
    ("nullable-attached-method-target", re.compile(r"\)\s*std\.mem\.Allocator\.Error!\?ResolvedTarget\s*\{"), frozenset()),
    ("runtime-error-wrapper", re.compile(r"\bmakeRuntimeErrorExprAt\b"), frozenset()),
    ("monotype-source-fn-arg-walk", re.compile(r"\blookupCurriedFunctionArgVarInStore\(typed_cir_module\.typeStoreConst\(\),"), frozenset()),
    ("monotype-source-fn-ret-walk", re.compile(r"\blookupFunctionRetVarInStore\(typed_cir_module\.typeStoreConst\(\),"), frozenset()),
    ("monotype-source-fn-arity-walk", re.compile(r"\bfunctionArgCountInStore\(typed_cir_module\.typeStoreConst\(\),"), frozenset()),
    ("monotype-source-curried-result-walk", re.compile(r"\blookupCurriedFunctionResultVarInStore\(typed_cir_module\.typeStoreConst\(\),"), frozenset()),
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
            for category, regex, allowed_paths in RULES:
                if rel in allowed_paths:
                    continue
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
