# Issue 13105: producer evidence through a shared error row

Issue: https://github.com/roc-lang/roc/issues/13105

`test/cli/Issue13105WhereRowProducer.roc` is a standalone form of the issue:
a `match` on the result of a generic helper names a tag (`Gone`) that reaches
the matched row only through the caller's `where` clause. The branch runs, and
no Unmatchable Pattern warning is reported for it.

The matched row and the `where` requirement's row share one extension. The
unifier rebuilds a related row flat, so the extension keeps its own entry for
`Gone` while the requirement's merged row holds a copy, and the producer in
the caller's `where` clause clears only the copy it meets. The unifier
journals such copies of a marked tag, and the judgment reads a tag as
pattern-only only when no entry journaled as the same tag was produced
(design.md "Pattern-Only Tags").

The shape, without methods or nominal types:

```text
errors      = [Gone(pattern-only), ..tail]
requirement = [Bad, ..errors]
result      = [Bad, ..errors]
declared    = [Bad, Gone(produced), ..other_tail]
unify(requirement, declared)
```

`src/check/test/issue_13105_test.zig` covers the source program and a tag no
`where` clause produces, which is still reported. The journal itself is
covered by the unifier tests in `src/check/test/unify_test.zig`. The CLI suite
runs the program under both lowering strategies ("issue 13105" in
`src/cli/test/parallel_cli_runner.zig`).

## Focused validation

Run from the repository root:

```sh
zig build run-test-zig-module-check -Dtest-filter='issue 13105' -- --test-filter 'issue 13105'
zig build roc
zig-out/bin/roc test --no-cache test/cli/Issue13105WhereRowProducer.roc
```
