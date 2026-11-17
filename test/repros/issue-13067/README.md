# Issue 13067: imported nominal record update diagnostic

Issue: https://github.com/roc-lang/roc/issues/13067

**Not reproduced in the current checkout.** With Zig 0.17.1 and the Debug
compiler reporting `debug-7c9ea425`, the issue's exact three-file input
produces the intended diagnostic, not a crash or a wrong-field report.
No compiler changes were needed for this issue.

## Reproduction attempt and regression coverage

`main.roc`, `Model.roc`, and `Project.roc` retain the issue's package and
imported nominal declaration, including its `Other` alias.

```sh
zig build roc
zig-out/bin/roc check --no-cache test/repros/issue-13067/Project.roc
```

Exit 1, as appropriate for invalid source:

```text
The type of the field items is incompatible.

You are trying to update the items field to be the type:

    List({ extra: Bool, name: Str })

But it should be:

    List({ name: Str })
```

Twelve additional fresh-directory runs using `--no-cache --jobs=1`
all return the same correct diagnostic. Ordinary worker selection was
also checked.

The new test in `src/check/test/cross_module_test.zig:8` uses the existing
cross-module `TestEnv`, asserts exactly one type error, and requires the
incompatible-`items` message and the extra field. It passes:

```sh
zig build -j2 run-test-zig-module-check \
  -Dtest-filter='issue 10886' -Dtest-filter='issue 13067' -- \
  --test-filter 'issue 10886' --test-filter 'issue 13067'
```

The initial combined reproduction run had two failing reproductions for
10886; 13067 and the two existing 10886 controls passed. After implementing
the 10886 fix, the full checker module passes, including all of these tests.

## Current mechanism and find-fix conclusion

The current code preserves the distinction between outer record-update
diagnostic ownership and the nested list-element mismatch:

- `Check.resumeRecordUpdateCheck` (`src/check/Check.zig:33441`) lends the
  base field as construction context but gives the full base/probe relation
  ownership of the final diagnostic (`33475`, `33517`).
- `projectExpectedAggregateShape` (`31517`) and
  `commitProjectedStoredValue` (`31572`) roll back unsuccessful contextual
  relations rather than publishing a partial child mismatch as the
  outer record-update error.
- `unifyRecordInContext` (`9066`) requests expected-record evidence.
  `retainOuterRecordMismatch` (`src/check/unify.zig:2899`) retains the
  actual opened nominal backing for the owning outer relation.
- `buildRecordUpdate` (`src/check/report.zig:4543`) reads frozen record
  evidence and renders the field payloads, which correctly show the
  differing list-element records.

The symptom in the issue is consistent with confusing these ownership
levels, but this investigation does **not** establish the original
nightly's exact faulty instruction or a historical fixing commit. No
historical Git diagnostics were used.

The recommended action is to retain this passing regression coverage and
the current producer-owned, frozen diagnostic-evidence contract, not to
change functioning compiler code speculatively. A further compiler fix
requires an input that still fails here. There is no confirmed current
defect for which a new correctness/performance fix can honestly be proposed.
