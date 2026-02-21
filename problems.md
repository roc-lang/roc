# Code Review: `lower-mir` Branch

## Test Coverage Gaps

### Most Impactful Gaps (prioritized)

1. **No end-to-end LIR pipeline test** — The full MIR->MirToLir->LIR RC Insert->LirCodeGen->execute chain is never tested as a unit. All fx tests and eval tests use the old mono path.

2. **No test for early_return inside a match branch** — Exercises the interaction between match per-branch RC ops and early_return cleanup.

3. **No test for nested lambdas with captures** — A lambda inside a lambda where the inner captures from the outer scope, exercising `countUsesInto` capture detection.

4. **No tests for `while_loop` with RC symbols** — Loop bodies with refcounted values across iterations.

5. **No tests for `crash`/`runtime_error` in if-then-else branches** — Would test `.noreturn` handling in codegen.

6. **No test for multiple `early_return` expressions in the same block** — Each should independently compute cleanup decrefs.

7. **No test for closure-based self-recursive calls through TailRecursion** — Tests the `isTailCallToTarget` path that checks `closure.self_recursive`.

8. **No aarch64-specific codegen tests** — All tests pick host architecture; no explicit cross-architecture coverage.

9. **No tests for `lowerLowLevel`** — The low-level op mapping (`mapLowLevel`, `lowLevelToBinop`), special cases (`num_is_negative`/`num_is_positive`/`num_is_zero`/`num_negate`), and `emitZeroLiteral`.

10. **No tests for match guards in MirToLir** — All match tests use `MIR.ExprId.none` for guards.

11. **No tests for `lowerForLoop`, `lowerWhileLoop`, `lowerReturn`, `lowerDbg`, `lowerExpect`** — None of the imperative control flow lowering is tested.

12. **No tests for multi-payload single-tag union** — `lowerTag` code path for single-tag unions with multiple payloads (emits tuple) is untested.

13. **No i128/u128 literal edge case tests** — The u128 path, "value fits in i64 but target is u128" path, and negative i128 values.

14. **LirCodeGen tests only verify "code was generated" (non-empty buffer)** — No execution tests that verify generated code produces correct results.

15. **No tests for `match_stmt` in TailRecursion** — `switch_stmt` is tested but `match_stmt` is not.

16. **No tests for float NaN comparison** — NaN equality bug was fixed but no regression test exists yet.

---

## Positive Changes Worth Noting

The branch includes several genuine correctness fixes to existing code:

- **`getI128Parts` signedness fix** — negative i64 values were zero-extended instead of sign-extended
- **`.rem` vs `.mod` rename** — corrects the semantic mismatch (truncated remainder, not mathematical modulus)
- **`putCaptureStruct`/`putCaptureUnion` alignment** — was summing sizes without alignment padding
- **`Span.empty()` initialization** — changed `start: undefined` to `start: 0`
- **`num_from_str` signedness detection** — replaced fragile arithmetic with explicit layout matching
- **Record field reordering** in MirToLir — ensures codegen writes fields to correct offsets
- **Deterministic RC op ordering** via sorted keys
- **LIR `addExpr`/`addPattern` atomicity** — use `ensureUnusedCapacity` + `appendAssumeCapacity` to prevent inconsistent state on OOM
- **LIR `LirStmt` as `union(enum)`** — enables RC insertion to distinguish mutations from declarations
- **Debug assertions** on `.none` IDs, duplicate symbol defs, span length mismatches
- **`dispatchUnionClosure` offset fix** — now reads discriminant from correct layout offset and accesses captures at payload offset 0, instead of hardcoding +8
- **`countUsesInto` match pattern scoping fix** — pattern-bound symbols in match branches now register into the branch-local scope instead of polluting the outer scope
