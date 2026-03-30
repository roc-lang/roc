# MIR/LIR/Codegen Correctness Checklist

Use this after making fixes. Only check an item when the `Problematic state` is gone and the `Should look like` condition is true.

1. [ ] `expect` failure path in dev codegen must be non-returning.
Problematic state: `expect` failure calls `roc_crashed` but can continue if that callback returns (`src/backend/dev/LirCodeGen.zig` around lines 10020/10024; callback is `void` in `src/builtins/host_abi.zig` around line 83).
Should look like: Any `expect` failure path is guaranteed to stop control flow immediately (e.g. explicit trap/unreachable after callback, or ABI-level non-returning contract enforced end-to-end).
How to verify: In `LirCodeGen.zig`, there is no reachable continuation path after emitting/calling the crash routine.

2. [ ] Runtime-error path in `generateLookupCall` must not return dummy values.
Problematic state: On error, code emits crash call and then returns placeholder `i64` (`src/backend/dev/LirCodeGen.zig` around lines 12980-12983).
Should look like: Runtime-error path is terminal and never fabricates a value to keep execution going.
How to verify: `generateLookupCall` has no "crash + fallback return value" branch.

3. [ ] Unresolved-symbol codegen path must not rely on raw `unreachable` as recovery.
Problematic state: Unresolved symbol path uses `unreachable` (`src/backend/dev/LirCodeGen.zig` around line 5001).
Should look like: Either the invariant is proven before codegen entry, or there is an explicit debug-only assertion at the invariant boundary with no late-stage recovery branch.
How to verify: Unresolved-symbol handling is removed from deep codegen path or replaced by a clear invariant assertion point.

4. [ ] Unimplemented low-level ops must not runtime-panic in the backend.
Problematic state: Several low-level ops hit panic paths at codegen time (`src/backend/dev/LirCodeGen.zig` around lines 3692-3705).
Should look like: Unsupported ops are rejected earlier by invariant checks, and backend codegen no longer contains runtime panic fallback for these ops.
How to verify: No "TODO/unimplemented panic" branch remains for those low-level op cases.

5. [ ] Discriminant switch generation TODO fallback must be eliminated.
Problematic state: Codegen still has a TODO fallback `if/else` chain for discriminants (`src/backend/dev/LirCodeGen.zig` around lines 10401-10403).
Should look like: Deterministic, complete discriminant-switch lowering with no temporary fallback logic.
How to verify: TODO fallback branch is gone and replaced by final switch strategy.

6. [ ] Procedure lookup must not silently degrade to O(N) scan.
Problematic state: Call path falls back to linear scan over procedures (`src/backend/dev/LirCodeGen.zig` around lines 13010-13020).
Should look like: Call resolution is deterministic via direct index/map, and missing entries fail fast via invariant assertion (no silent slow-path recovery).
How to verify: No O(N) scan fallback remains in normal call emission path.

7. [ ] `str_inspect` naming must not degrade to `"?"` placeholders.
Problematic state: Multiple MIR->LIR locations hardcode unknown names as `"?"` (`src/lir/MirToLir.zig` around lines 2013, 2077, 2250, 2296-2297, 2314).
Should look like: Either stable identifier-free formatting is used by design, or real names are propagated from allowed data sources; no placeholder fallback strings.
How to verify: No production path hardcodes `"?"` for inspect-name recovery.

8. [ ] `lookup_required` resolution must not be string-name heuristics plus runtime error type fallback.
Problematic state: `lookup_required` logic matches names by text and falls back to `runtime_err_type` (`src/mir/Lower.zig` around lines 642, 652, 661).
Should look like: Resolution uses explicit typed identity, and unresolved cases fail via invariant checks instead of type-level recovery placeholders.
How to verify: No text-based matching + `runtime_err_type` recovery remains in this path.

9. [ ] Method dispatch misses in MIR must not fabricate `runtime_err_type`.
Problematic state: Dispatch miss cases fall back to `runtime_err_type` (`src/mir/Lower.zig` around lines 1752, 1973).
Should look like: Dispatch table must be complete for reachable calls; misses trigger invariant failure at construction time.
How to verify: Miss branches no longer return/propagate `runtime_err_type`.

10. [ ] Pending/external lookup error paths must not end in generic `unreachable`.
Problematic state: `e_lookup_pending` and unresolved external import paths currently use `unreachable` (`src/mir/Lower.zig` around lines 580-581, 632-636).
Should look like: These are prevented or explicitly diagnosed at an earlier invariant boundary with loud debug assertions.
How to verify: No raw `unreachable` remains for user-reachable unresolved lookup states.

11. [ ] Typed fraction fallback must not default silently to `Dec`.
Problematic state: One typed-frac path falls back to `Dec` (`src/mir/Lower.zig` around lines 341-347).
Should look like: Fraction type is derived from real constraints or rejected by invariant assertion; no silent default type substitution.
How to verify: No "if unknown then Dec" fallback behavior remains.

12. [ ] Nominal compatibility must not default `true` for non-builtin nominals.
Problematic state: Compatibility check returns unconditional `true` outside builtin cases (`src/mir/Lower.zig` around lines 2368-2377).
Should look like: Compatibility is computed from explicit nominal identity/rules; unknown cases fail invariant checks instead of permissive success.
How to verify: No unconditional success branch for non-builtin nominal compatibility.

13. [ ] Def lookup by symbol must not match only `ident.idx`.
Problematic state: `findDefExprBySymbol` effectively matches only identifier index and ignores attributes (`src/mir/Lower.zig` around lines 2137-2139).
Should look like: Symbol identity comparison is complete and collision-safe for all fields that define uniqueness.
How to verify: Lookup key includes full symbol identity, not a partial projection.

14. [ ] Missing symbol metadata must not depend on debug panic + release `unreachable`.
Problematic state: Lowering path can panic in debug and hit `unreachable` in release when symbol metadata is missing (`src/mir/Lower.zig` around lines 284-291).
Should look like: Missing metadata is impossible by construction at this stage, with checks concentrated at data-construction boundaries.
How to verify: No deep lowering path has to recover from or branch on absent symbol metadata.

15. [ ] Type-var seeding must not silently ignore OOM/error (`catch {}` / `catch return`).
Problematic state: OOM/error is dropped in several type-var seeding paths (`src/mir/Lower.zig` around lines 1777, 2333, 2345).
Should look like: Allocation failures are propagated or explicitly surfaced; invariants are not silently weakened on allocation error.
How to verify: No empty `catch` or silent early-return remains in these seeding paths.

16. [ ] Monotype flex/rigid fallback defaults must be removed.
Problematic state: Flex/rigid type handling can default to `unit`/`dec` (`src/mir/Monotype.zig` around lines 347-356).
Should look like: Flex/rigid are resolved by constraints or rejected; no fallback concrete-type substitution.
How to verify: No branch maps unresolved flex/rigid directly to default concrete monotypes.

17. [ ] Tag-union row-extension walk must not truncate on alias/flex/rigid/error fallback nodes.
Problematic state: Row-extension traversal stops on alias/flex/rigid/err-like states (`src/mir/Monotype.zig` around lines 537-549).
Should look like: Traversal either fully resolves row tails or reports invariant violation; no partial truncation fallback.
How to verify: Traversal no longer treats unresolved tails as successful termination.

18. [ ] `NominalHint` metadata must not leak module identity into monotype-era logic.
Problematic state: `NominalHint` stores module-indexed identity (`src/mir/Monotype.zig` around lines 184-187, 194, 680-683) and is consumed in Lower (`src/mir/Lower.zig` around lines 2087-2092).
Should look like: MIR/monotype nominal identity is module-agnostic (or opaque symbol-based) post-lowering; module provenance does not survive as a required runtime key.
How to verify: No MIR/monotype API requires module index to interpret nominal identity.

19. [ ] LIR symbol-def registration must not permit overwrite in release.
Problematic state: LIR store has debug-only duplicate assert but can overwrite in release (`src/lir/LirExprStore.zig` around lines 389-391).
Should look like: Duplicate registrations are structurally impossible or hard-failed before insert; release behavior cannot silently replace existing entries.
How to verify: Insert path enforces uniqueness in all build modes.

20. [ ] MIR symbol-def registration must not unconditionally overwrite prior mapping.
Problematic state: MIR registration overwrites existing mapping without guard (`src/mir/MIR.zig` around lines 709-711).
Should look like: Duplicate symbol definitions are rejected as invariant violations, not "last write wins."
How to verify: Registration logic checks and rejects duplicates deterministically.
