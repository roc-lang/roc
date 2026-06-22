# Bug repro + ideal-fix plan (open bug issues since #9635)

Issues considered: open, opened since #9635, Type **Bug** (or `bug` label), no "panic" in title.

Each item below has: the regression **test** (RED before the fix, GREEN after, no edits), the
**root cause** (file:line + mechanism), and an **ideal-fix checklist** of the concrete changes
required — optimizing for perfect correctness first, then maximum performance, with no
fallbacks/heuristics (per `AGENTS.md` / `.rules`).

| Issue | Test | Status |
|---|---|---|
| [#9712](#9712--1--10--100-crashes) | `test/snapshots/repro_issue_9712_mod_floordiv_precedence.md` | **DONE** (fix landed, repro GREEN) |
| [#9733](#9733--nested-expect-is-ignored) | `src/compile/test/hoisted_constants_test.zig` (`issue 9733`) | **DONE** (fix landed, test GREEN) |
| [#9686](#9686--sort-differs-in-imported-type-module) | `src/eval/test/eval_issue_tests.zig` (`issue 9686`) | **DONE** (fix landed, test GREEN) |
| [#9635](#9635--assigned-to-itself-false-positive) | `test/snapshots/repro_issue_9635_self_assignment.md` | **DONE** (fix landed, repro GREEN) |
| [#9740](#9740--numeric-literal-0-inferred-dec-not-u64) | `test/snapshots/repro_issue_9740_numeric_literal_dec.md` | **DONE** (fix landed, repro GREEN) |
| [#9725](#9725--record-cannot-be-a-dict-key) | `test/snapshots/repro_issue_9725_record_dict_key.md` (+ 4 runtime eval tests) | **DONE** (fix landed, repro + eval tests GREEN) |
| #9645 | — | **already fixed** (host-ABI migration; verified end-to-end) |
| #9670 | — | **out of scope** — fixed separately by PR #9688; intentionally not implemented or tested here |

## Running the tests

All in-scope work is complete and verified: eval suite **1387/0**, zig unit tests **2817/0**,
LSP **41/41**, `zig build -Dno-bin` clean.

```bash
# Snapshot repros — all GREEN now.
./zig-out/bin/snapshot test/snapshots/repro_issue_9635_self_assignment.md          --check-expected  # GREEN
./zig-out/bin/snapshot test/snapshots/repro_issue_9740_numeric_literal_dec.md      --check-expected  # GREEN
./zig-out/bin/snapshot test/snapshots/repro_issue_9712_mod_floordiv_precedence.md  --check-output    # GREEN
./zig-out/bin/snapshot test/snapshots/repro_issue_9725_record_dict_key.md          --check-expected  # GREEN
# whole suite also enforces these via the `snapshot validation` zig test

zig build run-test-eval -- --filter "issue 9686"       # GREEN (cross-module alias dispatch)
zig build run-test-eval -- --filter "issue 9725"       # GREEN (record/tuple/tag-union Dict keys round-trip)
zig build run-test-zig  -- --test-filter "issue 9733"  # GREEN (2 expect roots collected)
```

Snapshot mechanism: false-positive-diagnostic repros set `# EXPECTED` = `NIL` (the correct outcome is
"no diagnostic"); the checker regenerates the diagnostics fresh and compares to `NIL`. The repl repro
sets `# OUTPUT` to the correct value. No `skip=true`/xfail anywhere.

> **#9670 is out of scope for this branch.** It is fixed by a separate PR (#9688) whose root cause is a
> generalization-rank bug (a generalized var lifted to the outermost rank while checking nominal/alias
> arguments). This branch contains no #9670 repro or fix, to stay fully decoupled from that PR.

---

## #9635 — "assigned to itself" false positive

**Test:** `test/snapshots/repro_issue_9635_self_assignment.md` (`--check-expected`).
RED: emits `INVALID ASSIGNMENT TO ITSELF` on the 2nd/3rd reference; expected `NIL`.

**Root cause.** Self-reference detection in `src/canonicalize/Can.zig:6875-6883` uses an unsound
node-index threshold (`found_pattern_idx >= defining_patterns_start ⇒ self-ref`). Top-level forward
references are resolved on demand by allocating placeholder patterns *during* body canonicalization
(`Can.zig:6989-7009`), so the 2nd/3rd use of a top-level value inside a local binding gets a placeholder
whose index is above the threshold and is misflagged. The 1st use takes the `.not_found` path and is
correctly spared (which is why only the 2nd/3rd fire).

**Ideal fix — checklist** (use the explicit bound-pattern set, not an index range):
- [ ] Replace the `defining_patterns_start: ?u32` field on `Self` (`Can.zig:~314`) with the explicit set of
  pattern indices bound by the current declaration's pattern (e.g. a scratch span / small set).
- [ ] At both sites that set it today (`Can.zig:~6232-6237` and `~7992-7997`), compute that set with the
  existing `collectBoundVarsToScratch` (`Can.zig:4374`) on the declaration's pattern, and save/restore it
  around nested declarations exactly as the old field was.
- [ ] Change the check at `Can.zig:6875-6883` to
  `is_self_ref = (found_pattern_idx == defining_pattern) or (found_pattern_idx ∈ defining_bound_patterns)`;
  delete the `>= def_start` threshold branch.
- [ ] Confirm `ident` patterns are members of the set so the `defining_pattern` scalar branch folds in
  cleanly (or keep it for non-tuple defining-pattern shapes).
- [ ] Regression-check genuine self-refs still error: `a = a`; local AND top-level `(x, y) = (x, y)`.
- [ ] `--check-expected` repro → GREEN; run full `snapshot validation`.

---

## #9670 — out of scope (fixed by PR #9688)

Intentionally not implemented or tested on this branch. PR #9688 fixes it; its root cause is a
generalization-rank bug — when checking the arguments of a nominal or alias, an already-generalized
variable was lifted to the outermost rank, effectively un-generalizing it and causing the rigid to leak
into a call-site result (the spurious self-mismatch). This branch carries no #9670 repro or fix so it
stays fully decoupled from that PR.

---

## #9740 — numeric literal `0` inferred `Dec` not `U64`

**Test:** `test/snapshots/repro_issue_9740_numeric_literal_dec.md` (`--check-expected`).
RED: `TYPE MISMATCH` `List(a), Dec, U64 -> List(a)` vs `List(a), U64, U64 -> List(a)`; expected `NIL`.

**Root cause.** At the generalization boundary, `defaultLiteralsAtGeneralizationBoundary`
(`src/check/Check.zig:14308-14342`) seeds reachability from a `recursive_def` constraint using **only
`func.ret`**, deliberately excluding `func.args` (comment `14316-14320`). The literal `0` in a recursive
call's argument position is therefore treated as unreachable and committed to `Dec`
(`commitLiteralDefaultHead`, `14534-14545`) **before** the deferred `recursive_def` constraint unifies it
with the annotated `U64`. The arg-exclusion was an over-correction to keep `fib("bad arg")` reportable.

**Ideal fix — checklist** (seed literal args pinned by a concrete annotated param):
- [ ] In `defaultLiteralsAtGeneralizationBoundary`, `.recursive_def` seeding block (`14321-14342`): resolve
  `eql.expected` (the def's annotated type / `pat_var`) to its function structure alongside the
  already-resolved `eql.actual` function.
- [ ] For each arg index `i` where both sides have an arg var, inspect resolved `expected.args[i]`; if it is
  concrete/structured (e.g. `U64`), call `collectReachableVars(actual.args[i], &boundary_reachable_vars)`.
- [ ] Leave `expected.args[i]` that are still flex/rigid (unannotated params) unseeded — preserving current
  behavior for unannotated recursive functions.
- [ ] Verify: repro → GREEN; a `fib("bad arg")`-style genuine mismatch still reports; unannotated recursive
  functions' inference unchanged.
- [ ] Do **not** make the downstream `recursive_def` unification "tolerate" a `Dec` against `U64` (consumer
  band-aid that masks real mismatches).

---

## #9725 — record cannot be a Dict key

**Test:** `test/snapshots/repro_issue_9725_record_dict_key.md` (`--check-expected`).
RED: `MISSING METHOD: to_hash` on `{ a: c, b: d }`; expected `NIL`. (Same record works as a `Set` element.)

**Root cause.** In the structural-dispatch branch `src/check/Check.zig:15489-15519`, **only `is_eq`** has a
structural-derivation path (`typeSupportsIsEq` + `satisfyDerivedIsEqConstraint` + an `e_structural_eq` CIR
node the backends lower field-by-field). Every other method, incl. `to_hash`, falls to the `else` →
`.not_nominal` → rendered `MISSING METHOD`. `Dict.insert` requires `k.to_hash` (`Builtin.roc:2381-2382`);
`Set.insert` requires only `is_eq` (`Builtin.roc:2764-2765`) — hence Set works, Dict doesn't.

**Ideal fix — checklist** (add structural `to_hash` derivation mirroring `is_eq`):
- [ ] Add `typeSupportsToHash` / `varSupportsToHashInternal` (parallel to `typeSupportsIsEq` /
  `varSupportsIsEqInternal` at `~15694` / `~16115`): record/tuple/tag-union is hashable iff every component
  is; functions are not; nominals defer to backing var + any builtin `to_hash` binding (Str/Bool/num/List all
  declare `to_hash`); flex/rigid optimistically `true` (same soundness argument as `is_eq`).
- [ ] Add `satisfyDerivedToHashConstraint` (parallel to `satisfyDerivedIsEqConstraint` at `~16068`): unify the
  constraint fn signature against `(self, Hasher) -> Hasher`, then mark the dispatch for structural-hash
  lowering.
- [ ] Add a dedicated derived-`to_hash` CIR node + rewrite (parallel to `e_structural_eq` /
  `rewriteDerivedIsEqMethodCallAsStructuralEq` at `~13098`) representing "thread the `Hasher` through each
  field/element," matching the hand-written `List.to_hash` shape (`Builtin.roc:1097-1103`).
- [ ] Add `nominalSupportsDerivedToHash` (parallel to `nominalSupportsDerivedIsEq` at `~15867`) so nominal
  records/wrappers derive it via their backing type.
- [ ] In the structural-dispatch branch (`15489-15519`), add the `to_hash` case before the `else`.
- [ ] Wire `to_hash` derivation into the nominal-dispatcher and where-clause-instantiation paths that
  special-case `is_eq` today (`15109-15150`, `15322-15364`).
- [ ] Make the ambiguity reporter skip derived `to_hash` placeholders as it does `is_eq` (`~5642`).
- [ ] Implement lowering of the new structural-hash node in **each** backend (interpreter, dev, LLVM, wasm).
- [ ] Verify: repro → GREEN; records, tuples, and tag-unions all usable as Dict keys; Set unaffected.
- [ ] Do **not** patch the Builtin side (dropping `to_hash` from `Dict`, or a naive `Set`-style impl) — that
  leaves the real gap and breaks any API needing a hashed structural key.

---

## #9712 — `1 % 10 // 100` crashes

**Test:** `test/snapshots/repro_issue_9712_mod_floordiv_precedence.md` (`--check-output`).
RED: `got 'Crash', expected '0.0'`.

**Root cause.** The Pratt precedence table `src/parse/Parser.zig:6225-6228` gives `*`/`/`/`//`/`%` strictly
descending distinct binding powers (`32/33`, `30/31`, `28/29`, `26/27`). With the driver at
`Parser.zig:3350-3372` this makes `//` bind tighter than `%`, so `1 % 10 // 100` parses as `1 % (10 // 100)`
= `1 % 0` → modulo-by-zero crash. Correct grouping is one left-associative multiplicative group →
`(1 % 10) // 100` = `0` (displayed `0.0` under current numeric defaulting; the `.0` is correct, not the bug).

**Ideal fix — checklist:**
- [ ] In `src/parse/Parser.zig` `bin_op_bp_table` (`6225-6228`), set `OpStar`, `OpSlash`, `OpDoubleSlash`,
  `OpPercent` all to `{ .left = 32, .right = 33 }` (one left-associative group; `right > left` makes a
  following same-group op fail `left >= min_bp`).
- [ ] Confirm the group stays strictly above additive (`+`/`-` at `22/23`) so no other precedence relations
  change.
- [ ] `--check-output` repro → `0.0`, no crash; regenerate and eyeball existing binop snapshots
  (`binops.md`, `binop_omnibus__*`, `mono_nested_binop_parens.md`) — they should be unchanged.
- [ ] (Out of scope for #9712, track separately: interpreter crashes on `% 0` while a compiled file returns
  the dividend — a distinct downstream divergence only reachable via the mis-grouping.)

---

## #9686 — sort differs in imported type module

**Test:** `src/eval/test/eval_issue_tests.zig`, case `issue 9686: imported type-module alias element sorts
correctly`, asserting `inspect_str = "[100, 90, 75]"`.
RED: cross-module program fails to lower (`TypeCheckError` / dispatch invariant). Confirmed cross-module
specific: an inline copy of the same logic evaluates to `[100, 90, 75]`.

**Root cause.** `a < b`/`a > b` desugar to `is_lt`/`is_gt` static dispatches on the element type. When the
element is an imported type alias `Score : U64`, owner resolution treats the alias as its own distinct
owner instead of unwrapping to `U64`. In monotype lowering an imported alias is a `.named` node with
`builtin_owner = null` (`src/postcheck/monotype/lower.zig:1349-1362`), and `ownerHead`
(`src/postcheck/monotype/type.zig:204-215`) does **not** unwrap aliases (unlike the digest path at
`type.zig:264-273` and `resolvedPayload` at `lower.zig:9686-9697`). So `methodOwnerFromType` returns owner
`Score`, `lookupMethodTarget` finds no `is_lt`/`is_gt`, and `Common.invariant` (`lower.zig:5877`) is hit —
panic/compile-failure in debug, UB selecting a garbage target in release (the issue's `[70, 75, 40]`).
Inline, the alias collapses to `U64` during checking; removing `Score : U64` (using `U64`) makes it a
nominal carrying `builtin_owner = .u64`; removing the annotation leaves an owner-less flex →
`RuntimeError`. The checker side has the same gap (`static_dispatch_registry.zig:1137-1147`).

**Ideal fix — checklist** (make dispatch owner resolution alias-transparent):
- [ ] In `src/postcheck/monotype/type.zig` `ownerHead` (`204-215`): for a `.named` whose `kind == .alias`
  (and `builtin_owner == null`), recurse into `backing.ty` and return that owner — handling alias-over-alias
  and alias-over-nominal uniformly.
- [ ] In `src/check/static_dispatch_registry.zig` `methodOwnerForCheckedPayload` (`1137-1147`): for an
  `.alias` payload, resolve the owner from the alias `backing` (recursively) so checker plans resolve eagerly
  to `.resolved_target` rather than relying on the monotype fallback.
- [ ] (Consistency) populate `builtin_owner` for alias `.named` nodes from the backing in
  `lower.zig:1349-1362` and `instTypePayload` (`~3162-3172`).
- [ ] Verify: eval repro → `[100, 90, 75]` on all backends; the inline case stays correct; check the
  no-annotation variant now resolves once a concrete alias is known (or document remaining behavior).

---

## #9733 — nested `expect` is ignored

**Test:** `src/compile/test/hoisted_constants_test.zig`, test `issue 9733: nested expect statements are
collected as test roots`, asserting `countCompileTimeRootKind(app_artifact, .expect) == 2`.
RED: `expected 2, found 1`.

**Root cause.** `CompileTimeRootTable.fromModule` (`src/check/checked_artifact.zig:18150-18162`) collects
`.expect` roots by walking only `module_env.all_statements` — the module's **top-level** statement span. A
nested `expect` lives as an `s_expect` inside an `e_block`'s statement list, so it is never collected, never
becomes a `test_expect` root, and is never run/counted. Everything downstream is correct (nested expects are
canonicalized, type-checked, even lowered/executed inside the outer block), but `roc test` decides pass/fail
solely from collected roots' returned bool — so the nested failure is invisible and it reports
"All (1) tests passed."

**Ideal fix — checklist:**
- [ ] In `CompileTimeRootTable.fromModule` (`checked_artifact.zig:18150-18162`), replace the
  top-level-only `all_statements` walk with a walk over **all** `s_expect` statements reachable in the
  module's checked bodies (recurse into block statement lists wherever they appear).
- [ ] For each discovered `s_expect`, append a `.expect` compile-time root exactly as today:
  `.source = .{ .statement = statement_idx }`, `.expr = checkedExprIdForSource(body)`,
  `.checked_type = checkedTypeIdForVar(varFrom(body))`, `.payload = .expect`.
- [ ] Ensure no double-counting and that existing top-level expects are still collected.
- [ ] Verify: unit repro → count `2`; and `roc test` on the repro program reports a **failed** expect
  (the nested `expect 3 == 4`), no longer "All tests passed".

---

## #9645 — ZigGlue stale ABI / bad size checks

**Status: already fixed** on the current tree (the host-ABI-symbols migration landed after the issue's
`f081a87b` baseline). A fails-now test is impossible because the bug no longer exists.

**Verified end-to-end** by running `roc glue src/glue/src/ZigGlue.roc <out>
test/postcheck/platform_required_init/platform/main.roc` (its `program : { init!, render! }` is the trigger):
- Symptom 1 (size checks): the function-field record is `__AnonStruct7 = extern struct { @"init!":
  *anyopaque, @"render!": *anyopaque }` (16 bytes, correct assertion); function fields are `*anyopaque`
  (8 bytes) not nested closure-env structs; record sizing routes through shared `src/layout/field_order.zig`.
  Generated glue **compiles cleanly** under `zig build-obj` (a wrong `@sizeOf` would `@compileError`).
- Symptom 2 (entrypoint ABI): direct C ABI — `roc_init_for_host(arg0: __AnonStruct21) callconv(.c) Try`,
  `roc_render_for_host(arg0: RocBox, arg1: __AnonStruct21) ...` — zero `ret_ptr`/`arg_ptr`/`*RocOps`-as-arg0.

**Checklist:**
- [ ] No code change required.
- [ ] Confirm the existing `glue_zig_bang_record_fields` CLI case
  (`src/cli/test/parallel_cli_runner.zig`) still guards both sub-bugs (it asserts function-fields-as-
  `*anyopaque`, natural-ABI entrypoints, absence of `ret_ptr`/`arg_ptr`, and compiles the output).
- [ ] (Optional) Add a deeper-nesting / record-returning-entrypoint glue case as an extra guard — note it
  is GREEN now, not a repro.
- [ ] Close the issue with the verification evidence above.
