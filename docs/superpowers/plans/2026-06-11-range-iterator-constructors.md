# Range → generic iterator constructors (remove `to`/`until`) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development. Steps use checkbox (`- [ ]`) syntax.

> **As-built deviations (this doc records original intent; the merged code differs):**
> - **Known length, not `Unknown`.** The plan deferred length as a later optimization (`len_if_known = Unknown`). The merged implementation instead gives ranges a **Known** length via a per-type `steps_between : T,T -> [Known(U64), Unknown]` builtin in the `where` clause: exclusive uses `start.steps_between(end)`; inclusive adds 1 (with `add_checked` overflow → `Unknown`, and `start > end` → `Known(0)`). `U128`/`I128` use `to_u64_try` (overflow → `Unknown`); `Dec` is `Unknown`. This restores exact preallocation on `collect`/`from_iter` and exact `take_last`/`drop_last` without materialization — the cost being that `steps_between` is now part of the rangeable interface.
> - **Dedicated Check arm removed.** Ranges canonicalize to a plain `Iter.exclusive_range`/`inclusive_range` call (`synthesizeIterMemberLookup`) and flow through ordinary call-checking; there is no longer a separate range Check arm. `CalledVia.range` is recorded as provenance for future range-specific diagnostics.

**Goal:** Make numeric ranges (`1..<5`, `1..=5`) desugar to two generic `Iter` iterator constructors instead of per-type `to`/`until` methods, then delete `to`/`until` so ranges are the only surface spelling — while keeping the dedicated Check arm (for range-specific error messages).

**Architecture:** Add generic `Iter.exclusive_range`/`Iter.inclusive_range` (associated functions on `Iter`, like `Iter.custom`), generic over `num` with a minimal `where` clause (`is_lt`/`is_lte` + `add_checked` + `from_numeral`) and `len_if_known = Unknown`. The Check arm for the `range_to_excluding`/`range_to_including` binops (which stay as CIR binop nodes from canonicalization) is retargeted: instead of a receiver static-dispatch to `to`/`until`, it rewrites the binop into an **`e_type_dispatch_call` on `Iter`** (`Iter.inclusive_range(lhs, rhs)`), reusing the existing type-dispatch resolution + lowering machinery (the same path `Iter.custom(...)` uses, lowered via `StaticDispatchPlanId` in `monotype/lower.zig`). Parse and canonicalization are unchanged. Front-end only, **no compiler changes** — see the finding below.

**Tech stack:** Zig compiler + builtin Roc source. **jj**, not git. Build/test via wrappers: `zig run .claude/zig-test-llm.zig -- <step>` and `zig run .claude/zig-llm.zig -- <step>`. Commit footer: `Co-Authored-By: Claude Opus 4.8 (1M context) <noreply@anthropic.com>`.

**Key finding — generic numeric literals already work (empirically verified):** A generic function may use a numeric literal of its type parameter by declaring `where [num.from_numeral : Numeral -> Try(num, [InvalidNumeral(Str)])]`. This needs **zero compiler change** — confirmed by a probe (`inc = |x| x.add_checked(1)` over a generic `a` with the from_numeral where clause type-checks). It works because constraint *satisfaction* is matched by method **name** (`fn_name`), never by `origin` (Check.zig:9616/9647/9748): a literal's deferred `from_numeral`-origin constraint is satisfied by a where-clause constraint of the same name. It's sound because the literal keeps its own `from_numeral` constraint (with its `NumeralInfo` payload), so range-validation against the concrete type still fires at instantiation; and the unresolved-dispatcher error gate (Check.zig:2435) treats both `from_numeral` and `where_clause` origins as legitimate. This is exactly Feldman's original `Iter(num) where [num.from_literal, ...]` sketch. **Consequence:** the constructors use `start.add_checked(1)` directly (with `from_numeral` in the where clause) — there is NO `succ_checked` method and NO separate "build a feature" step.

**Why this shape (decided with the user):**
- `len_if_known = Unknown`: keeps the "rangeable" `where` interface minimal (`is_lt`/`is_lte` + `add_checked` + `from_numeral`) so it doesn't leak counting constraints (`sub_checked`/`to_u64_try`) into polymorphic userland range code. Length is a reversible optimization that can be added later without an interface break. (Costs exact preallocation on collect/`from_iter` and forces materialization in `take_last`/`drop_last`; pure `for`/fold iteration is unaffected.)
- Keep the binop CIR node + dedicated Check arm: range-specific diagnostics (bound mismatch, non-numeric bound) instead of generic call-arg errors.
- Use `add_checked(1)` + a `from_numeral` where-constraint (NOT a `succ_checked` method): no new public API on numeric types; the literal `1` is a plain literal of the generic `num`.

**Current branch state (important for execution):** A first cut of Task 1 was already committed (jj `vplpnzup` / `8d3227bd`) using a `succ_checked` method workaround (added to all 11 numeric types). That approach is **superseded** by this finding. Task 1 below now includes unwinding it: switch the constructor bodies back to `add_checked(1)`, add `from_numeral` to the where clauses, and **delete the 11 `succ_checked` methods**. (Execution may rework that commit in place or abandon+redo — implementer's choice, as long as the final Commit 1 contains the constructors with `add_checked(1)` + `from_numeral` and no `succ_checked`.)

**Commit sequencing (each commit independently green):**
1. Add the two generic constructors (additive — `to`/`until` still present & used).
2. Retarget the Check arm to the constructors + regenerate the 8 `range_*` snapshots + fix the one check unit test (`to`/`until` still present but now unused by ranges).
3. Delete the 22 `to`/`until` methods + convert the `repl/*_range_{to,until}.md` snapshots to range syntax.

---

## Task 1: Add generic `Iter.exclusive_range` / `Iter.inclusive_range` (jj commit 1)

**Files:**
- Modify: `src/build/roc/Builtin.roc` (inside the `Iter(item) :: { ... }.{ ... }` method block, near `custom` ~line 385)

Context: `Iter.custom` (line ~384) is an associated function on `Iter`. `range_done : () -> Iter(item)` (top-level, ~line 9502) returns an empty iterator. The per-type `U8.until` (line 2771) / `U8.to` (line 2734) build the `Iter` record directly with a recursive `step` closure. The new constructors are exactly those bodies generalized over `num`: operators `<`/`<=` desugar to the `is_lt`/`is_lte` constraints; `add_checked` is a constraint; **`from_numeral` is the constraint that lets the literal `1` be typed as `num`** (the key finding above); recursion preserved; length `Unknown`.

- [ ] **Step 1.0: Unwind the `succ_checked` workaround**

The already-committed Commit 1 added `succ_checked : T -> Try(T, [Overflow])` to the 11 numeric types and used `start.succ_checked()` in the constructors. Delete all 11 `succ_checked` method definitions (grep `succ_checked :` in `src/build/roc/Builtin.roc`) and switch the constructor bodies to `start.add_checked(1)` with `num.from_numeral` added to the `where` clause (Step 1.1). (You can rework the existing commit in place, or abandon it and recreate cleanly — the end state is what matters.)

- [ ] **Step 1.1: The two constructors (final form)**

In `src/build/roc/Builtin.roc`, inside the `Iter` method block (after `custom`'s body, before `iter`/`next`), add:

```roc
		## Iterator over `num` values from `start` up to but not including `end`.
		## Returns an empty iterator if `start >= end`. This is what `start..<end` desugars to.
		exclusive_range : num, num -> Iter(num)
			where [num.is_lt : num, num -> Bool, num.add_checked : num, num -> Try(num, [Overflow]), num.from_numeral : Numeral -> Try(num, [InvalidNumeral(Str)])]
		exclusive_range = |start, end| {
			len_if_known: Unknown,
			step: ||
				if start < end {
					One(
						{
							item: start,
							rest: match start.add_checked(1) {
								Ok(next) => if next < end {
									Iter.exclusive_range(next, end)
								} else {
									range_done()
								}
								Err(Overflow) => range_done()
							},
						},
					)
				} else {
					Done
				},
		}

		## Iterator over `num` values from `start` up to and including `end`.
		## Returns an empty iterator if `start > end`. This is what `start..=end` desugars to.
		inclusive_range : num, num -> Iter(num)
			where [num.is_lte : num, num -> Bool, num.add_checked : num, num -> Try(num, [Overflow]), num.from_numeral : Numeral -> Try(num, [InvalidNumeral(Str)])]
		inclusive_range = |start, end| {
			len_if_known: Unknown,
			step: ||
				if start <= end {
					One(
						{
							item: start,
							rest: match start.add_checked(1) {
								Ok(next) => if next <= end {
									Iter.inclusive_range(next, end)
								} else {
									range_done()
								}
								Err(Overflow) => range_done()
							},
						},
					)
				} else {
					Done
				},
		}
```

Notes / things to verify while implementing:
- Match the exact indentation/style of the surrounding block (tabs as used in the file).
- Confirm the `where`-clause syntax against existing examples: `List.sum` (~line 1550) uses `where [item.plus : item, item -> item, item.default : item]`. The `from_numeral` constraint signature mirrors the one a user type declares — see `src/check/test/custom_num_type_test.zig`: `from_numeral : Numeral -> Try(MyNum, [InvalidNumeral(Str)])`. `Numeral` is referenceable unqualified in builtin scope (that test uses it bare).
- Confirm `range_done()` is in scope from inside the `Iter` block as used by the existing `to`/`until` (it is — they call it unqualified). If name resolution differs for the `Iter` block, match however `custom`'s body references siblings.
- `<` / `<=` desugar to `is_lt` / `is_lte`; `add_checked(1)`'s literal `1` is typed `num` via the `from_numeral` constraint. All three (plus `add_checked`) must appear in the `where` clause. If the build reports a missing/unsatisfied constraint, the fix is to align the `where`-clause method name to what the checker expects — NOT to reintroduce `succ_checked`. (The probe in the Key Finding confirms `add_checked(1)` + `from_numeral` type-checks generically.)

- [ ] **Step 1.2: Build (compiles the builtin + type-checks the constructors)**

Run: `zig run .claude/zig-llm.zig --`
Expected: build succeeds. A type error here means the generic constructor doesn't type-check (e.g. a `where`-clause method-name mismatch, or `range_done`/recursion issue) — fix it now; this is the riskiest step.

- [ ] **Step 1.3: Prove it works at runtime (the key risk: generic recursive Iter constructor monomorphizes + runs)**

The constructors are unused by ranges yet, so add a throwaway expression test, OR use the existing `repl` snapshot harness pattern to eval a direct call. Simplest: temporarily run a snippet through the snapshot tool, or write a one-off check test in `src/check/test/` that type-checks `Iter.exclusive_range(1, 5)` to `Iter(_)`. At minimum, type-check must pass; ideally fold one to confirm runtime. Remove any throwaway before committing (keep only real tests).

Run: `zig run .claude/zig-test-llm.zig -- run-test-zig`
Expected: `OK` (no regressions — purely additive).

- [ ] **Step 1.4: Commit**

```
jj commit -m "Add generic Iter.exclusive_range / inclusive_range constructors

Generic over num with where [num.is_lt/is_lte, num.add_checked,
num.from_numeral] and len_if_known = Unknown, built like the per-type
to/until bodies. The from_numeral constraint lets the literal +1 be typed
as the generic num (no new method needed). These become the desugar target
for range syntax; to/until are untouched for now."
```
(+ Co-Authored-By footer.)

---

## Task 2: Retarget the Check range arm to the constructors (jj commit 2)

**REVISED after a blocker + user decision.** The original Task 2 ("rewrite the binop in Check into an `e_type_dispatch_call`") is IMPOSSIBLE as specced: `e_type_dispatch_call` carries its dispatcher exclusively as a `type_var_alias_stmt: Statement.Idx` that must point to a canonicalization-produced `s_type_var_alias` statement (consumers at `static_dispatch_registry.zig:573` and `checked_artifact.zig:3964` unconditionally read `.s_type_var_alias.type_var_anno`), and Check can NEVER create CIR nodes (node-index ↔ type-var-index are 1:1; `preflightForTypeChecking` only reserves span room). **Decision (user): canonicalization desugars ranges directly to a plain `e_call`** of the generic constructor — the exact proven form qualified builtin calls already take (`Str.is_empty(...)` canonicalizes to `e_call { func: e_lookup_external (builtin), ... }`, verified in `test/snapshots/arrow_qualified_functions.md`). A new `CalledVia.range` variant records provenance for error messages. Consequences: the CIR `Binop.Op.range_to_excluding/range_to_including` ops and the dedicated Check arm are REMOVED (ranges are no longer binops in CIR); the chained-range diagnostic STAYS in Can (it inspects AST operator tokens, before desugar); parse/formatter are untouched (AST still has range bin_op nodes).

**Files:**
- Modify: `src/base/mod.zig` (`CalledVia` enum ~line 62: append a `range` variant with a doc comment, like `string_interpolation`)
- Modify: `src/canonicalize/Can.zig` (the `finish_binop` range handling ~line 9897: desugar to `e_call` instead of `e_binop`)
- Modify: `src/canonicalize/Expression.zig` (remove `range_to_excluding`/`range_to_including` from `Binop.Op` — they're the LAST two variants, so removal is encoding-stable)
- Modify: `src/canonicalize/RocEmitter.zig` (drop the two ops from `binopToStr` + precedence switch)
- Modify: `src/check/Check.zig` (DELETE the `.range_to_excluding, .range_to_including` arm in `checkBinopExpr`; ranges flow through the ordinary `.e_call` path)
- Modify: `src/canonicalize/ModuleEnv.zig` (`CommonIdents`: add `exclusive_range`/`inclusive_range` idents if Can needs interned idents to build the lookup) + `test/serialization_size_check.zig` if so
- Modify: `src/canonicalize/test/range_test.zig` (canonicalization tests now expect `e_call`, not `e_binop`)
- Modify: `src/check/test/range_test.zig` (recalibrate desugar-dependent assertions; fix stale header comment)
- Regenerate: the 8 `test/snapshots/range_*.md`

- [ ] **Step 2.1: Investigate the lookup-synthesis mechanism FIRST**

Find how Can canonicalizes a qualified builtin call like `Iter.custom(...)` / `Str.is_empty(...)` into `e_call { func: e_lookup_external, ... }`: which function resolves (type ident, member ident) → the `e_lookup_external` payload (Builtin module import idx + target node idx + member ident). `Iter` is auto-imported (`builtin_auto_imported_types`, Can.zig ~856). Can legally creates nodes, so the range desugar synthesizes the same func node programmatically. If the resolution genuinely cannot be synthesized without source tokens, STOP and report BLOCKED with specifics.

- [ ] **Step 2.2: TDD — update can + check tests to describe the new desugaring**

- `src/canonicalize/test/range_test.zig`: the two tests asserting `binop.op == .range_to_excluding/.range_to_including` must instead assert the canonicalized expr is an `e_call` (and, if cheap, that `called_via == .range`). The chained-range test (`1..<5..<10` → `e_runtime_error` with `range_op_chained`) stays as is.
- `src/check/test/range_test.zig`: the test asserting `assertLastDefTypeContains("until")` for `|start, finish| start..<finish` now expects the constructor's constraints (likely `is_lt`; calibrate on the red run). Error tests (`1..<"five"` TYPE MISMATCH, `"a"..<"z"` MISSING METHOD) should still hold via the call-arg/where-constraint paths but CALIBRATE the exact titles from the red run. Fix the stale header comment (lines 1-3) to one coherent statement about desugaring to the constructors.
Run both: `zig run .claude/zig-test-llm.zig -- run-test-zig-module-can -- --test-filter "range"` and `... run-test-zig-module-check -- --test-filter "range"` → expect FAIL (still old desugar).

- [ ] **Step 2.3: Add `CalledVia.range` and desugar in Can**

Append `range` to `CalledVia` (`src/base/mod.zig:62`) with a doc comment ("This call is the result of desugaring range syntax, e.g. `1..<5` becomes `Iter.exclusive_range(1, 5)`"). In Can's `finish_binop` range handling (where `op` would be `range_to_excluding`/`range_to_including`): after the existing chained-range AST check, instead of building an `e_binop`, synthesize the func `e_lookup_external` for `Iter.exclusive_range`/`Iter.inclusive_range` (mechanism from Step 2.1), build the args span from the already-canonicalized lhs/rhs, and `addExpr` an `e_call { func, args, called_via = .range }` with the binop's region. Then remove the two ops from `Binop.Op` and fix every exhaustive switch the compiler reports (RocEmitter `binopToStr` + precedence; the Check arm — delete it entirely; anything else the compiler finds).

- [ ] **Step 2.4: Green the tests**

`zig run .claude/zig-test-llm.zig -- run-test-zig-module-can` → OK; `... run-test-zig-module-check` → OK (pin calibrated strings). The eager-Iter property must survive: `r : Iter(U8); r = 0..<10` pins U8 (the constructor's declared `-> Iter(num)` provides it through normal call checking); `for i in 1..=5` still types.

- [ ] **Step 2.5: Regenerate the 8 range snapshots**

`zig run .claude/zig-llm.zig -- run-snapshot-tool`. CANONICALIZE sections now show `e-call` with `e-lookup-external` (the same shape as `Str.is_empty` calls), not `e-dispatch-call (method "until")`. TYPES unchanged (`Iter(Dec)`/`Iter(U8)`; for-loop types). Error snapshots: chained + bare-`..` unchanged; `range_missing_method_error.md` will change shape — verify it's still a comprehensible error and report what it says. `jj diff --stat`: ONLY the listed files + the 8 snapshots; NO `repl/*` drift (`to`/`until` untouched until Task 3).

- [ ] **Step 2.6: Widen**

`zig run .claude/zig-llm.zig --`, `zig run .claude/zig-test-llm.zig -- run-test-zig`, `zig run .claude/zig-llm.zig -- run-check-snapshots` → all green.

- [ ] **Step 2.7: Commit**

```
jj commit -m "Canonicalize ranges to Iter.exclusive_range/inclusive_range calls

Desugar range syntax in canonicalization to a plain e_call of the generic
Iter constructors (the proven qualified-builtin-call form), with a new
CalledVia.range recording provenance. Removes the range CIR binop ops and
the dedicated Check arm; ranges now type-check through ordinary call
checking, whose declared Iter(num) return preserves annotation pinning and
for-loop integration. to/until remain (now unused by ranges); removed next."
```
(+ footer.)

---

## Task 3: Delete `to`/`until` and convert their REPL snapshots (jj commit 3)

**Files:**
- Modify: `src/build/roc/Builtin.roc` (delete 22 `to`/`until` defs + doc blocks)
- Modify: `src/canonicalize/ModuleEnv.zig` (remove `until`/`to` `CommonIdents` if now unused — verify no other consumer) + `test/serialization_size_check.zig` if idents removed
- Modify: `test/snapshots/repl/*_range_to.md` and `*_range_until.md` (convert to range syntax)
- Possibly modify: any other file grep finds calling `.to(`/`.until(` as a range method

- [ ] **Step 3.1: Delete the 22 methods**

In `src/build/roc/Builtin.roc`, delete each `to`/`until` definition **and its leading `## ...` doc-comment block** for: U8 (2734/2771), I8 (3157/3194), U16 (3631/3668), I16 (4114/4151), U32 (4605/4642), I32 (5126/5163), U64 (5637/5677), I64 (6203/6246), U128 (6739/6782), I128 (7351/7397), Dec (8176/8211). (Line numbers are pre-edit anchors; re-locate by grepping `^\s+(to|until) :` since earlier deletions shift them. Delete bottom-up to keep line numbers stable.) Keep `range_done` (still used by the new constructors).

- [ ] **Step 3.2: Remove now-unused `until`/`to` idents**

If nothing else references `CommonIdents.until`/`.to` after Task 2 (grep `idents.until`/`idents.to` in `src/`), remove them from the struct + `insert()` + `find()`, and adjust `test/serialization_size_check.zig`. If something still uses them, leave them and note it.

- [ ] **Step 3.3: Convert the REPL range snapshots to range syntax**

For every `test/snapshots/repl/*_range_to.md` and `*_range_until.md`: in the SOURCE/input lines, mechanically rewrite method calls to range operators:
- `A.to(B)` → `A..=B`
- `A.until(B)` → `A..<B`

e.g. `Iter.fold(1.U32.to(5.U32), [], ...)` → `Iter.fold(1.U32..=5.U32, [], ...)`, and `Iter.fold(-2.I64.until(2.I64), ...)` → `Iter.fold(-2.I64..<2.I64, ...)`.
(Verify the typed-literal range tokenizes/parses: `1.U32..=5.U32` is `(1.U32) ..= (5.U32)`. If a particular form doesn't parse, find the correct typed-range spelling or annotate; if a file becomes untestable, delete it and note why — the runtime behavior is also covered by `range_for_loop` and could be supplemented.)

The expected fold RESULTS in these snapshots must be unchanged (same numbers), since `..=`/`..<` produce the same sequences `to`/`until` did.

- [ ] **Step 3.4: Catch any stragglers**

Run: `grep -rn "\.to(\|\.until(" src/build/roc/Builtin.roc test/snapshots src --include="*.md" --include="*.roc" --include="*.zig"` — confirm no remaining range-method call sites (ignore unrelated `.to`/`custom`/etc.). Fix any.

- [ ] **Step 3.5: Regenerate + gate**

Run: `zig run .claude/zig-llm.zig --` (builtin must still compile without `to`/`until`)
Run: `zig run .claude/zig-llm.zig -- run-snapshot-tool` then review `jj diff --stat`: expect the converted `repl/*_range_*` files to update (and the input lines now show `..=`/`..<`). No missing-method errors anywhere.
Run: `zig run .claude/zig-test-llm.zig -- run-test-zig`
Run: `zig run .claude/zig-llm.zig -- run-check-snapshots`
Run: `zig run .claude/zig-llm.zig -- minici` (fix per-section, rerun that section, then full)
Expected: all green.

- [ ] **Step 3.6: Commit**

```
jj commit -m "Remove deprecated to/until range methods

Delete the 22 per-type to/until iterator methods now that ranges desugar
to the generic Iter constructors; ranges are the only spelling. Convert the
per-type REPL range snapshots from to()/until() method calls to ..= / ..<
syntax (same sequences, same results)."
```
(+ footer.)

---

## Out of scope
- Range patterns (separate follow-up).
- Adding `Known` length back (a later optimization; would need an encapsulated per-type count primitive to avoid leaking counting constraints).
- Open-ended ranges.
