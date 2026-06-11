# Range → generic iterator constructors (remove `to`/`until`) Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development. Steps use checkbox (`- [ ]`) syntax.

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

**Files:**
- Modify: `src/check/Check.zig` (the `.range_to_excluding, .range_to_including` arm in `checkBinopExpr`, ~line 8270; model the rewrite on the existing type-dispatch rewrite at ~line 6540-6585 which calls `replaceExprWithTypeDispatchCall`)
- Modify: `src/check/test/range_test.zig` (the assertion that referenced `until`)
- Regenerate: the 8 `test/snapshots/range_*.md`

Context: `replaceExprWithTypeDispatchCall(store, expr_idx, type_var_alias_stmt, method_name, method_name_region, args, constraint_fn_var)` (NodeStore.zig ~1264) rewrites an expr into an `e_type_dispatch_call` — dispatch on a *type* (here `Iter`) for an associated function. `Iter`'s type statement index is already available as `self.builtin_ctx.builtin_indices.iter_type` (used by `mkIterVar` at Check.zig:1241). The new method-name idents (`exclusive_range`/`inclusive_range`) must be interned — add them to `CommonIdents` (the `until`/`to` idents added previously are the model: struct field + `insert()` + `find()` in `src/canonicalize/ModuleEnv.zig`). Type-dispatch calls resolve + lower through existing machinery (`monotype/lower.zig` `.type_dispatch_call => lowerDispatchExpr`).

- [ ] **Step 2.1: Add `exclusive_range` / `inclusive_range` idents**

In `src/canonicalize/ModuleEnv.zig` `CommonIdents`: add `exclusive_range`/`inclusive_range` fields + `insert()` (`Ident.for_text("exclusive_range")` / `"inclusive_range"`) + `find()` entries, mirroring how `until`/`to` were added. (Note: `until`/`to` idents may be removed in Task 3; leave them for now.) The serialization size check (`test/serialization_size_check.zig`) will need bumping by the 2 new idents — update it.

- [ ] **Step 2.2: Update the check unit test first (TDD: make it describe the new desugaring)**

In `src/check/test/range_test.zig`, the test `"range over generic operands carries an until where-constraint"` asserts `assertLastDefTypeContains("until")`. After retargeting, a polymorphic `|start, finish| start..<finish` will carry the constructor's constraints instead. Update the assertion to match the new reality — determine the actual rendered type on the red run (likely contains `is_lt`/`add_checked` rather than `until`) and pin it. Keep the `Iter(Dec)` / `Iter(U8)` / for-loop tests unchanged (they should still hold).

Run: `zig run .claude/zig-test-llm.zig -- run-test-zig-module-check -- --test-filter "range"`
Expected: FAIL (arm still dispatches to `to`/`until`; assertion mismatch).

- [ ] **Step 2.3: Retarget the arm**

Rewrite the `.range_to_excluding, .range_to_including` arm in `checkBinopExpr`:
- Keep the range-specific validation that produces good errors: unify `lhs_var` with `rhs_var` (bounds must match) with the existing range error context; on failure set expr to `.err` and return.
- Pick the method ident: `range_to_excluding => exclusive_range`, `range_to_including => inclusive_range`.
- Resolve/instantiate the constructor's type via the **type-dispatch** path (model on Check.zig ~6540-6585): the dispatcher type is `Iter` (via `builtin_indices.iter_type`), args are `[lhs, rhs]`. Build the `constraint_fn_var` the same way that path does, set the expression's result var to the call's `Iter(num)` return, and rewrite the binop via `self.cir.store.replaceExprWithTypeDispatchCall(expr_idx, iter_type_stmt, method_ident, region, args_span, constraint_fn_var)`.
- Remove the old `mkBinopConstraint`/`publishBinopDispatchExpr`-to-`to`/`until` logic and the `mkIterVar`-only path if the type-dispatch resolution now produces the `Iter(num)` result. (If you still need `mkIterVar` to shape the expected return, keep it — but the constructor's declared `-> Iter(num)` should provide it.)

IMPORTANT: the exact in-scope names and the type-dispatch resolution helper must match what's actually at Check.zig:6540-6585. Read that path and the current arm carefully; adapt rather than copy verbatim. If type-dispatch resolution needs metadata the arm can't easily produce, fall back to: build the `e_type_dispatch_call` and attach a `StaticDispatchConstraint` for the method on the `Iter` dispatcher exactly as the dot-call path does.

- [ ] **Step 2.4: Run check tests**

Run: `zig run .claude/zig-test-llm.zig -- run-test-zig-module-check -- --test-filter "range"`
Expected: PASS (pin the calibrated `assertLastDefTypeContains` string).
Then: `zig run .claude/zig-test-llm.zig -- run-test-zig-module-check` → `OK`.

- [ ] **Step 2.5: Regenerate the 8 range snapshots**

Run: `zig run .claude/zig-llm.zig -- run-snapshot-tool`
Review: `test/snapshots/range_{exclusive,inclusive,annotated,for_loop,precedence_and_fmt,chained_error,bare_double_dot_error,missing_method_error}.md`. The CANONICALIZE section should now show an `e-type-dispatch-call` (or however type-dispatch renders) to `inclusive_range`/`exclusive_range` on `Iter`, not a dispatch to `to`/`until`. TYPES still `Iter(Dec)` / `Iter(U8)`. Confirm `jj diff --stat` shows ONLY these 8 (+ any expected) changed — no unrelated drift. If `repl/*_range_*` snapshots drift here, that means `to`/`until` were affected — they shouldn't be (still present); investigate.

- [ ] **Step 2.6: Widen**

Run: `zig run .claude/zig-llm.zig --`
Run: `zig run .claude/zig-test-llm.zig -- run-test-zig`
Run: `zig run .claude/zig-llm.zig -- run-check-snapshots`
Expected: all green, drift only in the 8 range snapshots.

- [ ] **Step 2.7: Commit**

```
jj commit -m "Desugar ranges to Iter.exclusive_range/inclusive_range

Retarget the range Check arm: rewrite range_to_excluding/range_to_including
binops into a type-dispatch call on Iter to the generic range constructors,
instead of a receiver dispatch to per-type to/until. Keeps the dedicated arm
for range-specific bound-mismatch errors. to/until remain (now unused by
ranges); removed next."
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
