# Non-Recursive Type Checker — Design

**Date:** 2026-06-28
**Status:** Approved (design); implementation pending
**Scope of this doc:** Full reusable architecture, plus implementation spec scoped to `checkExpr` only. `checkPattern` and type-annotation generation are follow-up specs that reuse this pattern.

## Goal & Motivation

Make the type-checking phase (`src/check/Check.zig`) not depend on native call-stack
recursion, so that **arbitrarily deep, user-controllable expression nesting cannot
overflow the native stack**.

- **Primary driver:** stack-overflow safety. Deeply nested expressions (generated
  code, long binop chains, deep blocks/records/calls) must check in O(1) native
  stack frames regardless of input depth.
- **Secondary benefit:** more control over allocation and memory locality
  (explicit, pre-allocated stacks), which we lean into where free.

This mirrors what the parser (`src/parse/Parser.zig`, `runExprStatementKernel`) and
canonicalization (`src/canonicalize/Can.zig`, `runExprKernel`) already do: replace the
native call stack with an explicit, heap-allocated work stack driven by a state-machine
loop.

## Scope

**In scope (surfaces that produce input-scaled native depth):**
- `checkExpr` (expression tree) — **this spec.**
- `checkPattern` (pattern tree) — follow-up spec, same pattern.
- Type-annotation generation (`generateAnnotationType`) — follow-up spec, same pattern.

**Out of scope (explicit boundary):**
- `unify` / `occurs` (`src/check/unify.zig`, `src/check/occurs.zig`). These recurse over
  the type graph itself. Deep *inferred types* can still overflow them independently of
  `checkExpr`. This is a known, accepted limitation of this effort; the interim depth
  guard (below) provides a clean error rather than a crash in the meantime.

## Fidelity Requirement

**Preserve inference results; diagnostics may reorder.**
- Final inferred types must be byte-for-byte identical (snapshot types must not move).
- Diagnostic *ordering* may shift if it simplifies the kernel, but no diagnostic may be
  added or dropped, and no inferred type may change.
- Practical implication: the order of `unify` calls and scope push/pop *within* a node
  must be preserved, because it determines which side of a mismatch is reported as
  "expected" vs "actual". The enter/exit decomposition removes native frames only; it
  does not reorder unifications.

## Approach: Incremental Trampoline (Approach B)

Chosen over a big-bang unified kernel (Approach A) and a big-stack offload (Approach C)
because it is the only option that fits all three constraints: incremental
checkExpr-first scope, verifiable result-preservation, and true depth-bounding at the end.

### Core model — reify the `checkExpr` frame

A native recursive call works because its frame holds locals that survive across the
recursive child calls, and the return address resumes the parent afterward. We reify that
frame into an explicit heap struct. The locals that live across `self.checkExpr(child, …)`
calls (derived from `Check.zig:9477–9582` prologue and `11260–11362` epilogue) are:

```zig
const CheckFrame = struct {
    expr_idx: CIR.Expr.Idx,
    expected: Expected,            // flowed down (replaces the `expected` param)
    step: Step,                    // where in this frame's lifecycle we are

    // captured in the prologue, needed again in the epilogue:
    expr_var: Var,
    expr_var_raw: Var,
    mb_anno_vars: ?AnnoVars,
    should_generalize: bool,
    hoist_frame: HoistFrame,
    prev_instantiation_source: ?CIR.Expr.Idx,

    // accumulated across children:
    does_fx: bool,

    // per-node-kind loop/scratch state for interleaving nodes:
    kind_state: KindState,
};
```

Two properties keep this tractable and behavior-preserving:

1. **No operand buffer needed.** A child's inferred type is already reachable from its idx
   via `ModuleEnv.varFrom(child_idx)`, and unification writes into the shared store. So the
   exit step recovers child types by var — there is no `child_slots`-style operand stack
   (unlike canon, which returns `CIR.Expr.Idx` values). The only thing that flows up
   explicitly is the `does_fx` bool, which a child's exit ORs into its parent frame.
2. **Prologue and epilogue are shared across all node kinds.** They are written once (in
   `.enter` and `.exit` respectively), frame-field-driven. Only the *middle* (child
   scheduling + per-kind result unification) is kind-specific.

### Memory model — single `Self`-owned stack, allocated at init

**All kernel storage lives on the `Check` struct, allocated once at init, never per call.**

```zig
// field on Self:
check_frame_stack: std.ArrayList(CheckFrame) = .empty,

// Check.init:  self.check_frame_stack = try .initCapacity(gpa, 256);
// Check.deinit: self.check_frame_stack.deinit(gpa);
```

Per top-level entry we reuse the backing buffer via a **saved base high-water-mark**
(same technique as the parser saving `scratch_top`, `Parser.zig:2682`):

```zig
fn checkExprIter(self, root_idx, root_expected) bool {
    const base = self.check_frame_stack.items.len;   // save
    self.pushEnter(root_idx, root_expected);
    while (self.check_frame_stack.items.len > base) {
        // process top frame, pop on completion
    }
    // stack back at `base`; buffer retained for reuse
}
```

Consequences:
- **No per-call allocation churn.** Buffer grows to the deepest expression in the module,
  then stops. Predictable memory and cache behavior.
- **Re-entrancy-safe for the escape hatch.** A re-entrant `checkExprIter` (called from an
  unmigrated node's recursive children) pushes above the current high-water mark, drains
  its own frames back to its own `base`, and returns — never touching outer frames (LIFO).
  One field serves arbitrarily nested re-entrancy with zero extra allocation.
- **`CheckFrame` must be cheap to memcpy** (ArrayList may grow/realloc): all scalars +
  small structs. Anything heap-ish is referenced by handle, not owned inline.
- The ArrayList length is the depth metric; a single `if (len > LIMIT)` converts a
  would-be overflow into a clean `error.NestingTooDeep` diagnostic.

### Frame lifecycle — enter / resume / exit

```zig
const Step = enum {
    enter, exit,
    if_after_cond, if_after_body,        // resume points within an if
    match_after_cond, match_after_branch,
    block_after_stmt, block_after_final,
    binop_after_lhs, binop_after_rhs,
};

const KindState = union(enum) {
    none,
    if_loop:    struct { cursor: u32, branch_acc: ?Var, branch_var: Var, last_branch: IfBranch.Idx },
    match_loop: struct { cursor: u32, /* accumulators */ },
    block_loop: struct { cursor: u32, /* stmt-scope bookkeeping */ },
    binop:      struct { /* operator, resolution state */ },
};
```

- **`.enter`:** run the shared prologue (set `instantiation_source_expr`, push
  generalization rank, generate annotation var, begin hoist frame) → fill frame fields →
  dispatch on node kind. Leaf/fixed-arity nodes do their work and fall to `.exit`. Branch
  nodes re-push self (as `.exit` or a resume step) then push child `.enter` frames in
  reverse order.
- **resume steps** (`*_after_*`): run the between-child unification using `kind_state`
  (e.g. `checkIfElseExpr` unifies each cond to `Bool` and each body into `branch_acc`,
  `Check.zig:12815–12876`), advance the `cursor`, then either push the next child + re-push
  self, or fall to `.exit`. These accumulators were native loop locals; now they live in
  the frame across child checks. Directly mirrors canon's
  `match_next`/`match_after_guard`/`match_after_body` (`Can.zig:15715`).
- **`.exit`:** run the per-kind result unification (tail of the switch arm), then the
  shared epilogue (`Check.zig:11260–11362`: annotation reconciliation, error tracking,
  static-dispatch constraints, generalization incl. cycle handling, `hoist_frame.finish`).
  The `defer`-based rank pop (`9520–9537`) becomes an explicit exit step reading
  `frame.should_generalize` + cycle state. Propagate `does_fx` up into the parent frame.

`checkPattern` calls embedded in expr nodes (lambda args `10440–10443`, match patterns,
block decl patterns) remain native recursive `checkPattern` calls for this spec (patterns
are a separate surface). This is a deliberate, bounded scoped gap.

### Escape hatch — incremental, bisectable migration

- The current `checkExpr` is renamed `checkExprRecursive` — kept byte-for-byte, still
  natively recursive. New driver `checkExprIter` is added alongside. Public `checkExpr`
  becomes a thin wrapper calling `checkExprIter`.
- A per-node-kind dispatch decides migrated vs not. **The escape-hatch check happens
  before the shared prologue runs**, so an unmigrated node is a pure passthrough to
  `checkExprRecursive` (which runs the whole subtree — prologue + body + epilogue —
  natively). This keeps behavior identical to today for unmigrated kinds (snapshot
  stability during migration) and avoids any double prologue/epilogue.

```zig
// at the top of a frame's `.enter`, before the iterative prologue runs:
switch (cir.store.getExpr(frame.expr_idx)) {
    .e_block, .e_binop, .e_call, .e_if, .e_match,
    .e_list, .e_tuple, .e_record, .e_dot_access, ... => { /* iterative path */ },
    else => {
        // passthrough: run the WHOLE node natively, then finish this frame.
        const fx = try self.checkExprRecursive(frame.expr_idx, frame.expected);
        self.popFrame();                 // this frame is done as a unit
        self.orDoesFxIntoParent(base, fx); // propagate up (no-op at root)
        continue;                        // next frame
    },
}
```

The same passthrough serves the root call (the root is just the first frame pushed above
`base`; when it finishes there is no parent, so `orDoesFxIntoParent` records the final
result for `checkExprIter` to return).

- **Re-entrancy** (Section: memory model) means a migrated node nested under an unmigrated
  one is still iteratively bounded; only chains of consecutive unmigrated kinds recurse
  natively.

### Interim depth guard (ties to Approach C)

A recursion-depth counter on `Self`, incremented in `checkExprRecursive`, converts a
would-be native overflow into a clean `error.NestingTooDeep` diagnostic at a configurable
limit. Provides safety from day one of the migration (not just at the end). Once all
unbounded-depth kinds are migrated, remaining `checkExprRecursive` users are bounded
(patterns excepted, per scope) and the guard is a cheap belt-and-suspenders check.

## Migration Order

A small warm-up kind validates the machinery, then unbounded-depth kinds hardest-spine-first:

0. **`e_tuple_access` (warm-up)** — exactly one child, then post-child unification that
   reads the child's resolved var (`Check.zig:9909`). No scope, no statement loop. Validates
   the enter→schedule-child→exit decomposition and the differential harness on a real kind
   at minimal risk, *before* tackling the hardest case.
1. **`e_block` + `checkBlockStatements`** — deepest real-world nesting. **Two drivers:**
   `e_block` delegates to `checkBlockStatements` (`Check.zig:12180`), itself a recursive
   driver (~11 `checkExpr`/`checkPattern` sites across 8 statement kinds, accumulators
   `does_fx`/`diverges`/`blocks_later_hoists`, scope save/restore, flag toggles). Both must
   be flattened so nested blocks are genuinely depth-bounded. Owns scope enter/exit
   (`beginHoistLexicalScope`). **→ MANUAL REVIEW GATE (see below).**
2. **`e_binop`** — operator chains; fixed-arity two-child + between-unify + static dispatch
   (~500 lines, `Check.zig:13346`; deferred past the gate deliberately).
3. **`e_call` / `e_apply`** — deep call/argument nesting; variable-arity child loop.
4. **`e_if`, `e_match`** — accumulator-heavy interleavers.
5. **`e_list` / `e_tuple` / `e_record`** — variable-arity.
6. **`e_dot_access` / field-access chains** — deep right-spine.
7. Remaining fixed-arity kinds (`unary`, interpolation, …) — mechanical.

**This plan (Phase 1) covers items 0–1 only**, stopping at the review gate. Items 2–7 are a
follow-up plan written after the gate validates the approach.

Kinds that cannot produce input-scaled depth stay escape-hatched indefinitely (YAGNI; we
do not migrate for uniformity's sake).

### Manual review gate after `e_block`

After `e_block` is migrated, **migration pauses for manual human review** before any other
kind is migrated. The author verifies:
- Full snapshot suite: inferred types unchanged; any diagnostic churn is pure reordering.
- The deep-nesting stress test (below) for nested blocks fails before and passes after.
- The enter/exit/resume + scope-lifetime decomposition reads correctly.

Only after sign-off does migration proceed to `e_binop` and beyond. This validates the
whole approach on the hardest representative case before scaling out.

## Verification Strategy

- **Snapshot suite is the oracle.** After each kind migrates, run the full snapshot/test
  suite. Inferred types must not move; diagnostic ordering may shift (allowed) and is
  eyeballed to confirm pure reordering — never a changed type or added/dropped error.
- **Per-kind bisectability.** One kind per change → any regression points at exactly one
  node kind's decomposition.
- **Targeted build/test first** (project workflow): exercise the specific module + a
  focused nesting test before the full `zig build roc` / snapshot run.
- **Deep-nesting stress test**, added up front: a generated expression nested ~100k deep
  (block, binop, call, list) that overflows the native stack *today*. Executable
  definition of done — must fail before migration, pass after each spine kind lands; also
  the standing regression guard for the stack-safety goal.
- **Differential check (REQUIRED):** a **two-pass, module-level** equivalence check. The
  same module is type-checked twice in independent `Check` instances over independent type
  stores — once with the iterative driver forced fully off (a `force_recursive` flag that
  escape-hatches every kind, i.e. the exact current behavior), once with the iterative
  driver enabled — and the final inferred type var of every def/expr is compared for
  structural equality. (We cannot run both paths back-to-back on a single env: both mutate
  the shared union-find store, so the second pass would observe the first's mutations.
  Hence two independent passes, not an inline per-expr comparison.) This is the strongest
  "results preserved" guarantee and is a hard requirement, not optional. It must be in
  place **before** the first kind is migrated, run in debug/test builds across a corpus
  (the snapshot inputs serve as the corpus), and stay enabled until all in-scope kinds are
  migrated. On mismatch it reports the offending def/expr and both type renderings to make
  regressions trivially bisectable. Release builds pay no cost (test-only harness).

## Known Scoped Gaps (documented, not bugs)

- `checkPattern` and type-annotation generation remain recursive (follow-up specs reusing
  this pattern).
- Deep inferred types can still overflow recursive `unify`/`occurs` (explicit boundary).
- The interim depth guard covers all three with a clean error rather than a crash.
