# TRMC + TCE Implementation Plan for the New Roc Zig Compiler

Companion to `TRMC_RESEARCH.md`, which documents the old Rust implementation this plan ports.

## Context

Without TRMC (Tail Recursion Modulo Constructor), any Roc function that builds a recursive data type with a non-tail recursive call — `repeat`, `map`, `filter` over linked lists, tree builders — overflows the stack on large inputs. This is a correctness gap, not just performance: the interpreter hard-crashes at `max_call_depth = 1024` (`src/eval/interpreter.zig:242`), and the compiled backends overflow native stacks. The new compiler also has **no plain tail-call elimination (TCE)** at all, so even `sum (n-1) (acc+n)` crashes. The old Rust compiler solved both in one pass (`apply_trmc`, commit `ae47cc5171`, researched in `TRMC_RESEARCH.md`).

### Key design decisions

1. **Value-hole design** (not a literal port). The old compiler represented a recursive union value as a single tagged pointer; heap cells held payloads only, so the hole was a pointer-sized slot and `UnionFieldPtrAtIndex` (GEP) computed interior field addresses. The new compiler represents a recursive union as a **by-value blob** (payload struct at offset 0 + in-cell discriminant, `TagUnionData`); recursion is broken only at struct-field edges, which get layout `box(U)` (`shouldBoxRecursiveSlotEdge` → `recursiveSlotLayout` → `insertBox`, `src/layout/store.zig:1053-1094`), and the heap allocation in LIR is the `box_box` low-level (`assignBoxBoundary`, `src/postcheck/solved_lir_lower.zig:1696-1716`), not the tag construction. Therefore the hole is re-aimed at *where a child union value lives*: the interior of a box cell (or the by-value return slot). No GEP op is needed — a box's data pointer **is** the address of the child slot. Allocation counts and final memory layout come out bit-identical to non-TRMC output.
2. **One PR delivers everything always-on**: layout + ops + all four backends + the pass, no feature flag, **TRMC and full TCE together** (no follow-up PR for TCE).
3. **Benchmark ports (CFold, NQueens, RBTreeCk) are acceptance criteria**, not optional extras.
4. **A `tail_transform` side table** (field on `LirProcSpec`) records what the pass did per proc, serving debug output (`ROC_PRINT_TRMC`), test-harness assertions, and a precise interpreter-validator relaxation.
5. **RC-churn peephole in scope** (Option A below): store the alias-chain head + reorder `ptr_cast`, making the loop transfer-clean; the general ARC transfer-into-constructor optimization (Option B) is a named follow-up prioritized by benchmark numbers.

---

## 1. Detection pass

### Eligibility (per proc)

Iterate `store.proc_specs.items` by index `i` (the index *is* the `LirProcSpecId`). Skip a proc when:
- `proc.body == null`, `proc.hosted != null`, or `proc.abi != .roc`
- For TRMC: `layouts.getLayout(proc.ret_layout).tag != .tag_union` (TCE has no return-layout requirement)

Self-call = any `assign_call` whose `proc` field equals `@enumFromInt(i)` (calls reference callees by `LirProcSpecId`; mutual recursion is naturally excluded). `assign_call_erased` never qualifies.

### The LIR shape to match (verified against `solved_lir_lower.zig`)

`lowerMatchInto`/`lowerIfInto` (`solved_lir_lower.zig:2272-2310`) lower branch results **into a join param** and jump to a "done" join whose `body` is the continuation — for a top-level match, literally the `ret ret_local` stmt from `lowerExprReturn` (`:1359-1363`). Nominal wrapping adds an `assign_ref .local` alias hop (`assignNominalBoundary` → `assignBoxBoundary` equal-layout path, `:1686-1707`). So the canonical `repeat` candidate chain is:

```
r: U      = assign_call <self>(c_v, c_n)        # U = proc ret layout (tag_union)
f1: box(U) = assign_low_level box_box [r]        # heap cell, blob copied in
p: P      = assign_struct (t_v, f1)              # payload struct (always a struct, even 1-field:
                                                 #  buildPayloadRef always builds a struct node)
t: U      = assign_tag {variant, disc, payload=p}
res       = assign_ref .local(t)                 # nominal alias hop (0 or more)
jump j_done                                      # join j_done: params=[res], body = (transitively) ret res
```

### Algorithm

**Step 1 — return-join map.** One walk of the proc body: for every `join j` with `params.len == 1`, compute `returnsParam(j)`: chase `j.body` — `ret p` where `p == params[0]` → yes; `jump k` → recurse into `k` requiring the same local; anything else → no. (Nested matches thread the *same* target local through nested done-joins, so this chase terminates trivially.)

**Step 2 — candidate walk.** DFS over the CFG (linear `next` chains; `switch_stmt` branches + `default_branch` + optional `continuation`; `join` body + remainder) with a `TrmcCandidateSet` ported from the Rust pass:

- Small fixed-capacity array interning candidate `LocalId`s + three `u64` bitsets: `active`, `confirmed`, `invalid`. **If a proc exceeds 64 candidates, skip the proc gracefully** (the Rust ArrayVec panicked).
- Candidate state machine per candidate, tracking the *current alias* local: `active(r)` → `boxed(b)` → `in_struct(p)` → `tagged(t)` → alias hops.
- Transitions (each is the *only* blessed use; the matched stmt advances the state):
  - `r = assign_call <self>(...)` → insert `active(r)`; record the stmt id and its predecessor (for later unlinking).
  - `b = assign_low_level box_box [r]` where local `b` has layout `box(U)`, `U == proc.ret_layout` → `boxed(b)`. (This is the recursion witness — no separate "is the union recursive" layout check needed.)
  - `p = assign_struct(fields)` where fields contain `b` **exactly once** → `in_struct(p)`; record the field position. Defensive arm: `assign_tag{payload = b}` directly.
  - `t = assign_tag{payload = p}` where `t`'s layout is `U` → `tagged(t)`; record `variant_index`/`discriminant` and the box stmt id.
  - `y = assign_ref .local(x)` where `x` is the current alias → alias hop, **recording the full chain** (the transform stores the chain *head*, see §3); also accept `.nominal`/`.list_reinterpret` defensively.
- **Confirmation (TRMC)**: current alias `x` is "returned" — terminal stmt is `ret x`, or `jump j` with `returnsParam(j) == x`. Sets `confirmed`, clears `active`.
- **Plain-tail detection (TCE)**: candidate still in `active(r)` state and terminal is `ret r` / `jump j` with `returnsParam(j) == r` → record as a *plain tail call* site.
- **Invalidation**: any other use of any tracked local (in any state, including confirmed) permanently invalidates that candidate — even retroactively un-confirming, exactly like the Rust `retain`. Per-stmt use detection mirrors `localValueUsedInPath` (`src/lir/arc.zig:1544-1650`): check `assign_*` operands, `set_local` value *and* target, `switch_stmt.cond`, `ret.value`, `debug.message`, `expect.condition`, `assign_call`/`assign_low_level` arg spans, struct/list/tag field spans. `incref/decref/free` cannot appear (ARC runs after us and panics on pre-existing RC ops).
- Path terminals (`ret`, `jump`, `crash`, `runtime_error`) end that branch; a candidate confirmed in one branch and *used* in another gets invalidated (shared mutable set, no save/restore — Rust semantics).

**Step 3 — decide, per proc** (mirrors old `apply_trmc`):
- ≥1 confirmed TRMC candidate → **TRMC transform** (which also converts the proc's plain tail self-calls to jumps).
- else ≥1 plain tail self-call → **TCE transform**.
- else → untouched.

Multi-candidate procs are supported: all confirmed candidates transform (each branch consumes its own); e.g. a binary-tree builder `Node (build ...) (build ...)` confirms the call feeding the constructor — the other recursive call stays a normal call (stack grows with tree *depth*, not size — same as the old compiler).

### Must-reject examples (encoded as tests, §6)

- Result used before the constructor (passed to `length`, inspected by a `match`, used twice in one constructor) → invalidated.
- Constructor built but not returned → never confirmed.
- Non-`tag_union` return with non-tail recursion (e.g. `fib`) → neither TRMC nor TCE.
- Mutual recursion pair; recursive call via erased/HOF call → never candidates.

## 2. New IR nodes required

### Layout: `LayoutTag.ptr` — `src/layout/layout.zig`, `src/layout/store.zig`

- Add `ptr` to `LayoutTag` (`enum(u4)`, 10/16 values used — `layout.zig:18-29`); packed exactly like `box` (inner `Idx` in `data`), constructor `Layout.ptr(idx)`, reuse `getIdx()`.
- `src/layout/store.zig`: add `insertPtr(elem: Idx) !Idx` (mirror `insertBox`, `store.zig:497`); `layoutContainsRefcounted` → **false** (this is what makes hole/head/cell locals invisible to ARC); `layoutSizeAlign` → `targetUsize` size/alignment. The compiler's exhaustive `LayoutTag` switches (intern key building, size/align, debug formatting, `rc_helper.zig:111` → no-op plan) will be flagged by the compiler — handle each like `box`-but-not-refcounted or like `opaque_ptr`.
- Backend layout classification: wasm `WasmLayout.zig` → `.i32` pointer (same as `box`); dev/LLVM → pointer-sized scalar; interpreter sizing helpers → usize.
- Semantics note (document on the type): `ptr(T)` is compiler-internal, never refcounted, never a struct field / tag payload / list element. Invariants are the pass author's responsibility (same contract as the old `LayoutRepr::Ptr`).

### LowLevel ops — `src/base/LowLevel.zig` (append after `box_unbox`)

| op | args | target layout | semantics | `rcEffect()` (exhaustive switch at `LowLevel.zig:507`) |
|---|---|---|---|---|
| `ptr_alloca` | — | `ptr(T)` | reserve a zeroed stack/frame slot sized+aligned for `T`; yield its address. Emitted once per proc entry (pre-loop) — backends may hoist to the prologue. | `none()` |
| `box_alloc_zeroed` | — | `box(T)` | heap cell via `allocateWithRefcount` (rc=1), payload **zero-filled**; yield data ptr. Bit-identical to a `box_box` whose payload is all zeroes. | `allocates()` |
| `ptr_store` | `[ptr, value]` | `zst` (unit) | `memcpy(*ptr, value, sizeOf(value's layout))` | consumes arg 1 (the value — ownership moves into the structure); arg 0 not refcounted |
| `ptr_load` | `[ptr]` | `T` | copy `sizeOf(target layout)` bytes out of `*ptr` | `none()` (target becomes ARC-owned via its refcounted layout — verified sound: ARC's ret protocol emits a net-zero incref/decref pair, `arc.zig:487-494`) |
| `ptr_cast` | `[p]` | `ptr(T)` | identity bits (used for `box(U)` → `ptr(U)`: the cell local stays `box`-typed and ARC-owned; the hole alias is untyped-for-ARC) | `none()` |

Sizing rule: ops never carry layouts — sizes come from the **target local's layout** (`ptr_alloca`, `box_alloc_zeroed` via inner of `ptr`/`box`; `ptr_load`) or the **value arg's layout** (`ptr_store`). This is why the typed `ptr(inner)` layout exists instead of reusing `opaque_ptr`.

### `LirProcSpec.tail_transform` side table — `src/lir/LIR.zig`

Add to `LirProcSpec`:

```zig
pub const TailTransform = enum(u8) { none, trmc, tce };
tail_transform: TailTransform = .none,
```

Set by the pass on every transformed proc (the old compiler's analogue: clearing `is_self_recursive` post-transform). Three consumers:
1. **Debug flag**: when env var `ROC_PRINT_TRMC` is set (checked once in `Trmc.run`), print one line per transformed proc to stderr — `trmc: <symbol> (N construct sites, M tail calls)` / `tce: <symbol> (M tail calls)` — plus the full before/after IR dump when `ROC_PRINT_IR_AFTER_TRMC` is set. Zero cost when unset.
2. **Test harness**: `test_helpers` exposes a lookup so tests assert through the *full pipeline* that a named proc was (or was not) transformed — strengthening the must-transform/must-reject suites beyond structural LIR walking (locate the proc the way `lir_inline_test.zig` does, then check the field).
3. **Interpreter validator precision**: the null-box-pointer relaxation (§5) applies only inside frames whose proc has `tail_transform == .trmc`, instead of weakening the validator globally.

### LirStore — no structural changes needed

`addLocal`/`addLocalSpan`/`addCFStmt`/`getCFStmtPtr` (in-place `next` splicing)/`getProcSpecPtr`/`getLocalPtr` already support everything (append-only arrays; replaced stmts become tolerated garbage, same as ARC's rewrite style). Fresh `JoinPointId` = (max id appearing in the proc) + 1 — ids only need per-proc uniqueness (`arc.zig:1437-1444` panics on duplicates). **`proc.frame_locals` must be rebuilt** (sorted, including all new locals) because the interpreter binary-searches it and `unreachable`s on a miss (`interpreter.zig:352-366`); `proc.join_points` does **not** need updating (ARC rebuilds it after us via `writeProcJoinPoints`, `arc.zig:54`).

`src/lir/lir_image.zig`: bump `FORMAT_VERSION` (line 22) — new enum values + the new proc field change serialized images.

## 3. Transformation

New file `src/lir/trmc.zig`, exported from `src/lir/mod.zig`:

```zig
pub fn run(store: *LirStore, layouts: *layout_mod.Store) ResourceError!void
```

Per transformed proc (construction blueprint: the hand-built join/jump loop in the dev test `src/backend/dev/LirCodeGen.zig:14542-14706`):

**New locals** (then rebuild `frame_locals`): `fresh_i` (one per proc arg, same layouts), `initial/hole/head : ptr(U)` where `ptr_u = layouts.insertPtr(proc.ret_layout)`, plus per-rewrite temporaries (`final: U` per ret site, `h2: ptr(U)` per construct site, `st: zst` per store). New join id `J`.

**Proc spec edits** (`getProcSpecPtr`): `args := addLocalSpan(fresh_*)`; `body := <the join stmt below>`; `frame_locals := sorted old ∪ new`; `tail_transform := .trmc`.

**Entry** — original arg locals become join params, so the body needs no renaming (same trick as the Rust pass, which kept old symbols as joinpoint params):

```
join J (params = [orig_args..., hole, head]) {
  body      = <original body, rewritten below>
  remainder =
    initial = assign_low_level ptr_alloca ()
    set_local orig_arg[i] := fresh[i]   (.initialize_join_param)   // each i
    set_local hole        := initial    (.initialize_join_param)
    set_local head        := initial    (.initialize_join_param)
    jump J
}
```

**Every `ret v` stmt** in the original body (all of them — they are the base/non-TRMC cases) is replaced in place (predecessor `next` re-pointed via `getCFStmtPtr`):

```
st    = assign_low_level ptr_store (hole, v)    // write the finished value into the hole
final = assign_low_level ptr_load (head)        // final: U — the outermost blob
ret final
```

**Each confirmed construct site** — original `... r=call(a_0..a_k); ...; b=box_box[r]; ...; p=struct(..b..); t=tag{p}; [x=ref.local(t)]; jump j_done`:

1. Unlink the `r = assign_call` stmt (`predecessor.next := call.next`). Intervening statements are untouched — the candidate analysis guarantees they don't mention `r`.
2. Rewrite the `b = box_box [r]` stmt in place to `b = assign_low_level box_alloc_zeroed ()`. **`b` keeps its `box(U)` layout and LocalId**, so the downstream `assign_struct`/`assign_tag` are untouched and ARC accounts the cell exactly as it does for non-TRMC construction (no validator/coercion special cases for struct fields).
3. **Peephole (Option A, in scope)**: splice `h2 = ptr_cast(b)` immediately **after** the `box_alloc_zeroed` (before the `assign_struct`), so the struct field is `b`'s last use — `ptr_cast` is non-consuming, this costs nothing and unlocks the future ARC transfer optimization (Option B). Unlink any alias-hop stmts (`x = assign_ref .local(t)`) on this path and store the **chain head** `t` directly.
4. Replace the terminal (`jump j_done` or `ret x`) with:

```
st = assign_low_level ptr_store (hole, t)        // chain HEAD, not the alias — t is provably dead
                                                 // after this on every loop path (reassigned before
                                                 // any use), so ARC's preserveConsumedArgMask
                                                 // (arc.zig:1313) cannot fire a protective incref
set_local orig_arg[i] := a_i  (.initialize_join_param)   // the recorded call-arg locals, in order
set_local hole        := h2   (.initialize_join_param)
jump J                                            // head untouched — params are plain mutable locals
```

No parallel-move hazard: `assign_call` args are always fresh temps (`lowerExprsToTemps`, `solved_lir_lower.zig:3137-3141`), never the param locals themselves — add a debug assert. This mirrors how `lowerContinue` (`:2391-2405`) + `prependJoinParamInitializers` (`:3154-3177`) already write loop-back params (same `.initialize_join_param` mode, which ARC treats as non-releasing initialization, `arc.zig:403-422`).

**Each plain tail self-call** (`r = call(a..)` + terminal return of `r`): unlink the call; replace the terminal with the `set_local` block + `jump J` (hole/head unchanged — same as the Rust commit "in TRMC, still apply normal TCE"; required for parity on `filter`-shaped functions).

**TCE-only transform** (procs with plain tail self-calls but no confirmed constructor): identical wrapper minus hole/head/ptr machinery — join `J` with params = original args, entry `set_local` + `jump J`, each tail self-call site → `set_local args + jump J`, all `ret` stmts untouched. `tail_transform := .tce`. Shares the return-join map, candidate walk, wrapper builder, and param threading with TRMC.

**Jump arguments** don't exist in this LIR (`jump` carries only a `JoinPointId`); all threading is via the `set_local ... (.initialize_join_param)` writes above — the existing loop convention; no backend needs phi nodes.

### Why this is correct w.r.t. ARC (runs after us)

`local_contains_refcounted` is computed over *all* locals at `Arc.insert` entry (`arc.zig:24-28`), so our new locals are covered. `ptr(U)` locals are invisible (not refcounted). The cell `b` stays `box(U)`/owned: struct construction increfs it (`retainSpan`, `arc.zig:1663`), the jump's release-difference decrefs it — netting rc=1 owned by the parent node, identical to non-TRMC accounting (this incref/decref pair is parity with today's `box_box` path; removing it globally is the Option B follow-up). `ptr_store` consumes the node value — and because we store the dead chain-head, the consume is a clean ownership transfer with no protective incref. `ptr_load`'s result is owned-with-an-actual-+1 (the structure has exactly one reference per cell); ARC's ret protocol (`retainLocalIfRc` + `releaseAll`, `arc.zig:487-494`) emits a net-zero incref/decref pair. The alias-hop move optimization (`arc.zig:296-303`) already handles `assign_ref .local` of dead sources, so untouched non-TRMC paths stay clean. Partially-built cells are zero-filled, and `decref/incref/free` are null-safe (`src/builtins/utils.zig:436,478,510`), so an in-flight structure is decref-safe — though nothing can reach it mid-loop anyway (only via hole/head, which ARC ignores).

## 4. Pass insertion point

`src/lir/checked_pipeline.zig`, inside `lowerCheckedModulesToLir`, between line 214 (`errdefer lowered.deinit()`) and line 216:

```zig
try Trmc.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
try Arc.insert(&lowered.lir_result.store, &lowered.lir_result.layouts);
```

Always on — no flag. Rationale for the position:
- **Before `Arc.insert`** (same as the old pipeline: `apply_trmc` before `insert_inc_dec_operations`): TRMC deletes a call and changes an allocation site; ARC must see the *final* data flow to place retains/releases. ARC also panics if it encounters pre-existing RC statements, so TRMC must not run after it.
- **After `SolvedLirLower.run`**: detection needs the final monomorphic LIR shapes (`box_box` boundaries, done-joins), and the inliner (`SolvedInline`, which runs before lowering) has already simplified bodies.
- State required: `*LirStore` (mutable) and `*layout.Store` (mutable, for `insertPtr`) — both owned by `lowered.lir_result` at this point. Note `Arc.insert` takes `*const` layouts; TRMC needs `*`.

This single call site covers every consumer: CLI, eval tests (both the native and the `wasm_lowered` u32 compile in `test_helpers.compileInspectedProgramImpl`), snapshot tool, glue, playground.

## 5. Backend changes

All four backends get the five ops in this PR. Each has an exhaustive (or must-be-made-explicit) LowLevel switch:

### `src/eval/interpreter.zig` — `evalLowLevel` (`:3803`, exhaustive)

- `ptr_alloca`: arena-allocate a zeroed `sizeOf(inner)` cell (`allocAlignedByteSlice`, `:670`), store its address into a pointer-sized `Value` (write pattern: `boxed.write(usize, @intFromPtr(...))`, `:2805`; u32 on wasm-lowered targets).
- `box_alloc_zeroed`: `allocRocDataWithRc` (`:690`) + `@memset(0)` — exactly `evalBoxBox` (`:6697`) minus the copy-in. Goes through RocOps so allocation counting works.
- `ptr_store`/`ptr_load`: memcpy by the value/target layout size (read-through pattern: `readBoxedDataPointer`, `:6454`).
- `ptr_cast`: copy the pointer word.
- Debug validator: add a `.ptr` arm (no-op) to `debugAssertValueMatchesLayoutAt` (`:907+`); in the `.box` arm (`:952-987`), allow a null data pointer **when the current frame's proc has `tail_transform == .trmc`** (a legal in-flight hole) — panicking as before in all other procs, so the bug detector keeps its strength. Add a `.ptr` identity arm to `coerceExplicitRefValueToLayout` (`:6653`).
- Stack-safety payoff: TRMC/TCE'd procs loop via join/jump (`:1844-1853`), escaping the 1024 `max_call_depth` cap.

### `src/backend/dev/LirCodeGen.zig` — `generateLowLevel` (`:1365`, exhaustive; x86-64 + aarch64 via comptime target)

- `ptr_alloca`: `codegen.allocStackSlot(size)` (`x86_64/CodeGen.zig:370`, `aarch64/CodeGen.zig:380`) + `zeroStackArea` + LEA of the slot into a `general_reg` (`addLeaArg` pattern, `:1419`). FrameBuilder already patches the final frame size.
- `box_alloc_zeroed`: `callBuiltin(allocateWithRefcountC)` (pattern at `:9311` and box_box `:3450-3505`) + inline zero of the payload (the dev box path currently does *not* zero cells — TRMC requires the zero-fill, emit it here).
- `ptr_store`/`ptr_load`: `emitSizedStoreMem`/`emitSizedLoadMem` (`:7782`/`:7748`) + `copyChunked` for blob-sized copies through the pointer register (box_box/box_unbox are the templates, `:3450-3557`).
- `ptr_cast`: register move.
- `.ptr` layout classification: pointer-sized scalar (like `box`), `ValueLocation.general_reg`/8-byte stack slot.

### `src/backend/wasm/WasmCodeGen.zig` — `generateLowLevel` (`:8195`, exhaustive)

- `ptr_alloca`: `allocStackMemory(size, align)` (`:5602-5612`, shadow-stack bump) + zero-fill + materialize `fp + offset` via `emitFpOffset` into an i32 local.
- `box_alloc_zeroed`: `emitHeapAllocConst` (box_box template at `:9662`) + zero stores (or `memory.fill`).
- `ptr_store`/`ptr_load`: `emitMemCopy`/`emitStoreToMemSized`/`emitLoadOpForLayout` at offset 0 from the pointer local.
- `ptr_cast`: i32 local copy. `.ptr` valtype: `.i32` (same as `box` in `WasmLayout.zig:93`).
- Sizes come from the **wasm-lowered (u32) layout store** the backend already holds — pointer fields are 4 bytes there; nothing special needed beyond using `layoutSizeAlign` as the existing ops do.

### `src/backend/llvm/MonoLlvmCodeGen.zig` — `emitLowLevel` (`:1036`)

⚠ This switch has an `else => emitNumericConversionOrCrash` fallback (`:1108`) — without explicit arms the new ops **compile silently and crash at runtime**. Add explicit arms before the `else`:
- `ptr_alloca`: `wip.alloca` of the inner size + `zeroBytes` (every LIR local is already an entry alloca, `:561-576`; the op site is pre-loop so a `.normal` alloca executes once).
- `box_alloc_zeroed`: call `roc_builtins_allocate_with_refcount` + `zeroBytes` — exactly the existing `allocAggregateTarget` `.box` branch (`:1008-1024`) minus the payload copy.
- `ptr_store`/`ptr_load`: `copyBytes` between the pointer and the value/target slot; `ptr_cast`: pointer copy (LLVM ptrs are untyped).
- Joins are already basic blocks (`emitJoin`/`emitJump`, `:1462/:1480`); nothing else needed.

## 6. Verification and test strategy

### 6.1 Unit tests for detection + transformation (no full pipeline)

Two styles, both in/alongside `src/lir/trmc.zig` and a new `src/eval/test/trmc_lir_test.zig` (registered like `lir_inline_test.zig`, `build.zig:3380`):

- **Hand-built LIR** (template: dev test `LirCodeGen.zig:14542-14706`): construct a proc directly in a `LirStore` (call → box_box → struct → tag → ret), run `Trmc.run`, walk `store.getCFStmt(...)` asserting: a `join` with `n_args+2` params exists; no `assign_call` to self remains; `box_alloc_zeroed`/`ptr_store`/`ptr_load`/`ptr_alloca` appear with the right targets; `frame_locals` is sorted and complete; `tail_transform` is set correctly. Best for invalidation edge cases hard to phrase in surviving Roc source: result used twice in one constructor, result used by an `expect`, candidate confirmed in one branch and used in another (retroactive un-confirm), >64 candidates (graceful skip).
- **Pipeline-driven structural** (template: `src/eval/test/lir_inline_test.zig:45-130`): compile Roc source through `lowerCheckedModulesToLir`, then assert via **`tail_transform`** (primary — survives ARC untouched) plus post-ARC structure (joins, absence of self `assign_call`, presence of ptr ops).

**Must transform — TRMC** (`tail_transform == .trmc`): `repeat`, linked-list `map`, `filter` (TRMC branch + plain-tail branch both rewritten), a 3-field constructor with the recursive call in each field position, a nested-match producer (exercises `returnsParam` chasing through nested done-joins), binary-tree `build` (two recursive calls — exactly one confirmed, the other remains a real `assign_call`).

**Must transform — TCE** (`tail_transform == .tce`): accumulator `sum`, countdown, a function whose tail self-call sits behind nested match/if.

**Must NOT transform** (`tail_transform == .none`): result passed to `length` before the constructor; constructor assigned but a different value returned; `fib`-shaped non-tail recursion; mutual recursion pair; recursive call inside an erased/HOF call.

### 6.2 Integration / golden tests (end-to-end)

New `src/eval/test/eval_trmc_tests.zig` with data-driven `TestCase`s, registered alongside `eval_recursive_data_tests.zig` in the parallel runner (`src/eval/test/parallel_runner.zig` runs every case on interpreter + dev + wasm, +llvm with `--llvm`, requiring **byte-identical inspect output across backends** — that cross-backend equality is itself the strongest golden check). Run via `zig build test-eval --summary all -- --test-filter "trmc"`.

- `repeat` + `length`, n=5 → `5` (port of old `linked_list_trmc`, `gen_primitives.rs:4506`).
- `repeat` n=3 with the **full list inspected** (exercises construction + inspect traversal + teardown of the TRMC-built structure on every backend).
- `map`, `filter` (mixed TRMC + plain-tail branches — port of `linked_list_filter`, whose golden IR was the old pass's flagship test).
- Binary-tree build (depth ~12) + fold/sum.
- TCE: accumulator `sum 1_000_000` (also a stack gate, below).
- A must-NOT-transform control case (result-used-twice function) asserting unchanged output.
- **Allocation parity**: `allocations_at_most` expectation (`parallel_runner.zig:108,124-127`, checked at `:899`) on `repeat 3` with an `I64` element — TRMC must not change the count (n cells for n≥1; the outermost node is by-value). Pin the exact number.

### 6.3 Benchmark ports — acceptance criteria

Port the three old benchmark programs (`git show ae47cc5171:crates/cli_testing_examples/benchmarks/{CFold,NQueens,RBTreeCk}.roc`) to current Roc syntax (match expressions, `|args|` lambdas, `:=` nominal types) as eval test cases:

- **CFold** — builds and folds large recursive `Expr` trees (TRMC on the builders); run at a depth whose node count exceeds the interpreter's 1024 frame cap so it fails without TRMC.
- **NQueens** — list-building + backtracking (TRMC + TCE together).
- **RBTreeCk** — red-black tree insertion (recursive constructors, multiple variants, multi-field nodes).

Each runs on all four backends with a pinned expected output at a size that (a) crashes pre-TRMC/TCE and (b) keeps CI time reasonable. These are **required for merge**, both as correctness gates and as the fixture for measuring the Option-B ARC follow-up. (Timing/perf tracking itself is out of scope; correctness at depth is the gate.)

### 6.4 Stack-safety correctness test (the key gate)

- **Interpreter gate (cheap, deterministic)**: TestCase `repeat 0 100_000 |> length` → `100000`. The interpreter's `max_call_depth = 1024` guarantees this **crashes today** and passes only with TRMC. Twin TCE gate: `sum` to 1_000_000. These are the primary correctness gates and need no OS stack tricks.
- **Native dev gate**: same cases at n=1_000_000 run in the parallel runner's forked subprocess; a stack overflow is reported as `signal_death` (SIGSEGV) without killing the harness — failing pre-TRMC, passing post.
- **Small-stack determinism** (avoids needing 10⁶ recursions to observe overflow): a dedicated unit test in `src/eval/test/trmc_stack_safety_test.zig` that runs `devEvaluatorStrWithStats` (`src/eval/test_helpers.zig:1944-2012` — plain function, codegen + `callRocABI` on the calling thread) on `std.Thread.spawn(.{ .stack_size = 256 * 1024 }, ...)` with n=50_000. Without TRMC that overflows a 256 KB stack reliably; with TRMC it completes. (Same trick works for the LLVM path via `llvmEvaluatorInspectedStr`.)
- **Wasm**: n=100_000 through the bytebox runner (`src/eval/wasm_runner.zig`); verify bytebox's call-depth/stack config makes the pre-TRMC failure observable, tune n if needed.

### 6.5 ARC correctness (no leak, no double-free)

- **Leak check**: dedicated unit test using `RuntimeHostEnv` (`src/eval/RuntimeHostEnv.zig`): every RocOps allocation is tracked (`allocation_tracker`, `:113`); after eval → consume result → release, call `checkForLeaks()` (`:151-153`, errors if any allocation is live). Run for: `repeat` with `I64` elems, `repeat` with `Str` elems (exercises loop-carried refcounted join params), and a TRMC'd list that is built, traversed, and dropped. Debug builds also poison rc slots on free (`:33-36`), so a double-free (e.g. head decremented twice) faults loudly.
- **"Decremented exactly once"**: the leak check + poison check together pin this — a missed decref leaks (caught by `checkForLeaks`), an extra decref double-frees (caught by poisoning / the test allocator). If a failure needs diagnosis, the interpreter's `performRawRcPlan` paths are easy to instrument temporarily.
- **Allocation-count parity** (6.2) closes the remaining gap (e.g. an extra unfreed-but-balanced cell).

### 6.6 Snapshot / IR golden tests

No LIR pretty-printer exists today (only Debug-mode stderr dumpers, `interpreter.zig:1241,1860`). Add a small one:
- New `src/lir/debug_print.zig`: `writeProc(store, layouts, proc_id, writer)` — compact stable text (locals with layouts, stmt chain, joins indented).
- Golden tests in `trmc_lir_test.zig`: dump `repeat`'s proc **before and after** `Trmc.run` (and one TCE'd proc) and compare with `std.testing.expectEqualStrings` against inline expected text — any inadvertent change to the transformation shows up as a reviewed diff. (Inline goldens rather than snapshot files keeps this out of the `zig build snapshot` machinery, which is for .md corpus snapshots.)
- Dev affordances: `ROC_PRINT_TRMC=1` lists transformed procs via `tail_transform`; `ROC_PRINT_IR_AFTER_TRMC=1` dumps the IR of each transformed proc to stderr (mirrors the old debug flag).

### 6.7 Regression guard

- `src/eval/test/eval_recursive_data_tests.zig` runs on all four backends with zero skip flags; several of its recursive builders will now be TRMC'd, and the runner's byte-identical-output requirement makes the whole suite an automatic regression net. Same for the list/str suites and `test-eval-host-effects`.
- Iterate with narrow steps (`zig build test-eval`, `test-cli`); full `zig build test` + `zig build snapshot` (regenerate, never hand-edit) before merge.

## 7. Delivery — one PR, ordered milestones

Everything lands in a single always-on PR. Suggested commit/milestone order, each independently green:

1. **Ops layer**: `LayoutTag.ptr` + five LowLevel ops + `rcEffect` arms + all four backend implementations + `lir_image` `FORMAT_VERSION` bump. Ops are dead code at this point; unit-test them with hand-built LIR procs (interpreter + dev evaluator).
2. **The pass**: `src/lir/trmc.zig` (detection + TRMC transform + TCE transform + `tail_transform` + peephole + debug flags) + hand-built-LIR unit tests + `src/lir/debug_print.zig` + golden before/after tests.
3. **Wire-up + full suite**: pipeline call in `checked_pipeline.zig` + interpreter validator changes + e2e suites (6.2, 6.4, 6.5) + existing-suite regression pass.
4. **Benchmarks**: CFold/NQueens/RBTreeCk ports (6.3) green on all four backends.

**Acceptance criteria for merge**:
- Interpreter gates: `repeat` n=100_000 and `sum` n=1_000_000 pass (both crash without the pass).
- 256 KB-thread dev gate passes; wasm deep gate passes; llvm lane (`--llvm`) passes.
- `checkForLeaks()` clean on I64 and Str TRMC lists; allocation-parity counts pinned.
- CFold, NQueens, RBTreeCk correct on all four backends at stack-breaking sizes.
- All existing suites green: full `zig build test`, snapshots regenerated.
- Golden IR dumps + `tail_transform` assertions for the must/must-not sets.

**Named follow-up (separate PR, not in scope)**: ARC transfer-into-constructor optimization ("Option B") — extend `assign_struct`/`assign_list`/`assign_tag` handling with `callArgOwnership`-style transfer for owned-and-dead field locals (machinery exists: `arc.zig:1336`, `canMoveSetLocalValue` `arc.zig:1277`), eliminating the per-node cell incref/decref pair in **all** constructor code (TRMC and non-TRMC alike — today every `box_box`'d recursive field pays it). The TRMC loop is already prepared for it (ptr_cast ordered before the struct). Prioritize using the benchmark fixtures.

## 8. Open questions and risks

1. **Residual RC churn**: with the Option-A peephole, the stored node is a clean transfer; the remaining per-iteration pair is the cell's `retainSpan` incref + jump decref — exact parity with today's non-TRMC constructor output, removed program-wide by the Option-B follow-up. Verify the golden IR dump shows no `preserveConsumedArgMask` incref before `ptr_store` (it provably shouldn't, since the chain head is reassigned-before-use on every loop path).
2. **Loop-carried refcounted join params** (e.g. a `Str` element param written each iteration with `.initialize_join_param`): ARC's `canMoveSetLocalValue`/jump-release machinery already governs existing loops, but TRMC'd procs are a new producer of this shape — the `Str`-element leak test (6.5) is the specific guard. If it leaks, the fix is choosing `.replace_existing` for loop-back writes; decide empirically.
3. **`switch_stmt.continuation`**: lowering appears to always use done-joins (`boolSwitchNoContinuation`), but the field exists — the detection walk must traverse it; verify whether any producer sets it non-null (grep during implementation).
4. **Exotic candidate shapes**: `assign_tag{payload = b}` with no struct (shouldn't occur — `buildPayloadRef` always builds a struct, `type_layout_resolver.zig:425-453`) and the same call result appearing twice in one constructor (analysis confirms then the transform must guard — invalidate instead, mirroring the Rust hole). Both covered by defensive arms + unit tests.
5. **Wasm deep-recursion observability**: confirm bytebox's stack/call limits make the pre-TRMC failure visible at n=100_000 (read `src/eval/wasm_runner.zig` config during implementation; tune n).
6. **>64 candidates in one proc**: skip the proc (never panic) — unit-tested.
7. **`lir_image` compatibility**: enum + field additions; `FORMAT_VERSION` bump invalidates caches — confirm nothing persists images across compiler versions in tests.
8. **Snapshot corpus**: program outputs shouldn't change, but if any snapshot embeds allocation counts or IR-adjacent details, regenerate with `zig build snapshot` (never hand-edit).
9. **TCE interaction with `loop_continue`/`loop_break` markers**: source-level loops already lower to joins; TCE only fires on `assign_call`-to-self, which loop-shaped procs don't contain — verify against the `lowerContinue` shapes during implementation.
10. **Benchmark syntax porting**: CFold/NQueens/RBTreeCk use 2023-era Roc syntax; porting to current syntax (match, `|args|`, `:=`) may hit unimplemented language features — if one does, substitute a behavior-equivalent reduction and note it in the test file.
