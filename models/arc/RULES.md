# ARC model: rules

Certifier rejections print their `C-` or `M-` rule name in brackets. The machine reports a violation by its name
(USE-AFTER-FREE, DOUBLE-FREE, LEAK, SHARED-MUTATION, STALE-REUSE, VALUE-MISMATCH); the `M-`, `O-` and `R-` names below
label the machine's cases in this table and in some source comments. Design sections are in `design.md`. Compiler sites
name the function or row that implements the rule, not line numbers.

There are three judgments:

1. **The heap machine** (`lean/RocArc/Machine.lean`, `M-`, `O-` and `R-` rules): runs a dump's root procedures on a
   refcounted heap and reports memory errors. This is the ground truth for bugs.
2. **The certifier judgment** Σ ⊢ p (`lean/RocArc/Certify.lean`, `C-` rules): an independent restatement of the debug
   borrow certifier as an abstract interpretation of each procedure.
3. **R-Equiv** (`Machine.lean`, `rocarc equiv`): with in-place reuse on and off, every root returns the same value.

## Heap machine

| Rule | What it says | design.md | Compiler site |
|---|---|---|---|
| M-Val | Values are scalars (possibly unknown), handles to cells, inline aggregates (struct, or tag with variant and discriminant), or `uninit`. Structs and tags are by value; only lists, strings, boxes and erased callables are cells. | ARC Borrow Inference / Resources Over Layouts | `layout.zig` `LayoutTag` |
| M-Static | A cell with count 0 is static: `incref` and `decref` leave it alone and it is never unique. Literals (`str`, `bytes`, `static_data`) are static cells. | Compile-Time Evaluation And Static Storage | `immortal_locals.zig` (`REFCOUNT_STATIC_DATA` is 0) |
| M-Inc | `incref x n` adds n to every cell reachable from x without entering the heap. A dead cell is **USE-AFTER-FREE**. | RC Planning and Materialization | LIR `incref` |
| M-Dec | `decref` / `free` subtracts 1 from each such cell. At 0 the cell dies and its children are released. A dead cell is **DOUBLE-FREE**. | Releasing The Last Reference | LIR `decref`, `free` |
| M-DecInit | `decref_if_initialized` releases only an initialized value. Initializedness is read from the value, not from `cond & mask` (gap G4). | RC Planning and Materialization | LIR `decref_if_initialized` |
| M-Read | A low-level op reads the allocation behind each top-level handle of each argument, except its `representationArgs` (`list_len`, `list_capacity`, `list_prefetch` read only the by-value header). A dead cell is **USE-AFTER-FREE**. | Constraints Per Statement Form | `LowLevel.zig` `representationArgs` |
| M-Alias | `ref.local`, `ref.nominal`, `ref.list_reinterpret` and `set` copy the value: no count change, no read. | Lifetimes Over Statement Structure | |
| M-Proj | `ref.field` and `ref.tag_payload*` select inline. `take` has no runtime effect; it is ownership bookkeeping. | Field Takes From Dying Aggregates | |
| M-Ctor | `list(xs)` allocates a cell with count 1 holding the operands, whose units move in. `struct` and `tag` build inline values. | Constraints Per Statement Form | `arc_certify.zig` header |
| M-Call | A call runs the callee in a new frame. Parameter ownership is whatever the explicit RC statements make it. | Interprocedural Solving | |
| M-Join | `join j` registers the body and runs the remainder; `jump j` continues at the body in the same frame. | | `debug_print.zig` `join` |
| M-Switch | A known scrutinee picks its branch. An unknown one forks every branch, up to `--forks` per path; after that the first branch is taken. | | |
| M-Leak | At the root's `ret`, every live, non-static cell not reachable from the returned value is a **LEAK**. Only leak roots are reported. | Releasing The Last Reference | |
| M-Unique | A runtime-uniqueness op on argument cell c. In `--checked` mode it writes in place iff `count(c) == 1`, otherwise it copies. In the default (stamped) mode, an argument with its `unique=` bit set is written in place unconditionally, and `count(c) != 1` is **SHARED-MUTATION**. | Uniqueness Inference; In-Place Mutation Interaction | `RcEffect.may_runtime_uniqueness_check_args`; `arc_certify.zig` `certifyUniqueArgs` |
| M-InPlaceUnchecked | `list_set_in_place_unsafe` and `list_map_write_unsafe` always write in place, so `count != 1` is **SHARED-MUTATION** in both modes. | In-Place List Transforms | their `RcEffect` rows |

### Op semantics, from the generated `RcEffect` rows (`lean/RocArc/OpTable.lean`)

| Rule | Row fields | Meaning in final LIR |
|---|---|---|
| O-InPlace | `uniqCheck`, `consume`, `aliasConsumed` | M-Unique on the first such argument; the result is that cell or its copy |
| O-Retain | `retain_args` | The op stores the handle and adds no count. ARC materialized the retain as an explicit `incref` before the op, or transferred the caller's unit (`arc.zig` `retainMaskedArgs`). |
| O-RetainResult | `retain_result` | The result aliases storage that stays live; ARC emits the `incref` after the op. |
| O-Borrow | `result_borrows_args` | The result is an element of the borrowed argument (by index if known, else element 0), unowned |
| O-Share | `result_shares_args` with retain | A slice: the result is the same cell, paid for by the explicit `incref` or the caller's unit |
| O-ShareOwn | `result_shares_args` without retain or consume | The op retains the shared allocation itself. A tag-union result (`str_from_utf8`) is taken to be `Ok` holding the handle, so the `Err` path of such an op is not explored. A struct result (`*_from_str_prefix`) holds the handle at the field the procedure later reads as a `str` or `list`. |
| O-UniqueQuery | `list_owned_unique`, `list_slack_unique`, `list_map_can_reuse` | Exact: 1 iff the cell has count 1, else 0. Loops cache these flags to pick an in-place write later, so they are not forked as unknown. |
| O-DeadLender | `deadLenderArg` (`box_unbox`, `erased_capture_load`) | Never in final LIR: ARC rewrites it to the borrowed variant plus explicit counts. The machine reports *stuck* if one appears. |
| O-Consume | `consume` without alias | Moved into a fresh result cell, or released if the result is a scalar |
| O-Fresh | `may_allocate` or `result_unique`, with a refcounted result | A new cell with count 1 |
| O-Exact | `list_len`, `list_get_unsafe`, `list_append_unsafe`, `list_prepend`, `list_set`, `list_replace_unsafe`, `list_concat`, `list_with_capacity`, integer arithmetic and comparisons on known scalars | Exact element tracking |

### In-place reuse and dismantling

| Rule | What it says | design.md | Compiler site |
|---|---|---|---|
| R-Reuse | An in-place write *moves* the cell: it gets a fresh address with the same count and contents, and the old address becomes dead and records where the cell went. The op's result is the new address, so every other reference that existed before the write now names a dead address. This applies to every runtime-uniqueness op taking its in-place branch and to `list_map_prepare_reuse` on a count-1 list. | In-Place Mutation Interaction; Destination-Passing Results and Allocation Reuse | `LowLevel.zig` `consumesArgsReturningConsumedArgs`; `loop_append_promote.zig` |
| R-Stale | A touch (count op, op read) of an address that died by R-Reuse is **STALE-REUSE**: the program observes a cell after it was overwritten in place, through a reference other than the one the write consumed and returned. | In-Place Mutation Interaction | |
| R-MapWrite | `list_map_write_unsafe` and `list_set_in_place_unsafe` write without a check and without moving: the map loop writes repeatedly through the one handle `list_map_prepare_reuse` returned. `count != 1` stays SHARED-MUTATION. | In-Place List Transforms | `loop_append_promote.zig` |
| R-Take | A `take` has no runtime effect (M-Proj). Dismantling is checked through what it moves: a later release of the whole container is DOUBLE-FREE (M-Dec), and a later read of the slot after the taken value was reused in place is STALE-REUSE. | Field Takes From Dying Aggregates | `arc_dismantle.zig` |
| R-Pure | The unoptimized reference semantics: every in-place-capable op copies, count statements and op-internal releases do nothing, nothing is freed, and there is no leak check. `list_map_write_unsafe` writes into the copy `list_map_prepare_reuse` made. | | |
| R-Equiv | For each root, the stamped run (reuse on) and the R-Pure run return the same value on each path. Values are rendered through the heap; an unknown scalar (`?`) matches anything. Paths are compared pairwise only when both runs finish the same number of paths and the stamped run has no violation. A difference is **VALUE-MISMATCH**. | | |

## Certifier judgment Σ ⊢ p

The design's "Debug Borrow Certifier" section and the `arc_certify.zig` module header list checks (a) to (h); the rules
below restate (a) to (g). Check (h), atomicity and host visibility, is not modeled.

Joins are walked once per distinct entry summary, the design's "one walk per distinct summary". The summary covers only
the join's live-in locals: those its body, or a join body reachable from it by `jump`, mentions. Values that no live-in
local names contribute one bit: whether any of them is owned, non-residual and unbalanced. Born-uniqueness is not part of
the key; it is merged by "and", as the design's lattice does.

| Rule | Check | Compiler site (`arc_certify.zig` unless noted) |
|---|---|---|
| C-Param | Owned params start with one unit; params with a `borrowed=` bit start borrowed (the caller is the lender) | `certifyProcAbiMetadata` |
| C-Lit | Literals are static: no unit, always live. The dump is after `ImmortalLocals.elide`. | `immortal_locals.zig` `elide` |
| C-Alias | An alias names the same value | module header |
| C-Proj, C-Read | Every read finds its value live: a unit remains, it is static, every lender is live, or its holder is live | check (a); `requireLive` |
| C-Take | A take marks the slot taken and gives the result its own unit. It claims every container above and leaves them residual. | check (f); `bindPayloadRead` |
| C-ReadAfterTake | Reading a taken slot is rejected | check (f) |
| C-Claim, C-ClaimTwice | Spending a borrowed projection of an owned container claims that slot's unit, at most once. Any held unit of a non-static container can be claimed, including one an `incref` added to a borrowed value; claims are tried before a C-Ctor's transient negative spend. | `tryClaim`; `arc_dismantle.zig` |
| C-Ctor | `list`, `struct` and `tag` spend one unit per refcounted operand (transiently negative is allowed); the operand then lives through its holder | module header |
| C-StaticPart | A projection of static data is static. A `struct` or `tag` whose refcounted operands are all static is static. | |
| C-Call, C-CallBorrow, C-CallOwned | A borrowed param needs a live argument; an owned param spends a unit. The result is owned, or borrowed if the callee has `ret_borrowed`. | check (e) |
| C-Op, C-OpConsume, C-OpRead | `consume` and `retain` positions spend a unit; other positions need liveness, except `representationArgs`. `result_borrows_args` and `retain_result` results are borrowed. | check (e) |
| C-Inc, C-Dec | `incref` needs a live value; `decref` and `free` need a unit | checks (a), (b) |
| C-Ret, C-RetBorrowed, C-Crash | `ret` spends the value's unit, or needs liveness if `ret_borrowed`; `crash lN` spends the message | |
| C-Balance | At every terminal, every owned, non-residual value has balance 0 | check (c); `checkLeaks` |
| C-Jump | A join body is walked per distinct entry summary (above) | check (d) |
| C-InitSwitch | `switch_initialized_payload` walks the initialized branch only when the payload is bound on this path (G4) | |
| C-UniqueStamp, C-Birth, C-BirthTake, C-BirthAgg, C-BirthAlias | Check (g), independently of the solver: a stamped argument must be born-unique and hold exactly one unit. Births are a fresh `list`, an op with `result_unique`, a call to a `unique=true` callee, a take moving the only unit out of a unique container, a struct or tag whose refcounted operands are all unique, or an op result that is a consumed argument's own allocation (`result_aliases_consumed_args`) when that argument was born-unique. Any `incref` destroys uniqueness. | check (g); `certifyUniqueArgs` reuses `arc_solve.computeProcUniqueness`, which this rule does not |
| C-Unsupported | A statement the certifier model does not interpret (`dyn.*`, erased calls, string matches). Printed as `UNSUPPORTED`, not counted. | |

The certifier model also gives up (printed `GAVE-UP`, not counted as a rejection) in three cases:

| Rule | When |
|---|---|
| M-Growth | More than 2000 distinct entry summaries at one join. The real certifier merges at joins, while this model enumerates them. |
| M-Budget | More than 200000 statements walked in one procedure |
| M-UniqueSeed | A stamped argument derives from a parameter. Its uniqueness comes from the variant's `unique` seeds, which the dump does not print (G3). |

## Theorems (`lean/RocArc/Theorems.lean`)

| Name | Status | Statement |
|---|---|---|
| `certifier_sound` | stated, `sorry` | If every procedure certifies and the ops conform to their rows, the `--checked` exploration of every root reports no violation |
| `unique_args_equiv` | stated, `sorry` | If no path reports SHARED-MUTATION, the stamped and checked runs agree on whether any violation occurs. The proof plan is a lock-step simulation; the two differ only at a stamped op. |
| `Reuse.reuse_sound` | stated, `sorry` | If the stamped run has no violation and both runs finish the same number of paths, the stamped and R-Pure runs return the same values (R-Equiv). A VALUE-MISMATCH with no violation is a counterexample, and so a model bug. |
| `incAddr_static` | proven | `incref` on a static cell leaves the heap unchanged (M-Static) |
| `Reuse.forward_moves` | proven | R-Reuse's heap update: the new address is the old heap size, count and contents are kept, and the old cell is dead and forwarded |

## Fixtures (`tests/`)

| File | Expected | Pins |
|---|---|---|
| `ok.dump` | clean in every judgment | an alias of a list kept live by an `incref`, so `list_set` copies |
| `stale.dump` | STALE-REUSE | the same without the `incref`: the alias is read after `list_set` reused the list in place |
| `hidden.dump` | VALUE-MISMATCH only | `list_set_in_place_unsafe`, then a read through the consumed name: no count error, caught only by R-Equiv |
| `shared.dump` | SHARED-MUTATION when stamped, clean when checked, C-UniqueStamp | a `unique=` stamp on a list with count 2 |
| `leak.dump` | LEAK, C-Balance | a missing `decref` of an alias |
| `smoke.roc` | clean, all tests pass | one real compilation at `--specialize=yes` and `--opt=speed` |
