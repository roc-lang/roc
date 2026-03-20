# Analysis WIP

This document records the current state of investigation into the remaining dev-backend snapshot failures, with emphasis on:

- `List.fold_rev` crashing in the dev backend with `Use-after-free: decref on already-freed memory`
- `Num.mod_by` still behaving like remainder in the dev backend
- related list snapshot regressions that remained during this round

The goal is to let the next person resume from the current evidence instead of repeating the same hypotheses.

## Current snapshot failures seen during this round

Running:

```sh
zig build snapshot
```

still reported these relevant REPL mismatches:

- `test/snapshots/repl/list_fold_rev_basic.md`
- `test/snapshots/repl/list_fold_rev_subtract.md`
- `test/snapshots/repl/num_mod_by.md`
- `test/snapshots/repl/list_take_first.md`
- `test/snapshots/repl/list_take_first_all.md`
- `test/snapshots/repl/list_take_last.md`
- `test/snapshots/repl/list_take_last_all.md`
- `test/snapshots/repl/list_drop_first.md`
- `test/snapshots/repl/list_drop_last.md`
- `test/snapshots/repl/list_tags.md`

The two `fold_rev` cases still crashed with:

```txt
Dev backend crash: Use-after-free: decref on already-freed memory
```

The `num_mod_by` cases still showed remainder-style sign behavior:

- expected `2`, got `-1`
- expected `-1`, got `2`

The list `take`/`drop`/`tags` failures remained unchanged through this round as well, which matters because it suggests there may be a broader dev-backend issue around list helpers or compiled builtin procedures, not only the specific `fold_rev` path.

## What was already known before this writeup

Prior investigation had already established:

- a speculative RC change in [`src/lir/rc_insert.zig`](/home/lbw/Documents/Github/roc/src/lir/rc_insert.zig) treating outer RC bindings in `while` loops as loop-carried did not move `fold_rev`
- that patch was reverted
- `fold_rev` still looked like a specialized path involving `list_get_unsafe`, not generic while-loop tail cleanup
- `num_mod_by` looked independent of the `fold_rev` crash

That prior conclusion still holds after this round.

## What I did in this round

I focused on two questions:

1. Is the `fold_rev` crash coming from generic LIR RC insertion, or from a more specialized dev-backend/helper path?
2. Is there an ABI/cache mismatch in dev backend compiled procedures/lambdas that could explain wrong list behavior and bad decref calls?

### Files examined closely

- [`src/lir/MirToLir.zig`](/home/lbw/Documents/Github/roc/src/lir/MirToLir.zig)
- [`src/lir/rc_insert.zig`](/home/lbw/Documents/Github/roc/src/lir/rc_insert.zig)
- [`src/lir/OwnershipNormalize.zig`](/home/lbw/Documents/Github/roc/src/lir/OwnershipNormalize.zig)
- [`src/backend/dev/LirCodeGen.zig`](/home/lbw/Documents/Github/roc/src/backend/dev/LirCodeGen.zig)
- [`src/build/roc/Builtin.roc`](/home/lbw/Documents/Github/roc/src/build/roc/Builtin.roc)
- [`src/snapshot_tool/main.zig`](/home/lbw/Documents/Github/roc/src/snapshot_tool/main.zig)
- [`src/builtins/utils.zig`](/home/lbw/Documents/Github/roc/src/builtins/utils.zig)

### Immediate observations

#### 1. `fold_rev` builtin shape

In [`src/build/roc/Builtin.roc`](/home/lbw/Documents/Github/roc/src/build/roc/Builtin.roc), `fold_rev` is:

```roc
fold_rev = |list, init, step| {
    var $state = init
    var $index = list.len()

    while $index > 0 {
        $index = $index - 1
        item = list_get_unsafe(list, $index)
        $state = step(item, $state)
    }

    $state
}
```

So the hot path is:

- borrowed outer `list`
- mutable loop index/state
- `list_get_unsafe(list, index)`
- call user step function with the element

This matches the earlier suspicion that `list_get_unsafe` is the critical operation in the failing shape.

#### 2. MIR/LIR ownership model still looks intentional

Relevant lowering/ownership pieces still look internally consistent:

- `MirToLir` recognizes `list_get_unsafe` as borrowing from its list source
- `OwnershipNormalize.sourceRefForAliasedExpr` maps `list_get_unsafe` back to the list source ref
- `rc_insert.exprAliasesManagedRef` also treats `list_get_unsafe` as aliasing its source

This is why the earlier generic `while_loop` RC hypothesis became less convincing.

#### 3. The snapshot trace flag is currently not useful

In [`src/snapshot_tool/main.zig`](/home/lbw/Documents/Github/roc/src/snapshot_tool/main.zig), `--trace-eval` exists as a CLI flag, but the trace hookup is commented out:

```zig
// if (config.trace_eval) {
//     repl_instance.setTraceWriter(stderrWriter());
// }
```

So:

```sh
./zig-out/bin/snapshot --trace-eval test/snapshots/repl/list_fold_rev_basic.md
```

does not currently produce the detailed execution trace one might expect. It only reproduces the mismatch.

## Concrete experiments and what they proved

### Experiment 1: add an RC unit test for `while` + `list_get_unsafe` + refcounted element

I added a targeted `rc_insert` unit test intended to exercise a `while` loop over a borrowed list where `list_get_unsafe` returns a refcounted element, to see whether RC insertion itself was clearly emitting a bad decref on the element binding.

That test passed.

Result:

- the generic RC insertion pass did not immediately reproduce the dev-backend crash in isolation
- this weakens the case that the bug is in generic `rc_insert` while-loop logic

I reverted the test afterward; it was only diagnostic.

### Experiment 2: break on `decrefDataPtrC` in `gdb`

I ran:

```sh
gdb -batch -ex 'set debuginfod enabled off' \
    -ex 'break utils.decrefDataPtrC' \
    -ex run \
    -ex 'print alignment' \
    -ex 'print elements_refcounted' \
    -ex 'print bytes_or_null' \
    -ex 'bt 12' \
    --args ./zig-out/bin/snapshot test/snapshots/repl/list_fold_rev_basic.md
```

What I saw on the first breakpoint was important:

- `alignment = 4155046688`
- `roc_ops = 0x3`
- other arguments looked nonsensical

That is not a normal call signature for:

```zig
decrefDataPtrC(bytes_or_null, alignment, elements_refcounted, roc_ops)
```

Expected would be something like:

- small integer alignment
- valid pointer-sized `roc_ops`

This means at least one call site reaching `decrefDataPtrC` is entering with corrupted ABI/state, not merely “correct call, wrong extra decref”.

This was a key data point:

- it pushed the investigation toward dev backend call lowering / codegen state corruption
- it made “simple over-decref only” less likely as the whole story

Important caveat:

- this breakpoint may not have caught the final crashing call specifically
- but even the first hit already showed bad arguments, which is enough to flag backend codegen as suspicious

### Experiment 3: lambda compiled-proc cache key too coarse

Hypothesis:

- `compileLambdaAsProc` caches machine code too aggressively
- if imported polymorphic lambdas reuse the same cache entry across different concrete layouts, that could cause ABI/layout corruption

Relevant code in [`src/backend/dev/LirCodeGen.zig`](/home/lbw/Documents/Github/roc/src/backend/dev/LirCodeGen.zig):

- `lambdaCacheKey(...)`
- `compiled_lambdas`

I tried changing the cache key to include:

- `lambda_expr_id`
- hidden arg count
- return layout
- concrete parameter layouts

Then I rebuilt and reran snapshots.

Result:

- no change in the failing snapshots
- `fold_rev` still crashed
- list `take`/`drop` snapshots still failed

Conclusion:

- this was not the primary cause of the currently observed failures

I reverted that speculative change.

### Experiment 4: `proc_registry` keyed too coarsely

Hypothesis:

- top-level/builtin compiled procedures in the dev backend are cached only by symbol
- polymorphic builtins could therefore reuse a proc compiled for the wrong concrete argument layouts
- this would match the fact that several builtin list operations were failing, not just `fold_rev`

Relevant code:

- `proc_registry`
- `compileProc`
- `generateLookupCall`

I changed the proc cache key from raw symbol to a hash of:

- symbol
- concrete argument layouts

and updated call lookup paths accordingly.

Result after `zig build snapshot`:

- still no change in the failing snapshots

Conclusion:

- this was not sufficient to explain the failures

I reverted that speculative change too.

### Experiment 5: compiled-proc `roc_ops` handoff hole

While reviewing compiled-proc codegen, I found what looked like a genuine ABI hole:

- `bindLambdaParams` explicitly receives the trailing `roc_ops` argument into `R12`/`X20`
- `compileLambdaAsProc` explicitly reserves/protects that register
- `bindProcParams` did not do the same
- `compileProc` did not mirror the same register reservation logic

This looked promising because:

- builtin/helper procedures compiled via `compileProc` often call other builtins
- corrupted `roc_ops` would produce exactly the kind of invalid builtin call behavior seen in `gdb`

I implemented the obvious fix:

- reserve `R12`/`X20` in `compileProc`
- capture the trailing `roc_ops` argument in `bindProcParams`

Result:

- still no movement in the failing snapshots

Conclusion:

- either compiled procedures are not the path causing these failures
- or the real bug is elsewhere and this hole was incidental / not exercised by the failing cases

I reverted this speculative change too.

## What did not change throughout this round

Across all of the above experiments, the following remained stable:

- `fold_rev_basic` crashed
- `fold_rev_subtract` crashed
- `num_mod_by` still produced remainder-style sign behavior
- `list_take_*`, `list_drop_*`, and `list_tags` still misbehaved

That negative evidence is valuable. It means the following ideas are now lower-priority:

- generic `while` RC bookkeeping in `rc_insert`
- lambda proc cache key only
- proc registry cache key only
- compiled-proc `roc_ops` handoff only

## Current best interpretation

### `fold_rev`

The strongest interpretation after this round is:

- the `fold_rev` crash is still tied to a specialized list/helper/codegen path around `list_get_unsafe`
- the problem is likely not in the high-level RC counting model alone
- at least one dev-backend builtin call path is being reached with corrupted arguments or corrupted preserved state

The earlier diagnosis is still directionally right:

- something on the generated path reachable from `list_get_unsafe` / loop element handling is treating a borrowed source as if it had consumable ownership, or is entering the decref builtin with corrupted call state

The new detail from this round is that the bad call may involve ABI/register corruption, not just an extra decref.

### `num_mod_by`

`num_mod_by` still appears to be independent.

Nothing in this round moved it, and it still behaves exactly like raw signed remainder in dev backend output.

That suggests one of:

- `.num_mod_by` is bypassing the intended adjustment path in dev codegen
- the wrong low-level op is reaching codegen
- the adjustment logic exists but is not actually exercised for the failing concrete path

## Most relevant code locations for the next person

### Builtin shape

- [`src/build/roc/Builtin.roc`](/home/lbw/Documents/Github/roc/src/build/roc/Builtin.roc)
  - `fold_rev`
  - `list_get_unsafe`

### MIR to LIR ownership/lowering

- [`src/lir/MirToLir.zig`](/home/lbw/Documents/Github/roc/src/lir/MirToLir.zig)
  - `runtimeListElemLayoutFromMirExpr`
  - `lowLevelExprBorrowsFromLookup`
  - `exprAliasesManagedRef`
  - `borrowBindingSemanticsForExpr`
  - `lowerWhileLoop`

### Ownership normalization

- [`src/lir/OwnershipNormalize.zig`](/home/lbw/Documents/Github/roc/src/lir/OwnershipNormalize.zig)
  - `sourceRefForAliasedExpr`
  - `analyzeExpr` cases for `for_loop`, `while_loop`, `low_level`

### RC insertion

- [`src/lir/rc_insert.zig`](/home/lbw/Documents/Github/roc/src/lir/rc_insert.zig)
  - `exprAliasesManagedRef`
  - `countConsumedValueInto`
  - `countBorrowOwnerDemandValueInto`
  - `processForLoop`
  - `processWhileLoop`
  - tests around `fold`/`while` loop cleanup

### Dev backend codegen

- [`src/backend/dev/LirCodeGen.zig`](/home/lbw/Documents/Github/roc/src/backend/dev/LirCodeGen.zig)
  - `generateLowLevel` cases for list operations
  - `generateLookupCall`
  - `resolveLambdaCodeOffset`
  - `compileLambdaAsProc`
  - `compileProc`
  - `generateForLoop`
  - `generateWhileLoop`
  - `emitListDecref`
  - `emitStrDecref`
  - `emitBoxDecref`
  - `.num_mod_by` handling

### Builtin decref implementation

- [`src/builtins/utils.zig`](/home/lbw/Documents/Github/roc/src/builtins/utils.zig)
  - `decrefDataPtrC`
  - `decref_ptr_to_refcount`

## Recommended next debugging steps

These are the highest-value next actions, in order.

### 1. Instrument the exact dev-backend call site that reaches `decrefDataPtrC`

Do not start with another high-level RC fix.

Instead, instrument the dev backend around:

- `emitListDecref`
- `emitStrDecref`
- `emitBoxDecref`

Suggested approach:

- temporarily log which LIR expression / layout / symbol triggered each decref emission
- include:
  - layout idx
  - alignment
  - whether elements are refcounted
  - whether the source location is `stack`, `stack_str`, `list_stack`, etc.
- if possible, log the generated code offset or enclosing proc/lambda identity

Why:

- `gdb` already showed at least one bad `decrefDataPtrC` call signature
- the immediate need is to identify which emitted call site is malformed

### 2. Correlate the crashing snapshot with the generated LIR/proc structure

For `List.fold_rev([1, 2, 3], 0, |x, acc| acc * 10 + x)`:

- dump the final LIR reaching dev codegen
- identify whether the loop body is compiled as:
  - direct `while_loop`
  - compiled proc
  - nested lambda proc
  - helper wrapper around a builtin lookup

The question to answer is:

- where exactly does the decref get introduced relative to `item = list_get_unsafe(...)` and `step(item, state)`?

### 3. Verify whether the malformed `decrefDataPtrC` call is x86_64 call-lowering corruption

Because the first `gdb` breakpoint showed garbage args, inspect:

- call argument placement for immediate + register + trailing `roc_ops`
- preservation of `R12` across nested calls
- whether `CallBuilder` is being given values in already-clobbered temporaries

Good suspects:

- list/str decref emission immediately after another call
- call sequences inside nested compiled lambdas/procs
- any path where a temp register holding a pointer is freed/reused before the call is emitted

### 4. For `num_mod_by`, trace from MIR op to dev codegen branch

Do not assume the `.num_mod_by` code in `LirCodeGen` is actually what the failing snapshot is using.

Verify:

1. what MIR op is produced
2. what LIR low-level op is produced
3. which dev backend code path handles it

Specifically inspect:

- [`src/eval/interpreter.zig`](/home/lbw/Documents/Github/roc/src/eval/interpreter.zig) for expected semantics
- [`src/backend/dev/LirCodeGen.zig`](/home/lbw/Documents/Github/roc/src/backend/dev/LirCodeGen.zig) `.num_mod_by` handling
- whether the failing path reaches `.num_rem_by` instead

### 5. Compare with the `list_take_*` failures

The `list_take_*` / `list_drop_*` / `list_tags` regressions are likely not noise.

They may share one of:

- wrong compiled helper/proc path for list builtins
- wrong list return ABI handling
- wrong alias/ownership handling for list data

If one common helper or call path is found between:

- `fold_rev`
- `take/drop`
- `tags`

that will likely be the faster route than debugging `fold_rev` in isolation.

## Commands used during this round

Useful for reproduction:

```sh
zig build snapshot
./zig-out/bin/snapshot test/snapshots/repl/list_fold_rev_basic.md
./zig-out/bin/snapshot --trace-eval test/snapshots/repl/list_fold_rev_basic.md
zig build test-lir -- --test-filter "RC while loop borrowed refcounted list element does not decref element binding"
```

The `--trace-eval` command currently does not provide the expected detail because the writer hookup in the snapshot tool is commented out.

`gdb` command used:

```sh
gdb -batch -ex 'set debuginfod enabled off' \
    -ex 'break utils.decrefDataPtrC' \
    -ex run \
    -ex 'print alignment' \
    -ex 'print elements_refcounted' \
    -ex 'print bytes_or_null' \
    -ex 'bt 12' \
    --args ./zig-out/bin/snapshot test/snapshots/repl/list_fold_rev_basic.md
```

## Final state of the tree after this round

No speculative fix from this round was intentionally kept.

This writeup should be considered the artifact to carry forward from the round, not a landed code change.
