# Boxy LIR Status Report

Continuation of the boxy (`--specialize=no`) implementation effort. This report reflects the state as of the end of the 2026-07-01 working session and supersedes the previous handoff. Older narrative sections were dropped; design context that still applies (host ABI invariant, descriptor model, representation planning) is unchanged from the previous version and from the design discussion.

## Acceptance bar (agreed with Richard)

- Full existing test suite green with boxy active as the default for dev/interpreter (`zig build test`, snapshots, `test/echo/*`, executable examples), with the lss path staying green.
- All four LIR consumers verified: interpreter, dev backend, LLVM, wasm.
- No local benchmarking (machine too noisy) — add boxy to the CI benchmark suite instead.
- Run `~/dotfiles/roc_smoke_test.sh` as an end-stage acceptance check.
- Checkpoint commits as verified progress lands; cleanup/squash before review.
- Perfect correctness first, then maximum performance given correctness — do not blanket-disable fast paths where a real safety proof is possible.

## Fixed and committed this session

### fd8b1a5906 — Resolve boxy box payload descriptors like box readers do

`List.map(|name| name.trim())` corrupted `List(Str)` elements. Root cause was NOT the in-place map path (that path is not even taken in boxy mode — `list_map_can_reuse` is folded to 0 because the generic worker's list layout is not a concrete `.list`). The real chain:

1. The callable adapter around the transform boxes the concrete `Str` result via `assign_boxy_box` with a `payload_desc` that came from the erased target side.
2. Descriptors attached to box values legitimately come in two conventions: describing the box itself (`payload_layout == box layout`, payload in `nested_descs[0]`) or describing the payload directly. All box READERS normalize via `boxyBoxAllocationPayloadDesc`; `assign_boxy_box` did not.
3. With a box-self descriptor, the interpreter materialized the payload INTO the box layout, hit the canonical box-of-ZST branch, dropped the payload entirely, and double-boxed a null pointer. Exit boundaries then read undefined memory (0xaa) as `Str`s.

Fix: `assign_boxy_box` now normalizes `payload_desc` through `boxyBoxAllocationPayloadDesc` like every reader, and when the target-side descriptor carries no payload information at all (fully erased box desc, no nested), it falls back to the statement's `source_desc`, which describes the exact payload being stored. The target local's descriptor is set to whichever descriptor truthfully describes the stored payload (matters for RC).

Regression test: `test/echo/boxy_map_trim.roc`, registered in `parallel_cli_runner.zig`.

### cc162220b3 — Flatten row-extension payloads in target-guided boxy tag materialization

`test/echo/issue_9588.roc` (open error union propagated through `?`) hit "boxy descriptor had no tag variant with discriminant 1". A value sitting in the row-extension slot (discriminant == local variant count, by convention — see `boxyTagExtDiscriminant`) reached `materializeBoxyTagPayloadToLayoutWithTargetDesc`, which only looked at local variants. The sibling function without a target descriptor already special-cased the extension. Fix: resolve the extension descriptor and recursively materialize the extension union payload into the expected target union. issue_9588 passes now.

Also committed: `requireBoxyTagVariantByDiscriminant` prints the descriptor's variant list on invariant failure (genuinely useful diagnostic, Debug only).

## Current state of all_syntax_test (boxy interpreter)

Runs to completion (no crash; one segfault was observed once early in the session and has not reproduced since the two fixes — watch for it). Output is wrong in these clustered ways vs the expected baseline in `parallel_cli_runner.zig`:

1. **Open-union argument descriptor mismatch (semantic, match correctness)** — see next section. Manifests as `color_to_str(Blue)` printing "red".
2. **Str.inspect through boxy descriptors loses metadata**: records print as tuples (field names missing), opaque nominal types print through (`<opaque>` → `("my_secret_key",)`), tag payloads dropped (`Err(NoFirstError(ListWasEmpty))` → `Err`).
3. **Dec scalars mishandled**: printed as raw i128 mantissas (`42.0` → `42000000000000000000`), or scaled wrongly (`stringify(12345)` → `"0.000000000000012345"`), and at least one wrong VALUE (`while_loop(5)` prints `0` instead of `10.0`) — the descriptor/scalar-kind for Dec is being lost somewhere between literal lowering and numeric ops/inspect.
4. **stderr is polluted** by large amounts of pre-existing debug instrumentation (the expected stderr is exactly `[dbg] 42.0`), so the exact-match test cannot pass until instrumentation cleanup happens.
5. Possible byte-level diff on the unicode-escape line (uninvestigated).

## Diagnosed, not yet fixed: open-union hidden descriptor correspondence

Minimal repro (delete after fixing; the bisect files were `test/echo/_bisect_tmp*.roc`, since removed):

```roc
question_postfix : List(Str) -> Try(I64, _)
question_postfix = |strings| {
    first_str = strings.first()?
    first_num = I64.from_str(first_str)?
    Ok(first_num)
}

color_to_str : [Red, Green, ..] -> Str
color_to_str = |color| match color {
    Red => "red"
    Green => "green"
    _ => "other color"
}

main! = |_args| {
    _ = question_postfix(["1", "not a number", "100"])
    echo!(color_to_str(Blue))   # prints "red"; must print "other color"
    Ok({})
}
```

Without the `question_postfix` call the program prints the right answer — but for the WRONG reason (verified: the worker reads a garbage discriminant (170) through a mismatched descriptor and happens to fall into the `_` branch). Both variants violate the same contract.

Mechanism (all verified by instrumentation):

- At the call site, `Blue` is constructed as a static `assign_tag` (discriminant 0) in the CALL-side union rep (`[Blue:0, Green:1, Red:2]`, its descriptor was static desc 9 in the repro) and boxed with that payload descriptor. This part is fine: the box payload is stored under a descriptor that truthfully describes it.
- The hidden descriptor ARG passed to the worker, however, is the WORKER's declared-shape descriptor (static desc 7 = `[Green:0]` + `tag_ext` = `[Red:0]` — note the odd row ordering), which cannot describe `Blue` at all. The worker's `boxy_tag_match` reads the value's discriminant bytes through this wrong descriptor/layout (stored layout 76 read as layout 57) and takes the wrong branch.
- Why the wrong descriptor gets passed, two layers:
  1. In the plan, `materializeWorkerCallHiddenDescriptorArgs` maps worker hidden-descriptor params to call args by a tandem tree walk over the worker's FN-TYPE rep children vs call-side reps, with an order assertion (`params[next] == worker_desc`). But the worker's hidden params were collected on the worker's PARAM-PATTERN reps (33/34 in the repro), which in the poisoned program are DIFFERENT rep ids from the fn-type's arg child rep (30) for the same type. The arg pass therefore consumes nothing, and the ret pass sweeps the params up with `source_arg_index=null` and the WRONG call-side rep (the ret's). In the clean program the two rep instances happen to coincide (28), so the mapping "works". Same-type-different-rep = fragile identity; adding unrelated definitions (question_postfix) perturbs rep allocation and flips the outcome.
  2. In the lowerer, `lowerDirectCallHiddenDescriptorArgs`'s source-value path (`sourceValueDescriptorLocalForHiddenArg`) requires the call-side rep to have its own descriptor requirement — call-site value reps generally don't have one, so even with a correct `source_arg_index` the code falls back to the worker's bound requirement descriptor (the declared-shape one). The hidden descriptor must instead be derived from the CALL-side value: either the arg local's own descriptor or a materialization from the call-side rep (`descriptorMaterializationForSourceRep(arg.rep)` — arg.rep IS the call-side rep when the plan mapping is correct).

Recommended fix shape (matches the systemic-analysis prescription "record facts at derivation time, don't re-derive by matching"):

- When the plan collects a worker's hidden descriptor params (worker side), record for each param where it came from (arg index and/or rep path), instead of relying on the call-site tandem walk + order assertion to rediscover it. Then `materializeWorkerCallHiddenDescriptorArgs` maps each param directly to the call arg's rep, and `source_arg_index` is correct by construction.
- At the call site, pass a descriptor that describes the VALUE: prefer the source arg local's descriptor / call-side rep materialization over the worker's declared-shape binding. (Note: naively relaxing the canonical-rep gate in `sourceValueDescriptorLocalForHiddenArg` breaks other cases — tried and reverted; test/echo/boxy_map_trim.roc went silent. Whatever change is made must keep the whole echo corpus green.)
- The worker-side declared descriptor (static 7 style) is still needed for constructing values inside the worker; only the VALUE-describing role must come from the caller.

Also worth fixing/verifying nearby: static desc 7's shape `[Green local, ext=[Red]]` for declared `[Red, Green, ..]` — the row ordering that puts a declared tag in the extension is at minimum surprising and worth understanding while in there.

## Backend scope discovery

All three machine-code backends currently REJECT boxy LIR statements (`assign_boxy_*`, `assign_call_dict`, `boxy_tag_match`): dev and wasm panic with "boxy LIR statement reached … codegen before boxy codegen is implemented"; LLVM returns `error.CompilationFailed`. Only the interpreter executes boxy LIR today. Implementing boxy codegen in all three backends is a major remaining work item (tracked as task #8), not a verification pass.

## Other open items

- Interpreter debug value-shape checker for lists recurses with `layout_val.getIdx()` instead of the resolved element layout (`listElemLayout`); would have caught the map/trim corruption earlier. Small fix, not yet done.
- Remove the large pre-existing debug instrumentation (hard-coded proc/stmt/layout IDs across `interpreter.zig`, `lower.zig`) and clean accidental whitespace churn in `src/lir/arc.zig` around `assign_ref`/`assign_boxy_box`. Required before stderr-exact tests can pass.
- Focused regression tests still to add: record destructuring from dynamic boxed records, list-boundary capacity preservation (reserve → boundary → append_unsafe), dynamic tag materialization with ZST payloads, open-union argument passing (the repro above), Dec through erased calls, inspect of records/opaques/tag payloads.
- Boxy pass-through list boundaries rebuild lists per iteration in loops (observed O(n²) rebuilds in the map fallback loop). Correctness first, but this wants a same-rep fast path once descriptors are trustworthy.
- CI benchmarks for boxy (task #9), `roc_smoke_test.sh` run (task #10) once the suite is green.

## Debugging playbook that worked this session

- The interpreter is the semantic oracle; add temporary Debug prints, iterate `zig build roc && zig-out/bin/roc --opt=interpreter <repro>`. Each rebuild is a few minutes.
- Shrink failures aggressively: copy `all_syntax_test.roc`, replace `main!` with a minimal body, binary-search the poisoning statement (a small python driver over the statement list works well; files must live in `test/echo/` because of the `../../README.md` import).
- Print raw statement ranges (`store.getCFStmt` over an id range) to read lowered LIR; statement ids are global and allocation-ordered, so a lowering site's statements cluster.
- `builtins.utils.DebugRefcountTracker` (enable at `eval()` entry, `printHistory` on a refcount address = `(data_ptr & ~7) - 8`) attributes RC events per allocation; 0xaa payload bytes mean freed-or-never-written memory (Zig Debug fill).
- Static descriptor tables can be dumped from `self.boxy_tables.type_descs` with variant names/discriminants — comparing those between a passing and failing variant of the same program localizes descriptor bugs fast.
- WATCH DISK: repeated debug builds grow `.zig-cache` unboundedly (313GB found this session; the disk filled mid-checkout). `rm -rf .zig-cache` is safe and costs one cold build.
