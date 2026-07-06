# Boxy LIR Status Report

Continuation of the boxy (`--specialize=no`) implementation effort. This report reflects the state as of the end of the 2026-07-01 working session and supersedes the previous handoff. Older narrative sections were dropped; design context that still applies (host ABI invariant, descriptor model, representation planning) is unchanged from the previous version and from the design discussion.

## Acceptance bar (agreed with Richard)

- Full existing test suite green with boxy active as the default for dev/interpreter (`zig build test`, snapshots, `test/echo/*`, executable examples), with the lss path staying green.
- All four LIR consumers verified: interpreter, dev backend, LLVM, wasm.
- No local benchmarking (machine too noisy) — add boxy to the CI benchmark suite instead.
- Run `~/dotfiles/roc_smoke_test.sh` as an end-stage acceptance check.
- Checkpoint commits as verified progress lands; cleanup/squash before review.
- Perfect correctness first, then maximum performance given correctness — do not blanket-disable fast paths where a real safety proof is possible.

## Fixed and committed (fourth stretch, 2026-07-06)

- 2b7d59ad57 — iterator-loop descriptor bindings snapshot/restored (`lowerIteratorForInto`); fixes var_interp_segfault's "local used before assignment" (worker-wide `descriptor_bound` leak suppressed rebinds in later-lowered/earlier-executed regions).
- 3e4624e3c1 — host_boxed_fn_boundary passes end to end. Chain of fixes: (1) Box(fn) behind ALIAS chains now collapses to flat erased_callable in boxy layouts (`aliasResolvedRep` in layouts.zig) matching lss's host ABI convention; (2) proc returns carry the returned local's actual layout (`EvalProcResult.layout`) so box-self descriptors survive the concrete(20)/dynamic(18) box relabel; (3) box-self detection accepts box-family labels on both sides (`boxyDescIsBoxSelfForBoxValue` — a dynamic box interior is never a bare box value); (4) target-guided tag materialization encodes variants living in the TARGET row's extension; (5) tag construction converts payloads through the variant's own payload descriptors (`writeConstructedVariantPayload`); (6) unbox/box recognize pure relabels of already-boxed values and alias instead of rewrapping (RC statements assume aliasing).
- 1a5ce9e783 — static dictionaries for anonymous structural types: is_eq slots are `structural_eq`-marked (`BoxyMethodSlot.structural_eq`) and the interpreter fulfills them via `boxyValuesEqual` with the concrete type's descriptor in the slot; plan skips worker planning for them. Const restoration handles empty/all-zst records; `list_capacity` reports at least len (canonical zero-width lists store capacity 0).
- ab80d4bf11 — concrete tag union → bare-discriminant-layout boundary (all-zst-payload target union or scalar): switch on source discriminant, positional write (both sides alphabetical over the same checked tag set). Fixes Try(zstT, [ListWasEmpty..]) crossing from the worker's row layout (payload+disc) into main's 1-byte union.

### zst_nested_singleton_shapes: 5/8 sections pass (was 0 — plan panic)
FIXED this stretch: const restoration under generic use-site types (restore through the definition's checked type + boundary conversion; `.dynamic`-rep tag restoration via `restoreConstDynamicTagInto`; open-row tag lookup `constTagPayloadTypesAllowOpen`; row-extension cycle guard) and the single-argument-tag payload descriptor convention: `tagVariantPayloadFieldLayout` records ONE desc covering the WHOLE payload for 1-arg tags, but five readers paired payload structs' FIELDS with per-index descs (RC drop walk, equality walk, inspect walk, `readBoxyTagPayloadByName`, both `writeBoxyTagVariantPayloadToDestination*`, `writeConstructedVariantPayload`) — all now check `pdescs[0].payload_layout == whole payload layout` first. Also: concrete match-pattern payload extraction adapts when the union's slot layout differs from the binder's layout, and the unbox/box relabel gates require the target box's element expectation to be erased or desc-matching.

STILL FAILING (3 sections, wrong output, no crash): non-zst `first ==`/`get ==`/pattern comparisons AFTER a preceding zst section. Fully traced: the eq site's checked type is the CLOSED row (concrete rep; RHS const restored inline via plain assign_tag), while `first` flows from the open-row instantiation (record{field=dynamic box}, layout struct{box}); at the eq, the LHS operand's inner field still holds the box pointer (num_is_eq compared 0x…30 pointer vs 1). The missed conversion joint is between the match binder (open-row layout) and the eq operand local (closed layout) — `assignTypedLocal`→`assignConcreteRecordToConcreteBoundary` field-wise path is the designed route; either it isn't reached (types equal at check level → binder local already closed → the desc-guided pattern read `assignBoxyTagPayloadForRepName` produced unconverted bytes) or its field conversion raw-copies. Next probe: dump the stmts writing the eq's LHS record local (the raw dump elides nested ops — print `.field`/`.tag_payload_struct` args explicitly), or trace `assignBoxyTagPayloadForRepName`'s runtime output. Minimal repro: zst-section + non-zst first== in one file (see git history for nz_min6.roc).

### Previous failure notes (superseded)

The non-zst sections fail in CONST RESTORATION UNDER GENERIC TYPES. Repro (`nonZstValueB` as argument to generic `.ok_or`): `restoreConstTagInto` panics "ConstStore tag restored with a non-tag-union representation" because `repForType(checked_ty)` is **`.dynamic`** — the const is restored at a use site whose checked types are the generic worker signature's typevars (`const_use.requested_source_ty_payload` is the generic type), and nested descent (e.g. `restoreConstRecordInto`'s `.dynamic` branch) hands down flex field types. The full test's later crash ("descriptor had no tag variant with discriminant 0 payload_layout=119", desc variants EMPTY) is the same root: a descriptor materialized from the dynamic rep has no tag variants. Fix direction: `restoreConstTagInto` needs a `.dynamic` arm constructing via `assign_boxy_tag` with the constructed-target descriptor machinery (mirror `restoreConstRecordInto`'s `.dynamic` branch), with payload types taken from the dynamic rep's `.tag_payload`-role children when present; alternatively restore the const through its concrete definition-side type and boundary-convert. Note `restoreConstPlannedTagInto` line ~4465 and the dynamic record branch at ~4355 as templates. Investigation nuance: `restoreConstUseInto`'s `requested_source_ty_payload` passed layout-equality checks against an erased (dynamic-box) target local, so the requested type itself resolves to a dynamic-containing rep at this use site — check whether the checker can request the const at its own concrete type instead (then the existing generic-argument boundary conversion handles erasure), versus synthesizing descriptors from the const node's own shape. ANSWERED by probe: `repForType(requested_ty)` at `restoreConstUseInto` is a concrete `.record` (and equals the expr type's rep) — the dynamism enters during DESCENT: `restoreConstRecordInto`'s concrete branch descends with `fields[source_index].ty` (from `constRecordFields(type_module, checked_ty)`), and `repForType` of that FIELD type is `.dynamic`. So the checked record type's field type node resolves to a flex/rigid var (likely the const's generalized scheme: the annotation's open union `[OneTag({..})]` ext var, or a var forwarding to the union). Fix candidates: resolve the field's checked type through var forwarding before `repForType`, or descend with the RECORD REP's child reps (which the concrete branch already has as `child.rep` for the field locals — a `.dynamic` child rep there means the plan itself recorded the field dynamically, pointing back at `tagUnionRepresentation`/`buildRepresentation` for vars inside generalized const schemes).

### repeating_pattern / index_oob (parked literal-pattern cluster): index_oob PASSES; repeating_pattern remains
Landed: `boxy_dynamic_num_literal` (runtime desc-guided literal encoding with an erased-desc fallback to the kind's default layout via a synthesized runtime scalar desc), list_sublist's erased `{start,len}` arg unboxing, concrete-to-dynamic-box nominal boundaries box instead of reinterpreting, the interpreter's erased-ABI arg coercion boxes/unboxes concrete values, and — important — the plan's `collectCallHidden{Descriptor,Dictionary}Args`/`materializeWorkerCallHiddenDictionaryArgs` loops re-read children BY INDEX because nested `analyzeType` during `ensureStaticDictionaryWorkers` grows the children pool mid-loop and dangles held slices (0xAAAA… reps = freed-memory reads).

repeating_pattern still fails: `n = s.len()` (u64 value) flows into generic `repeat_helper` whose hidden descriptor arg arrives ERASED (the call-side rep-equality gate in `sourceValueDescriptorLocalForHiddenArg` declines mismatched canonical reps, falling back to the declared erased desc — relaxing that gate previously broke boxy_map_trim), so the `n - 1` literal falls back to Dec while the value bytes are u64 → overflow, and list equality after generic repeat compares unequal. The principled fix is the numeric from_numeral dictionary path for literals + value-accurate hidden descs for numeric-concrete args.

### Known deficiencies (not test failures today)
- Conversion-leak class: borrow-mode unbox + representation-changing materialization leaks the fresh root allocations (arc solver's incref assumes aliasing). host_boxed tests leak 2-18 allocations (host cleans up; io_specs pass). Proper fix = owned-result contract for materializers + arc noteBirth for converting unboxes, or lowering-side mode split.
- Boundary conversions box/unbox roundtrip per level (performance; correctness first).

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

Stable (15/15 runs, exit 0; the flaky segfault is FIXED — see 1432a4a89f). stdout matches the expected baseline byte-for-byte EXCEPT one line:

- `Err(NoFirstError(ListWasEmpty))` prints as `Err(ListWasEmpty)` — the intermediate singleton zero-sized tag level is collapsed in the DESCRIPTOR chain: the Err variant's payload descriptor points directly at the inner `[ListWasEmpty]` union instead of `[NoFirstError(...)]`. The inspect walker is fine (it prints whatever the chain has). Investigate where the Err variant's payload desc-rep is chosen: `staticPayloadDescRefsForTagVariant` → `tagPayloadStorageDescRepForLayout` → `tagPayloadStorageDescRep`, or whether the PLAN's tag variant payload children already flattened the singleton union. The singleton-ZST-union descriptor builder (the "zero-sized boxy tag descriptor had multiple variants" path) builds name+payload_descs correctly when given the right rep, so the level loss is upstream of it.

stderr is exactly `[dbg] 42.0` (instrumentation cleanup landed in 2010f53561).

## Fixed and committed (third stretch, 2026-07-01)

- 2010f53561 — instrumentation cleanup (−2503 lines across interpreter.zig/lower.zig via subagent + arc.zig by hand); stderr byte-clean; failure-path diagnostics kept.
- 1432a4a89f — THE FLAKY SEGFAULT: `materializeBoxyListPayloadToLayoutWithTargetDesc` strode the source buffer by the element DESCRIPTOR's payload layout; a payload-direct element descriptor (describing boxed contents, 16B) walked an 8B-stride box buffer and increfed garbage read past the allocation. Stride now comes from the storage layout only. 15/15 stable after.
- edfcfa7dfe — inspect: Dec formatted via RocDec.format_to_buf (16-byte frac branch was missing); zero-sized tag payloads print their names via the variant payload descriptor (new appendZstTagInspect helper; the `.zst` inspect case also consults descriptor tag variants).
- c283d96f99 — inspect: record field names. New `boxy_field_names` pool (program.zig + lir_image.zig + interpreter BoxyTables + runtime desc copy) populated shape-driven (children with record_field roles — record-shaped descs exist on record, dynamic, AND nominal reps; kind-gating missed most of them). `appendStructInspect` prints `{ name: value }` when the descriptor supplies exactly one name per field.
- 4c4dee0f39 — inspect: `<opaque>`. Inspect authority = checked `nominal.is_opaque` (NOT the plan's opaque_nominal kind, which means opaque_without_backing). Recorded as `TypeRepresentation.inspect_opaque` when the plan visits the nominal, carried on BoxyTypeDesc, checked first in the walker.

## Fixed and committed (second stretch, 2026-07-01)

### f96c3df966 — Pass value-accurate descriptors for known call-site reps

Fixes the open-union argument bug (`color_to_str(Blue)` printing "red") AND the previously-intermittent segfaults. Two coordinated changes:

1. `sourceValueDescriptorLocalForHiddenArg` bailed out when the call-side rep had no descriptor requirement of its own, so hidden descriptor args fell back to the worker's declared-shape binding — which cannot describe call-site row instantiation (extra tags in an open union's extension) and was flat-out unable to represent `Blue`. Now it materializes a descriptor from the known call-side rep (`descriptorMaterializationForSourceRep`). The rep-equality gate stays: relaxing IT breaks other cases (tried; boxy_map_trim went silent).
2. `staticPayloadDescRefsForTagVariant` skipped per-variant payload descriptors when the payload rep "needed none" from the producer's view (concrete payloads like Str). But descriptors describe values flowing into workers whose payload view is erased, and those workers read tag payloads through the descriptor they were handed — tripping "boxy tag payload 0 for tag Dog had no descriptor to bind", and (before that check existed on a given path) reading payload bytes at wrong offsets, which is the best explanation for the intermittent segfaults that disappeared with this fix. Payload descriptors are now forced (`tagPayloadStorageDescRepForLayout(..., true)`).

Regression test: `test/echo/boxy_open_union_arg.roc` (the two-function repro; also exercises Red/Green staying correct).

Earlier hypotheses about the plan-side tandem walk misattributing params (previous report revision) were WRONG — the null-arg-index hidden args belonged to `question_postfix`'s return union, which is legitimately return-position. The plan mapping was fine; only the lowering fallback was wrong.

### f1996cbcdc — Encode Dec-defaulted numeric literals with the scaled Dec bit pattern

`lowerExprInto`'s `.num` fallback for concrete targets emitted raw integer bits regardless of the target's committed layout, so Dec-typed literals (the default for untyped numerals) were unscaled while other Dec producers were scaled. `while_loop(5)` compared a scaled counter against an unscaled limit, exited after one iteration, and printed 0. The fallback now routes through the same layout-directed encoding as the dynamic-target path (`assignBuiltinNumLiteralPayload`). All Dec VALUES in all_syntax are now correct; only inspect formatting remains. Regression test: `test/echo/boxy_dec_literals.roc`.

### Test-infrastructure notes from this stretch

- `boxy lowerer emits direct calls to planned imported workers` (postcheck unit test) is ORDER-DEPENDENT: passes alone, fails inside the full boxy-filtered `run-test-zig` binary — including on commits before any of today's changes. Some shared state leaks between tests in that binary. Worth fixing; it derailed a bisect today (looked like my regression, wasn't).
- The `lir_core` unit-test compile was broken by the WIP's `.runtime` variant on `BoxyDescRef` (non-exhaustive switch in a test); fixed in f96c3df966.
- `builtin_compiler` failed once during a test-pipeline build and passed on retry — builtin compilation runs the interpreter for compile-time eval, so pre-fix descriptor bugs could surface there; watch whether it recurs after the descriptor fixes.
- Unit test filtering works as `zig build run-test-zig -- --test-filter <text>` (args go after `--`).

## Backend scope discovery

All three machine-code backends currently REJECT boxy LIR statements (`assign_boxy_*`, `assign_call_dict`, `boxy_tag_match`): dev and wasm panic with "boxy LIR statement reached … codegen before boxy codegen is implemented"; LLVM returns `error.CompilationFailed`. Only the interpreter executes boxy LIR today. Implementing boxy codegen in all three backends is a major remaining work item (tracked as task #8), not a verification pass.

## Other open items

- Interpreter debug value-shape checker for lists recurses with `layout_val.getIdx()` instead of the resolved element layout (`listElemLayout`); would have caught the map/trim corruption earlier. Small fix, not yet done.
- Instrumentation cleanup: arc.zig is done (trace removed, tab-indent churn fixed); a subagent pass over interpreter.zig and lower.zig was in flight at the time of writing — verify stderr of hello.roc is empty and all_syntax stderr is exactly `[dbg] 42.0` before trusting stderr-exact tests.
- Focused regression tests still to add: record destructuring from dynamic boxed records, list-boundary capacity preservation (reserve → boundary → append_unsafe), dynamic tag materialization with ZST payloads, inspect of records/opaques/tag payloads once implemented.
- Boxy pass-through list boundaries rebuild lists per iteration in loops (observed O(n²) rebuilds in the map fallback loop). Correctness first, but this wants a same-rep fast path once descriptors are trustworthy.
- CI benchmarks for boxy (task #9), `roc_smoke_test.sh` run (task #10) once the suite is green.

## Debugging playbook that worked this session

- The interpreter is the semantic oracle; add temporary Debug prints, iterate `zig build roc && zig-out/bin/roc --opt=interpreter <repro>`. Each rebuild is a few minutes.
- Shrink failures aggressively: copy `all_syntax_test.roc`, replace `main!` with a minimal body, binary-search the poisoning statement (a small python driver over the statement list works well; files must live in `test/echo/` because of the `../../README.md` import).
- Print raw statement ranges (`store.getCFStmt` over an id range) to read lowered LIR; statement ids are global and allocation-ordered, so a lowering site's statements cluster.
- `builtins.utils.DebugRefcountTracker` (enable at `eval()` entry, `printHistory` on a refcount address = `(data_ptr & ~7) - 8`) attributes RC events per allocation; 0xaa payload bytes mean freed-or-never-written memory (Zig Debug fill).
- Static descriptor tables can be dumped from `self.boxy_tables.type_descs` with variant names/discriminants — comparing those between a passing and failing variant of the same program localizes descriptor bugs fast.
- WATCH DISK: repeated debug builds grow `.zig-cache` unboundedly (313GB found this session; the disk filled mid-checkout). `rm -rf .zig-cache` is safe and costs one cold build.
