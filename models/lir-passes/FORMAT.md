# ROC_LIR_PASS_DUMP format

The exporter is `src/lir/pass_dump.zig`, gated by the developer diagnostic `base.dev_diagnostics.Var.lir_pass_dump`
(`ROC_LIR_PASS_DUMP=<file>`). Only Debug and `-Ddev-diagnostics` compilers read it. It lands in its own PR.

The variable names a file. Each block is appended to it with one `write` on an `O_APPEND` descriptor, so blocks from
parallel workers never interleave. The file is never truncated: delete it before a run.

## Blocks

```
@@LPD begin program run=R stage=S label=LABEL proc=*
P/S/V lines for every procedure, then L lines, then D lines
@@LPD end

@@LPD begin phase run=R stage=S label=PHASE proc=N
@@LPD pre
P/S/V lines for procedure N, then L lines        (before the phase)
@@LPD post
P/S/V lines for procedure N, then L lines        (after the phase)
@@LPD end
```

- `run` counts the calls of `finishLoweredOutput` in the process. Compile-time evaluation and runtime lowering are
  separate runs.
- `stage` counts program blocks across the whole process. A phase block carries the stage of the most recent program
  block, which is the snapshot the model resolves its callees in.
- Program labels, in pipeline order (a label appears only when its pass is enabled):
  `lowered`, `after_trmc_segment` (branch_expectation and trmc), `after_tag_fusion`, `after_scalarize_prune`,
  `after_loop_append`, `after_range`, `after_box_reuse`, `after_return_slot`, `after_str_append`,
  `after_tag_reachability`. The first two follow tag-case fusion: without it, branch_expectation and trmc run in the
  same segment as scalarize and prune_join_params, and the first snapshot after `lowered` is `after_scalarize_prune`.
- Phase labels are the `ProcPasses.Phase` tags: `branch_expectation`, `trmc`, `tag_fusion` (tag-case fusion and
  known-tag jump threading), `prune_join_params`, `scalarize`, `loop_append`, `range`, `box_reuse`. A phase block is
  written only when the phase changed the procedure's text. The phases of one segment run on one shard, so a later
  phase's `pre` is the earlier phase's `post`.

## Lines

All ids are the store's own. `-` means empty or none. Lists are comma-separated. Strings and byte literals are hex.

`P id name=NAME args=L,L ret=LAYOUT body=STMT abi=ABI hosted=0|1 static_init=0|1 transform=T erased_capture=0|1 dyn_result=0|1`

The procedure's debug name (`-` if none; spaces and newlines become `_`), its argument locals, return layout, entry
statement (`-` for a bodiless procedure, which then has no S or V lines), ABI, tail transform (`none`, `tce`, `trmc`),
and whether it is hosted, a static initializer, takes an erased capture, or has a dynamic result.

`V local layout [dyn]`: one per argument and per local a printed statement mentions. `rc` lines do not add locals.

`S id KIND fields...` with successors given by statement id, so shared tails are explicit. Statements are printed
breadth-first from the body:

| Kind | Fields |
|---|---|
| `ref` | `T KIND SRC INDEX VARIANT DISC NEXT` (`KIND` is the `RefOp.Kind`: `local`, `discriminant`, `field`, `tag_payload`, `tag_payload_struct`, `list_reinterpret`, `nominal`) |
| `lit` | `T int N NEXT`, `T dec ATTOS NEXT`, `T f64 BITS NEXT`, `T f32 BITS NEXT` (bits in hex), `T str HEX NEXT`, `T bytes LEN HEX NEXT`, `T static SLOT NEXT`, `T proc_ref PROC NEXT` |
| `uninit` | `T NEXT` |
| `call` | `T PROC ARGS NEXT tail=0\|1` (`tail=1`: a recorded tail-call site) |
| `call_erased` | `T CLOSURE ARGS NEXT` |
| `packed_fn` | `T PROC CAPTURE NEXT` |
| `ll` | `T OP ARGS NEXT [dyn_elem_desc]` (`OP` is the low-level op tag) |
| `list`, `struct` | `T ITEMS NEXT` |
| `tag` | `T VARIANT DISC PAYLOAD NEXT` |
| `store_struct` | `DEST LAYOUT FIELDS NEXT` |
| `store_tag` | `DEST LAYOUT VARIANT DISC PAYLOAD NEXT` |
| `set` | `T V MODE NEXT` |
| `debug`, `expect` | `L NEXT` |
| `expect_err` | `L` |
| `dyn` | `T OPNAME NEXT` |
| `runtime_error`, `comptime_exhaustiveness_failed` | none |
| `rc` | `incref\|decref\|decref_if_initialized\|free L NEXT` |
| `switch` | `COND DEFAULT CONT V:STMT ...` |
| `sip` | `COND MASK PAYLOAD INITIALIZED UNINITIALIZED` |
| `str_match` | `SRC MATCH MISS arm PREFIX END STEPS` |
| `str_match_set` | `SRC MISS` then `arm MATCH PREFIX END STEPS` per arm |
| `join` | `ID PARAMS BODY REMAINDER` |
| `jump` | `ID` |
| `ret` | `L` |
| `crash` | `lit HEX` or `local L` |

In a string pattern, `END` is `exact` or `tail` and `STEPS` is `-` or `CAP:DELIM,...`, where `CAP` is a local or `_` for a
discarded capture and `DELIM` is hex (`-` when empty).

`L idx KIND ...`: one per layout reachable from a printed return layout, local layout or store layout:
`int u8..i128`, `frac f32|f64|dec`, `str`, `opaque_ptr`, `vector K`, `box|list|ptr CHILD`, `closure CAPTURES`,
`struct I:L I:L ...` (semantic field index and field layout, in storage order, padding dropped),
`tag_union disc_size=N P0 P1 ...` (payload layout per variant), or a bare tag: `zst`, `box_of_zst`, `list_of_zst`,
`erased_callable`, `hole`, `dynamic`. Layout ids below 17 are the layout store's fixed scalars (0 is Bool).

`D slot layout ...`: one per static data slot, program blocks only. The rest is `opaque` (a value only compile-time
evaluation or a ConstStore node supplies), `init PROC`, `callable PROC`, or `parts KIND ARG CHILD...` with `KIND` one of
`word N`, `product 0`, `list CAP`, `box 0`, `tag DISC`, `callable PROC`, and the children's slots. A D line's layout gets
no L line of its own.

## What the export drops

The model cannot see, and so cannot catch a bug in, any of these:

- Statement provenance, source regions, inline scopes, origin kinds.
- ARC-facing metadata: `RefExtra` (take kind, residual shell), a join's retained and maybe-uninitialized lists,
  `unique_args`, and a call's cold-path, frame-replacing and drive fields. The dumps are all taken before ARC, so no
  `rc` lines are expected.
- `default_is_cold` on a switch. The switch continuation is printed but the model ignores it.
- Low-level op extras other than `dyn_elem_desc`, such as `list_map_can_reuse`'s interchangeable widths and the SIMD
  concat count.
- The operands and descriptors of `dyn` statements. Procedures that use dynamic values are unsupported.
- Static data held only by compile-time results or ConstStore nodes (`D ... opaque`).
- A procedure's identity, frame locals, join-point list, erased-call argument plan, erased capture and reuse locals,
  emission unit and recorded shapes.
- Layout sizes, alignment and discriminant placement. The model is value-level.
- Everything from ARC on (ARC itself, frame callables, tail drive, immortal locals) and the backends.
