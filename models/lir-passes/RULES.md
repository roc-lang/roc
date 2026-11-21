# LIR pass model: rules

The model is `lir.py` (a value-level interpreter for the dumped LIR) driven by `check.py`. The dump format is in
[FORMAT.md](FORMAT.md). The compiler has no matching debug check yet; the right-hand columns name the design.md section
that states the rule and the code that has to keep it.

## Rules

Each rule is a finding kind `check.py` prints.

| Finding | Rule | design.md section | Compiler site |
|---|---|---|---|
| `divergence` | If `pre` returns or crashes within fuel on an input, `post` returns the equal value, or crashes with the same message, with the same ordered effect trace (`dbg`, failed `expect`s, hosted calls). | Procedure-Local LIR Rewrites | `src/lir/proc_passes.zig` and each phase: `branch_expectation.zig`, `trmc.zig`, `tag_case_fusion.zig`, `known_tag_jump.zig`, `scalarize_joins.zig`, `prune_join_params.zig`, `loop_append_promote.zig`, `range_prove.zig`, `box_reuse.zig` |
| `post-undef` | `post` must not leave the defined semantics where `pre` did not: no read of an uninitialized local, no `_unsafe` list or string op outside its precondition, no `proven_cannot_overflow` op that overflows. | Only the range prover mints the proven form; In-Place List Transforms | `range_prove.zig`, `loop_append_promote.zig` |
| `post-unbound` | `post` reads only locals some statement on the path wrote, and jumps only to joins in scope. | Procedure-Local LIR Rewrites | every phase |
| `post-fuel` | `post` finishes within 20 times the steps `pre` took (at least 100,000). | Procedure-Local LIR Rewrites | every phase |
| (both runs) | The rules above hold with all arguments unique and with all arguments shared. A promotion may take its unique path only when the value is unique (`list_slack_unique` is 0 for a shared list). | Uniqueness Inference; Shared loop exits | `loop_append_promote.zig`, `str_append.zig` |
| `deeper-recursion` | `post` does not recurse deeper than `pre` (more than one extra frame, past depth 8). TRMC and TCE must not add stack frames. | Tail Return Continuations | `trmc.zig` |
| `program-divergence` | Every zero-argument procedure (test roots, constants) gives the same result and effects on consecutive program snapshots. This localizes a bug to the stage between them, including the whole-program steps. | Procedure-Local LIR Rewrites; Destination-Passing Results and Allocation Reuse; Backend Builtins (string-append fusion); Tail Return Continuations (tag reachability) | `checked_pipeline.zig` (`finishLoweredOutput`), `return_slot.zig`, `str_append.zig`, `tag_reachability.zig` |
| `divergence` (steps) | For the whole-program steps `after_return_slot`, `after_str_append` and `after_tag_reachability` (every step with `--all-stages`), each procedure whose text changed is checked like a phase, in the context of the earlier snapshot plus any procedures the step created. | as above | as above |
| `tail-site-left` | After the trmc phase, no self call is still marked `tail=1` in a procedure that had one. | Tail Return Continuations | `trmc.zig`, `tail_call_builder.zig` |
| `lost-tail-call` | In the last snapshot, no self call reaches `ret` through aliases, join-parameter writes and jumps alone, if lowering recorded a `tail=1` self site in that procedure. Untagged tail self calls are only counted (`untagged_tail_self_call`): ARC later marks them frame-replacing, which the dump does not show. | Tail Return Continuations | `tail_call_builder.zig`, `trmc.zig` |

An input on which `pre` is undefined, out of fuel or unsupported is skipped and counted in `STATS` (`pre_*`). An input on
which `post` is unsupported is counted as `post_unsup`. Python errors inside the model count as unsupported
(`model-error`), never as findings.

## Inputs

A phase or step is run on the arguments its procedure was called with while the roots ran on the earlier snapshot (at
most 12 per procedure), plus `--rand N` values generated from the argument layouts. Hosted calls return a value chosen
from the callee's name and the call's position in the effect trace, so `pre` and `post` see the same answers.

## Semantics

From `src/lir/LIR.zig` and design.md:

- Locals are mutable. `join` runs its remainder; `jump` re-enters the join body after the `set initialize_join_param`
  writes that precede it.
- Struct field and tag payload indices are semantic indices. Bool (layout 0) values are the integers 0 and 1.
- Boxes and pointers are mutable cells, so TRMC holes work: `ptr_alloca`, `box_alloc_zeroed`, `ptr_store`, `ptr_cast`,
  `ptr_load`, and `store_struct` / `store_tag` through a pointer. Reading a zeroed cell is undefined.
- `compare` returns 0 for After, 1 for Before, 2 for Same. `list_sublist` takes the list and a `{len, start}` record.
- A string pattern capture stops at the first place its delimiter appears after the cursor, as the language reference
  says.
- Integer arithmetic wraps, crashes or is undefined according to the op's suffix (`_wrap`, `_crash_on_overflow`,
  `_proven_cannot_overflow`). Dec values are integers scaled by 10^18.
- Unsupported: dynamic-value (`.boxy`) procedures and `dyn` statements, erased calls, `switch_initialized_payload`,
  opaque static data, `dyn_elem_desc` ops, Dec multiplication through the integer ops, and any low-level op the
  interpreter does not list. These make the input unsupported, never a finding.
