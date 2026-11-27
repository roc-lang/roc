# Issue 13087: collection ownership

The regressions in `test/alloc-count/app_remove_worker.roc` and
`test/alloc-count/app_set_capture_outcomes.roc` isolate the worker convention
and the result-dependent handback, respectively. They check the contents of
the result and the still-live input as well as the allocation counter. The
list fixture counts bytes around the erased call, not around constructing the
closure or its inputs.

On arm64mac, with a ReleaseFast compiler, removing 1,000 U64 keys in a `while`
loop under `--opt=size --specialize=no` originally requested 22,400,000 bytes
for `Dict` and 19,398,000 for `Set` (1,500 requests each). With the cross-unit
base convention both loops request zero, as the exported dev workers and the
specialized size build already did. For a 4,096-element `List(Str)`, unique
success goes from 98,320 requested bytes to zero on both Boxy backends.
Shared success still copies; neither miss copies.

## Other size fixtures in the issue

The nine existing allocation tests cover *whole regions*, including closure
construction and, in some cases, growing lists or initializing state. They
are not nine reproductions of the checked-update copy. On the same machine
their `--opt=size --specialize=no` results did not change with either fix.
These counts are diagnostic observations, not new budgets. The bytes column
comes from running the same region with `Host.alloc_bytes!` in place of
`Host.alloc_count!`.

| `test/alloc-count/app_*.roc` | Requests | Requested bytes |
| --- | ---: | ---: |
| `closure_borrowed_call` (#12847) | 26 | 1,784 |
| `closure_record_match_replace` (#12846) | 1,000 | 80,000 |
| `closure_record_join_passthrough` (#12846) | 2,000 | 160,000 |
| `closure_nested_capture` (#12957), nested / flat | 100 each | 9,600 each |
| `closure_dict_capture` (#12957), beside / only / separate / twice / deep | 100 each | 12,000 / 11,200 / 12,000 / 12,000 / 12,800 |
| `closure_nested_capture_paths` (#12957), deep3 / deep4 / tagged / branchy | 100 each | 9,600 / 15,200 / 11,200 / 10,400 |
| `closure_capture_large_update` (#12957), dict only / dict deep / cols only / cols beside | 100 each | 11,200 / 12,800 / 9,600 / 10,400 |
| `whole_move_field_reads`, captured case | 100 | 9,600 |
| `closure_pair_capture` (#12841), inline / through update | 4,004 each | 368,312 each |

For example, 80 bytes per call cannot be a per-call copy of a 10,000-element
U64 list. These results are consistent with the CLI tests' separate closure
packing/reuse explanation, but do not attribute every request to an allocation
site. The specialized expectations remain intact. Frame-replacing tail-call
storage is a separate part of the issue.
