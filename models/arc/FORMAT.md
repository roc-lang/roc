# The `ROC_LIR_DUMP` format

The model reads the final LIR that `ROC_LIR_DUMP` prints, after ARC, reuse, dismantling and every LIR pass. The printer
is `src/lir/debug_print.zig`, called from `LirDump` in `src/lir/checked_pipeline.zig`. The value of `ROC_LIR_DUMP` is a
substring filter on procedure debug names; the empty string, as `run.sh` sets it, prints every procedure. The parser is
`lean/RocArc/Parse.lean`.

The dump is mixed into the rest of `roc test` output. The parser keeps only `=== LIR` blocks, each ending at a blank line.

## Procedures

```
=== LIR <unnamed> (p0, identity=…, fingerprint=…, borrowed=0x1, ret_borrowed=false, unique=false) ===
proc p0 args=[l0:list#1, l1:u64] ret=u64
  <statement chain, indented two spaces per level>
```

- Header: debug name, proc id, `borrowed=` (hex mask of borrowed params), `ret_borrowed=`, and `unique=` (whether the
  result is born unique). `identity` and `fingerprint` are ignored.
- `proc` line: typed params and return type. A body of `<no body>` is a procedure without a body.
- Types are `kind#index` (`list#25`, `struct_#40`, `tag_union#41`, `box#3`, `erased_callable#2`, …) or a primitive
  (`str`, `u64`, `bool`, `f64`, …). Only the kind is used.
- One dump holds every LIR program the run lowered (compile-time evaluation, each test batch). A program prints its procs
  in increasing id order, so the model starts a new program at every non-increasing id (`splitPrograms`).
- Roots are parameterless procs named `<unnamed>` (test bodies and compile-time roots). The machine runs only roots.

## Statements

Control (each body is a chain one level deeper):

| Form | Meaning |
|---|---|
| `join jN params=[…]` / `remainder:` / `body:` | a join point |
| `jump jN` | jump to a join |
| `switch lN default_cold=…` / `case K:` … / `default:` / optional `continuation:` | branch on a scalar or discriminant |
| `switch_initialized_payload cond=lC mask=0xM payload=lP …` / `initialized:` / `uninitialized:` | branch on an optional payload |
| `ret lN`, `crash lN`, `crash`, `runtime_error`, `expect_err lN`, `comptime_exhaustiveness_failed …` | terminals |

Simple statements:

| Form | Meaning |
|---|---|
| `lN:T = ref.local lM` / `ref.nominal` / `ref.list_reinterpret` / `ref.discriminant` | alias or read |
| `lN:T = ref.field lM[i]`, `ref.tag_payload lM vV[i]`, `ref.tag_payload_struct lM vV` | projection; a trailing `take` marks a move out of the container |
| `lN:T = literal K` / `literal str` / `literal bytes` / `literal static_data sK` / `literal proc_ref pK` / `literal f64 …` | literals |
| `lN:T = call pK(l…)` | direct call |
| `lN:T = low_level OP(l…) unique=0xM` | builtin op; `unique=` is the `unique_args` stamp mask, printed only when nonzero |
| `lN:T = list(l…)`, `struct(l…)`, `tag vV dD (lP)` | constructors |
| `incref lN xK rc=…`, `decref lN rc=…`, `free lN rc=…` | count statements; `rc=concrete(op,LAYOUT)` names the RC helper's layout |
| `decref_if_initialized cond=lC mask=0xM value=lV …` | conditional release |
| `set lA := lB (…)`, `init_uninitialized lN`, `debug lN`, `expect lN` | assignment and uses |

## What the export drops

Everything here is something the model cannot catch, or must approximate. README "Limits" says what each costs.

| Id | Dropped | What the model does instead |
|---|---|---|
| G1 | Per-statement `rc_effect` rows. The dump prints only the op name, so a row ARC substitutes for a statement is invisible. | Uses the op's default `RcEffect` row from `OpTable.lean` |
| G2 | A layout's fields and whether it is refcounted; only `kind#index` is printed | A struct or tag layout is refcounted if an RC helper in the same program names it (`rc=concrete(…,LAYOUT)`), closed under construction and projection (`inferRcLayouts`). Aggregates of other layouts are unchecked by the certifier model. |
| G3 | The variant's `unique` parameter seeds | A stamp on a parameter-derived value makes the certifier model give up (M-UniqueSeed) |
| G4 | The meaning of `cond & mask` in `decref_if_initialized` and `switch_initialized_payload` | Initializedness is read from the value itself |
| G5 | DAG sharing: a shared tail is printed once, then `<<already printed>>` | The tail is unsupported |
| G6 | String and float contents | Unknown scalars; switches on them fork |
| G7 | Erased-callable call plans and `.boxy` dynamic values (`call_erased`, `packed_erased_fn`, `dyn.*`, `store_struct`, `store_tag`, `view`), and `str_match` / `str_match_set` | Unsupported: a path reaching one stops as unsupported, and the certifier model prints `UNSUPPORTED`. This is why `run.sh` dumps at `--specialize=yes` and `--opt=speed`, which lower most programs without them. |
| G8 | Program boundaries | Inferred from proc ids (above) |
