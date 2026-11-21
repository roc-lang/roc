# `ROC_RANGE_PROVE_DUMP` format

The exporter is `RangeProveDump` in `src/lir/range_prove.zig`, behind `base.dev_diagnostics.Var.range_prove_dump`. It
comes from [#13362](https://github.com/roc-lang/roc/pull/13362).

For every proof round that walks the procedure and rewrites at least one statement, the compiler prints one block on stderr.
Each block is a single `std.debug.print`, so rounds of procedures proved in parallel do not interleave. Rounds that thread
Bool joins do not walk and print nothing.

```
RPD P <proc> <round>
A l<id>:<cls> ...         procedure arguments
B <body stmt>
S <id> <kind> ...         one line per statement reachable from the body, as the round found it
E <kind> <stmt> <a> <b>   one line per rewrite the round made, in walk order
RPD END
```

Everything else on stderr is ignored. Compiler warnings are written to stderr at the same time, so a block can start in
the middle of a line; `rpv.py` cuts blocks out by their `RPD P` and `RPD END` markers rather than by lines.

## Locals

`l<id>:<cls>`, where `<cls>` is one of `u8 u16 u32 u64 u128 i8 i16 i32 i64 i128 bool str zst list other`.

## Statements

`n` is the next statement.

| Line | Meaning |
|---|---|
| `lit n l:c V` | literal; `V` is an integer, `len=N` (a list literal of known length) or `x` (anything else) |
| `ref n l:c KIND l<src> INDEX VARIANT` | `KIND` is `local`, `field`, `discriminant`, `tag_payload`, `tag_payload_struct`, `list_reinterpret` or `nominal` |
| `ll n l:c OP DESC args...` | low-level op; `DESC=1` means the last argument is a dynamic element descriptor |
| `struct n l:c fields...` | struct literal |
| `list n l:c elems...` | list literal |
| `tag n l:c VARIANT DISC [payload]` | tag literal |
| `set n l:c l:c` | assignment |
| `havoc n [l:c]` | calls, erased calls, dynamic ops, `init_uninitialized`, `store_struct`, `store_tag` |
| `nop n` | `debug`, `expect`, `incref`, `decref`, `decref_if_initialized`, `free` |
| `switch l<cond> DEFAULT v:stmt ...` | switch on a value |
| `branch s1 s2 ...` | a choice the export does not model: `switch_initialized_payload`, `str_match`, `str_match_set` |
| `join j<id> BODY REMAINDER params...` | join point; `BODY` is what a jump enters, `REMAINDER` runs first |
| `jump j<id>` | jump to a join |
| `ret l:c`, `crash`, `stop` | ends a path |

## Events

| Line | Rewrite |
|---|---|
| `E cmp S t 0` | the comparison at S became the constant t |
| `E ovf S t 0` | the overflow predicate at S became the constant t |
| `E proven S 0 0` | the family op at S became `*_proven_cannot_overflow` |
| `E div S 0 0` | the checked division or remainder at S became plain |
| `E switch S v 0` | the switch at S was folded on the constant v |
| `E crash S 0 0` | the op at S was rewritten to a crash |
| `E chain S inner literal` | `x op c1 op c2` at S was folded into `x op (c1+c2)`; `inner` is the first op, `literal` the statement defining c2 |
| `E simd S n 0` | the count operand of `simd_concat_shift_bytes` at S was annotated as n |

## What the export drops

Everything dropped is something the validator cannot catch; it treats each as an unknown value.

- string contents and float literals (`lit ... x`);
- call targets: every call is `havoc`, so nothing flows through a call;
- what `store_struct` and `store_tag` store;
- `str_match` captures and which branch a `branch` takes;
- whether a switch default is cold;
- refcounting statements (`nop`);
- layouts beyond the class above: struct and tag shapes are rebuilt only from the statements that build them;
- rounds that thread Bool joins.
