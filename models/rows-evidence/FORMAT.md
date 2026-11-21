# Lifted IR export format

The Lifted IR checker (`lean/RowsEvidence/Lifted.lean`) reads the compiler's Monotype Lifted IR through this export. The
exporter is `src/postcheck/monotype_lifted/ir_export.zig`. This file describes version 2. The exporter is the
authoritative definition; this file and the exporter must change together.

## Producing exports

The exporter runs only in a Debug compiler or one built with `-Ddev-diagnostics`, and only when `ROC_LIFTED_IR_EXPORT`
names a directory. That variable comes from [#13403](https://github.com/roc-lang/roc/pull/13403). Lifted IR exists only under
`--specialize=yes`; the boxy strategy (`--specialize=no`) never builds it.

```bash
mkdir -p out/lifted
ROC_LIFTED_IR_EXPORT=out/lifted roc test --no-cache --specialize=yes Module.roc
ROC_LIFTED_IR_EXPORT=out/lifted roc build --no-cache --opt=dev --specialize=yes app.roc
```

`./run.sh check` does this for you. Every Monotype Lifted program the process builds (compile-time evaluation programs,
test programs and the runtime program) is written twice: once after lifting and once after SpecConstr. File name:
`<n>-<stage>.lifted`, with `<n>` the lowest number not yet taken in the directory (claimed by exclusive create, so
several processes can share one directory) and `<stage>` either `after_lift` or `after_spec_constr`.

## Lexical rules

- ASCII text, one record per line, `\n` line ends.
- Fields are separated by exactly one space.
- Integers are unsigned decimal. Every reference is an index into one of the
  program's own tables (types, locals, expressions, patterns, statements,
  functions, join points), never a pointer.
- `-` is an absent optional.
- A *name* (record field label, tag label, type name) is one token: bytes
  `0x21..0x7e` other than `%` are written as is; every other byte (and `%`)
  as `%XX` (two upper-case hex digits). The empty name is written `%`.
- A *span* is written as its length followed by that many items.
- Records appear in no particular order and may refer to records that come
  later. A parser should read the whole file and index each table by id.

## File structure

```
roc-lifted-ir 2
stage <after_lift|after_spec_constr>
types <N>
t ...            (exactly N lines, ids 0..N-1)
locals <M>
l ...            (exactly M lines)
fns <F>
f ...            (F lines, ids 0..F-1, each followed by the e/p/s lines first
                  reached from that function's body)
end
```

The version on the first line changes whenever a record changes shape.

## Types: `t <id> <kind> ...`

| kind | fields |
|---|---|
| `prim` | `<primitive>`: one of `bool str u8 i8 u16 i16 u32 i32 u64 i64 u128 i128 f32 f64 dec u8x16 i8x16 u16x8 i16x8 u32x4 i32x4 u64x2 i64x2` |
| `zst` | none |
| `hole` | none (demand hole; see C-Closed) |
| `erased` | none (erased digest dropped) |
| `list` | `<elem>` |
| `box` | `<elem>` |
| `tuple` | span of item types |
| `func` | span of argument types, then `<ret>` |
| `record` | `<n>`, then `n` × (`<name> <type>`), sorted by label as the store keeps them |
| `tags` | `<n>`, then `n` × (`<name>` + span of payload types), sorted by label |
| `named` | `<nominal\|opaque\|alias> <key> <name>` + span of argument types + `<backing\|->` |

A named type's `<key>` is `<module>:<decl>`: the first eight bytes of the
declaring module's identity digest in hex, and the source-declaration
discriminator or `-`. Two named types are the same definition when their keys
and names are equal.

Equality of types in the model is structural and coinductive over these
records (types are recursive through `named` backings and through any child).
The ids are not hash-consed by the export: two ids may describe equal types.

## Locals: `l <id> <type> <capture-id|->`

`<capture-id>` is the raw 32-bit `CaptureId` the local carries as a capture
slot, if any.

## Functions: `f <id> <args> <captures> <ret> <body|hosted>`

`<args>` and `<captures>` are typed-local spans: a length, then that many
`<local> <type>` pairs. `<body>` is an expression id.

## Expressions: `e <id> <type> <kind> ...`

Operands, by kind (`E` an expression id, `[E]` an expression span, `L` a
local, `T` a type, `TL` a typed-local span, `C` a capture span = length,
then that many `<capture-id> E` pairs, `N` a name):

| kind | operands |
|---|---|
| `local` | `L` |
| `unit` `unreachable` `uninitialized` `crash` `checked_error` `comptime_exhaustiveness_failed` | none |
| `int_lit` `frac_f32_lit` `frac_f64_lit` `dec_lit` `str_lit` `bytes_lit` | none (values dropped) |
| `uninitialized_payload` | `L` (the condition local) |
| `static_data_candidate` | `E\|-` (runtime initializer; `-` for a stored list) |
| `comptime_value` | `E` (initializer) |
| `typed_boundary` `row_widen` `nominal` `dbg` `expect` | `E` |
| `expect_err` `literal_rejected` | `E` (message) |
| `list` `tuple` | `[E]` |
| `record` | `<n>` × (`N E`) |
| `record_update` | `E` (base), then `<n>` × (`N E`) |
| `tag` | `N [E]` |
| `fn_ref` | `<fn> C` |
| `call_value` | `E [E]` (callee, arguments) |
| `call_proc` | `<fn\|-> <demand_shaped 0\|1> [E] C` (`-`: a pre-lift template callee) |
| `low_level` | `<op name> [E]` |
| `field_access` | `E <n> N...` (receiver, segment labels) |
| `tuple_access` | `E <index>` |
| `structural_eq` | `E E <negated 0\|1>` |
| `structural_hash` | `E E` (value, hasher) |
| `match_` | `E <n>` × (`<pat> <k> <stmt>...k <guard E\|-> <body E>`) |
| `if_` | `<n>` × (`<cond E> <body E>`), then `<else E>` |
| `if_initialized_payload` | `<cond E> <payload L> <initialized E> <uninitialized E>` |
| `try_sequence` | `<input E> <ok L> <err join\|-> <ok body E>` |
| `try_record_sequence` | `<input E> <value L> <value field N> <rest L> <rest field N> <err join\|-> <ok body E>` |
| `block` | `<n> <stmt>...n <final E>` |
| `loop_` | `TL [E] E` (parameters, initial values, body) |
| `break_` | `E\|-` |
| `continue_` | `[E]` |
| `join_point` | `<join id> TL <body E> <remainder E>` |
| `jump` | `<join id> [E] TL [E]` (arguments, rebound loop parameters, their values) |
| `return_` | `E T` (value, return target) |
| `let_` `lambda` `def_ref` `fn_def` | pre-lift forms; `let_` writes `<pat> E E`, the others no operands. Seeing one in a dump is a C-Lifted violation. |

## Patterns: `p <id> <type> <kind> ...`

| kind | operands |
|---|---|
| `bind` | `L` |
| `wildcard` `int_lit` `dec_lit` `frac_f32_lit` `frac_f64_lit` `str_lit` | none |
| `as` | `<pat> L` |
| `record` | `<n>` × (`N <pat>`) |
| `tuple` | `<n> <pat>...` |
| `list` | `<n> <pat>...`, then `<rest index\|-> <rest pat\|->` |
| `tag` | `N <n> <pat>...` |
| `nominal` | `<pat>` |
| `str_pattern` | `<n>` × `<capture pat\|->` |

## Statements: `s <id> <kind> ...`

| kind | operands |
|---|---|
| `let_` | `<pat> E <recursive 0\|1>` |
| `uninitialized` | `<pat>` |
| `expr` `expect` `dbg` | `E` |
| `return_` | `E T` |
| `crash` `checked_error` | none |

## What the export drops

Everything below is absent from the dump, so a model run on dumps cannot
catch a bug that lives only there.

- **Literal values**: integer, float, decimal, string and packed-list literal
  contents; crash and checked-error messages; string-pattern prefixes and
  delimiters; `uninitialized_payload` and `if_initialized_payload` bit masks.
- **Type provenance**: a tag's `checked_name`; record field `value_ty`,
  `kind_state` and `??` defaults; named types' `builtin_owner`,
  `declared_order`, backing `use`/`authority`, iterator representation tier,
  kind, depth and topology, and all but eight bytes of the module digest;
  a root-app type's `site` flag; erased-type digests. Consequence: the model
  cannot apply the iterator-tier relation (`Type.iteratorRelation`), the
  per-use lift through an inspectable backing (it needs the backing's `use`),
  or the builtin-`Bool` and `Fields`/`Field` exemptions that depend on these
  fields, so it reports those cases as violations.
- **Symbols and identities**: function symbols, `FnTemplate` source data,
  `spec_constr_pattern`, `root_identity`, `FnShapes`, inline scopes; local
  symbols, names, binders and checked capture provenance (only the final
  `CaptureId` is kept); join `retained` locals; call `is_cold`,
  `iterator_procedure`; try `err_is_cold`; loop/branch cold flags.
- **Program metadata**: roots, literal roots, layout and runtime-schema
  requests, static-data tables and `StaticDataCandidate` storage ids,
  compile-time value root descriptors (`ComptimeValue.root`), comptime
  sites, source locations and regions, procedure debug names, constant
  evidence tables, the name store itself (only label texts appear).
- **Unreachable storage**: expressions, patterns and statements no function
  body reaches.
- **Sequencing and sharing**: the dump records the tree, not whether a
  subtree is reached from two parents (a node reached twice is written
  once).

## Conventions seen in real exports

These are not part of the format, but the checker relies on them.

- Monotype writes `True` and `False` as `int_lit` at `Bool` (`lower.zig` `BodyContext.boolLiteral`). That type is the
  nominal `Builtin.Bool` over `[False, True]`, or `prim bool`.
- Record expressions list fields in source order; record types list them in label order.
- Equal types often have different ids (`t 2` and `t 5` can both be `prim str`), so the checker compares structurally.
- A `call_proc` with `demand_shaped 1` targets a shaped specialization (design.md "Shaped requests"). The callee's
  parameters and result carry holes and layout representatives: records become tuples in row order, nominals become their
  backings.
