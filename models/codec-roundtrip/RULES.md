# Rules

Each rule is a judgment the model checks on every generated case, the design.md section or language reference page
that states it, and the compiler and builtin code that implements it. Paths are relative to the repository root;
design.md sections are named, since its line numbers move.

A case is a type `T`, a value `v : T` and a second value `w : T`. The model computes the expected answer in Python
(`gen.py`); the compiler computes it in a test module (`roc test`) and in an app (`roc app.roc`, `roc build`), on
every config in [README.md](README.md).

## ENC: the encoder writes the documented format

`Json.to_str(v)` equals `gen.encode(T, v)`, the model's reference encoder. Its parts:

| Rule | Shape | Reference behavior |
|---|---|---|
| ENC-STR | `Str` | a quoted string, written byte by byte over the UTF-8: `"` and `\` escaped with a backslash, bytes 8, 9, 10, 12, 13 as `\b \t \n \f \r`, other bytes below 32 as `\u00xx` (lowercase hex), every other byte (DEL and all non-ASCII included) as is |
| ENC-INT | `U8` to `I128` | the decimal `to_str` of the value |
| ENC-DEC | `Dec` | `Dec.to_str`: integer part, `.`, the fraction without trailing zeros, or `0` when the fraction is zero |
| ENC-BOOL | `Bool` | `true` or `false` |
| ENC-REC | records, `{}` | an object with the fields in field-name order; an optional field (`Try(x, [Missing])`) holding `Err(Missing)` is left out, and one holding `Ok(x)` is written as `x` |
| ENC-TAG | tag unions | a tag without payloads is its name as a string; one payload is `{"Tag":payload}`; several are `{"Tag":[p1,p2,..]}` |
| ENC-TRY | `Try(ok, err)` | the tag union `[Ok(ok), Err(err)]`, except that `Try(ok, [Null])` writes `Ok(x)` as `x` and `Err(Null)` as `null` |
| ENC-SEQ | `List`, tuples, `Set` | an array; a set in its iteration order, which for `Set.from_list` of distinct items is insertion order |
| ENC-DICT | `Dict` | an object whose keys are written as strings (a `Str` key quoted, a number as its decimal text in quotes) |
| ENC-NOM | nominals | a nominal declaring `encoder_for : _` (`:=` or opaque `::`, generic or recursive) writes its backing; one with a hand-written `encoder_for` writes what that method writes |

Where it is stated:

- design.md "Structural Serialization Methods": derivation per shape (records through the format's record method, tag
  unions through its tag method, aliases through their expansion, nominals through their own method).
- docs/langref/parsers.md: records are objects, payload-free tags are strings, optional fields are
  `Try(x, [Missing])`, nominals opt in with `encoder_for : _`.
- `src/build/roc/Builtin.roc`, `Json` and `JsonEncoding`: `append_json_string_byte`, `encode_tag`, `encode_record`,
  `encode_tuple`, `encode_list`, `encode_dict`, `encode_null`, the `encode_<number>` methods and the `json_*_to_str`
  helpers; `src/builtins/dec.zig` `format_to_buf` for Dec.
- Derivation: `takeDerivedCodecBackingWalk`, `nullTryInfoFromNominal` and `missingTryInfoFromNominal` in
  `src/check/Check.zig`; lowering in `src/postcheck/monotype/lower.zig` (`lowerEncodeShapeToState` and the
  `encode_null` branch for a nullable `Try`) and `src/postcheck/boxy/codec.zig` (`classify`, `encodeShape`,
  `encodeTagUnion`, and the record encoder that skips a missing optional field).

The reference is derived from the builtin source above. A mismatch is a model bug until the source shows otherwise.

## RT: decoding what the encoder wrote gives the value back

`Json.parse(Json.to_str(v)) == Ok(v)`, with the parse typed at `T`.

- design.md "Structural Serialization Methods" (both directions are derived from one structural shape); docs/langref/parsers.md ("Now `Json.parse` and `Json.to_str` work with `Point`, using the same
  representation as its backing record").
- Parser derivation: the same checker walk; `src/postcheck/boxy/codec.zig` parser families and the Monotype parser
  lowering; `Json.parse_*` and `parse_tag_union_from_json` in `Builtin.roc`.

The generator is restricted so that it does not write a type whose encoding is ambiguous by design: a nullable `Try` whose `Ok` is itself a
nullable `Try` (both `Err(Null)` and `Ok(Err(Null))` write `null`).

## DEC: the decoder reads the documented format

`Json.parse(gen.encode(T, v)) == Ok(v)`: the parser accepts the reference encoding, independently of the compiler's
encoder. Same sources as RT.

## EQ: derived equality is structural equality

`(v == w) == gen.equal(T, v, w)` and `(v != w) == not gen.equal(T, v, w)`. Structural equality compares records
field by field, tags by name and then payloads, lists in order, sets and dicts as sets (order-independent), Dec by its
atto count, and nominals by their backing (every nominal the model writes declares `is_eq : _`).

- docs/langref/static-dispatch.md "Compiler-Derived Methods" and "Equality and Hashing"; docs/langref/records.md
  "Record Equality"; design.md "Checked Type Equivalence Classes" (derived structural `is_eq` and `to_hash` relate
  their operands like any dispatch) and "Methods On Open Rows".
- `Dict.is_eq` and `Set.is_eq` in `Builtin.roc`; structural equality lowering in `src/postcheck/monotype/lower.zig`
  (`beginStructuralEqualityAtNode`, `lowerStructuralEqFromOperands`).

## HASH: equal values hash equally

`Set.from_list([v, w]).len()` is 1 when `v` and `w` are structurally equal and 2 otherwise. A derived `to_hash` that
fed different data for equal values would leave two equal items in the set; one that fed the same data for different
values is still correct (`is_eq` separates them), so this rule only tests the first direction plus `is_eq` itself.

- docs/langref/static-dispatch.md "Equality and Hashing" ("whenever two values are equal, they must feed exactly the
  same data into the hasher"); docs/langref/dictionaries-and-sets.md "Keys and Hashing".
- `Dict.to_hash` and `Set.to_hash` (order-independent) in `Builtin.roc`; `beginStructuralHashAtNode` and
  `finishStructuralHashAtNode` in `src/postcheck/monotype/lower.zig`.

## NOPANIC: the compiler does not panic

No config panics or trips a compiler invariant, including on a program the checker rejects: design.md "Every
Rejection Is Explicit Recovery" (an invariant post-check hits on an error path is a missing checker recovery)
and AGENTS.md (invariants are debug assertions, so a Debug compiler that panics has found a compiler bug).
