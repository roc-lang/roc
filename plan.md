# Curried Structural Parser Implementation Plan

This plan implements the method-based structural parsing design described in
`design.md`, section `Structural Serialization Methods`. That section is the
source of truth for:

- `a.parser : encoding -> (state -> Try({ value : a, rest : state }, err))`
- direct state construction such as `Headers.{ raw }`
- `Fields(_shape)` and `Field(_shape)` opaque values
- compile-time field renaming with `Fields.rename_fields`
- `TryField` and `TryFieldCaseless` record events
- value parsers owning value consumption and returning the continuation state
- no runtime field maps, callback tables, or shape interpreters
- no runtime allocation on the benchmark header parsing path

The implementation must replace the current `parse_from`-based machinery. Do not
maintain both systems side by side.

## Design Reference Map

- [ ] Re-read `design.md`, `Structural Serialization Methods`, before each
  implementation phase that changes checking, lowering, or platform examples.
- [ ] Treat the public API examples in that section as normative:
  - [ ] convenience APIs such as `Headers.parse(raw)`;
  - [ ] structural method shape
    `a.parser : encoding -> (state -> Try({ value : a, rest : state }, err))`;
  - [ ] direct state construction such as `Headers.{ raw }`;
  - [ ] opaque `Fields(_shape)` / `Field(_shape)` metadata;
  - [ ] record events carrying `rest` at the value-start position;
  - [ ] construction-time field renaming through `Fields.rename_fields`;
  - [ ] no builtin user-facing `Decoder`, `Parser`, or `Encoding` type.
- [ ] Treat the phantom-type explanation in `design.md` as normative:
  `Field(_shape)` handles produced from a `Fields(_shape)` value are already
  tied to that exact record shape, so generated code must not need a
  release-build bounds check against the record arity.
- [ ] Treat the performance section in `design.md` as normative:
  transformed field data is built before the runtime lambda, SSO field names
  are the hot path, long field names still work, and the generated runtime scan
  must not allocate or build temporary maps.
- [ ] Treat the HTTP and JSON examples in `design.md` as the acceptance model for
  platform tests, while keeping the compiler fully format-agnostic.

## Definition Of Done

- [ ] The old `parse_from` structural derivation path is gone from the compiler
  and tests, except for any deliberately retained migration note that is not
  compiler source or Roc fixture source.
- [ ] The new `parser` derivation is implemented through ordinary static
  dispatch and direct generated code, not callback tables or a runtime shape
  interpreter.
- [ ] The HTTP header regression demonstrates zero runtime allocation in both
  the Zig host and Roc app.
- [ ] The HTTP and JSON regressions demonstrate field-order independence.
- [ ] The HTTP header regression demonstrates field-name renaming,
  case-insensitive matching, required fields, optional fields, mixed field
  value shapes, and continuation from each value parser's returned `rest`.
- [ ] Unsupported parser shapes fail during checking with static-dispatch errors
  instead of reaching post-check invariants.
- [ ] Optimized binary inspection confirms the hot header path has no allocator
  calls, no runtime field map, and no per-header runtime field-name conversion.
- [ ] Compile-time parser construction is fully supported: generated parser
  closures can be stored, restored, captured, and lowered as ordinary generated
  code without rebuilding field metadata at runtime.
- [ ] Targeted test steps pass first; then `zig build minici` passes by using the
  targeted-failure loop described at the end of this file.

## Design Invariants

- [ ] Preserve the method-based public model. Do not introduce builtin interface
  types for parsing or encoding.
- [ ] Keep the compiler format-agnostic. The compiler may know Roc record
  fields, tag labels, method names, and matching modes; it must not know JSON,
  HTTP headers, null, missing-header rules, kebab-case, camelCase, or any other
  format concept.
- [ ] Keep parsing effect-free. All parsing methods are pure functions from
  state to `Try`.
- [ ] Keep userspace safe. Do not expose raw byte indexing, unchecked memory
  primitives, raw field slot integers, or unchecked field construction.
- [ ] Keep runtime header parsing allocation-free in the regression platform.
  Compile-time allocation during parser construction is allowed.
- [ ] Keep the host allocation-free in the HTTP header regression platform.
  `roc_alloc` must still crash for that test.
- [ ] Keep record parsing one pass over the serialized record fields after the
  host has found the header section. The generated parser must not require a
  temporary map.
- [ ] Preserve seamless slices. Parsed `Str` values that are slices of input
  must remain slices into validated request memory or validated JSON input.
- [ ] Preserve order independence for records. HTTP header order and JSON object
  field order must not need to match Roc record field order.
- [ ] Preserve duplicate-field behavior. If an input repeats a field, keep the
  current chosen rule, expected to be last value wins unless tests reveal a
  documented existing rule.
- [ ] Unsupported structural shapes must fail during checking through missing
  static-dispatch methods, not as runtime parse failures.
- [ ] Runtime parse failures are only for input conditions known while scanning,
  such as invalid JSON syntax, bad header lines, missing required fields, or
  user-defined nominal parser errors.
- [ ] Do not add new language syntax. This change is method dispatch and
  compiler derivation only.
- [ ] Follow `design.md` post-check rules: later stages consume explicit checked
  data produced by earlier stages, and no later stage guesses missing
  information.

## Current Implementation Inventory

- [ ] Read the current implementation entry points before editing:
  - [ ] `src/check/static_dispatch_registry.zig`
  - [ ] `src/check/Check.zig`
  - [ ] `src/postcheck/monotype/lower.zig`
  - [ ] `src/build/roc/Builtin.roc`
  - [ ] `src/build/builtin_compiler/main.zig`
  - [ ] `src/canonicalize/BuiltinLowLevel.zig`
  - [ ] `test/http-headers/platform/Headers.roc`
  - [ ] `test/json-decoder/platform/Json.roc`
  - [ ] CLI tests named `ParseFrom*`
- [ ] Search for all old parse method names:
  - [ ] `rg -n "parse_from" src test examples design.md`
  - [ ] `rg -n "ParseFrom|parse_record_field|missing_record_field|ParseTagUnionSpec" src test`
  - [ ] `rg -n "Field\\(\\{ name : Str, value|Field\\.value|longest_field_len" src test`
- [ ] Record which uses are true implementation code, which are tests, and which
  are comments or docs.
- [ ] Confirm `design.md` remains the only design source for the new API.

## Builtin Surface To Add

- [ ] Update every builtin-indexing and canonicalization location that must know
  about new opaque builtin nominal types:
  - [ ] generated builtin node index collection;
  - [ ] builtin compiler index installation;
  - [ ] common identifier interning;
  - [ ] canonicalization auto-imports;
  - [ ] intrinsic annotation allowlists;
  - [ ] CheckedModule builtin nominal encoding;
  - [ ] post-check null-layout handling for opaque compile-time-only values;
  - [ ] glue/runtime encoding code that classifies builtin nominals.
- [ ] Add opaque compiler-owned field types to `Builtin.roc`:
  - [ ] `Fields(_shape) : opaque`
  - [ ] `Field(_shape) : opaque`
- [ ] Add builtin method annotations for field sets:
  - [ ] `Fields.rename_fields : Fields(_shape), (Str -> Str) -> Fields(_shape)`
  - [ ] `Fields.shortest_name : Fields(_shape) -> U64`
  - [ ] `Fields.longest_name : Fields(_shape) -> U64`
  - [ ] `Fields.iter : Fields(_shape) -> Iter(Field(_shape))`
  - [ ] `Fields.for_size : Fields(_shape), U64 -> Iter(Field(_shape))`
  - [ ] `Field.name : Field(_shape) -> Str`
- [ ] Do not provide crash bodies for compiler-generated builtin functions.
  Follow the existing builtin style: annotations only where the compiler
  supplies implementation.
- [ ] Ensure the phantom `_shape` type parameter is present in every public field
  set or field handle type that can cross userspace method boundaries.
- [ ] Ensure no public method exposes the internal field slot integer.
- [ ] Add or update comments in `Builtin.roc` explaining that these are
  compiler-generated opaque values used by structural parser derivation.
- [ ] Decide the exact public event tag names and keep them aligned with
  `design.md`:
  - [ ] `Field({ field : Field(_shape), rest : state })`
  - [ ] `TryField({ name : Str, rest : state })`
  - [ ] `TryFieldCaseless({ name : Str, rest : state })`
  - [ ] `Continue({ rest : state })`
  - [ ] `Done({ rest : state })`

## Method Names And Dispatch Registry

- [ ] Replace the old structural parse method name:
  - [ ] Remove special handling for `parse_from`.
  - [ ] Add special handling for `parser`.
  - [ ] Update identifier initialization so `parser` is interned wherever
    builtin method names are initialized.
- [ ] Update `src/check/static_dispatch_registry.zig`:
  - [ ] Replace the old `.parse_from` result mode with `.parser`.
  - [ ] Track that `parser` returns a function.
  - [ ] Track the `encoding` argument type separately from the returned runtime
    state argument type.
  - [ ] Preserve ordinary static dispatch for custom nominal `parser` methods.
- [ ] Update static-dispatch problem reporting:
  - [ ] Missing `parser` on a custom nominal type should mention `parser`.
  - [ ] Missing format methods should mention the exact missing method, such as
    `parse_str`, `parse_record_field`, `skip_record_field`, or `rename_field`.
  - [ ] Remove references to `parse_from` from user-facing errors.
- [ ] Update any tests that snapshot static-dispatch output.
- [ ] Ensure `parser` remains a normal method users can call directly:
  - [ ] `Shape.parser(encoding)` should work.
  - [ ] `({ foo: "x" }).encode_to(encoding)` should still work for encoding.

## Parser Type Shape In Checking

- [ ] Validate the derived method shape:
  - [ ] `a.parser : encoding -> (state -> Try({ value : a, rest : state }, err))`
  - [ ] The returned function must take exactly one runtime state argument.
  - [ ] The returned `Try` `Ok` payload must be a record with `value` and `rest`.
  - [ ] The `value` field must match the parsed shape.
  - [ ] The `rest` field must have the same state type as the runtime argument.
  - [ ] All parse errors in one derived parser must unify to the same error
    type.
- [ ] Validate the structural `encode_to` shape remains coherent with the new
  parse design:
  - [ ] `a.encode_to : a, encoding -> (state -> Try(state, err))`
  - [ ] Do not change encoding implementation unless needed for shared method
    infrastructure.
- [ ] Ensure checking derives `parser` for supported structural types:
  - [ ] `Str`
  - [ ] records
  - [ ] tag unions currently supported by the branch
  - [ ] `Try(Str, [Missing])`
  - [ ] `Try(value, [Missing])` where `value` has a parser, if this falls out
    naturally from the field-type machinery
  - [ ] custom nominal types with explicit `parser`
  - [ ] `U64`, at minimum for the new header `content_length` test
- [ ] Decide which numeric primitives are implemented now:
  - [ ] If only `U64` is implemented, unsupported numeric primitives must report
    missing method requirements during checking.
  - [ ] If all integer widths are implemented, add method validation and tests
    for each supported width.
- [ ] Ensure unsupported shapes do not reach post-check lowering as invariants.
  Checking must reject them first.

## Format Method Requirements

- [ ] For every derived parser, generate constraints on the encoding/state pair
  for exactly the needed shape methods.
- [ ] Required methods for strings:
  - [ ] `encoding.parse_str : state -> Try({ value : Str, rest : state }, err)`
- [ ] Required methods for `U64`:
  - [ ] `encoding.parse_u64 : state -> Try({ value : U64, rest : state }, err)`
  - [ ] If method naming follows a different existing numeric naming pattern,
    update this plan and tests before implementation.
- [ ] Required methods for records:
  - [ ] `encoding.rename_field : Str -> Str`
  - [ ] `encoding.parse_record_field : Fields(_shape), state -> Try(event, err)`
  - [ ] `encoding.skip_record_field : state -> Try(state, err)`
  - [ ] `encoding.missing_record_field : Str, state -> err` only when the target
    record has at least one required field.
  - [ ] `encoding.missing_optional_field : Str, state -> optional_err` only when
    the target record has at least one `Try(value, optional_err)` field.
- [ ] Required methods for optional-only records:
  - [ ] Do not require `missing_record_field` if no required field can be
    missing.
  - [ ] Keep the existing optional-only regression intent.
- [ ] Required methods for tag unions:
  - [ ] Keep the current opaque tag spec operations, updated to use `parser`
    instead of `parse_from`.
  - [ ] Do not add tag-name renaming in this change.
- [ ] Custom nominal parser requirements:
  - [ ] Use the custom nominal's explicit `parser`.
  - [ ] Eagerly call that parser constructor before the enclosing parser returns
    its runtime lambda.
  - [ ] Use the returned runtime parser at the matched value position.

## Fields And Field Runtime Representation

- [ ] Define the hidden representation for `Fields(_shape)` explicitly:
  - [ ] transformed field names as `Str`
  - [ ] hidden result positions for generated record construction
  - [ ] length metadata for `shortest_name` and `longest_name`
  - [ ] buckets for `for_size`
  - [ ] any SSO comparison metadata needed by generated dispatch
- [ ] Define the hidden representation for `Field(_shape)` explicitly:
  - [ ] transformed field name as `Str`
  - [ ] hidden result position
  - [ ] no user-visible slot number
- [ ] Ensure the phantom `_shape` parameter has no runtime representation.
- [ ] Ensure the only ordinary userspace way to obtain `Field(_shape)` is by
  iterating the corresponding `Fields(_shape)`.
- [ ] Use the phantom type to avoid runtime bounds checks in generated record
  parsers:
  - [ ] `encoding.parse_record_field` returning `Field(_shape)` proves the field
    belongs to the same field set passed into the method.
  - [ ] Generated code can dispatch by the hidden position without checking
    whether it is less than the record's field count.
  - [ ] Keep debug assertions only if they check compiler invariants and do not
    become release-build runtime checks.
- [ ] If implementation constraints make a release-build bounds check appear
  necessary, stop and revisit the representation. Do not silently keep a check
  that contradicts the phantom-type design.
- [ ] Ensure `Field.name(field)` returns the transformed field name.
- [ ] Ensure original field names are discarded from the transformed `Fields`
  returned by `Fields.rename_fields`.
- [ ] Ensure `Fields.rename_fields` rebuilds all length buckets from transformed
  names, not original names.
- [ ] Ensure `Fields.shortest_name` and `Fields.longest_name` operate on
  transformed names.
- [ ] Ensure `Fields.iter` and `Fields.for_size` are allocation-free iterators
  over already-built field data.
- [ ] Ensure runtime calls to `Fields.rename_fields` remain valid Roc code even
  if they allocate. The no-runtime-allocation guarantee is tested by constructing
  the parser at compile time.
- [ ] Ensure compile-time calls to `Fields.rename_fields` can allocate during
  checking and then store the transformed result as checked constant data.
- [ ] Add explicit checked constant support for storing and restoring `Fields`
  values.
- [ ] Add explicit checked constant support for storing and restoring generated
  parser closures that capture transformed `Fields` and nested runtime parser
  functions.
- [ ] Do not key generated parser closure captures only by source pattern
  binders. Generated parser construction can create capture values that have no
  source binder, so the checked constant representation must have explicit
  generated capture identities.
- [ ] Ensure restored compile-time parser closures participate in normal
  monomorphic lowering as generated code, rather than becoming an interpretation
  path or forcing parser construction back into the runtime request path.

## Derived Parser Construction

- [ ] Change derived parser lowering so a structural `parser` method returns a
  function.
- [ ] Decide which compiler stage owns each piece of derivation data, and make
  the ownership explicit:
  - [ ] checking owns user-facing static-dispatch requirements and shape
    validation;
  - [ ] checked output owns enough explicit data for post-check lowering;
  - [ ] post-check lowering consumes that checked data without reconstructing
    method requirements from names or source syntax;
  - [ ] compile-time evaluation owns any parser-construction constants that are
    evaluated during checking.
- [ ] Emit parser-construction work before returning the runtime lambda:
  - [ ] create the original compiler field set for the current record shape
  - [ ] call `Fields.rename_fields(original_fields, |name| encoding.rename_field(name))`
  - [ ] build nested field parsers eagerly
  - [ ] build tag payload parsers eagerly where required
  - [ ] build custom nominal parsers eagerly
  - [ ] return a lambda that captures only transformed field sets and nested
    runtime parser functions
- [ ] Ensure parser construction does not capture original record field names in
  the returned lambda.
- [ ] Ensure nested records each get their own `Fields(_nested_shape)` and
  repeat the same rename/rebucket construction.
- [ ] Ensure recursive nominal/tag-union parsers still terminate through the
  existing recursive function handling rather than trying to build an infinite
  parser value.
- [ ] Ensure type aliases expand to the same structural parser as their backing
  structure.
- [ ] Ensure generalized parser methods specialize normally when monomorphic
  types are requested.

## Runtime Record Loop

- [ ] Replace the old record loop that consumed `Field({ name, value, rest })`.
- [ ] Keep the userspace record event API slot-free. Userspace returns field
  handles or names and `rest`; it never returns or receives a raw record slot.
- [ ] Implement the new event loop:
  - [ ] call `encoding.parse_record_field(fields, state)`
  - [ ] on `Done({ rest })`, finish required/optional field state
  - [ ] on `Continue({ rest })`, continue the loop with `rest`
  - [ ] on `Field({ field, rest })`, dispatch directly by hidden field position
  - [ ] on `TryField({ name, rest })`, exact-match `name` against transformed
    fields
  - [ ] on `TryFieldCaseless({ name, rest })`, ASCII-caseless-match `name`
    against transformed fields
  - [ ] if `TryField` or `TryFieldCaseless` misses, call
    `encoding.skip_record_field(rest)` and continue with the returned state
- [ ] For matched fields, call the parser for that field's type from the
  value-start state.
- [ ] Continue the record loop from the matched field parser's returned `rest`.
- [ ] Do not use the record-field event's state as the post-value continuation
  for matched fields.
- [ ] Store parsed values in generated record state with the correct field type.
- [ ] Track required-field presence separately from field values.
- [ ] For optional fields:
  - [ ] missing field calls `encoding.missing_optional_field(field_name, rest)`
    and stores `Err(missing)` in the field
  - [ ] the compiler does not hardcode the optional absence tag name
  - [ ] present field produces `Ok(value)`
  - [ ] `Err(_)` matching remains accepted
  - [ ] postfix `?` use in tests continues to behave correctly
- [ ] For required fields:
  - [ ] missing field calls `encoding.missing_record_field(field_name, rest)`
  - [ ] the field name passed to `missing_record_field` is the transformed name
    because original names are discarded after rename
- [ ] Preserve last-value-wins behavior for repeated input fields, unless an
  existing test proves a different documented rule.
- [ ] Keep the generated record loop direct. Do not introduce callback dispatch
  or a shape interpreter.

## Matching And Performance

- [ ] Implement exact matching for `TryField`.
- [ ] Implement ASCII-caseless matching for `TryFieldCaseless`.
- [ ] Keep caseless matching generic string matching; do not encode HTTP header
  knowledge in the compiler.
- [ ] Implement SSO-oriented generated dispatch:
  - [ ] direct packed-small-string comparisons for common small field names
  - [ ] length buckets based on transformed field names
  - [ ] no allocation on the matching path
  - [ ] long-name path stays correct and allocation-free when the format uses
    field iteration and slice comparisons
- [ ] Ensure the generated dispatch can handle field order independent of input
  order.
- [ ] Ensure `Fields.for_size` narrows candidates using transformed name length.
- [ ] Ensure `Fields.iter` exposes all fields in a deterministic order, but do
  not make parser correctness depend on that order.
- [ ] Add a future optimization note only if needed; do not block correctness on
  perfect hashing.
- [ ] Verify generated code does not allocate or build a runtime map for record
  fields.

## Tag Union Integration

- [ ] Keep tag-union specs opaque and compiler-generated.
- [ ] Update tag-union derived parsing to use `parser` methods.
- [ ] Ensure tag-union payload parsing uses value-start state and returned
  `rest`, same as record fields.
- [ ] Keep externally tagged JSON examples working:
  - [ ] `{ "Admin": { "name": "Sam" } }`
- [ ] Do not implement tag-name renaming in this change.
- [ ] Ensure nested combinations work:
  - [ ] record containing tag union
  - [ ] tag union payload containing record
  - [ ] record containing nested record containing tag union
  - [ ] tag union payload containing record containing another tag union
- [ ] Ensure recursive tag unions either work through existing recursive method
  handling or fail during checking with a clear unsupported-shape diagnostic.

## Numeric And Non-Str Field Parsing

- [ ] Add `U64` parsing support sufficient for:
  - [ ] HTTP `Content-Length`
  - [ ] JSON string/record tests if needed later
- [ ] Define the expected format method:
  - [ ] `encoding.parse_u64 : state -> Try({ value : U64, rest : state }, err)`
- [ ] Implement userspace header `parse_u64` by parsing the value slice without
  allocation.
- [ ] Decide whether JSON numeric support is in this change:
  - [ ] If yes, implement JSON `parse_u64` for numeric JSON values.
  - [ ] If no, keep JSON proof-of-concept to records and strings and add a
    checking test that numeric JSON parsing reports a missing method.
- [ ] Add tests that prove field value parsers determine continuation state:
  - [ ] a `U64` field followed by another field
  - [ ] a `Str` field followed by another field
  - [ ] unknown field followed by known field through `skip_record_field`

## HTTP Header Platform Changes

- [ ] Replace `output.parse_from` constraints with `output.parser`.
- [ ] Replace the old `HeaderFormat` value/rest event API.
- [ ] Define the runtime state as the header value itself, for example:
  - [ ] `Headers := { raw : Str }.{ ... }`
  - [ ] value states if needed for parsing a current value slice
- [ ] Define a header encoding value, for example:
  - [ ] `HeaderEncoding.Caseless`
  - [ ] zero-sized or tag-union shape owned by the platform
- [ ] Implement `HeaderEncoding.rename_field`:
  - [ ] `content_length -> content-length`
  - [ ] `cache_control -> cache-control`
  - [ ] `x_auth_token -> x-auth-token`
  - [ ] run this in parser construction, not in the runtime header scan
- [ ] Implement `HeaderEncoding.parse_record_field`:
  - [ ] parse one CRLF-delimited line
  - [ ] return `Done` at end of header section
  - [ ] return bad-header error when a non-empty line lacks `:`
  - [ ] return `TryFieldCaseless({ name, rest: value_start })` for ordinary
    header lines
  - [ ] use `Fields.shortest_name` and `Fields.longest_name` where they can
    cheaply avoid impossible work
- [ ] Implement `HeaderEncoding.skip_record_field`:
  - [ ] skip from value-start state to the next header line
  - [ ] no allocation
- [ ] Implement `HeaderEncoding.missing_optional_field`:
  - [ ] return the header format's optional absence tag
  - [ ] do not require the compiler to know the tag name
- [ ] Implement `HeaderEncoding.parse_str`:
  - [ ] return a trimmed seamless slice for the current header value
  - [ ] continue from the end of that header line
- [ ] Implement `HeaderEncoding.parse_u64`:
  - [ ] parse ASCII decimal from the trimmed value slice
  - [ ] reject empty values
  - [ ] reject non-digits
  - [ ] reject overflow
  - [ ] return continuation state after the value
  - [ ] no allocation
- [ ] Keep `Headers.DecodeErr` public as an associated type on `Headers`.
- [ ] Update the host/app regression so the decoded record includes:
  - [ ] `content_length : U64`
  - [ ] `x_auth_token : Try(Str, [Missing])`
  - [ ] `cache_control : Str`
  - [ ] existing required/optional fields needed by prior tests
- [ ] Ensure the Roc app computes a response that proves all field values were
  parsed by the correct field parsers.
- [ ] Keep the host response construction allocation-free.
- [ ] Keep host UTF-8 validation for path and headers before passing `Str` to
  Roc.
- [ ] Keep the maximum content-length rejection behavior.

## JSON Platform Changes

- [ ] Replace `a.parse_from` constraints with `a.parser`.
- [ ] Replace old `JsonFormat` value/rest event shape.
- [ ] Define JSON runtime state as ordinary Roc data:
  - [ ] input cursor over `Str`
  - [ ] object cursor state if useful
  - [ ] value-start state for the current field value
- [ ] Define a plain JSON encoding with identity `rename_field`.
- [ ] Define a camel-case JSON encoding if used by tests:
  - [ ] `user_id -> userId`
  - [ ] `cache_control -> cacheControl`
- [ ] Implement `parse_record_field` to return `TryField`, not converted names.
- [ ] Implement `skip_record_field` for unknown JSON values:
  - [ ] string values
  - [ ] object values currently supported by the test platform
  - [ ] nested object braces as far as current tests require
- [ ] Keep JSON `Str` parsing strict:
  - [ ] JSON string succeeds
  - [ ] missing field is separate
  - [ ] `null` is not accepted by `Json.str` / plain `Str`
- [ ] Preserve JSON field order independence.
- [ ] Keep externally tagged JSON tag-union tests working.
- [ ] Do not implement JSON null-specific behavior in this change unless needed
  for existing tests.

## Obsolete Code To Delete Or Rewrite

- [ ] Delete or fully rewrite old `parse_from` static-dispatch special cases.
- [ ] Delete any temporary compatibility wrappers introduced during the
  migration. The final tree must not contain helper paths that silently emulate
  the old API.
- [ ] Delete or rename `ParseFrom*` CLI tests so the test names match `parser`.
- [ ] Delete old record-field event validation for:
  - [ ] `Field({ name : Str, value : state, rest : state })`
  - [ ] `longest_field_len`
  - [ ] uniform value-state storage
- [ ] Delete old runtime header name conversion from hot parsing paths:
  - [ ] ASCII lowercase allocation path
  - [ ] dash-to-underscore runtime conversion
  - [ ] any helper used only to produce old exact record field names at runtime
- [ ] Keep a compile-time `rename_field` helper for headers, but do not call it
  while scanning each runtime header line.
- [ ] Delete old checks that hardcode the header state shape in type lowering.
- [ ] Delete any obsolete compiler-generated record spec type used only by the
  old record parser API.
- [ ] Delete stale comments mentioning old parse method names.
- [ ] Delete stale design tests that expect old errors.
- [ ] Delete any compatibility path that lets old `parse_from` code keep
  compiling.
- [ ] After deletion, run:
  - [ ] `rg -n "parse_from|ParseFrom|longest_field_len|Field\\.value" src test design.md`
  - [ ] inspect every remaining match and justify or remove it

## Checking Tests To Add Or Update

- [ ] Add/rename CLI test: old `parse_from` method is not accepted.
- [ ] Add CLI test: missing `parser` on custom nominal type reports a clear
  static-dispatch error.
- [ ] Add CLI test: record parser missing `rename_field` reports missing
  `rename_field`.
- [ ] Add CLI test: record parser missing `parse_record_field` reports missing
  `parse_record_field`.
- [ ] Add CLI test: record parser missing `skip_record_field` reports missing
  `skip_record_field` when `TryField` can miss.
- [ ] Add CLI test: required-field record missing `missing_record_field` reports
  missing `missing_record_field`.
- [ ] Add CLI test: optional-only record does not require `missing_record_field`.
- [ ] Add CLI test: wrong `parse_record_field` event shape is rejected.
- [ ] Add CLI test: wrong `Field` phantom shape cannot be passed to another
  record parser.
- [ ] Add CLI test: users cannot construct `Field(_shape)` directly.
- [ ] Add CLI test: users cannot inspect or pattern-match opaque `Field` or
  `Fields`.
- [ ] Add CLI test: `Fields.iter` and `Fields.for_size` are usable from
  userspace parser code.
- [ ] Add CLI test: `Field.name` returns `Str` and typechecks only for a valid
  field handle.
- [ ] Add CLI test: unsupported structural field shape reports a checking error,
  not a post-check invariant.
- [ ] Add CLI test: custom nominal `parser` is used inside a derived record.
- [ ] Add CLI test: structural record `parser` is callable directly.

## HTTP Header Regression Tests

- [ ] Update the main platform regression to build the Roc app with the new
  `parser` API.
- [ ] Add required typed field test:
  - [ ] request contains `Content-Length: 123`
  - [ ] Roc record field is `content_length : U64`
  - [ ] response proves numeric value `123` was parsed as `U64`
- [ ] Add required string field test:
  - [ ] `Cache-Control: no-cache`
  - [ ] Roc record field is `cache_control : Str`
  - [ ] response proves the exact value length or value-dependent result
- [ ] Add optional field test:
  - [ ] `x_auth_token : Try(Str, [Missing])`
  - [ ] present case returns `Ok(value)`
  - [ ] absent case returns `Err(Missing)`
  - [ ] one test uses `Err(Missing)`
  - [ ] one test uses `Err(_)`
  - [ ] one test uses postfix `?`
  - [ ] a compiler-only fixture uses a non-`Missing` absence tag to prove the
    tag is format-owned
  - [ ] a compiler-only fixture uses an optional non-`Str` field
- [ ] Add mixed-shape record test:
  - [ ] required `U64`
  - [ ] required `Str`
  - [ ] optional `Str`
  - [ ] unknown headers before, between, and after known headers
- [ ] Add order-independence tests:
  - [ ] same known headers in Roc record order
  - [ ] reverse order
  - [ ] scrambled order with unknown headers between them
- [ ] Add case-insensitive header name tests:
  - [ ] `Cache-Control`
  - [ ] `cache-control`
  - [ ] `CACHE-CONTROL`
  - [ ] mixed-case variant
- [ ] Add dash/underscore rename tests:
  - [ ] `cache_control` field matches `Cache-Control`
  - [ ] `x_auth_token` field matches `X-Auth-Token`
- [ ] Add invalid header test:
  - [ ] non-empty line without `:` returns the bad-header error response or
    fails the request as the platform defines
- [ ] Add invalid U64 tests:
  - [ ] empty content length value
  - [ ] non-digit
  - [ ] overflow
- [ ] Add duplicate field test:
  - [ ] duplicate known header follows current chosen behavior
- [ ] Add no-allocation test:
  - [ ] `roc_alloc` crashes
  - [ ] request succeeds
  - [ ] therefore no Roc runtime allocation occurred
- [ ] Add no-host-allocation review/test guard:
  - [ ] no allocator is initialized in the Zig host
  - [ ] no allocator is passed through host parsing code
  - [ ] fixed buffers only
- [ ] Add binary string test for compile-time renaming:
  - [ ] build the final app in optimized mode
  - [ ] scan the final app binary for original Roc field names such as
    `cache_control` and `x_auth_token`
  - [ ] assert they are absent from runtime data
  - [ ] allow transformed names such as `cache-control` and `x-auth-token`
  - [ ] run against the final app binary, not the unstripped compiler binary

## JSON Regression Tests

- [ ] Update JSON proof-of-concept platform to the new `parser` API.
- [ ] Add identity rename record test:
  - [ ] Roc field `foo`
  - [ ] JSON key `"foo"`
- [ ] Add camel-case rename record test:
  - [ ] Roc field `user_id`
  - [ ] JSON key `"userId"`
  - [ ] original `user_id` does not need to appear in final runtime data when
    parser construction is compile-time evaluated
- [ ] Add missing field tests:
  - [ ] required `Str` missing fails
  - [ ] `Try(Str, [Missing])` missing produces `Err(Missing)`
  - [ ] present optional produces `Ok(value)`
- [ ] Add order-independence tests:
  - [ ] object keys in record order
  - [ ] reverse order
  - [ ] scrambled order with unknown keys
- [ ] Add unknown-key skip tests:
  - [ ] unknown string value before known key
  - [ ] unknown object value before known key
  - [ ] unknown key between two known keys
- [ ] Add nested record tests:
  - [ ] record containing nested record
  - [ ] nested record uses its own renamed `Fields`
  - [ ] nested object keys can be in any order
- [ ] Add tag-union nesting tests:
  - [ ] record field containing externally tagged union
  - [ ] tag payload containing record
  - [ ] nested record containing tag union
- [ ] Keep JSON `null` out of scope except for tests proving plain `Str` does
  not accept it if current JSON tests include that behavior.

## Compile-Time Constant And Binary Data Tests

- [ ] Add a test that constructs the parser at a compile-time-evaluated position.
- [ ] Verify transformed `Fields` are stored as checked constants or equivalent
  explicit checked data.
- [ ] Verify original field names are not captured by the returned runtime
  parser closure.
- [ ] Verify nested parser construction also discards original nested field
  names.
- [ ] Build an optimized app binary and scan strings:
  - [ ] original HTTP Roc names absent: `cache_control`, `content_length`,
    `x_auth_token`
  - [ ] transformed HTTP names present if referenced: `cache-control`,
    `content-length`, `x-auth-token`
  - [ ] original JSON snake_case names absent in camel-case test:
    `user_id`, `cache_control`
  - [ ] transformed JSON names present if referenced: `userId`, `cacheControl`
- [ ] Ensure debug/unstripped compiler symbols are not used for this assertion.

## Host And Platform Verification

- [ ] Keep the Zig host raw-socket and fixed-buffer model.
- [ ] Keep request memory alive for the full request.
- [ ] Validate UTF-8 for the path and full header section before producing Roc
  `Str`.
- [ ] Enforce maximum content length before reading/accepting oversized bodies.
- [ ] Keep body reading bounded by parsed content length.
- [ ] Keep host parsing of the first pass limited to:
  - [ ] finding blank line
  - [ ] finding `Content-Length`
  - [ ] validating limits
  - [ ] validating UTF-8
- [ ] Keep Roc parser responsible for header decoding.
- [ ] Ensure host does not build a header map.
- [ ] Ensure host does not allocate.
- [ ] Ensure Roc app response proves values were parsed by Roc parser methods.

## Source And Symbol Audits

- [ ] After implementation, run these searches and resolve every unexpected
  match:
  - [ ] `rg -n "parse_from|ParseFrom" src test examples design.md`
  - [ ] `rg -n "longest_field_len|Field\\.value|value : .*rest" src test`
  - [ ] `rg -n "Decoder|Parser\\(" src test design.md`
  - [ ] `rg -n "dispatch\\(" src/base/Builtin.roc src/check src/postcheck test`
  - [ ] `rg -n "cache_control|content_length|x_auth_token" test/http-headers`
- [ ] Inspect generated/optimized app binaries separately for string absence;
  source tests are allowed to contain original Roc field names.
- [ ] Check that no compiler stage hardcodes the header state shape.
- [ ] Check that no compiler stage mentions HTTP, JSON, camelCase, kebab-case,
  null, or missing-header rules except in tests or platform examples.
- [ ] Check that generated parser code calls direct static methods, not callback
  tables.

## Targeted Test Runs

Run targeted tests before the full CI-sized command. Use `--summary failures`
where supported.

- [ ] Before running tests, run focused source searches and clean up stale API
  names:
  - [ ] `rg -n "parse_from|ParseFrom" src test examples design.md`
  - [ ] `rg -n "longest_field_len|Field\\.value|Field\\(\\{ name : Str, value" src test`
  - [ ] `rg -n "Decoder\\.dispatch|dispatch\\(" src/build/roc src/check src/postcheck test`
- [ ] Check/build the compiler:
  - [ ] `zig build roc --summary failures`
- [ ] Run checking-focused tests:
  - [ ] `zig build run-test-zig-module-check --summary failures`
  - [ ] `zig build run-test-cli --summary failures`
- [ ] Run post-check/lowering focused tests:
  - [ ] `zig build run-test-zig-module-postcheck --summary failures`
  - [ ] `zig build run-test-zig-module-compile --summary failures`
- [ ] Run parser platform regressions:
  - [ ] `zig build run-test-zig-http-header-decoder-platform --summary failures`
  - [ ] `zig build run-test-zig-json-decoder-platform --summary failures`
- [ ] Run combined targeted command used on this branch:
  - [ ] `zig build run-test-zig-module-check run-test-zig-module-postcheck run-test-zig-http-header-decoder-platform run-test-zig-json-decoder-platform --summary failures`
- [ ] Run source checks:
  - [ ] `zig build run-check-tidy --summary failures`
  - [ ] `zig build run-check-unused-suppression --summary failures`
  - [ ] the repository compiler-stage wording audit build step
- [ ] Run any newly added focused tests directly if the build exposes a narrower
  step.

## Release Builds And Assembly Verification

- [ ] Build the Roc compiler in release mode with symbols:
  - [ ] `zig build roc -Doptimize=ReleaseFast -Dstrip=false --summary failures`
- [ ] Build the HTTP header platform/app with:
  - [ ] Zig host in `ReleaseFast`
  - [ ] Roc app with `--opt=speed`
- [ ] Save baseline and final disassembly under `/tmp` when comparing changes:
  - [ ] compiler relevant symbols if parser lowering is being audited
  - [ ] final platform app hot request handler
  - [ ] final Roc parser code for header record parsing if symbol names are
    available
- [ ] Use `xcrun llvm-objdump` on macOS:
  - [ ] `xcrun llvm-objdump --macho --disassemble --demangle <binary> > /tmp/<name>.s`
- [ ] Inspect the hot header parsing path for:
  - [ ] no allocator calls
  - [ ] no runtime field map construction
  - [ ] no callback-table dispatch
  - [ ] direct calls or inlined code for field value parsers
  - [ ] no runtime lowercase/dash conversion for each header name
  - [ ] caseless comparison over input header name slices and transformed static
    field names
  - [ ] `U64` parser consumes the value and returns the continuation state
- [ ] Scan optimized final app binary strings:
  - [ ] original field names absent where the test expects compile-time renaming
  - [ ] transformed field names present only as needed
- [ ] Record the disassembly files and the key observations in the final
  implementation summary.

## Full Verification Loop

- [ ] After targeted tests pass, run:
  - [ ] `zig build minici`
- [ ] If `zig build minici` fails in one section:
  - [ ] identify the failing section
  - [ ] run the narrow failing section directly
  - [ ] fix the issue
  - [ ] rerun that narrow section until it passes
  - [ ] rerun `zig build minici`
  - [ ] repeat this loop until `zig build minici` passes
- [ ] Do not use the full `zig build minici` command as the inner retry loop.
- [ ] After macOS verification, run target-specific checks needed for CI:
  - [ ] Windows mirror sync through `/Users/rtfeldman/.agents/roc-windows.sh sync`
  - [ ] Windows CLI tests if the changed code affects Windows:
    `/Users/rtfeldman/.agents/roc-windows.sh test-cli --sync`
  - [ ] Windows glue/platform tests if the changed code affects glue or platform
    ABI:
    `/Users/rtfeldman/.agents/roc-windows.sh test-glue --sync`
  - [ ] Any Linux/musl or cross-target steps exposed by the Roc build for the
    touched code paths
- [ ] Do not build directly from the Parallels shared folder on Windows.

## Final Review Checklist

- [ ] Re-read `design.md`, section `Structural Serialization Methods`, and
  confirm the implementation matches it.
- [ ] Re-read this `plan.md` and check off every completed item.
- [ ] Run `git diff --check`.
- [ ] Run source searches for obsolete API names one final time.
- [ ] Review the whole branch with code-review posture:
  - [ ] correctness bugs
  - [ ] missing checking errors
  - [ ] hidden runtime allocation
  - [ ] stale old API compatibility paths
  - [ ] format-specific knowledge in compiler code
  - [ ] test gaps
  - [ ] release-build checks that should be checked-stage errors
- [ ] Commit only after targeted verification is green for the implemented
  slice.
- [ ] Push after the commit if requested or if continuing the existing branch
  workflow.
