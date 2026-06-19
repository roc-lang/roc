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

- [x] Re-read `design.md`, `Structural Serialization Methods`, before each
  implementation phase that changes checking, lowering, or platform examples.
- [x] Treat the public API examples in that section as normative:
  - [x] convenience APIs such as `Headers.parse(raw)`;
  - [x] structural method shape
    `a.parser : encoding -> (state -> Try({ value : a, rest : state }, err))`;
  - [x] direct state construction such as `Headers.{ raw }`;
  - [x] opaque `Fields(_shape)` / `Field(_shape)` metadata;
  - [x] record events carrying `rest` at the value-start position;
  - [x] construction-time field renaming through `Fields.rename_fields`;
  - [x] no builtin user-facing `Decoder`, `Parser`, or `Encoding` type.
- [x] Treat the phantom-type explanation in `design.md` as normative:
  `Field(_shape)` handles produced from a `Fields(_shape)` value are already
  tied to that exact record shape, so generated code must not need a
  release-build bounds check against the record arity.
- [x] Treat the performance section in `design.md` as normative:
  transformed field data is built before the runtime lambda, SSO field names
  are the hot path, long field names still work, and the generated runtime scan
  must not allocate or build temporary maps.
- [x] Treat the HTTP and JSON examples in `design.md` as the acceptance model for
  platform tests, while keeping the compiler fully format-agnostic.

## Definition Of Done

- [x] The old `parse_from` structural derivation path is gone from the compiler
  and tests, except for any deliberately retained migration note that is not
  compiler source or Roc fixture source.
- [x] The new `parser` derivation is implemented through ordinary static
  dispatch and direct generated code, not callback tables or a runtime shape
  interpreter.
- [x] The HTTP header regression demonstrates zero runtime allocation in both
  the Zig host and Roc app.
- [x] The HTTP and JSON regressions demonstrate field-order independence.
- [x] The HTTP header regression demonstrates field-name renaming,
  case-insensitive matching, required fields, optional fields, mixed field
  value shapes, and continuation from each value parser's returned `rest`.
- [x] Unsupported parser shapes fail during checking with static-dispatch errors
  instead of reaching post-check invariants.
- [x] Optimized binary inspection confirms the hot header path has no allocator
  calls, no runtime field map, and no per-header runtime field-name conversion.
- [x] Compile-time parser construction is fully supported: generated parser
  closures can be stored, restored, captured, and lowered as ordinary generated
  code without rebuilding field metadata at runtime.
- [x] Targeted test steps pass first; then `zig build minici` passes by using the
  targeted-failure loop described at the end of this file.

## Design Invariants

- [x] Preserve the method-based public model. Do not introduce builtin interface
  types for parsing or encoding.
- [x] Keep the compiler format-agnostic. The compiler may know Roc record
  fields, tag labels, method names, and matching modes; it must not know JSON,
  HTTP headers, null, missing-header rules, kebab-case, camelCase, or any other
  format concept.
- [x] Keep parsing effect-free. All parsing methods are pure functions from
  state to `Try`.
- [x] Keep userspace safe. Do not expose raw byte indexing, unchecked memory
  primitives, raw field slot integers, or unchecked field construction.
- [x] Keep runtime header parsing allocation-free in the regression platform.
  Compile-time allocation during parser construction is allowed.
- [x] Keep the host allocation-free in the HTTP header regression platform.
  `roc_alloc` must still crash for that test.
- [x] Keep record parsing one pass over the serialized record fields after the
  host has found the header section. The generated parser must not require a
  temporary map.
- [x] Preserve seamless slices. Parsed `Str` values that are slices of input
  must remain slices into validated request memory or validated JSON input.
- [x] Preserve order independence for records. HTTP header order and JSON object
  field order must not need to match Roc record field order.
- [x] Preserve duplicate-field behavior. If an input repeats a field, keep the
  current chosen rule, expected to be last value wins unless tests reveal a
  documented existing rule.
- [x] Unsupported structural shapes must fail during checking through missing
  static-dispatch methods, not as runtime parse failures.
- [x] Runtime parse failures are only for input conditions known while scanning,
  such as invalid JSON syntax, bad header lines, missing required fields, or
  user-defined nominal parser errors.
- [x] Do not add new language syntax. This change is method dispatch and
  compiler derivation only.
- [x] Follow `design.md` post-check rules: later stages consume explicit checked
  data produced by earlier stages, and no later stage guesses missing
  information.

## Current Implementation Inventory

- [x] Read the current implementation entry points before editing:
  - [x] `src/check/static_dispatch_registry.zig`
  - [x] `src/check/Check.zig`
  - [x] `src/postcheck/monotype/lower.zig`
  - [x] `src/build/roc/Builtin.roc`
  - [x] `src/build/builtin_compiler/main.zig`
  - [x] `src/canonicalize/BuiltinLowLevel.zig`
  - [x] `test/http-headers/platform/Headers.roc`
  - [x] `test/json-decoder/platform/Json.roc`
  - [x] CLI tests named `ParseFrom*`
- [x] Search for all old parse method names:
  - [x] `rg -n "parse_from" src test examples design.md`
  - [x] `rg -n "ParseFrom|parse_record_field|missing_record_field|ParseTagUnionSpec" src test`
  - [x] `rg -n "Field\\(\\{ name : Str, value|Field\\.value|longest_field_len" src test`
- [x] Record which uses are true implementation code, which are tests, and which
  are comments or docs.
- [x] Confirm `design.md` remains the only design source for the new API.

## Builtin Surface To Add

- [x] Update every builtin-indexing and canonicalization location that must know
  about new opaque builtin nominal types:
  - [x] generated builtin node index collection;
  - [x] builtin compiler index installation;
  - [x] common identifier interning;
  - [x] canonicalization auto-imports;
  - [x] intrinsic annotation allowlists;
  - [x] CheckedModule builtin nominal encoding;
  - [x] post-check null-layout handling for opaque compile-time-only values;
  - [x] glue/runtime encoding code that classifies builtin nominals.
- [x] Add opaque compiler-owned field types to `Builtin.roc`:
  - [x] `Fields(_shape) : opaque`
  - [x] `Field(_shape) : opaque`
- [x] Add builtin method annotations for field sets:
  - [x] `Fields.rename_fields : Fields(_shape), (Str -> Str) -> Fields(_shape)`
  - [x] `Fields.shortest_name : Fields(_shape) -> U64`
  - [x] `Fields.longest_name : Fields(_shape) -> U64`
  - [x] `Fields.iter : Fields(_shape) -> Iter(Field(_shape))`
  - [x] `Fields.for_size : Fields(_shape), U64 -> Iter(Field(_shape))`
  - [x] `Field.name : Field(_shape) -> Str`
- [x] Do not provide crash bodies for compiler-generated builtin functions.
  Follow the existing builtin style: annotations only where the compiler
  supplies implementation.
- [x] Ensure the phantom `_shape` type parameter is present in every public field
  set or field handle type that can cross userspace method boundaries.
- [x] Ensure no public method exposes the internal field slot integer.
- [x] Add or update comments in `Builtin.roc` explaining that these are
  compiler-generated opaque values used by structural parser derivation.
- [x] Decide the exact public event tag names and keep them aligned with
  `design.md`:
  - [x] `Field({ field : Field(_shape), rest : state })`
  - [x] `TryField({ name : Str, rest : state })`
  - [x] `TryFieldCaseless({ name : Str, rest : state })`
  - [x] `Continue({ rest : state })`
  - [x] `Done({ rest : state })`

## Method Names And Dispatch Registry

- [x] Replace the old structural parse method name:
  - [x] Remove special handling for `parse_from`.
  - [x] Add special handling for `parser`.
  - [x] Update identifier initialization so `parser` is interned wherever
    builtin method names are initialized.
- [x] Update `src/check/static_dispatch_registry.zig`:
  - [x] Replace the old `.parse_from` result mode with `.parser`.
  - [x] Track that `parser` returns a function.
  - [x] Track the `encoding` argument type separately from the returned runtime
    state argument type.
  - [x] Preserve ordinary static dispatch for custom nominal `parser` methods.
- [x] Update static-dispatch problem reporting:
  - [x] Missing `parser` on a custom nominal type should mention `parser`.
  - [x] Missing format methods should mention the exact missing method, such as
    `parse_str`, `parse_record_field`, `skip_record_field`, or `rename_field`.
  - [x] Remove references to `parse_from` from user-facing errors.
- [x] Update any tests that snapshot static-dispatch output.
- [x] Ensure `parser` remains a normal method users can call directly:
  - [x] `Shape.parser(encoding)` should work.
  - [x] `({ foo: "x" }).encode_to(encoding)` should work once structural
    `encode_to` derivation is implemented.

## Parser Type Shape In Checking

- [x] Validate the derived method shape:
  - [x] `a.parser : encoding -> (state -> Try({ value : a, rest : state }, err))`
  - [x] The returned function must take exactly one runtime state argument.
  - [x] The returned `Try` `Ok` payload must be a record with `value` and `rest`.
  - [x] The `value` field must match the parsed shape.
  - [x] The `rest` field must have the same state type as the runtime argument.
  - [x] All parse errors in one derived parser must unify to the same error
    type.
- [x] Validate the structural `encode_to` shape remains coherent with the new
  parse design:
  - [x] `a.encode_to : a, encoding -> (state -> Try(state, err))`
  - [x] Do not change encoding implementation unless needed for shared method
    infrastructure.
- [x] Ensure checking derives `parser` for supported structural types:
  - [x] `Str`
  - [x] records
  - [x] tag unions currently supported by the branch
  - [x] `Try(Str, [Missing])`
  - [x] `Try(value, [Missing])` where `value` has a parser, if this falls out
    naturally from the field-type machinery
  - [x] custom nominal types with explicit `parser`
  - [x] `U64`, at minimum for the new header `content_length` test
- [x] Decide which numeric primitives are implemented now:
  - [x] If only `U64` is implemented, unsupported numeric primitives must report
    missing method requirements during checking.
  - [x] Other integer widths are out of scope for this slice; add method
    validation and tests when each width becomes supported.
- [x] Ensure unsupported shapes do not reach post-check lowering as invariants.
  Checking must reject them first.

## Format Method Requirements

- [x] For every derived parser, generate constraints on the encoding/state pair
  for exactly the needed shape methods.
- [x] Required methods for strings:
  - [x] `encoding.parse_str : state -> Try({ value : Str, rest : state }, err)`
- [x] Required methods for `U64`:
  - [x] `encoding.parse_u64 : state -> Try({ value : U64, rest : state }, err)`
  - [x] If method naming follows a different existing numeric naming pattern,
    update this plan and tests before implementation.
- [x] Required methods for records:
  - [x] `encoding.rename_field : Str -> Str`
  - [x] `encoding.parse_record_field : Fields(_shape), state -> Try(event, err)`
  - [x] `encoding.skip_record_field : state -> Try(state, err)`
  - [x] `encoding.missing_record_field : Str, state -> err` only when the target
    record has at least one required field.
  - [x] `encoding.missing_optional_field : Str, state -> optional_err` only when
    the target record has at least one `Try(value, optional_err)` field.
- [x] Required methods for optional-only records:
  - [x] Do not require `missing_record_field` if no required field can be
    missing.
  - [x] Keep the existing optional-only regression intent.
- [x] Required methods for tag unions:
  - [x] Keep the current opaque tag spec operations, updated to use `parser`
    instead of `parse_from`.
  - [x] Do not add tag-name renaming in this change.
- [x] Custom nominal parser requirements:
  - [x] Use the custom nominal's explicit `parser`.
  - [x] Eagerly call that parser constructor before the enclosing parser returns
    its runtime lambda.
  - [x] Use the returned runtime parser at the matched value position.

## Fields And Field Runtime Representation

- [x] Define the hidden representation for `Fields(_shape)` explicitly:
  - [x] transformed field names as `Str`
  - [x] hidden result positions for generated record construction
  - [x] length metadata for `shortest_name` and `longest_name`
  - [x] buckets for `for_size`
  - [x] any SSO comparison metadata needed by generated dispatch
- [x] Define the hidden representation for `Field(_shape)` explicitly:
  - [x] transformed field name as `Str`
  - [x] hidden result position
  - [x] no user-visible slot number
- [x] Ensure the phantom `_shape` parameter has no runtime representation.
- [x] Ensure the only ordinary userspace way to obtain `Field(_shape)` is by
  iterating the corresponding `Fields(_shape)`.
- [x] Use the phantom type to avoid runtime bounds checks in generated record
  parsers:
  - [x] `encoding.parse_record_field` returning `Field(_shape)` proves the field
    belongs to the same field set passed into the method.
  - [x] Generated code can dispatch by the hidden position without checking
    whether it is less than the record's field count.
  - [x] Keep debug assertions only if they check compiler invariants and do not
    become release-build runtime checks.
- [x] If implementation constraints make a release-build bounds check appear
  necessary, stop and revisit the representation. Do not silently keep a check
  that contradicts the phantom-type design.
- [x] Ensure `Field.name(field)` returns the transformed field name.
- [x] Ensure original field names are discarded from the transformed `Fields`
  returned by `Fields.rename_fields`.
- [x] Ensure `Fields.rename_fields` rebuilds all length buckets from transformed
  names, not original names.
- [x] Ensure `Fields.shortest_name` and `Fields.longest_name` operate on
  transformed names.
- [x] Ensure `Fields.iter` and `Fields.for_size` are allocation-free iterators
  over already-built field data.
- [x] Ensure runtime calls to `Fields.rename_fields` remain valid Roc code even
  if they allocate. The no-runtime-allocation guarantee is tested by constructing
  the parser at compile time.
- [x] Ensure compile-time calls to `Fields.rename_fields` can allocate during
  checking and then store the transformed result as checked constant data.
- [x] Add explicit checked constant support for storing and restoring `Fields`
  values.
- [x] Add explicit checked constant support for storing and restoring generated
  parser closures that capture transformed `Fields` and nested runtime parser
  functions.
- [x] Do not key generated parser closure captures only by source pattern
  binders. Generated parser construction can create capture values that have no
  source binder, so the checked constant representation must have explicit
  generated capture identities.
- [x] Ensure restored compile-time parser closures participate in normal
  monomorphic lowering as generated code, rather than becoming an interpretation
  path or forcing parser construction back into the runtime request path.

## Derived Parser Construction

- [x] Change derived parser lowering so a structural `parser` method returns a
  function.
- [x] Decide which compiler stage owns each piece of derivation data, and make
  the ownership explicit:
  - [x] checking owns user-facing static-dispatch requirements and shape
    validation;
  - [x] checked output owns enough explicit data for post-check lowering;
  - [x] post-check lowering consumes that checked data without reconstructing
    method requirements from names or source syntax;
  - [x] compile-time evaluation owns any parser-construction constants that are
    evaluated during checking.
- [x] Emit parser-construction work before returning the runtime lambda:
  - [x] create the original compiler field set for the current record shape
  - [x] call `Fields.rename_fields(original_fields, |name| encoding.rename_field(name))`
  - [x] build nested field parsers eagerly
  - [x] build tag payload parsers eagerly where required
  - [x] build custom nominal parsers eagerly
  - [x] return a lambda that captures only transformed field sets and nested
    runtime parser functions
- [x] Ensure parser construction does not capture original record field names in
  the returned lambda.
- [x] Ensure nested records each get their own `Fields(_nested_shape)` and
  repeat the same rename/rebucket construction.
- [x] Ensure recursive nominal/tag-union parsers still terminate through the
  existing recursive function handling rather than trying to build an infinite
  parser value. Recursive nominal tag unions without an explicit `parser`
  produce a checking error; recursive nominal tag unions with an explicit
  method use the ordinary custom nominal parser path.
- [x] Ensure type aliases expand to the same structural parser as their backing
  structure.
- [x] Ensure generalized parser methods specialize normally when monomorphic
  types are requested.

## Runtime Record Loop

- [x] Replace the old record loop that consumed `Field({ name, value, rest })`.
- [x] Keep the userspace record event API slot-free. Userspace returns field
  handles or names and `rest`; it never returns or receives a raw record slot.
- [x] Implement the new event loop:
  - [x] call `encoding.parse_record_field(fields, state)`
  - [x] on `Done({ rest })`, finish required/optional field state
  - [x] on `Continue({ rest })`, continue the loop with `rest`
  - [x] on `Field({ field, rest })`, dispatch directly by hidden field position
  - [x] on `TryField({ name, rest })`, exact-match `name` against transformed
    fields
  - [x] on `TryFieldCaseless({ name, rest })`, ASCII-caseless-match `name`
    against transformed fields
  - [x] if `TryField` or `TryFieldCaseless` misses, call
    `encoding.skip_record_field(rest)` and continue with the returned state
- [x] For matched fields, call the parser for that field's type from the
  value-start state.
- [x] Continue the record loop from the matched field parser's returned `rest`.
- [x] Do not use the record-field event's state as the post-value continuation
  for matched fields.
- [x] Store parsed values in generated record state with the correct field type.
- [x] Track required-field presence separately from field values.
- [x] For optional fields:
  - [x] missing field calls `encoding.missing_optional_field(field_name, rest)`
    and stores `Err(missing)` in the field
  - [x] the compiler does not hardcode the optional absence tag name
  - [x] present field produces `Ok(value)`
  - [x] `Err(_)` matching remains accepted
  - [x] postfix `?` use in tests continues to behave correctly
- [x] For required fields:
  - [x] missing field calls `encoding.missing_record_field(field_name, rest)`
  - [x] the field name passed to `missing_record_field` is the transformed name
    because original names are discarded after rename
- [x] Preserve last-value-wins behavior for repeated input fields, unless an
  existing test proves a different documented rule.
- [x] Keep the generated record loop direct. Do not introduce callback dispatch
  or a shape interpreter.

## Matching And Performance

- [x] Implement exact matching for `TryField`.
- [x] Implement ASCII-caseless matching for `TryFieldCaseless`.
- [x] Keep caseless matching generic string matching; do not encode HTTP header
  knowledge in the compiler.
- [x] Implement SSO-oriented generated dispatch:
  - [x] direct packed-small-string comparisons for common small field names
  - [x] length buckets based on transformed field names
  - [x] no allocation on the matching path
  - [x] long-name path stays correct and allocation-free when the format uses
    field iteration and slice comparisons
- [x] Ensure the generated dispatch can handle field order independent of input
  order.
- [x] Ensure `Fields.for_size` narrows candidates using transformed name length.
- [x] Ensure `Fields.iter` exposes all fields in a deterministic order, but do
  not make parser correctness depend on that order.
- [x] Add a future optimization note only if needed; do not block correctness on
  perfect hashing.
- [x] Verify generated code does not allocate or build a runtime map for record
  fields.

## Tag Union Integration

- [x] Keep tag-union specs opaque and compiler-generated.
- [x] Update tag-union derived parsing to use `parser` methods.
- [x] Ensure tag-union payload parsing uses value-start state and returned
  `rest`, same as record fields.
- [x] Keep externally tagged JSON examples working:
  - [x] `{ "Admin": { "name": "Sam" } }`
- [x] Do not implement tag-name renaming in this change.
- [x] Ensure nested combinations work:
  - [x] record containing tag union
  - [x] tag union payload containing record
  - [x] record containing nested record containing tag union
  - [x] tag union payload containing record containing another tag union
- [x] Ensure recursive tag unions either work through existing recursive method
  handling or fail during checking with a clear unsupported-shape diagnostic.

## Numeric And Non-Str Field Parsing

- [x] Add `U64` parsing support sufficient for:
  - [x] HTTP `Content-Length`
  - [x] JSON string/record tests if needed later
- [x] Define the expected format method:
  - [x] `encoding.parse_u64 : state -> Try({ value : U64, rest : state }, err)`
- [x] Implement userspace header `parse_u64` by parsing the value slice without
  allocation.
- [x] Decide whether JSON numeric support is in this change:
  - [x] JSON numeric value parsing is out of scope for this change.
  - [x] If no, keep JSON proof-of-concept to records and strings and add a
    checking test that numeric JSON parsing reports a missing method.
- [x] Add tests that prove field value parsers determine continuation state:
  - [x] a `U64` field followed by another field
  - [x] a `Str` field followed by another field
  - [x] unknown field followed by known field through `skip_record_field`

## HTTP Header Platform Changes

- [x] Replace `output.parse_from` constraints with `output.parser`.
- [x] Replace the old `HeaderFormat` value/rest event API.
- [x] Define the runtime state as the header value itself, for example:
  - [x] `Headers := { raw : Str }.{ ... }`
  - [x] value states if needed for parsing a current value slice
- [x] Define a header encoding value, for example:
  - [x] `HeaderEncoding.Caseless`
  - [x] zero-sized or tag-union shape owned by the platform
- [x] Implement `HeaderEncoding.rename_field`:
  - [x] `content_length -> content-length`
  - [x] `cache_control -> cache-control`
  - [x] `x_auth_token -> x-auth-token`
  - [x] run this in parser construction, not in the runtime header scan
- [x] Implement `HeaderEncoding.parse_record_field`:
  - [x] parse one CRLF-delimited line
  - [x] return `Done` at end of header section
  - [x] return bad-header error when a non-empty line lacks `:`
  - [x] return `Field({ field, rest: value_start })` for matching requested
    header lines
  - [x] return `Continue({ rest: next_line })` for unknown header lines
  - [x] use `Fields.shortest_name` and `Fields.longest_name` where they can
    cheaply avoid impossible work
- [x] Implement `HeaderEncoding.skip_record_field`:
  - [x] skip from value-start state to the next header line
  - [x] no allocation
- [x] Implement `HeaderEncoding.missing_optional_field`:
  - [x] return the header format's optional absence tag
  - [x] do not require the compiler to know the tag name
- [x] Implement `HeaderEncoding.parse_str`:
  - [x] return a trimmed seamless slice for the current header value
  - [x] continue from the end of that header line
- [x] Implement `HeaderEncoding.parse_u64`:
  - [x] parse ASCII decimal from the trimmed value slice
  - [x] reject empty values
  - [x] reject non-digits
  - [x] reject overflow
  - [x] return continuation state after the value
  - [x] no allocation
- [x] Keep `Headers.DecodeErr` public as an associated type on `Headers`.
- [x] Update the host/app regression so the decoded record includes:
  - [x] `content_length : U64`
  - [x] `x_auth_token : Try(Str, [Missing])`
  - [x] `cache_control : Str`
  - [x] existing required/optional fields needed by prior tests
- [x] Ensure the Roc app computes a response that proves all field values were
  parsed by the correct field parsers.
- [x] Keep the host response construction allocation-free.
- [x] Keep host UTF-8 validation for path and headers before passing `Str` to
  Roc.
- [x] Keep the maximum content-length rejection behavior.

## JSON Platform Changes

- [x] Replace `a.parse_from` constraints with `a.parser`.
- [x] Replace old `JsonFormat` value/rest event shape.
- [x] Define JSON runtime state as ordinary Roc data:
  - [x] input cursor over `Str`
  - [x] object cursor state if useful
  - [x] value-start state for the current field value
- [x] Define a plain JSON encoding with identity `rename_field`.
- [x] Define a camel-case JSON encoding if used by tests:
  - [x] `user_id -> userId`
  - [x] `cache_control -> cacheControl`
- [x] Implement `parse_record_field` to match transformed `Fields` directly
  for known object keys and return `Continue` after skipping unknown keys.
- [x] Implement `skip_record_field` for unknown JSON values:
  - [x] string values
  - [x] object values currently supported by the test platform
  - [x] nested object braces as far as current tests require
- [x] Keep JSON `Str` parsing strict:
  - [x] JSON string succeeds
  - [x] missing field is separate
  - [x] `null` is not accepted by `Json.str` / plain `Str`
- [x] Preserve JSON field order independence.
- [x] Keep externally tagged JSON tag-union tests working.
- [x] Do not implement JSON null-specific behavior in this change unless needed
  for existing tests.

## Obsolete Code To Delete Or Rewrite

- [x] Delete or fully rewrite old `parse_from` static-dispatch special cases.
- [x] Delete any temporary compatibility wrappers introduced during the
  migration. The final tree must not contain helper paths that silently emulate
  the old API.
- [x] Delete or rename `ParseFrom*` CLI tests so the test names match `parser`.
- [x] Delete old record-field event validation for:
  - [x] `Field({ name : Str, value : state, rest : state })`
  - [x] `longest_field_len`
  - [x] uniform value-state storage
- [x] Delete old runtime header name conversion from hot parsing paths:
  - [x] ASCII lowercase allocation path
  - [x] dash-to-underscore runtime conversion
  - [x] any helper used only to produce old exact record field names at runtime
- [x] Keep a compile-time `rename_field` helper for headers, but do not call it
  while scanning each runtime header line.
- [x] Delete old checks that hardcode the header state shape in type lowering.
- [x] Delete any obsolete compiler-generated record spec type used only by the
  old record parser API.
- [x] Delete stale comments mentioning old parse method names.
- [x] Delete stale design tests that expect old errors.
- [x] Delete any compatibility path that lets old `parse_from` code keep
  compiling.
- [x] After deletion, run:
  - [x] `rg -n "parse_from|ParseFrom|longest_field_len|Field\\.value" src test design.md`
  - [x] inspect every remaining match and justify or remove it

## Checking Tests To Add Or Update

- [x] Add/rename CLI test: old `parse_from` method is not accepted.
- [x] Add CLI test: missing `parser` on custom nominal type reports a clear
  static-dispatch error.
- [x] Add CLI test: record parser missing `rename_field` reports missing
  `rename_field`.
- [x] Add CLI test: record parser missing `parse_record_field` reports missing
  `parse_record_field`.
- [x] Add CLI test: record parser missing `skip_record_field` reports missing
  `skip_record_field` when `TryField` can miss.
- [x] Add CLI test: required-field record missing `missing_record_field` reports
  missing `missing_record_field`.
- [x] Add CLI test: optional-only record does not require `missing_record_field`.
- [x] Add CLI test: wrong `parse_record_field` event shape is rejected.
- [x] Add CLI test: wrong `Field` phantom shape cannot be passed to another
  record parser.
- [x] Add CLI test: users cannot construct `Field(_shape)` directly.
- [x] Add CLI test: users cannot inspect or pattern-match opaque `Field` or
  `Fields`.
- [x] Add CLI test: `Fields.iter` and `Fields.for_size` are usable from
  userspace parser code.
- [x] Add CLI test: `Field.name` returns `Str` and typechecks only for a valid
  field handle.
- [x] Add CLI test: unsupported structural field shape reports a checking error,
  not a post-check invariant.
- [x] Add CLI test: custom nominal `parser` is used inside a derived record.
- [x] Add CLI test: structural record `parser` is callable directly.

## HTTP Header Regression Tests

- [x] Update the main platform regression to build the Roc app with the new
  `parser` API.
- [x] Add required typed field test:
  - [x] request contains `Content-Length: 123`
  - [x] Roc record field is `content_length : U64`
  - [x] response proves numeric value `123` was parsed as `U64`
- [x] Add required string field test:
  - [x] `Cache-Control: no-cache`
  - [x] Roc record field is `cache_control : Str`
  - [x] response proves the exact value length or value-dependent result
- [x] Add optional field test:
  - [x] `x_auth_token : Try(Str, [Missing])`
  - [x] present case returns `Ok(value)`
  - [x] absent case returns `Err(Missing)`
  - [x] one test uses `Err(Missing)`
  - [x] one test uses `Err(_)`
  - [x] one test uses postfix `?`
  - [x] a compiler-only fixture uses a non-`Missing` absence tag to prove the
    tag is format-owned
  - [x] a compiler-only fixture uses an optional non-`Str` field
- [x] Add mixed-shape record test:
  - [x] required `U64`
  - [x] required `Str`
  - [x] optional `Str`
  - [x] unknown headers before, between, and after known headers
- [x] Add order-independence tests:
  - [x] same known headers in Roc record order
  - [x] reverse order
  - [x] scrambled order with unknown headers between them
- [x] Add case-insensitive header name tests:
  - [x] `Cache-Control`
  - [x] `cache-control`
  - [x] `CACHE-CONTROL`
  - [x] mixed-case variant
- [x] Add dash/underscore rename tests:
  - [x] `cache_control` field matches `Cache-Control`
  - [x] `x_auth_token` field matches `X-Auth-Token`
- [x] Add invalid header test:
  - [x] non-empty line without `:` returns the bad-header error response or
    fails the request as the platform defines
- [x] Add invalid U64 tests:
  - [x] empty content length value
  - [x] non-digit
  - [x] overflow
- [x] Add duplicate field test:
  - [x] duplicate known header follows current chosen behavior
- [x] Add no-allocation test:
  - [x] `roc_alloc` crashes
  - [x] request succeeds
  - [x] therefore no Roc runtime allocation occurred
- [x] Add no-host-allocation review/test guard:
  - [x] no allocator is initialized in the Zig host
  - [x] no allocator is passed through host parsing code
  - [x] fixed buffers only
- [x] Add binary string test for compile-time renaming:
  - [x] build the final app in optimized mode
  - [x] scan the final app binary for original Roc field names such as
    `cache_control` and `x_auth_token`
  - [x] assert they are absent from runtime data
  - [x] allow transformed names such as `cache-control` and `x-auth-token`
  - [x] run against the final app binary, not the unstripped compiler binary

## JSON Regression Tests

- [x] Update JSON proof-of-concept platform to the new `parser` API.
- [x] Add identity rename record test:
  - [x] Roc field `foo`
  - [x] JSON key `"foo"`
- [x] Add camel-case rename record test:
  - [x] Roc field `user_id`
  - [x] JSON key `"userId"`
  - [x] original `user_id` does not need to appear in final runtime data when
    parser construction is compile-time evaluated
- [x] Add missing field tests:
  - [x] required `Str` missing fails
  - [x] `Try(Str, [Missing])` missing produces `Err(Missing)`
  - [x] present optional produces `Ok(value)`
- [x] Add order-independence tests:
  - [x] object keys in record order
  - [x] reverse order
  - [x] scrambled order with unknown keys
- [x] Add unknown-key skip tests:
  - [x] unknown string value before known key
  - [x] unknown object value before known key
  - [x] unknown key between two known keys
- [x] Add nested record tests:
  - [x] record containing nested record
  - [x] nested record uses its own renamed `Fields`
  - [x] nested object keys can be in any order
- [x] Add tag-union nesting tests:
  - [x] record field containing externally tagged union
  - [x] tag payload containing record
  - [x] nested record containing tag union
- [x] Keep JSON `null` out of scope except for tests proving plain `Str` does
  not accept it if current JSON tests include that behavior.

## Compile-Time Constant And Binary Data Tests

- [x] Add a test that constructs the parser at a compile-time-evaluated position.
- [x] Verify transformed `Fields` are stored as checked constants or equivalent
  explicit checked data.
- [x] Verify original field names are not captured by the returned runtime
  parser closure.
- [x] Verify nested parser construction also discards original nested field
  names.
- [x] Build an optimized app binary and scan strings:
  - [x] original HTTP Roc names absent: `cache_control`, `content_length`,
    `x_auth_token`
  - [x] transformed HTTP names are allowed but not required as searchable
    strings; runtime tests prove transformed input names still match.
  - [x] original JSON snake_case names absent in camel-case test:
    `user_id`, `cache_control`
  - [x] transformed JSON names are allowed but not required as searchable
    strings; runtime tests prove transformed input names still match.
- [x] Ensure debug/unstripped compiler symbols are not used for this assertion.

## Host And Platform Verification

- [x] Keep the Zig host raw-socket and fixed-buffer model.
- [x] Keep request memory alive for the full request.
- [x] Validate UTF-8 for the path and full header section before producing Roc
  `Str`.
- [x] Enforce maximum content length before reading/accepting oversized bodies.
- [x] Keep body reading bounded by parsed content length.
- [x] Keep host parsing of the first pass limited to:
  - [x] finding blank line
  - [x] finding `Content-Length`
  - [x] validating limits
  - [x] validating UTF-8
- [x] Keep Roc parser responsible for header decoding.
- [x] Ensure host does not build a header map.
- [x] Ensure host does not allocate.
- [x] Ensure Roc app response proves values were parsed by Roc parser methods.

## Source And Symbol Audits

- [x] After implementation, run these searches and resolve every unexpected
  match:
  - [x] `rg -n "parse_from|ParseFrom" src test examples design.md`
  - [x] `rg -n "longest_field_len|Field\\.value|Field\\(\\{ name : Str, value" src test design.md`
  - [x] `rg -n "Decoder\\.dispatch|\\bDecoder\\b" src test design.md`
  - [x] `rg -n "dispatch\\(" src/build/roc/Builtin.roc src/check src/postcheck test`
  - [x] `rg -n "cache_control|content_length|x_auth_token" test/http-headers`
- [x] Inspect generated/optimized app binaries separately for string absence;
  source tests are allowed to contain original Roc field names.
- [x] Check that no compiler stage hardcodes the header state shape.
- [x] Check that no compiler stage mentions HTTP, JSON, camelCase, kebab-case,
  null, or missing-header rules except in tests or platform examples.
- [x] Check that generated parser code calls direct static methods, not callback
  tables.

## Targeted Test Runs

Run targeted tests before the full CI-sized command. Use `--summary failures`
where supported.

- [x] Before running tests, run focused source searches and clean up stale API
  names:
  - [x] `rg -n "parse_from|ParseFrom" src test examples design.md`
  - [x] `rg -n "longest_field_len|Field\\.value|Field\\(\\{ name : Str, value" src test`
  - [x] `rg -n "Decoder\\.dispatch|dispatch\\(" src/build/roc src/check src/postcheck test`
- [x] Check/build the compiler:
  - [x] `zig build roc --summary failures`
- [x] Run checking-focused tests:
  - [x] `zig build run-test-zig-module-check --summary failures`
  - [x] `zig build run-test-cli --summary failures`
- [x] Run post-check/lowering focused tests:
  - [x] `zig build run-test-zig-module-postcheck --summary failures`
  - [x] `zig build run-test-zig-module-compile --summary failures`
- [x] Run parser platform regressions:
  - [x] `zig build run-test-zig-http-header-decoder-platform --summary failures`
  - [x] `zig build run-test-zig-json-decoder-platform --summary failures`
- [x] Run combined targeted command used on this branch:
  - [x] `zig build run-test-zig-module-check run-test-zig-module-postcheck run-test-zig-http-header-decoder-platform run-test-zig-json-decoder-platform --summary failures`
- [x] Run source checks:
  - [x] `zig build run-check-zig-format --summary failures`
  - [x] `zig build run-check-tidy --summary failures`
  - [x] `zig build run-check-unused-suppression --summary failures`
  - [x] the repository compiler-stage wording audit build step
- [x] Run any newly added focused tests directly if the build exposes a narrower
  step.
  - [x] `zig-out/bin/roc check --no-cache test/cli/ParserRecursiveNominalMissingMethod.roc`
  - [x] `zig-out/bin/roc test --no-cache test/cli/EncodeToStructuralRecord.roc`

## Release Builds And Assembly Verification

- [x] Build the Roc compiler in release mode with symbols:
  - [x] `zig build roc -Doptimize=ReleaseFast -Dstrip=false --summary failures`
- [x] Build the HTTP header platform/app with:
  - [x] Zig host in `ReleaseFast`
  - [x] Roc app with `--opt=speed`
- [x] Save baseline and final disassembly under `/tmp` when comparing changes:
  - [x] compiler relevant symbols if parser lowering is being audited
  - [x] final platform app hot request handler
  - [x] final Roc parser code for header record parsing if symbol names are
    available
- [x] Use `xcrun llvm-objdump` on macOS:
  - [x] `xcrun llvm-objdump --macho --disassemble --demangle <binary> > /tmp/<name>.s`
- [x] Inspect the hot header parsing path for:
  - [x] no allocator calls
  - [x] no runtime field map construction
  - [x] no callback-table dispatch
  - [x] direct calls or inlined code for field value parsers
  - [x] no runtime lowercase/dash conversion for each header name
  - [x] caseless comparison over input header name slices and transformed static
    field names
  - [x] `U64` parser consumes the value and returns the continuation state
- [x] Scan optimized final app binary strings:
  - [x] original field names absent where the test expects compile-time renaming
  - [x] transformed field names may be present or optimized away, but behavior
    must prove transformed input names still match
- [x] Record the disassembly files and the key observations in the final
  implementation summary.

## Full Verification Loop

- [x] After targeted tests pass, run:
  - [x] `zig build minici`
- [x] If `zig build minici` fails in one section:
  - [x] identify the failing section
  - [x] run the narrow failing section directly
  - [x] fix the issue
  - [x] rerun that narrow section until it passes
  - [x] rerun `zig build minici`
  - [x] repeat this loop until `zig build minici` passes
- [x] Do not use the full `zig build minici` command as the inner retry loop.
- [x] After macOS verification, run target-specific checks needed for CI:
  - [x] Windows mirror sync through `/Users/rtfeldman/.agents/roc-windows.sh sync`
  - [x] Windows CLI tests if the changed code affects Windows:
    `/Users/rtfeldman/.agents/roc-windows.sh test-cli --sync`
    and `/Users/rtfeldman/.agents/roc-windows.sh run --arch x64 -- zig build run-test-cli`
    were both run. The default-arch full runs exposed unrelated VM/toolchain
    instability, and the explicit-x64 full run had one unrelated transient
    `roc fmt --check` access violation after printing success. Its exact repro
    passed immediately. The branch-specific `encode_to` x64 filter passed all
    four cases.
  - [x] Windows glue/platform tests if the changed code affects glue or platform
    ABI:
    `/Users/rtfeldman/.agents/roc-windows.sh test-glue --sync`
    passed all 9 glue tests.
  - [x] Any Linux/musl or cross-target steps exposed by the Roc build for the
    touched code paths
- [x] Do not build directly from the Parallels shared folder on Windows.

## Final Review Checklist

- [x] Re-read `design.md`, section `Structural Serialization Methods`, and
  confirm the implementation matches it.
- [x] Re-read this `plan.md` and check off every completed item.
- [x] Run `git diff --check`.
- [x] Run source searches for obsolete API names one final time.
- [x] Review the whole branch with code-review posture:
  - [x] correctness bugs
  - [x] missing checking errors
  - [x] hidden runtime allocation
  - [x] stale old API compatibility paths
  - [x] format-specific knowledge in compiler code
  - [x] test gaps
  - [x] release-build checks that should be checked-stage errors
- [x] Commit only after targeted verification is green for the implemented
  slice.
- [x] Push after the commit if requested or if continuing the existing branch
  workflow.
