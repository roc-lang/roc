# Generated Parser Hot Path Optimization Plan

This plan covers the next optimization pass for the structural `parser_for`
pipeline described in `design.md`, especially the `Structural Serialization
Methods` section. The public Roc API must not change. The work is entirely about
making the existing generated implementation lower to code that is closer to
hand-written C/Rust HTTP parsing code while preserving the format-agnostic,
pure, safe, allocation-free invariants.

The current optimized HTTP header example works correctly and does not allocate
at runtime, but disassembly shows that the generated Roc parser still has much
larger code and stack frames than picohttpparser and may/httparse:

- Roc `_roc__proc_e3`, the main generated parser/app proc, is about 247 KB of
  code and reserves about 4 KB of stack.
- pico `phr_parse_request` is about 720 bytes of code and reserves about 80
  bytes of stack.
- may/httparse's parser core is a few KB of code with roughly 112-160 byte
  parser frames, and the comparable may per-connection read/parse/respond loop
  is about 3.3 KB with about 1.5 KB of stack.

The goal is not to copy either library's public API. The goal is to keep Roc's
method-based structural parsing API and make the generated hot path much closer
to the code shape a hand-written C/Rust parser would produce.

## Global Invariants

- [ ] No public API changes.
  - Success criteria:
    - Existing platform examples keep using `Headers.parse`, `Json.parse`,
      `parser_for`, `Fields(_shape)`, `Field(_shape)`, and format methods as
      described in `design.md`.
    - No new builtin user-facing `Decoder`, `Parser`, `Encoding`, slot, unsafe,
      raw-byte-indexing, or field-index API appears.

- [ ] The compiler remains format-agnostic.
  - Success criteria:
    - Compiler code does not hardcode HTTP headers, JSON, `Missing`, `Null`,
      kebab-case, camelCase, content-length, `foo`, `bar`, or any benchmark
      field name.
    - All format-specific behavior remains in Roc platform/test format code or
      ordinary format methods.

- [ ] Runtime allocation remains forbidden on the HTTP header benchmark path.
  - Success criteria:
    - The Zig host still has no allocator.
    - The HTTP platform's `roc_alloc` still crashes.
    - End-to-end HTTP header tests still pass, proving no runtime Roc allocation
      is taken.
    - Optimized disassembly still has no reachable hot-path allocator call in
      the request parse/respond path.

- [ ] Seamless slices remain the representation for parsed input strings.
  - Success criteria:
    - Header path and header values remain borrowed slices into validated request
      memory.
    - JSON string paths that can borrow validated input still return slices.
    - No optimization introduces a copied or normalized field-name/value string
      on the hot parse path.

- [ ] Required/missing field behavior is unchanged.
  - Success criteria:
    - Required fields still produce the format's missing-record-field error when
      absent.
    - Optional `Try(value, [Missing])` fields still get `Err(missing)` using the
      format's `missing_optional_field`.
    - Existing tests for `Err(Missing)`, `Err(_)`, and `?` remain passing.

- [ ] Order independence is preserved.
  - Success criteria:
    - HTTP headers decode correctly regardless of input header order.
    - JSON object fields decode correctly regardless of object field order.
    - Duplicate-field behavior remains whatever the current tests/documentation
      specify.

## Phase 1: Establish The Baseline And Regression Harness

- [x] Save fresh baseline disassembly before making further optimizer changes.
  - Tasks:
    - Build the Roc binary through the existing ReleaseFast host and
      `roc build --opt=speed` path:
      `zig build run-test-zig-http-header-decoder-platform`.
    - Dump the Roc binary:
      `xcrun llvm-objdump --macho --disassemble --demangle zig-out/bin/http_header_decoder_server_prebuilt > /tmp/roc_http_header_before_next_hotpath_pass.s`.
    - Build/dump picohttpparser under `/tmp`.
    - Build/dump the comparable may_minihttp example under `/tmp`.
  - Success criteria:
    - Baseline files exist for Roc, pico, and may.
    - The report captures function code extents, stack reservations, total
      binary/object size, and relevant calls for all three.
    - Completed baseline report:
      `/tmp/roc-http-header-disasm-baseline-next-report.txt`.
    - Completed baseline disassemblies:
      `/tmp/roc-http-header-disasm-baseline-next/roc_http_header.s`,
      `/tmp/roc-http-header-disasm-baseline-next/picohttpparser_arm64_o3.s`,
      and `/tmp/roc-http-header-disasm-baseline-next/may_minihttp_roc_compare.s`.

- [x] Add automated or scripted extraction for the metrics we keep comparing.
  - Tasks:
    - Produce a small local script or documented command sequence that extracts:
      - function start symbols;
      - approximate function extents from adjacent symbols;
      - prologue stack reservations;
      - calls to allocator, string equality helpers, caseless equality helpers,
        crash helpers, and parser helper procs;
      - total `__TEXT` and on-disk size.
    - Keep the script in a test/support location if it is generally useful, or
      document the exact commands in this plan if it remains ad hoc.
  - Success criteria:
    - The same commands can be rerun after each phase.
    - The output is stable enough to compare before/after numbers without hand
      scanning tens of thousands of disassembly lines.
    - Completed script: `tools/http-header-disasm-report.sh`.

- [x] Add focused regression tests before changing lowering behavior.
  - Tasks:
    - Confirm existing HTTP and JSON platform tests cover:
      - required fields;
      - optional fields;
      - field order independence;
      - mixed field shapes such as `Str` and `U64`;
      - value parsers returning the continuation `rest`;
      - unknown field/header skipping.
    - Add missing tests before changing the implementation.
  - Success criteria:
    - Any broken parser state, `Try`, field-dispatch, or continuation behavior
      is caught by a targeted test before the final disassembly step.
    - Current focused coverage was confirmed with:
      `zig build run-test-zig-http-header-decoder-platform` and
      `zig build run-test-zig-json-decoder-platform`.
    - Existing HTTP coverage includes required fields, optional fields, field
      order independence, mixed `Str`/`U64` fields, value parsers returning
      continuation `rest`, unknown header skipping, duplicate known headers,
      malformed headers, UTF-8 validation, and max content-length rejection.
    - Existing JSON coverage includes required fields, optional fields, field
      order independence, nested records, nested tag unions, custom nominal
      parsing, top-level strings, duplicate fields, unknown field skipping,
      missing required fields, invalid UTF-8, and invalid JSON.

## Phase 2: Lower Generated Record Parser State To Presence Bits Plus Payload Locals

Today the generated parser loop no longer carries one aggregate
`record_state` tuple at the Monotype level, but the compiled output still
materializes and zeroes many stack-shaped slot values. The intended code shape
is a set of scalar locals:

- one presence bitset for the record;
- one payload local per field whose value was actually parsed;
- no fake zero/default value for missing required fields;
- no release-build read of a payload unless the corresponding presence bit is
  known to be set.

- [x] Identify the first IR stage where payload locals can be represented
      without sentinel/default values.
  - Tasks:
    - Inspect Monotype, lifted Monotype, Lambda Solved, Lambda Mono, and LIR
      value representations.
    - Confirm whether any stage has an explicit `undefined` or uninitialized
      local concept that satisfies the `design.md` rule against sentinel values.
    - If no such representation exists, add one in the correct IR instead of
      using crash/default placeholders.
  - Success criteria:
    - The chosen representation can express "field payload storage exists, but
      may not be read until its presence bit is set."
    - Debug builds can assert invalid reads.
    - Release builds do not emit runtime checks for compiler invariants.
    - Conclusion: this cannot be implemented as a Monotype-only rewrite without
      reintroducing fake values, because loop/join parameters require every
      carried value to have an initial value. The right boundary is LIR with
      ARC/certifier support for conditionally initialized parser payload cells:
      presence bits are ordinary initialized locals, while payload cells are
      assigned only on `Field` hits and may be read only in branches dominated
      by the corresponding presence bit.
    - A post-ARC backend-only rewrite is not acceptable. `Arc.insert` runs the
      debug ARC certifier immediately, and all backends must keep following
      explicit LIR RC rather than inferring parser slot semantics.

- [ ] Replace generated slot tag values with presence bits in the lowered parser
      state.
  - Tasks:
    - Lower each record parser's state to:
      - `presence_bits` or a small fixed set of machine-word bitsets;
      - scalar payload locals for each parsed field;
      - cursor state local.
    - Use one bit per record field.
    - Keep the phantom `Field(_shape)` guarantee: no field-handle bounds checks
      are introduced in release builds.
    - For records wider than one machine word, use multiple explicit words
      rather than a heap allocation.
  - Success criteria:
    - The generated parser no longer initializes every field slot to `Missing`
      at loop entry.
    - Disassembly for the HTTP example no longer has broad `q0` zeroing of fake
      slot/tag aggregates at the start of `_roc__proc_e3`.
    - The generated parser updates one presence bit and one payload local on a
      matched field.

- [ ] Lower required-field finishing to presence-bit checks.
  - Tasks:
    - At `Done`, test presence bits for required fields.
    - For absent required fields, call the format's `missing_record_field`.
    - For present required fields, read the corresponding payload local.
    - Do not create or inspect a `Missing/Present` tag for required fields.
  - Success criteria:
    - Required missing behavior is unchanged.
    - Required present fields read only initialized payload locals.
    - Disassembly shows bit tests and direct payload loads instead of tag
      construction/destruction for required fields.

- [ ] Lower optional-field finishing without fake values.
  - Tasks:
    - For optional fields, test the presence bit.
    - If present, construct `Ok(payload)`.
    - If absent, call `missing_optional_field` and construct `Err(missing)`.
    - Do not initialize optional payloads on loop entry.
  - Success criteria:
    - Existing optional-field tests pass.
    - Disassembly shows absent optional fields are handled at finish time, not by
      prebuilt `Missing` slot values.

- [ ] Keep ARC correct for scalarized field payloads.
  - Tasks:
    - Ensure ARC insertion sees explicit ownership for payload locals that may be
      initialized conditionally.
    - Ensure decrefs are emitted only for initialized payloads on error paths,
      early returns, and duplicate-field overwrites.
    - Ensure backends do not infer RC behavior; they only lower explicit LIR RC
      statements.
  - Success criteria:
    - ARC certifier passes.
    - Tests with repeated/overwritten string fields do not leak or double free.
    - LIR has explicit conditional cleanup based on presence bits where needed.

- [ ] Add regression coverage for wide and nested records.
  - Tasks:
    - Add a record with more fields than fit in one simple small bitset if the
      implementation supports multi-word bitsets now.
    - Add nested records and tag unions to make sure independent presence bitsets
      do not collide.
  - Success criteria:
    - Nested parser tests pass.
    - Disassembly does not reintroduce aggregate slot tuples for nested records.

## Phase 3: Lower Generated Parser `Try` Sequencing To Direct Control Flow Through LIR

The current Monotype expression shape can avoid explicit helper-return records,
but generated parser code still flows through many stack-shaped `Try` and
`{ value, rest }` temporaries. The intended code shape is direct:

- call format method;
- branch on Ok/Err;
- on Ok, update cursor/state scalar locals and jump/continue;
- on Err, return the error immediately;
- avoid materializing aggregate `Try({ value, rest }, err)` values when the
  producer and consumer are both compiler-generated parser code.

- [ ] Identify all compiler-generated parser `Try` producer/consumer pairs.
  - Tasks:
    - Inventory generated calls to:
      - `parse_record_field`;
      - `skip_record_field`;
      - field value parsers;
      - `missing_record_field`;
      - `missing_optional_field`;
      - tag-union parser operations.
    - Mark which are public-format calls whose `Try` value must remain an
      ordinary Roc value, and which are internal generated immediately-consumed
      control-flow edges.
  - Success criteria:
    - No user-authored `Try` semantics are changed.
    - Only generated internal parser edges are eligible for direct-control-flow
      lowering.

- [ ] Add an explicit direct parser control-flow lowering path.
  - Tasks:
    - Lower internal generated parser `Try` sequences into branches in the IR
      stage that feeds LIR.
    - Represent success continuations as block/loop jumps with scalar locals.
    - Represent error continuations as direct returns of the format error.
    - Preserve ordinary `Try` values for code that observes them as Roc data.
  - Success criteria:
    - LIR for the generated HTTP parser has direct conditional branches instead
      of stack allocation for immediately-consumed `Try` records.
    - No user-visible Try behavior changes in non-parser tests.

- [ ] Remove stack-shaped `{ value, rest }` temporaries where the value is
      immediately destructured by generated code.
  - Tasks:
    - Field value parsers may still publicly return
      `Try({ value, rest }, err)`, but generated call sites should lower the
      returned fields into direct locals when possible.
    - Avoid building a temporary record only to access `.value` and `.rest`.
  - Success criteria:
    - Disassembly shows fewer stack stores/loads around field parser calls.
    - Function frame size for `_roc__proc_e3` decreases.

- [ ] Keep direct-control-flow lowering format-agnostic.
  - Tasks:
    - Trigger the optimization from generated parser provenance and IR shape, not
      from HTTP/JSON-specific names.
    - Do not detect source strings or format module paths.
  - Success criteria:
    - The same lowering applies to HTTP, JSON, and future structural formats.

## Phase 4: Emit Direct Static Field Dispatch For Known `Fields`

The generated parser already has compile-time knowledge of transformed
`Fields(_shape)`. The hot path should not walk a generic iterator or call a
generic `Str` helper for every candidate when the concrete field set is known.
For small SSO field names, generated code should compare immediate packed words
and dispatch directly to the field parser.

- [ ] Add a compiler-owned field-dispatch plan for transformed `Fields`.
  - Tasks:
    - Use transformed field names only, after `Fields.rename_fields`.
    - Preserve field handles through the phantom `_shape` type.
    - Store enough compile-time metadata for:
      - field length;
      - packed SSO words;
      - selected discriminating word lane;
      - exact/caseless matching mode;
      - field result position.
  - Success criteria:
    - Runtime parser code does not reconstruct a field plan.
    - Original unrenamed field names do not appear in optimized runtime data
      when parser construction happened at compile time.

- [ ] Generate an SSO exact-match dispatcher.
  - Tasks:
    - Group fields by SSO size class as described in `design.md`.
    - Choose a discriminating word lane for each group.
    - Compare one word per candidate on the hot miss path.
    - Verify the full SSO key only after a discriminator hit.
    - Dispatch directly to the matched field parser.
  - Success criteria:
    - For static small field names, disassembly has immediate word compares, not
      a call to `roc_builtins_str_equal`.
    - Hot miss path compares one selected word per candidate in the relevant
      size class.
    - Long fields still work via a correct allocation-free fallback.
    - Intermediate progress: static exact `TryField` dispatch now uses explicit
      precomputed renamed field bytes when parser construction happened at
      compile time. Lowering emits the internal `str_is_eq_static_small`
      operation for transformed field names up to 24 bytes, and LLVM lowers that
      op to direct guarded byte comparisons rather than a call to
      `roc_builtins_str_equal`. Runtime-created parsers and long transformed
      names still use the ordinary `Str.is_eq` fallback. Verified with
      `zig build run-test-zig-http-header-decoder-platform`,
      `zig build run-test-zig-json-decoder-platform`, and disassembly dumps
      `/tmp/roc_http_after_static_exact.s` and
      `/tmp/roc_json_camel_direct_after_static_exact.s`, which contain no calls
      to `roc_builtins_str_equal`,
      `roc_builtins_str_equal_static_small`, or
      `roc_builtins_str_caseless_ascii_equals`. This does not yet complete the
      word-lane bucketed dispatcher described above.

- [ ] Generate an SSO ASCII-caseless dispatcher.
  - Tasks:
    - Reuse the SWAR ASCII case-folding algorithm from
      `Str.is_caseless_eq`/`roc_builtins_str_caseless_ascii_equals`.
    - Inline the fast path for static transformed field names when the field set
      is known.
    - Avoid allocating lowercased copies.
    - Correctly fall back for non-ASCII or SWAR-ineligible bytes.
  - Success criteria:
    - HTTP `TryFieldCaseless` or direct `Field` matching does not call the
      generic caseless helper on the hot static-field path.
    - Edge-case tests for mixed case, underscore, dash, non-ASCII, and different
      lengths still pass.

- [ ] Keep `Fields.iter` and `Fields.for_size` correct for userspace.
  - Tasks:
    - Users may still iterate field sets at runtime.
    - Runtime iteration must remain allocation-free.
    - The optimized generated dispatcher must not require changing the public
      `Fields` representation or behavior.
  - Success criteria:
    - Existing userspace format code using `Fields.for_size` keeps working.
    - Runtime-created parsers still work even if parser construction was not
      evaluated at compile time; they may be slower, but must be correct.

- [ ] Handle static and runtime parser-construction paths with one semantics.
  - Tasks:
    - Avoid a different public compilation model depending on whether
      `parser_for` ran at compile time.
    - Let compile-time field values optimize down to immediates.
    - Let runtime field values use the same ordinary Roc value semantics with an
      expected performance cost.
  - Success criteria:
    - Tests cover both compile-time-created parsers and runtime-created parsers.
    - Both paths produce the same parse results.
    - Only the compile-time path is expected to have immediate static field
      compares in disassembly.

## Phase 5: Defer Full Roc `Str` Construction For Borrowed Header Values

Header parsing currently hands Roc `Str` values around as slices. That is
correct, but the generated parser may still build full Roc `Str` values earlier
than necessary. The hot path should internally carry pointer/length or an
equivalent borrowed slice descriptor until the final record field requires a
real Roc `Str` value.

- [ ] Identify where borrowed field names and values become full Roc `Str`
      values.
  - Tasks:
    - Trace host header slice construction.
    - Trace `Headers.roc` record-field parsing.
    - Trace Monotype and LIR lowering for `Str` value parser results.
  - Success criteria:
    - The plan for deferring `Str` construction is based on explicit IR data, not
      inferred source names or backend guessing.

- [ ] Introduce an internal borrowed-string parse value where appropriate.
  - Tasks:
    - Represent borrowed input as pointer/length plus lifetime/ownership facts
      already guaranteed by host validation and request lifetime.
    - Keep this internal to generated parser lowering; do not expose unsafe
      pointer/length values to userspace.
    - Convert to an ordinary Roc `Str` only when a user-visible Roc value is
      produced.
  - Success criteria:
    - Userspace still sees ordinary `Str`.
    - No allocations are introduced.
    - Host lifetime and UTF-8 validation invariants remain explicit.

- [ ] Use deferred string construction for field names where generated dispatch
      can consume raw slice metadata.
  - Tasks:
    - Exact/caseless generated dispatch should be able to compare against
      pointer/length bytes directly.
    - It should not need to construct a Roc `Str` for an unknown field name just
      to skip it.
  - Success criteria:
    - Unknown header skip path has no Roc `Str` construction.
    - Matched field dispatch for static fields has direct byte/word compares.
    - Intermediate progress: `test/http-headers/platform/Headers.roc` now uses
      `Fields.for_size` and returns direct `Field` events for known headers.
      Unknown headers return `Continue` after the format consumes the line, so
      they no longer enter generated record string-dispatch or
      `skip_record_field`. The format still represents the parsed header name
      as a safe Roc `Str` slice; the lower-level borrowed pointer/length
      representation remains open.

- [ ] Use deferred string construction for `Str` field values.
  - Tasks:
    - When the target field type is `Str`, carry the borrowed slice through the
      parser result and build the Roc `Str` at final record construction or the
      latest equivalent point.
    - When the target field type is not `Str`, call that field's parser from the
      value-start state and avoid constructing a temporary `Str` first.
  - Success criteria:
    - Mixed-shape record tests still pass.
    - The `content_length : U64` style path does not construct a temporary value
      string if the U64 parser can consume the state directly.

## Phase 6: Outline Cold Decode-Error And Crash Paths

The current generated parser proc includes large amounts of cold error,
missing-field, decref, and crash-path code. The hot path should be straight-line
or locally-branching parser code. Cold paths should be outlined so they do not
inflate the main parser frame and instruction footprint.

- [ ] Inventory cold paths inside the generated parser proc.
  - Tasks:
    - Identify calls/branches to:
      - `roc_builtins_roc_crashed`;
      - missing required field errors;
      - bad header/json errors;
      - ARC cleanup after errors;
      - invariant traps;
      - allocation failure paths from non-hot support code.
  - Success criteria:
    - Every major cold region in `_roc__proc_e3` is categorized.

- [ ] Add cold outlining for generated parser error paths.
  - Tasks:
    - Move rarely-taken error construction and crash code into separate generated
      cold procs or cold blocks that LLVM can place out of line.
    - Keep hot branches short: test, branch to cold label/proc, continue.
    - Preserve correct ARC cleanup on the cold path.
  - Success criteria:
    - `_roc__proc_e3` code size decreases materially.
    - Hot-path disassembly has fewer interleaved crash/error blocks.
    - Correctness tests for errors still pass.

- [ ] Mark or structure cold compiler-generated paths in a backend-appropriate
      way.
  - Tasks:
    - Prefer explicit IR/procedure structure over backend heuristics.
    - If LLVM cold attributes are used, they must be emitted from explicit
      compiler provenance, not guessed from source names.
  - Success criteria:
    - The implementation follows `design.md`: later stages consume explicit data
      from earlier stages and do not guess.

## Phase 7: Revisit The Host's 16 KiB Stack Buffer

The host currently keeps the no-allocation invariant by using a large stack
buffer. That keeps behavior simple, but it makes the host frame much larger than
the C/Rust comparison. We should keep "no allocator" while reducing stack
pressure.

- [x] Decide the correct no-allocation storage model for request bytes.
  - Options to evaluate:
    - fixed static storage if the test server is single-connection/single-thread;
    - per-connection fixed arena embedded in a connection object;
    - smaller stack buffer plus a strict maximum header/content length;
    - compile-time configured fixed buffer owned by the host server state.
  - Success criteria:
    - No Zig allocator is introduced.
    - The host still rejects content/header lengths over the configured maximum
      before accepting the bytes.
    - The memory lifetime still safely covers the Roc request parse call.
    - Implemented as a smaller fixed stack buffer in
      `test/http-headers/platform/host.zig`: 2 KiB request storage with a 1 KiB
      maximum content length. The host rejects `Content-Length: 1025` before
      reading a body.

- [x] Preserve correctness of UTF-8 validation and request slicing.
  - Tasks:
    - Validate path and full header section before constructing Roc `Str` values.
    - Keep body/content-length handling unchanged except for maximum length
      enforcement if needed.
  - Success criteria:
    - Invalid UTF-8 tests still fail correctly.
    - Valid request tests still pass.
    - Roc never receives invalid `Str`.
    - Verified with `zig build run-test-zig-http-header-decoder-platform`.

- [x] Reduce the host frame in optimized disassembly.
  - Success criteria:
    - `_main`/`_host.cMain` no longer reserves roughly 16 KiB of stack for the
      request buffer.
    - The new storage is visible in disassembly or symbols as static/fixed
      storage or a smaller frame.
    - Verified in
      `/tmp/roc-http-header-disasm-after-direct-field-report.txt`: `_main`
      total stack dropped from 16,480 bytes in the baseline report to 2,384
      bytes after reducing the fixed host request buffer.

## Phase 8: Optimize `Str.is_eq` / `roc_builtins_str_equal`

`Str.is_caseless_eq` has a SWAR-style fast path for ASCII-caseless comparison.
Plain `Str.is_eq` should get equivalent attention, especially for SSO strings
and static field comparisons. Exact equality is simpler than caseless equality:
length mismatch returns false, and equal small strings can be checked with
fixed-width word compares because unused SSO bytes are zeroed.

- [x] Inventory all string equality implementations.
  - Tasks:
    - Inspect:
      - `roc_builtins_str_equal`;
      - interpreter low-level `.str_is_eq`;
      - LLVM lowering for `.str_is_eq`;
      - Wasm `str_is_eq`;
      - list string equality if it reuses string equality;
      - any dev backend direct equality path.
  - Success criteria:
    - Every backend/runtime path for `Str.is_eq` is listed with its current
      behavior.
    - Current paths:
      - `roc_builtins_str_equal` in `src/builtins/dev_wrappers.zig` forwards to
        `strEqual`, which forwards to `RocStr.eql` in `src/builtins/str.zig`;
      - interpreter low-level `.str_is_eq` calls `builtins.str.strEqual`;
      - LLVM lowering emits a call to `roc_builtins_str_equal`;
      - dev backend calls the same wrapper through `wrapStrEqual`;
      - Wasm imports `roc_builtins_str_equal`.

- [x] Add a fast SSO equality path.
  - Tasks:
    - Check length first.
    - If both strings are SSO:
      - compare the inline words directly;
      - use one word on 32-bit SSO capacity and up to the required words on
        64-bit SSO capacity;
      - rely on zeroed unused SSO bytes, not tail masking.
    - If one string is SSO and the other is heap/seamless:
      - compare length;
      - compare the SSO inline bytes against the other pointer bytes with
        fixed-width safe loads only when proven safe, otherwise use bounded
        loads/memcmp.
    - If both are heap/seamless:
      - use length check plus efficient word/memcmp comparison.
  - Success criteria:
    - SSO vs SSO equality compiles to direct word compares in optimized builds.
    - No allocation or normalization occurs.
    - No out-of-bounds load is introduced for seamless slices.
    - `RocStr.eql` already routes SSO-vs-SSO through inline bytes. This pass
      changed the exact equality helper to use fixed u64 word comparisons and a
      safe final-u64 tail strategy, including safe 32-bit SSO tail handling and
      no overread for short seamless slices.

- [x] Share safe low-level primitives with caseless equality where appropriate.
  - Tasks:
    - Reuse helpers for:
      - SSO detection;
      - length extraction;
      - inline-byte pointer extraction;
      - safe word comparison boundaries.
    - Do not make caseless equality slower.
  - Success criteria:
    - Existing caseless equality disassembly remains at least as good as before.
    - Plain equality gets the same small-string attention without duplicate
      fragile code.
    - Plain equality now uses the same `readTailU64`/bounded tail strategy used
      by caseless equality for non-small tails, while keeping the caseless SWAR
      path unchanged.

- [x] Add edge-case tests for `Str.is_eq`.
  - Tasks:
    - Equal and unequal SSO strings of lengths 0 through SSO max.
    - Equal and unequal heap strings.
    - SSO vs heap/seamless equal and unequal strings.
    - Strings with embedded NUL bytes.
    - Unicode strings where byte equality is the expected semantic.
    - Different lengths with shared prefixes.
    - Boundary lengths around SSO max and SSO max plus one.
  - Success criteria:
    - Tests pass across dev/LLVM/Wasm paths that support string equality.
    - Optimized disassembly confirms SSO equality no longer calls a generic
      heavy helper when both operands are statically known SSO-compatible.
    - Added builtin tests for exact u64 SSO chunks, all small-string lengths,
      masked SSO tails, embedded NUL bytes, exact UTF-8 byte equality, the
      small/heap boundary, and short seamless slices at allocation end.
      Verified so far with `zig build run-test-zig-module-builtins`.
    - These tests caught and fixed an existing small-string equality bug: the
      old pointer/length fast return could treat two 9-byte small strings as
      equal when their first 8 bytes and length byte matched but byte 8 differed.
      The fast return now applies only to non-small strings.
    - Added `test/cli/StrIsEqEdgeCases.roc` to cover the user-visible
      `Str.is_eq` path for the same SSO lane boundaries and Unicode byte
      equality. Verified directly with
      `zig-out/bin/roc test --no-cache test/cli/StrIsEqEdgeCases.roc` and
      through the CLI runner with
      `zig build run-test-cli -- --suite subcommands --filter "Str.is_eq edge"`.
    - Verified optimized native code shape by running
      `zig build run-test-zig-module-builtins -Doptimize=ReleaseFast` and
      dumping the resulting builtin test binary to
      `/tmp/roc-builtins-releasefast-str-eq.s`. The `_str.RocStr.eql`
      disassembly shows direct 8-byte loads and compares for the SSO path, a
      masked tail path for partial lanes, and byte-at-a-time handling only for
      the bounded short-tail/seamless cases where fixed overreads are not safe.

- [ ] Use exact string fast paths in generated field dispatch when possible.
  - Tasks:
    - For static small field names, prefer generated direct compares over calling
      `Str.is_eq`.
    - Keep optimized `Str.is_eq` as the general fallback and for ordinary user
      code.
  - Success criteria:
    - Field-dispatch disassembly uses immediates/direct word compares.
    - Ordinary `Str.is_eq` also benefits outside generated parser code.
    - Intermediate progress: exact static field dispatch no longer calls
      generic `Str.is_eq` or a static equality helper in the optimized HTTP and
      JSON parser binaries. The current LLVM lowering emits guarded byte
      comparisons; the remaining work is to generate the word-lane SSO
      dispatcher and discriminating-lane ordering from Phase 4.

## Phase 9: Remove Remaining Generic Parser State Traffic

After the core phases above, inspect the generated HTTP parser again and remove
whatever generic parser machinery remains in the hot path without changing the
Roc API.

- [ ] Inspect hot loop register and stack traffic.
  - Tasks:
    - Identify stores/loads that exist only because generated parser state is
      represented too generically.
    - Identify any remaining aggregate copies of field sets, parser closures,
      parse results, or states.
  - Success criteria:
    - Every large stack object in `_roc__proc_e3` has a known reason.
    - Unnecessary stack objects have follow-up patches or are removed.

- [ ] Keep nested parser construction eager and runtime parser loops small.
  - Tasks:
    - Verify derived `parser_for` still constructs nested parsers before
      returning the runtime lambda.
    - Ensure runtime lambda closes over transformed static field data and nested
      parser functions, not original names or construction machinery.
  - Success criteria:
    - Compile-time parser construction tests still verify original field names
      are not in the final binary when renamed at compile time.
    - Runtime parser construction tests still pass.

- [ ] Avoid introducing backend-specific semantic logic.
  - Tasks:
    - LIR must contain explicit operations for presence bits, control flow,
      field dispatch, and ARC cleanup.
    - Backends lower the explicit LIR and do not infer parser semantics.
  - Success criteria:
    - Dev, LLVM, and Wasm backends either lower explicit operations correctly or
      have targeted unsupported-test coverage if a backend does not yet support
      this platform example.

## Phase 10: Verification Before Final Disassembly

- [ ] Run focused parser/platform tests after each phase.
  - Required commands:
    - `zig build run-test-zig-http-header-decoder-platform`
    - `zig build run-test-zig-json-decoder-platform`
  - Success criteria:
    - Both pass after every behavior-affecting phase.
    - Passed after the host-buffer and `Str.is_eq` changes:
      `zig build run-test-zig-http-header-decoder-platform` and
      `zig build run-test-zig-json-decoder-platform`.
    - Passed after the static exact-dispatch operation and lowering changes:
      `zig build run-test-zig-http-header-decoder-platform` and
      `zig build run-test-zig-json-decoder-platform`.

- [x] Run focused string equality tests after the `Str.is_eq` phase.
  - Tasks:
    - Use the most targeted builtin/interpreter/backend test steps available.
    - Add missing targeted steps if current coverage is too broad or too weak.
  - Success criteria:
    - SSO equality, heap equality, seamless-slice equality, and Unicode byte
      equality are all covered.
    - Verified with `zig build run-test-zig-module-builtins`,
      `zig-out/bin/roc test --no-cache test/cli/StrIsEqEdgeCases.roc`, and
      `zig build run-test-cli -- --suite subcommands --filter "Str.is_eq edge"`.

- [ ] Run broader compiler checks after targeted tests pass.
  - Required command:
    - `zig build minici`
  - Failure protocol:
    - If `minici` fails in one section, switch to that targeted failing section.
    - Fix the targeted failure.
    - Rerun the targeted failing section until it passes.
    - Return to `zig build minici` only after the targeted section passes.
  - Success criteria:
    - `zig build minici` passes.

- [ ] Check optimized allocation invariants.
  - Tasks:
    - Rebuild the HTTP header platform with ReleaseFast host and `--opt=speed`
      Roc app.
    - Confirm `roc_alloc` is still a crashing implementation in the test host.
    - Confirm the HTTP tests send all required request combinations and receive
      correct responses.
  - Success criteria:
    - No runtime allocation is possible on the tested path without failing the
      test.

- [ ] Check generated code shape before final report.
  - Tasks:
    - Search the optimized Roc disassembly for:
      - allocator calls on the hot path;
      - calls to generic `roc_builtins_str_equal` from static field dispatch;
      - calls to generic `roc_builtins_str_caseless_ascii_equals` from static
        field dispatch;
      - broad stack zeroing at parser entry;
      - large cold crash/error blocks interleaved into the hot parser proc.
  - Success criteria:
    - Any remaining instance is either removed or documented with the exact
      reason it is still necessary.
    - Current exact static-dispatch evidence: `/tmp/roc_http_after_static_exact.s`
      and `/tmp/roc_json_camel_direct_after_static_exact.s` contain no calls to
      `roc_builtins_str_equal`,
      `roc_builtins_str_equal_static_small`, or
      `roc_builtins_str_caseless_ascii_equals`. Broad parser stack zeroing and
      large parser frames remain visible, so the presence-bit/LIR scalarization
      and cold-path outlining phases remain open.

## Completion Checklist

The plan is not complete until every item below is true:

- [ ] Generated record parser state is represented as presence bits plus payload
      locals through the lowering path that reaches LIR.
- [ ] Missing required fields do not require initializing fake values.
- [ ] Optional absent fields are constructed at finish time from format
      `missing_optional_field`, not from preinitialized slot tags.
- [ ] Generated parser `Try` sequencing lowers to direct control flow through
      LIR where producer and consumer are compiler-generated parser code.
- [ ] Immediately-consumed `{ value, rest }` parser result records are avoided
      where direct locals are semantically equivalent.
- [ ] Static known `Fields` dispatch emits direct exact small-string compares.
- [ ] Static known `Fields` dispatch emits direct ASCII-caseless SWAR compares
      for eligible small strings.
- [ ] Generic iterator/string-helper dispatch is not used on the static SSO hot
      path.
- [ ] Runtime-created parsers still work correctly, even if they are slower than
      compile-time-created parsers.
- [ ] Full Roc `Str` construction for borrowed header field names/values is
      delayed until a user-visible Roc `Str` is actually needed.
- [ ] Unknown header skip paths do not construct temporary Roc `Str` values.
- [ ] Cold decode-error, missing-field, ARC-cleanup, and crash paths are outlined
      out of the main generated parser hot proc where practical.
- [x] The host no longer reserves roughly 16 KiB of stack for the request buffer,
      while still using no allocator and preserving request-memory lifetime.
- [x] `Str.is_eq` has a fast SSO path comparable in spirit to
      `Str.is_caseless_eq`, with tests and disassembly confirmation.
- [ ] HTTP header platform tests pass.
- [ ] JSON platform tests pass.
- [ ] String equality tests pass across the relevant execution paths.
- [ ] `zig build minici` passes after targeted failures, if any, have been fixed
      through the targeted-failure loop.
- [ ] Optimized HTTP header disassembly has no hot-path allocation, no hot-path
      runtime field map, and no per-header runtime field-name conversion.

## Final Step: Redo Disassembly And Compare Roc To pico And may

This must be the last step after every checklist item above is complete.

- [ ] Rebuild all comparison artifacts.
  - Roc:
    - `zig build run-test-zig-http-header-decoder-platform`
    - dump `zig-out/bin/http_header_decoder_server_prebuilt`
  - pico:
    - build `h2o/picohttpparser` with optimized arm64 macOS settings equivalent
      to the prior comparison.
    - dump the optimized object.
  - may:
    - build `Xudong-Huang/may_minihttp` release mode with the comparable
      `roc_compare` example.
    - dump the optimized binary.

- [ ] Produce a full side-by-side report.
  - Required Roc numbers:
    - on-disk binary size;
    - `__TEXT` size;
    - disassembly line count;
    - hot function names;
    - hot function code byte extents;
    - hot function stack reservations;
    - calls to allocator/crash/string-helper/parser-helper functions;
    - presence or absence of broad stack zeroing;
    - whether field dispatch is direct immediate/SWAR code.
  - Required pico numbers:
    - object size;
    - text size;
    - disassembly line count;
    - `phr_parse_request` code size and stack;
    - internal `parse_headers` code size and stack;
    - relevant helper code sizes.
  - Required may numbers:
    - binary size;
    - `__TEXT` size;
    - disassembly line count;
    - `httparse` request/header parser code sizes and stacks;
    - response encoder size/stack;
    - comparable per-connection read/parse/respond loop size/stack.

- [ ] Analyze remaining gaps without proposing Roc API changes.
  - Success criteria:
    - The report explains which differences are due to unavoidable Roc semantics,
      which are due to remaining compiler implementation overhead, and which are
      likely removable with further lowering/backend work.
    - Any new follow-up item is grounded in disassembly evidence from the final
      comparison.
