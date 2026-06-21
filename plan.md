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

- [x] No public API changes.
  - Success criteria:
    - Existing platform examples keep using `Headers.parse`, `Json.parse`,
      `parser_for`, `Fields(_shape)`, `Field(_shape)`, and format methods as
      described in `design.md`.
    - No new builtin user-facing `Decoder`, `Parser`, `Encoding`, slot, unsafe,
      raw-byte-indexing, or field-index API appears.
    - Verified by source review of the changed public platform examples and
      builtin surface: the changes add internal generated lowering/LIR/backend
      machinery only. The visible format API remains the method-based
      `parser_for`/`parse_record_field`/`parse_str`/`parse_u64` shape.

- [x] The compiler remains format-agnostic.
  - Success criteria:
    - Compiler code does not hardcode HTTP headers, JSON, `Missing`, `Null`,
      kebab-case, camelCase, content-length, `foo`, `bar`, or any benchmark
      field name.
    - All format-specific behavior remains in Roc platform/test format code or
      ordinary format methods.
    - Verified by source review of compiler changes: generated parser lowering
      keys off structural types, explicit method names from the format API,
      generated `Fields(_shape)` evidence, and tag labels from the generic
      parse-event type. HTTP/JSON behavior remains in
      `test/http-headers/platform/Headers.roc` and
      `test/json-decoder/platform/Json.roc`.

- [x] Runtime allocation remains forbidden on the HTTP header benchmark path.
  - Success criteria:
    - The Zig host still has no allocator.
    - The HTTP platform's `roc_alloc` still crashes.
    - End-to-end HTTP header tests still pass, proving no runtime Roc allocation
      is taken.
    - Optimized disassembly still has no reachable hot-path allocator call in
      the request parse/respond path.
    - Verified host-side invariants in `test/http-headers/platform/host.zig`:
      `roc_alloc`, `roc_realloc`, and `roc_dealloc` all print a diagnostic and
      abort; request storage is a fixed 2 KiB stack buffer; content length is
      rejected above 1 KiB or above the remaining buffer capacity; and the path
      plus full header section are UTF-8 validated before constructing Roc
      `Str` values.
    - Verified test-side invariants in
      `src/cli/test/http_header_decoder_platform_test.zig`: the end-to-end test
      exercises successful request combinations, invalid UTF-8, malformed
      content length, and `Content-Length: 1025` rejection, and every success or
      expected-failure server run asserts stderr contains none of
      `roc_alloc called`, `roc_realloc called`, or `roc_dealloc called`.
    - Verified by rerunning
      `zig build run-test-zig-http-header-decoder-platform` after the current
      static exact/caseless dispatch work.

- [x] Seamless slices remain the representation for parsed input strings.
  - Success criteria:
    - Header path and header values remain borrowed slices into validated request
      memory.
    - JSON string paths that can borrow validated input still return slices.
    - No optimization introduces a copied or normalized field-name/value string
      on the hot parse path.
    - Verified by the host/platform path: the HTTP host validates the fixed
      request buffer before constructing Roc strings, `Headers.roc` uses
      `find_first`/`trim` slice operations, and the platform allocator hooks
      abort if any heap allocation is reached. JSON platform tests keep the same
      allocation-aborting invariant for borrowed input parsing.

- [x] Required/missing field behavior is unchanged.
  - Success criteria:
    - Required fields still produce the format's missing-record-field error when
      absent.
    - Optional `Try(value, [Missing])` fields still get `Err(missing)` using the
      format's `missing_optional_field`.
    - Existing tests for `Err(Missing)`, `Err(_)`, and `?` remain passing.
    - Verified by the HTTP optional-header matrix, HTTP missing-required case,
      JSON optional-field matrix, and JSON missing-required case in the platform
      regression tests.

- [x] Order independence is preserved.
  - Success criteria:
    - HTTP headers decode correctly regardless of input header order.
    - JSON object fields decode correctly regardless of object field order.
    - Duplicate-field behavior remains whatever the current tests/documentation
      specify.
    - Verified by HTTP `record_order`, `reverse_order`, and `scrambled_order`
      requests in `src/cli/test/http_header_decoder_platform_test.zig`.
    - Verified by JSON `buildRecordOrderJson`, `buildReverseOrderJson`, and
      `buildReorderedJson` cases in
      `src/cli/test/json_decoder_platform_test.zig`, including nested object
      field reordering.
    - Duplicate-field behavior remains last-write-wins for the tested known
      fields, covered by the HTTP and JSON duplicate-known cases.

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
      explicit LIR RC rather than inferring parser slot ownership and initialization data.
    - Implemented the first explicit LIR primitive for this representation:
      `switch_initialized_payload`, which branches on an ordinary presence
      condition and carries the compiler-known relationship to the payload local
      whose initialization is being tested. ARC, ARC solving, ARC certification,
      TRMC, scalarized joins, debug printing, the interpreter, LLVM, Wasm, and
      the dev backend all traverse or lower this node explicitly.
    - Added certifier regressions proving an initialized RC payload certifies
      only the initialized branch, an uninitialized RC payload certifies only
      the uninitialized branch, and a read of an uninitialized payload is still
      rejected.
    - Verified the infrastructure with
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-backend`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`.

- [x] Replace generated slot tag values with presence bits in the lowered parser
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
    - Implemented in `src/postcheck/monotype/lower.zig`: generated record
      parser loops carry cursor state, one or more `U64` presence words, and one
      payload local per field. Initial payload values are compiler-only
      `uninitialized_payload` markers tied to their presence word/mask, not
      user-visible `Missing` tags or fake default payloads.
    - Payload loop params are carried before presence loop params so ARC cleanup
      of an overwritten old payload tests the old presence bit before the new
      presence bit is written. This ordering is documented in lowering and
      covered by an ARC regression.
    - Verified with `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`.

- [x] Lower required-field finishing to presence-bit checks.
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
    - Implemented by `parseRecordFieldFromPresencePayload`: finish-time field
      reads are behind the generated initialized-payload switch. Required
      absent fields call the format's `missing_record_field` and return the
      format error; present fields read the payload local only on the
      initialized branch.
    - Verified by the HTTP and JSON platform tests, including missing required
      header/object-field cases.

- [x] Lower optional-field finishing without fake values.
  - Tasks:
    - For optional fields, test the presence bit.
    - If present, construct `Ok(payload)`.
    - If absent, call `missing_optional_field` and construct `Err(missing)`.
    - Do not initialize optional payloads on loop entry.
  - Success criteria:
    - Existing optional-field tests pass.
    - Disassembly shows absent optional fields are handled at finish time, not by
      prebuilt `Missing` slot values.
    - Implemented by the same finish-time presence switch: present optional
      fields construct `Ok(payload)`, absent optional fields call
      `missing_optional_field` and construct the field's `Err(missing)` at
      finish time.
    - Verified by the HTTP optional-header matrix and JSON optional-field
      platform coverage.

- [x] Keep ARC correct for scalarized field payloads.
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
    - Intermediate progress: LIR and ARC now have an explicit conditional
      initialized-payload switch, and the certifier has direct branch-selection
      coverage for initialized and uninitialized RC payload states. This item is
      not complete until generated parser lowering emits that LIR for duplicate
      overwrites, error exits, and finish-time payload reads.
    - Completed by teaching ARC's join-body ownership seed that
      maybe-initialized join params are conditionally owned by construction,
      even when read-before-rebind analysis would otherwise classify them as
      borrowed. This makes overwriting a maybe-initialized parser payload emit
      `decref_if_initialized` for the old value before the payload write.
    - Added LIR ARC regressions for conditionally releasing a maybe-initialized
      join payload on loop exit and for overwriting a maybe-initialized payload
      before writing the new presence bit.
    - Fixed the dev backend's initialized-payload lowering so
      `switch_initialized_payload` and `decref_if_initialized` always test
      `(condition & mask) == mask`. The previous `mask == 1` shortcut compared
      the whole presence word to `1`, which was wrong after additional fields
      set other bits in the same word.
    - Removed the previous no-op Monotype cleanup scaffolding
      (`sequenceTryWithRecordCleanup` and related helpers); LIR/ARC now owns the
      conditional cleanup invariant explicitly.
    - Verified with `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-backend`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`.
    - Verified the dev-backend presence-mask regression with
      `zig build run-test-cli -- --suite subcommands --filter "stored parser Fields metadata"`;
      adjacent runtime/static parser metadata filters
      `runtime prepared` and `supports userspace Fields.rename_fields` also
      pass.

- [x] Add regression coverage for wide and nested records.
  - Tasks:
    - Add a record with more fields than fit in one simple small bitset if the
      implementation supports multi-word bitsets now.
    - Add nested records and tag unions to make sure independent presence bitsets
      do not collide.
  - Success criteria:
    - Nested parser tests pass.
    - Disassembly does not reintroduce aggregate slot tuples for nested records.
    - Existing JSON platform coverage exercises nested records and nested tag
      unions through `nested.mode`, `status`, and `pair`, including reordered
      nested object fields and duplicate nested values.
    - Added direct postcheck unit coverage for the generated parser
      presence-word helpers across 64-bit boundaries: field counts 0, 1, 64,
      65, 128, and 129; field indexes 0, 63, 64, 127, and 128; and masks at the
      same boundaries. Verified with `zig build run-test-zig-module-postcheck`.
      This proves the multi-word arithmetic used by generated wide-record
      parser state at the exact boundary where a second or third presence word
      is needed.
    - A source-level 65-field parser stress test was intentionally not added to
      the normal suite because it currently compiles too slowly for targeted
      iteration. The low-level boundary test is the exact invariant the
      multi-word implementation relies on, and nested runtime behavior remains
      covered by the JSON platform test.

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

- [x] Identify all compiler-generated parser `Try` producer/consumer pairs.
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
    - No user-authored `Try` behavior are changed.
    - Only generated internal parser edges are eligible for direct-control-flow
      lowering.
    - Completed for the current generated parser paths. The generated record
      loop sequences `parse_record_field`, `skip_record_field`, matched field
      parsers, finish-time record construction, and missing-field helpers
      through the internal `sequenceTry` helper in `src/postcheck/monotype/lower.zig`.
      Those edges are now represented by an internal `try_sequence` Monotype
      expression rather than by a user-authored `Try` match. Public format
      methods still return ordinary Roc `Try` values.

- [x] Add an explicit direct parser control-flow lowering path.
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
    - Implemented internal `try_sequence` nodes in Monotype and Lambda Mono.
      Both LIR lowerers now lower those nodes by evaluating the input `Try`,
      switching on `Ok`/`Err`, binding the `Ok` payload only on the success
      branch, and forwarding the `Err` payload directly into the output `Try`.
    - The expression walkers, lifter, Lambda solver, specialization pass,
      debug Lambda Mono path, and direct solved-LIR path all traverse or lower
      the node explicitly. This is still format-agnostic and does not expose a
      new public Roc API.
    - Verified with `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-backend`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`.

- [x] Remove stack-shaped `{ value, rest }` temporaries where the value is
      immediately destructured by generated code.
  - Tasks:
    - Field value parsers may still publicly return
      `Try({ value, rest }, err)`, but generated call sites should lower the
      returned fields into direct locals when possible.
    - Avoid building a temporary record only to access `.value` and `.rest`.
  - Success criteria:
    - Disassembly shows fewer stack stores/loads around field parser calls.
    - Function frame size for `_roc__proc_e3` decreases.
    - Implemented an internal `try_record_sequence` node for generated parser
      call sites whose successful `Try.Ok` payload is immediately destructured
      as `{ value, rest }`. LIR lowering now binds those two locals directly
      from the Ok tag payload with existing `.tag_payload` field reads, so the
      generated matched-field path no longer creates an Ok payload record local
      just to read `.value` and `.rest`.
    - This remains format-agnostic: the node records explicit record field ids
      from the generated parser, and backends still lower ordinary LIR
      reference operations rather than guessing parser-specific behavior.
    - Verified with `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-backend`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`.
    - Refreshed optimized disassembly report:
      `/tmp/roc-http-header-disasm-after-try-record-report.txt`. The main
      generated parser proc is now `_roc__proc_d1` at 98,504 code bytes and
      3,568 total stack bytes. This is down from the previous
      `TryFieldCaseless` static-dispatch report's `_roc__proc_e4` at about
      103,444 code bytes, confirming the immediately-consumed `{ value, rest }`
      result records were removed from that generated parser path. The stack
      frame is still much larger than pico/may, so the remaining stack traffic
      is tracked by Phases 5, 6, and 9.

- [x] Keep direct-control-flow lowering format-agnostic.
  - Tasks:
    - Trigger the optimization from generated parser provenance and IR shape, not
      from HTTP/JSON-specific names.
    - Do not detect source strings or format module paths.
  - Success criteria:
    - The same lowering applies to HTTP, JSON, and future structural formats.
    - Implemented through internal Monotype/Lambda Mono `try_sequence` and
      `try_record_sequence` expression nodes plus the generic structural record
      parser lowering in `src/postcheck/monotype/lower.zig`. The lowering keys
      off generated parser result shapes and explicit field ids, not module
      names, format names, HTTP/JSON strings, or source paths.
    - Verified the same path with both
      `zig build run-test-zig-http-header-decoder-platform` and
      `zig build run-test-zig-json-decoder-platform`, plus
      `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-lir`, and
      `zig build run-test-zig-module-backend`.

## Phase 4: Emit Direct Static Field Dispatch For Known `Fields`

The generated parser already has compile-time knowledge of transformed
`Fields(_shape)`. The hot path should not walk a generic iterator or call a
generic `Str` helper for every candidate when the concrete field set is known.
For small SSO field names, generated code should compare immediate packed words
and dispatch directly to the field parser.

- [x] Add a compiler-owned field-dispatch plan for transformed `Fields`.
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
    - Implemented through the generated parser `ParserPrecomputedPlan` and the
      generated `Fields(_shape)` evidence type. When parser construction runs
      at compile time, lowering receives transformed field names, lengths, and
      field-handle metadata directly instead of rebuilding a dispatch plan at
      runtime.
    - The plan preserves the phantom `_shape` relationship between
      `Fields(_shape)` and `Field(_shape)`, so generated dispatch does not need
      release-build field-index bounds checks for compiler-derived handles.
    - Verified renamed metadata and name bounds with
      `zig build run-test-cli -- --suite subcommands --filter "renamed Fields metadata"`,
      `zig build run-test-cli -- --suite subcommands --filter "renamed Fields name bounds"`,
      `zig build run-test-cli -- --suite subcommands --filter "supports userspace Fields.rename_fields"`,
      and stored/runtime parser filters.

- [x] Generate an SSO exact-match dispatcher.
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
    - Intermediate progress: exact static dispatch now selects a deterministic
      discriminating 8-byte lane for each equal-length transformed field-name
      group, probes that lane with the internal
      `str_static_small_word_eq` operation, and performs the full static-small
      exact verification only after a lane hit. The runtime helper has tests for
      full lanes, short seamless slices at allocation end, and out-of-range
      lanes. Verified with `zig build run-test-zig-module-builtins -- --test-filter staticSmallWordEq`,
      `zig build run-test-zig-module-builtins`,
      `zig build run-test-zig-http-header-decoder-platform`,
      `zig build run-test-zig-json-decoder-platform`,
      `zig build run-test-wasm-static-lib`, and
      `zig build run-test-cli -- --suite subcommands --filter "Str.is_eq edge"`.
      Optimized disassembly dumps
      `/tmp/roc_http_after_word_lane.s` and
      `/tmp/roc_json_camel_direct_after_word_lane.s` contain no calls to
      `roc_builtins_str_equal`, `roc_builtins_str_equal_static_small`,
      `roc_builtins_str_static_small_word_eq`, or
      `roc_builtins_str_caseless_ascii_equals`.
    - Completed for the exact-match path. The later caseless item completes the
      analogous `TryFieldCaseless` path used by the HTTP benchmark, while the
      remaining frame/code-size work is tracked separately in Phases 5, 6, and
      9.

- [x] Generate an SSO ASCII-caseless dispatcher.
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
    - Implemented an internal `str_static_small_word_caseless_eq` low-level
      operation for generated static field-name dispatch. LLVM lowers it to the
      same fixed-word SWAR predicate used by `Str.is_caseless_eq`; Wasm, dev
      backend, and the interpreter all have corresponding support.
    - Lowering now uses selected static SSO word-lane probes for both exact and
      ASCII-caseless generated record dispatch, then performs full static-small
      verification only on a discriminator hit.
    - Verified with
      `zig build run-test-zig-module-builtins -- --test-filter staticSmallWordCaselessEq`,
      `zig build run-test-zig-module-builtins`,
      `zig build run-test-cli -- --suite subcommands --filter "stored parser Fields metadata"`,
      `zig build run-test-zig-http-header-decoder-platform`,
      `zig build run-test-zig-json-decoder-platform`, and
      `zig build run-test-wasm-static-lib`.
    - Optimized disassembly dumps
      `/tmp/roc_http_after_caseless_static_lane.s` and
      `/tmp/roc_json_camel_direct_after_caseless_static_lane.s` contain no
      references to `roc_builtins_str_equal`,
      `roc_builtins_str_equal_static_small`,
      `roc_builtins_str_static_small_word_eq`,
      `roc_builtins_str_static_small_word_caseless_eq`, or
      `roc_builtins_str_caseless_ascii_equals`. The large parser-state frame and
      cold allocation/error paths remain separate open work: in the refreshed
      HTTP dump, `_roc__proc_11b` still reserves `0x1640` bytes of stack.
    - Follow-up LLVM lowering cleanup removed the two backend-local stack
      temporaries from the static word compare helper by threading the loaded
      runtime word and active-byte mask through SSA phi values. Verified with
      `zig build run-test-zig-module-builtins -- --test-filter staticSmallWord`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`. Refreshed comparison
      report `/tmp/roc-http-header-disasm-after-static-lane-ssa-report.txt`
      still shows `_roc__proc_11b` at 260088 code bytes and 4192 total stack
      bytes, so the remaining frame/code-size gap is the generic parser-state
      and cold/error-path machinery tracked by later phases.
    - The HTTP header platform now returns `TryFieldCaseless` from
      `parse_record_field` instead of doing a userspace `Fields.for_size` loop
      and `Str.caseless_ascii_equals` before returning a direct `Field`. This
      puts the motivating header parser on the generated static caseless
      dispatch path. Verified with
      `zig build run-test-zig-http-header-decoder-platform`,
      `zig build run-test-zig-json-decoder-platform`,
      `zig build run-test-cli -- --suite subcommands --filter "stored parser Fields metadata"`,
      and optimized LLVM/disassembly output
      `/tmp/roc_http_after_tryfield_caseless_headers.s` and
      `/tmp/roc_http_after_tryfield_caseless_headers.ll`.
    - The refreshed optimized HTTP IR/disassembly contains no references to
      `roc_builtins_str_equal`, `roc_builtins_str_caseless_ascii_equals`, or
      the static string-helper symbols. The main generated parser proc changed
      from `_roc__proc_11b` at about 261,328 code bytes in
      `/tmp/roc_http_after_runtime_error_helper.s` to `_roc__proc_e4` at about
      103,444 code bytes. The proc still has 2,434 allocas and 255 memsets in
      IR, so the remaining large gap is parser-state/control-flow shape rather
      than field-name dispatch.

- [x] Keep `Fields.iter` and `Fields.for_size` correct for userspace.
  - Tasks:
    - Users may still iterate field sets at runtime.
    - Runtime iteration must remain allocation-free.
    - The optimized generated dispatcher must not require changing the public
      `Fields` representation or behavior.
  - Success criteria:
    - Existing userspace format code using `Fields.for_size` keeps working.
    - Runtime-created parsers still work even if parser construction was not
      evaluated at compile time; they may be slower, but must be correct.
    - Verified userspace `Fields.rename_fields` and `Fields.for_size` with
      `zig build run-test-cli -- --suite subcommands --filter "supports userspace Fields.rename_fields"`.
    - Verified `Fields.iter`, `Fields.for_size`, shortest/longest bounds, and
      renamed field metadata with
      `zig build run-test-cli -- --suite subcommands --filter "renamed Fields name bounds"` and
      `zig build run-test-cli -- --suite subcommands --filter "renamed Fields metadata"`.
    - Verified stored and runtime-prepared parser construction with
      `zig build run-test-cli -- --suite subcommands --filter "runtime prepared"`.

- [x] Handle static and runtime parser-construction paths with one generated-path behavior.
  - Tasks:
    - Avoid a different public compilation model depending on whether
      `parser_for` ran at compile time.
    - Let compile-time field values optimize down to immediates.
    - Let runtime field values use the same ordinary Roc value behavior with an
      expected performance cost.
  - Success criteria:
    - Tests cover both compile-time-created parsers and runtime-created parsers.
    - Both paths produce the same parse results.
    - Only the compile-time path is expected to have immediate static field
      compares in disassembly.
    - Verified with
      `zig build run-test-cli -- --suite subcommands --filter "runtime prepared"`,
      which exercises stored parser construction, runtime-prepared fields, and
      consistent parse results through the interpreter/dev CLI runner.
    - Verified stored parser metadata through the dev backend with
      `zig build run-test-cli -- --suite subcommands --filter "stored parser Fields metadata"`.

## Phase 5: Defer Full Roc `Str` Construction For Borrowed Header Values

Header parsing currently hands Roc `Str` values around as slices. That is
correct, but the generated parser may still build full Roc `Str` values earlier
than necessary. The hot path should internally carry pointer/length or an
equivalent borrowed slice descriptor until the final record field requires a
real Roc `Str` value.

- [x] Identify where borrowed field names and values become full Roc `Str`
      values.
  - Tasks:
    - Trace host header slice construction.
    - Trace `Headers.roc` record-field parsing.
    - Trace Monotype and LIR lowering for `Str` value parser results.
  - Success criteria:
    - The plan for deferring `Str` construction is based on explicit IR data, not
      inferred source names or backend guessing.
    - Completed trace:
      - The host constructs the request header section as a borrowed Roc string
        after UTF-8 validation in `test/http-headers/platform/host.zig`; the
        backing bytes live in the fixed request buffer for the whole Roc call.
      - `test/http-headers/platform/Headers.roc` immediately uses safe `Str`
        operations for structural parsing: `parse_record_field_from_headers`
        calls `headers.find_first("\r\n")` and `headers.find_first(":")` to
        produce the header line, field-name slice, and value-start slice.
        `take_header_value` calls `find_first("\r\n")` and then `trim()` to
        produce header-value slices.
      - The builtin implementation confirms why this still leaves generic
        string machinery in the generated proc: `src/builtins/str.zig`
        `findFirst` returns `retainedSlice` values and `strTrim` consumes or
        re-slices an ordinary `RocStr`. The low-level RC table in
        `src/base/LowLevel.zig` marks `str_find_first` as sharing/retaining its
        input and `str_trim` as a runtime-uniqueness operation.
      - The refreshed HTTP IR `/tmp/roc_http_current.ll` shows the main
        generated parser proc `_roc__proc_d1` with 1,602 allocas, 237 memsets,
        and many RC helper calls before LLVM optimization. The final optimized
        disassembly report
        `/tmp/roc-http-header-disasm-after-try-record-report.txt` still shows
        `_roc__proc_d1` at 98,504 code bytes and 3,568 total stack bytes. The
        tests prove the success path does not call the aborting host allocator,
        so this traffic is ordinary string/ARC/cold-path machinery around
        borrowed slices, not runtime heap allocation on the accepted request
        path.

- [ ] Introduce an internal borrowed-string parse value where appropriate.
  - Tasks:
    - Represent borrowed input as pointer/length plus lifetime and ownership metadata
      already guaranteed by host validation and request lifetime.
    - Keep this internal to generated parser lowering; do not expose unsafe
      pointer/length values to userspace.
    - Convert to an ordinary Roc `Str` only when a user-visible Roc value is
      produced.
  - Success criteria:
    - Userspace still sees ordinary `Str`.
    - No allocations are introduced.
    - Host lifetime and UTF-8 validation invariants remain explicit.
    - Additional progress: optimized SSO string helper paths now avoid pulling
      allocator-capable construction branches into the generated parser. In
      `src/builtins/str.zig`, `Str.trim`, `Str.trim_start`, `Str.trim_end`, and
      the small-source branch of `substringUnsafe` construct small strings with
      an internal stack-only helper instead of routing through generic
      `RocStr.init` / `RocStr.fromSlice`. This does not introduce the internal
      borrowed-string parse value described by this item, so the item remains
      open.
    - Verification for the helper progress:
      `zig build run-test-zig-module-builtins`,
      `zig build run-test-zig-http-header-decoder-platform`, and optimized
      disassembly report
      `/tmp/roc-http-header-disasm-after-sso-substring/roc_http_header.s`.
      The report shows the full Roc binary now has `roc_alloc` textual count
      `0`, down from `50` in
      `/tmp/roc-http-header-disasm-after-cold-call-backout` and `28` after only
      the trim SSO change.
    - Current design boundary: the public structural parsing API still requires
      userspace format code to return ordinary `Str` values for parsed field
      names (`TryField({ name : Str, rest : state })` and
      `TryFieldCaseless({ name : Str, rest : state })`) and for `parse_str`
      values. Under the current "no public API changes" invariant, generated
      dispatch cannot receive a non-`Str` borrowed pointer/length candidate from
      userspace. The remaining work in this item therefore requires a real
      internal representation/API design decision, not a backend-only
      optimization.

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
    - Intermediate progress: `test/http-headers/platform/Headers.roc` now returns
      `TryFieldCaseless` with the parsed header-name slice and value-start
      state. Generated static dispatch handles known headers, and unknown
      headers use the format's `skip_record_field` from the value-start state.
      This removes the userspace field iterator/string-helper dispatch from the
      HTTP hot path, but the parsed field name is still an ordinary safe Roc
      `Str` slice rather than the lower-level borrowed pointer/length
      representation tracked by this phase.
    - Additional intermediate progress: the header format now uses
      `Fields.shortest_name(fields)` and `Fields.longest_name(fields)` to return
      `Continue` immediately for header names whose byte length cannot match any
      transformed record field. That avoids generated field dispatch and value
      skipping for known-impossible names while preserving order independence and
      missing-field behavior. It still constructs the safe `name : Str` slice
      before the length check, so the no-temporary-`Str` success criterion
      remains open.
      The existing HTTP matrix directly exercises this path through
      `long_unknown_header_name` in
      `src/cli/test/http_header_decoder_platform_test.zig`.
    - Current design boundary: unknown headers whose names fall within the
      possible transformed field-name length range still need an ordinary
      `name : Str` to satisfy the public `TryFieldCaseless` event. The compiler
      cannot compare raw candidate bytes that userspace has not provided, and
      userspace has no safe public raw-byte candidate type. Completing this item
      without changing the public parse-event shape would require hardcoding a
      source pattern or format, which violates the format-agnostic compiler
      invariant.
    - Rejected direct-`Field` probe: changing
      `test/http-headers/platform/Headers.roc` to iterate `Fields.iter(fields)`
      and call `Str.drop_prefix_caseless_ascii` on each transformed field name
      passed `zig build run-test-zig-http-header-decoder-platform`, but it was
      not a performance-equivalent solution. A fresh optimized report at
      `/tmp/roc-http-header-disasm-direct-field-rebuilt-report.txt` grew the Roc
      binary from 119,168 to 119,584 bytes, grew Roc disassembly from 12,804 to
      15,633 lines, increased the largest parser proc from 28,124 to 28,156 code
      bytes, and added several helper procs. The probe was backed out. This
      confirms the remaining work needs an explicit compiler-owned borrowed
      candidate representation, or an explicit safe parse-event/API shape that
      lets generated dispatch receive candidate bytes and length without a
      userspace `Str` key.

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
    - Current design boundary: `parse_u64` is an ordinary userspace format
      method. In the HTTP example it can receive the value-start `Headers`
      state, but today's safe userspace string API exposes the header value to
      `U64.from_str` as a `Str` slice. Avoiding that temporary Roc `Str` while
      staying format-agnostic would require either a different safe parsing
      primitive or a different parse-event/value-parser contract.

## Phase 6: Outline Cold Decode-Error And Crash Paths

The current generated parser proc includes large amounts of cold error,
missing-field, decref, and crash-path code. The hot path should be straight-line
or locally-branching parser code. Cold paths should be outlined so they do not
inflate the main parser frame and instruction footprint.

- [x] Inventory cold paths inside the generated parser proc.
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
    - Current categorized regions in the refreshed HTTP LLVM/disassembly output:
      - never-taken allocation branches from generic inlined string helpers and
        support code; `_roc__proc_d1` contains textual calls to `_roc_alloc`,
        but the end-to-end platform tests keep the aborting allocator hooks from
        being reached;
      - `Headers.DecodeErr.BadHeader` construction and propagation from
        malformed lines, bad `U64` parsing, malformed content length, and
        invalid request structure;
      - missing required field finishing through the format's
        `missing_record_field`;
      - optional absent-field finishing through `missing_optional_field`;
      - ARC cleanup/dealloc paths for overwritten, error, and early-return
        parser states;
      - runtime-error/crash helper calls for compiler invariant violations and
        impossible generated-parser cases.
    - Final inventory status: complete for the current implementation. The
      final disassembly confirms the same categories remain: cold allocation
      support branches, missing-field construction, bad-header/error
      propagation, runtime-error/crash helpers, and ARC cleanup/dealloc paths.
      The separate cold-outlining items remain open because these regions are
      still present inside the main generated parser proc.

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
    - Intermediate progress: generic LIR `.runtime_error` sites in the LLVM
      backend now call one internal cold `roc_runtime_error` helper instead of
      inlining the same `emitCrashBytes("hit a runtime error")` sequence at
      every site. Verified with `zig build run-test-zig-module-backend`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`.
    - Refreshed optimized HTTP LLVM/disassembly output:
      `/tmp/roc_http_after_runtime_error_helper.ll` and
      `/tmp/roc_http_after_runtime_error_helper.s`. The IR has one
      `define internal void @roc_runtime_error`, the helper disassembles to
      about 4,068 bytes, and generated runtime-error sites call it instead of
      duplicating the crash body. `_roc__proc_11c` dropped from about 56,980
      bytes after static-byte deduplication to about 53,144 bytes; the main
      generated parser proc `_roc__proc_11b` only dropped from about 261,488 to
      261,328 bytes, so broader decode-error/missing-field/ARC cold outlining
      remains open.
    - Additional progress: the LLVM runtime-error helper is now marked `cold`
      and, on targets where Roc crash lowering is known not to return,
      `noreturn`. Linux AArch64 is deliberately excluded because its current
      eval crash path returns to the host. Verified with
      `zig build run-test-zig-module-backend`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`.
    - Additional progress: initialized-payload branch weighting is now carried
      as explicit parser-lowering data instead of being inferred by the LLVM
      backend. Required-field missing branches set `uninitialized_is_cold`;
      optional-field absence leaves it unset because optional absence is
      ordinary parser behavior. Verified with
      `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-backend`,
      `zig build run-test-wasm-static-lib`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`. The refreshed optimized
      report
      `/tmp/roc-http-header-disasm-after-explicit-payload-hint-report.txt`
      matches the optional-finish metrics: `_roc__proc_db` remains at 93,024
      code bytes and 3,104 total stack bytes.
    - Additional parser-finish progress: optional fields now finish through an
      initialized-payload expression that directly produces the optional
      `Try(...)` field value and then falls through to the shared record
      construction continuation. This removes an immediately-consumed generated
      `Try(field, err)` layer for optional fields without changing required
      field missing-error behavior. Verified with
      `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-backend`,
      `zig build run-test-wasm-static-lib`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`.
    - Additional branch-structure progress: ordinary LIR `switch_stmt` now
      carries an explicit `default_is_cold` bit. Generated parser
      `try_sequence` and `try_record_sequence` lowering set that bit for the
      Err-propagation default branch, ARC preserves it while rewriting switch
      bodies, and the LLVM backend lowers the one-branch cold-default shape to
      a weighted conditional branch instead of an unweighted LLVM `switch`.
      This is still not full outlining, but it moves the generated parser Err
      edge out of the "backend guessed this was cold" category. Verified with
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-backend`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`. Added an ARC regression
      asserting that cold-default switch metadata survives the ARC rewrite.
    - Additional provenance cleanup: `try_sequence` and
      `try_record_sequence` now carry an explicit `err_is_cold` field from
      Monotype generation through Monotype specialization, Lambda Mono, and
      both LIR lowerers. The lowerers no longer assume every node of that kind
      has a cold Err edge; they consume explicit IR data produced when the
      generated parser sequence is built. Verified with
      `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-backend`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`.
    - Additional ARC cleanup outlining progress: LLVM-generated RC drop/free
      helpers are now marked `noinline` when declared. The helper kind is the
      explicit RC operation selected by LIR ARC insertion, so the backend does
      not infer parser-specific behavior; it simply preserves the helper
      boundary instead of letting LLVM clone recursive teardown bodies into hot
      callers. Verified with `zig build run-test-zig-module-backend`,
      `zig build run-test-zig-json-decoder-platform`, and
      `zig build run-test-zig-http-header-decoder-platform`.
      Refreshed optimized report:
      `/tmp/roc-http-header-disasm-rc-helper-noinline-report.txt`. Compared
      with `/tmp/roc-http-header-disasm-restored-length-bounds`, the Roc binary
      dropped from 233,504 bytes to 119,136 bytes, `__TEXT` dropped from
      196,608 bytes to 81,920 bytes, and the largest generated parser proc
      dropped from about 153,228 code bytes to about 28,144 code bytes. The
      parser proc now calls `_roc_llvm_rc_decref_*` helpers and contains no
      direct `_roc_dealloc` calls. This does not complete the item because the
      parser frame is still large at about 4,192 total stack bytes and
      decode-error/missing-field construction is still not broadly outlined.
    - Additional cold-call provenance progress: generated required-field
      `missing_record_field` calls now carry an explicit `is_cold` bit from
      Monotype direct calls through Monotype Lifted, specialization, Lambda
      Mono, solved inlining, LIR, ARC, and LLVM. Cold calls are not inlined by
      the direct-call wrapper/specialization paths, ARC preserves the metadata,
      and LLVM emits `cold`/`noinline` call-site attributes from that explicit
      IR bit rather than guessing from names. Optional missing-field calls are
      deliberately not marked cold because optional absence is ordinary parser
      behavior. Added an ARC regression for preserving cold direct-call
      metadata. Verified with `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-backend`,
      `zig build run-test-wasm-static-lib`,
      `zig build run-test-zig-http-header-decoder-platform`, and
      `zig build run-test-zig-json-decoder-platform`.
      Refreshed optimized report:
      `/tmp/roc-http-header-disasm-cold-call-attrs-report.txt`. The required
      missing-field helper is now a tiny out-of-line proc (`_roc__proc_e1`,
      12 code bytes), and the largest parser proc moved from 28,144 to 28,124
      code bytes. The parser frame remains 4,192 total stack bytes, so this is
      useful provenance and a small code-size cleanup but still not full cold
      outlining.

- [ ] Mark or structure cold compiler-generated paths in a backend-appropriate
      way.
  - Tasks:
    - Prefer explicit IR/procedure structure over backend heuristics.
    - If LLVM cold attributes are used, they must be emitted from explicit
      compiler provenance, not guessed from source names.
  - Success criteria:
    - The implementation follows `design.md`: later stages consume explicit data
      from earlier stages and do not guess.
    - Intermediate progress: compiler-generated `.runtime_error` lowering now
      routes through an explicit LIR runtime-error statement and an explicit
      backend helper with cold/noreturn attributes where valid. This item stays
      open until decode-error construction, missing-field construction, and
      conditional ARC cleanup are also structured so the main generated parser
      proc is not carrying large interleaved cold regions.
    - Additional progress: LIR `switch_initialized_payload` now carries the
      explicit `uninitialized_is_cold` bit produced by generated parser
      lowering. LLVM consumes that bit for branch weights; dev, Wasm, the
      interpreter, ARC, and LIR tools preserve or ignore the explicit bit.
      This still does not outline the cold blocks themselves, so the
      broader item remains open.
    - Additional progress: ordinary LIR `switch_stmt` now carries the explicit
      `default_is_cold` bit for parser-generated `Try` Err propagation. LLVM
      consumes that bit only for the one-branch switch shape by emitting a
      weighted conditional branch with the Ok arm likely and the Err default
      arm cold. The bit is produced by parser lowering and preserved by ARC,
      with a focused ARC regression proving preservation. This still does not
      outline the cold block bodies themselves, so this checklist item remains
      open.
    - Additional progress: parser-generated `Try` sequencing carries its cold
      Err provenance as an `err_is_cold` field before LIR exists. That keeps
      the branch metadata explicit across Monotype, specialization, Lambda Mono,
      and LIR lowering instead of making the LIR lowerer infer coldness from a
      node kind. This still does not outline the cold block bodies themselves,
      so this checklist item remains open.
    - Additional progress: LLVM RC drop/free helpers are now explicitly kept
      out of line with `noinline`. This is a backend-appropriate use of
      attributes driven by explicit LIR ARC helper operations, not by source
      names or parser-shaped guesses. It outlines recursive teardown helper
      bodies from the generated parser proc, but it does not yet outline every
      cold decode-error, missing-field, or cleanup branch body, so this item
      remains open.
    - Additional progress: direct calls can now carry explicit cold provenance.
      Generated required-field missing-error calls set this bit before LIR
      exists; specialization, solved inlining, LIR ARC, debug printing, and LLVM
      preserve or consume the bit. LLVM uses it to emit cold/noinline call-site
      attributes, and ARC has a focused regression proving the metadata
      survives rewriting. This is still not broad outlining of every cold block,
      so the item remains open.

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
    - Unicode strings where byte equality is the expected byte-level result.
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

- [x] Use exact string fast paths in generated field dispatch when possible.
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
    - Intermediate progress: exact static field dispatch now uses the Phase 4
      discriminating word-lane probe before full verification for transformed
      field names up to 24 bytes. The general optimized `Str.is_eq` path remains
      the fallback for runtime-created parsers and long field names.
    - Completed for generated static SSO exact dispatch: refreshed optimized
      HTTP and JSON disassembly after the static lane work contains no
      references to `roc_builtins_str_equal`,
      `roc_builtins_str_equal_static_small`, or
      `roc_builtins_str_static_small_word_eq`; generated static exact dispatch
      uses direct lowered compares.

## Phase 9: Remove Remaining Generic Parser State Traffic

After the core phases above, inspect the generated HTTP parser again and remove
whatever generic parser machinery remains in the hot path without changing the
Roc API.

- [x] Inspect hot loop register and stack traffic.
  - Tasks:
    - Identify stores/loads that exist only because generated parser state is
      represented too generically.
    - Identify any remaining aggregate copies of field sets, parser closures,
      parse results, or states.
  - Success criteria:
    - Every large stack object in `_roc__proc_e3` has a known reason.
    - Unnecessary stack objects have follow-up patches or are removed.
    - Intermediate evidence after the LLVM proc-frame slot cleanup:
      `allocProcLocalSlots` now allocates only the current proc's explicit
      `args` and `frame_locals` instead of every local in the whole LIR store.
      This cut unrelated procs in fresh IR from roughly 3000 allocas each to
      their real frame-local counts, for example `_roc__proc_117` dropped to
      133 allocas. The main generated HTTP parser proc still has 2434 allocas
      in `/tmp/roc_http_after_frame_slots.ll`, so its stack traffic is produced
      by its own generated parser/app body and remains open work rather than a
      global LLVM backend slot-allocation issue.
    - Intermediate evidence after removing LLVM entry zeroing for local slots:
      local storage is now allocated as uninitialized stack space, and only
      operations that construct zero-requiring values emit explicit zeroing.
      In `/tmp/roc_http_after_uninit_locals.ll`, `_roc__proc_117` remains at
      133 allocas but drops to 19 memsets, while `_roc__proc_11b` remains at
      2434 allocas and drops to 255 memsets. The `_roc__proc_11b` prologue in
      `/tmp/roc_http_after_uninit_locals.s` now reserves `0x1480` bytes of
      local stack plus the 96-byte save area, and no longer contains the broad
      one-memset-per-frame-local entry zeroing pattern. The remaining stack and
      memset traffic is generated by parser aggregates, parser result values,
      and cold/error machinery, so this item remains open.
    - Intermediate evidence after direct `try_record_sequence` lowering:
      `/tmp/roc_http_current.ll` shows the refreshed main generated parser proc
      `_roc__proc_d1` at 1,602 allocas and 237 memsets before LLVM optimization,
      with the largest alloca buckets coming from repeated 224-byte, 24-byte,
      32-byte, 8-byte, and 200-byte temporary values. The optimized comparison
      report `/tmp/roc-http-header-disasm-after-try-record-report.txt` shows
      `_roc__proc_d1` at 98,504 code bytes and 3,568 total stack bytes. This is
      a measurable improvement from the previous `_roc__proc_e4` shape, but the
      remaining large frame is still generated by the parser/app body itself,
      not by global LLVM frame-slot allocation.
    - Intermediate evidence after hoisting generated `Fields(_shape)` out of
      the parser loop: `lowerParseRecordFromState` now binds the generated
      field-set value once before entering the record loop and passes that local
      to each `parse_record_field` call. Verified with
      `zig build run-test-zig-http-header-decoder-platform`,
      `zig build run-test-zig-json-decoder-platform`,
      `zig build run-test-zig-module-lir`, and
      `zig build run-test-zig-module-backend`.
      The refreshed report
      `/tmp/roc-http-header-disasm-after-fields-hoist-report.txt` shows the
      main generated Roc parser proc at 93,012 code bytes and 3,136 total stack
      bytes, down from 96,368 code bytes and 3,568 total stack bytes before the
      hoist. The remaining large alloca buckets in
      `/tmp/roc_http_after_fields_hoist.ll` are mostly 8-byte, 24-byte,
      1-byte, 32-byte, 2-byte, 224-byte, and 40-byte temporaries from
      string/state/Try/error/ARC machinery. Capturing an already-built `Fields`
      value in a stored parser closure would require a typed precomputed
      capture representation; the current precomputed-capture path is
      string-specific.
    - Intermediate evidence after optional-field direct finish:
      `/tmp/roc_http_after_optional_finish.ll` reduces the same main proc to
      1,576 allocas and 213 memsets from 1,604 allocas and 237 memsets in
      `/tmp/roc_http_after_fields_hoist.ll`. The optimized report
      `/tmp/roc-http-header-disasm-after-optional-finish-report.txt` shows
      `_roc__proc_db` at 93,024 code bytes and 3,104 total stack bytes. This is
      a stack improvement from the previous 3,136-byte frame with effectively
      unchanged code size. The remaining stack objects are still dominated by
      string/state/Try/error/ARC temporaries and required-field finish paths, so
      this item remains open.
    - Intermediate evidence after explicit initialized-payload branch hints:
      `/tmp/roc-http-header-disasm-after-explicit-payload-hint-report.txt`
      matches the optional-finish code-shape metrics, and
      `/tmp/roc_http_after_explicit_payload_hint.ll` contains branch-weight
      metadata emitted from explicit LIR provenance rather than from a backend
      assumption.
    - Final inspection evidence:
      `/tmp/roc-http-header-disasm-final-current/roc_http_header.s` shows
      `_roc__proc_db` at 93,024 code bytes with a 3,008-byte local stack frame
      plus 96 save bytes. Its prologue still writes parser construction state
      and zeroed vector lanes into the frame, so the remaining stack traffic is
      from generated parser state, string/state/Try/error values, and ARC/cold
      cleanup paths. The field-dispatch string-helper calls are gone, so the
      remaining high-impact follow-up is state/cold-path lowering rather than
      field-name matching.

- [x] Keep nested parser construction eager and runtime parser loops small.
  - Tasks:
    - Verify derived `parser_for` still constructs nested parsers before
      returning the runtime lambda.
    - Ensure runtime lambda closes over transformed static field data and nested
      parser functions, not original names or construction machinery.
  - Success criteria:
    - Compile-time parser construction tests still verify original field names
      are not in the final binary when renamed at compile time.
    - Runtime parser construction tests still pass.
    - Verified by the JSON camel-case binary-omission checks in
      `src/cli/test/json_decoder_platform_test.zig`, which assert original
      snake_case field names are absent after compile-time renaming.
    - Verified runtime/stored construction with:
      `zig build run-test-cli -- --suite subcommands --filter "runtime prepared"`,
      `zig build run-test-cli -- --suite subcommands --filter "stored parser Fields metadata"`,
      and
      `zig build run-test-cli -- --suite subcommands --filter "supports userspace Fields.rename_fields"`.

- [x] Avoid introducing backend-specific parser decisions.
  - Tasks:
    - LIR must contain explicit operations for presence bits, control flow,
      field dispatch, and ARC cleanup.
    - Backends lower the explicit LIR and do not infer parser-specific behavior.
  - Success criteria:
    - Dev, LLVM, and Wasm backends either lower explicit operations correctly or
      have targeted unsupported-test coverage if a backend does not yet support
      this platform example.
    - Implemented behavior are carried by explicit Monotype/Lambda/LIR nodes
      and low-level operations: `try_sequence`, `try_record_sequence`,
      `uninitialized_payload`, `switch_initialized_payload`,
      `if_initialized_payload`, `str_static_small_word_eq`, and
      `str_static_small_word_caseless_eq`.
    - Dev, LLVM, Wasm, interpreter, ARC, ARC solving/certification, TRMC,
      scalarized joins, debug printing, and LIR image serialization all handle
      the explicit nodes. Backends lower those nodes directly; they do not
      infer HTTP/JSON/parser-specific behavior from source names or value shapes.
    - Verified with `zig build run-test-zig-module-backend`,
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-postcheck`,
      `zig build run-test-wasm-static-lib`,
      and the parser platform tests.

## Phase 10: Verification Before Final Disassembly

- [x] Run focused parser/platform tests after each phase.
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
    - Passed after the static ASCII-caseless dispatch operation and lowering
      changes: `zig build run-test-zig-http-header-decoder-platform` and
      `zig build run-test-zig-json-decoder-platform`.
    - Passed after the LLVM proc-frame slot allocation cleanup:
      `zig build run-test-zig-http-header-decoder-platform` and
      `zig build run-test-zig-json-decoder-platform`.
    - Passed after removing LLVM entry zeroing for local slots:
      `zig build run-test-zig-http-header-decoder-platform`,
      `zig build run-test-zig-json-decoder-platform`, and
      `zig build run-test-cli -Dcli-test-llvm=true -- --suite subcommands --filter "Str.is_eq edge"`.
    - Passed after adding LLVM static-byte deduplication:
      `zig build run-test-zig-http-header-decoder-platform`,
      `zig build run-test-zig-json-decoder-platform`, and
      `zig build run-test-zig-module-backend`.
    - Passed after adding HTTP field-length bound `Continue` skipping:
      `zig build run-test-zig-http-header-decoder-platform`.
    - Passed again after the dev-backend initialized-payload mask fix and plan
      cleanup:
      `zig build run-test-zig-http-header-decoder-platform`,
      `zig build run-test-zig-json-decoder-platform`, and
      `zig build run-test-zig-module-postcheck`.
    - Passed after optional-field direct finish:
      `zig build run-test-zig-http-header-decoder-platform`,
      `zig build run-test-zig-json-decoder-platform`,
      `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-backend`, and
      `zig build run-test-wasm-static-lib`.
    - Passed after explicit initialized-payload branch hints:
      `zig build run-test-zig-http-header-decoder-platform`,
      `zig build run-test-zig-json-decoder-platform`,
      `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-backend`, and
      `zig build run-test-wasm-static-lib`.
    - Passed after outlining LLVM RC drop/free helper bodies with `noinline`:
      `zig build run-test-zig-module-backend`,
      `zig build run-test-zig-json-decoder-platform`, and
      `zig build run-test-zig-http-header-decoder-platform`.
    - Passed after adding explicit cold direct-call provenance for generated
      required-field missing-error calls:
      `zig build run-test-zig-module-postcheck`,
      `zig build run-test-zig-module-lir`,
      `zig build run-test-zig-module-backend`,
      `zig build run-test-wasm-static-lib`,
      `zig build run-test-zig-http-header-decoder-platform`, and
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

- [x] Run broader compiler checks after targeted tests pass.
  - Required command:
    - `zig build minici`
  - Failure protocol:
    - If `minici` fails in one section, switch to that targeted failing section.
    - Fix the targeted failure.
    - Rerun the targeted failing section until it passes.
    - Return to `zig build minici` only after the targeted section passes.
  - Success criteria:
    - `zig build minici` passes.
    - Verified with `zig build minici` after following the targeted-failure
      loop:
      - fixed `run-check-zig-lints` by documenting the new public ARC solver
        condition type;
      - fixed `run-check-tidy` by deleting dead helpers left behind by earlier
        parser-lowering iterations;
      - fixed `run-check-unused-suppression` by removing unused parameters and
        making the relevant ARC read-before-rebind helpers plain helpers;
      - fixed the stage-data wording audit by tightening `plan.md` wording to
        use concrete stage/output terms;
      - refreshed and staged the macOS dev-output snapshot hashes after the
        macOS deployment target change to 11.0;
      - reran the full `zig build minici`, which passed end to end.

- [x] Check optimized allocation invariants.
  - Tasks:
    - Rebuild the HTTP header platform with ReleaseFast host and `--opt=speed`
      Roc app.
    - Confirm `roc_alloc` is still a crashing implementation in the test host.
    - Confirm the HTTP tests send all required request combinations and receive
      correct responses.
  - Success criteria:
    - No runtime allocation is possible on the tested path without failing the
      test.
    - Verified by `zig build run-test-zig-http-header-decoder-platform`. The
      test host still exports aborting `roc_alloc`, `roc_realloc`, and
      `roc_dealloc` hooks, and the test harness asserts those diagnostics are
      absent for every accepted request and every expected request failure.

- [x] Check generated code shape before final report.
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
    - Current proc-frame cleanup evidence:
      `/tmp/roc_http_after_frame_slots.ll` shows backend-local allocation is no
      longer global across all procs, but `_roc__proc_11b` still has 2434 own
      frame allocas and broad parser-entry zeroing. This keeps this checklist
      item open.
    - Current local-slot initialization evidence:
      `/tmp/roc_http_after_uninit_locals.ll` shows the broad parser-entry
      zeroing caused by LLVM local-slot initialization has been removed. The
      optimized HTTP parser still has 2434 own-frame allocas and 255 explicit
      parser-construction memsets, so the scalarization, direct-control-flow,
      deferred-string, and cold-outlining phases remain open.
    - Current static-byte evidence:
      `/tmp/roc_http_after_static_bytes_cache.ll` has 13 `.roc.bytes` global
      definitions and exactly one `hit a runtime error` constant. This removes
      duplicate cold runtime-error data from the module. It does not change
      `_roc__proc_11b`'s parser-state alloca/memset counts, so hot parser
      scalarization and cold control-flow outlining remain open.
    - Current runtime-error helper evidence:
      `/tmp/roc_http_after_runtime_error_helper.ll` routes generated
      `.runtime_error` instructions through one internal `roc_runtime_error`
      helper. `_roc__proc_117` remains at 133 allocas and 19 memsets, while
      `_roc__proc_11b` remains at 2434 allocas and 255 memsets. This confirms
      the helper removes duplicated cold crash bodies but does not address the
      hot parser-state stack traffic.
    - Current HTTP static-dispatch evidence:
      `/tmp/roc_http_after_tryfield_caseless_headers.ll` and
      `/tmp/roc_http_after_tryfield_caseless_headers.s` contain no references
      to `roc_builtins_str_equal`, `roc_builtins_str_caseless_ascii_equals`, or
      static string-helper symbols. The main generated parser proc is now about
      103,444 code bytes, down from about 261,328 bytes before the HTTP format
      moved from userspace field lookup to generated `TryFieldCaseless`
      dispatch. The proc still has 2434 allocas and 255 memsets, so broad
      parser-state lowering remains open.
    - Final refreshed evidence:
      `/tmp/roc-http-header-disasm-final-current/roc_http_header.s` has no
      references to `roc_builtins_str_equal` or
      `roc_builtins_str_caseless_ascii_equals`. The main generated parser proc
      is `_roc__proc_db`, reserves 3,008 local stack bytes plus 96 save bytes,
      and is 93,024 code bytes. The entry no longer has the earlier whole-LIR
      local-slot zeroing, but it still has explicit parser-construction
      zeroing and in-proc cold/error/ARC cleanup regions. The complete Roc
      disassembly still contains 33 textual calls to `_roc_alloc` and 2 calls
      to `roc_runtime_error`; the HTTP platform tests keep the aborting
      allocator hooks from being reached on every accepted request and expected
      failure path.
    - Current RC-helper outlining evidence:
      `/tmp/roc-http-header-disasm-rc-helper-noinline/roc_http_header.s` has no
      references to `roc_builtins_str_equal` or
      `roc_builtins_str_caseless_ascii_equals`, no `_roc_alloc` textual
      references, and keeps direct `_roc_dealloc` calls out of the main
      generated parser proc. The largest generated parser proc is now
      `_roc__proc_dc` at 28,144 code bytes with 4,192 total stack bytes. Code
      size and binary size are much closer to the C/Rust comparison, but stack
      and parser-state traffic remain open.
    - Current cold-call provenance evidence:
      `/tmp/roc-http-header-disasm-cold-call-attrs/roc_http_header.s` keeps the
      same no-allocator/no-generic-string-helper shape. The required
      missing-field helper is now out of line as `_roc__proc_e1` at 12 code
      bytes, and `_roc__proc_dc` is 28,124 code bytes with the same 4,192 total
      stack bytes. This confirms the explicit cold call metadata is reaching
      optimized code, but the remaining stack frame and cold cleanup/error
      regions still require deeper generated parser lowering.

## Completion Checklist

The plan is not complete until every item below is true:

- [x] Generated record parser state is represented as presence bits plus payload
      locals through the lowering path that reaches LIR.
- [x] Missing required fields do not require initializing fake values.
- [x] Optional absent fields are constructed at finish time from format
      `missing_optional_field`, not from preinitialized slot tags.
- [x] Generated parser `Try` sequencing lowers to direct control flow through
      LIR where producer and consumer are compiler-generated parser code.
- [x] Immediately-consumed `{ value, rest }` parser result records are avoided
      where direct locals carry the same checked value.
- [x] Static known `Fields` dispatch emits direct exact small-string compares.
- [x] Static known `Fields` dispatch emits direct ASCII-caseless SWAR compares
      for eligible small strings.
- [x] Generic iterator/string-helper dispatch is not used on the static SSO hot
      path.
- [x] Runtime-created parsers still work correctly, even if they are slower than
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
- [x] HTTP header platform tests pass.
- [x] JSON platform tests pass.
- [x] String equality tests pass across the relevant execution paths.
- [x] `zig build minici` passes after targeted failures, if any, have been fixed
      through the targeted-failure loop.
- [x] Optimized HTTP header disassembly has no hot-path allocation, no hot-path
      runtime field map, and no per-header runtime field-name conversion.
      Current evidence is in
      `/tmp/roc-http-header-disasm-final-current/roc_http_header.s`: static
      field dispatch uses generated direct comparisons and does not call the
      generic string equality helpers. `_roc_alloc` calls remain in the full
      generated proc as cold/support-code paths, but the aborting allocator
      tests prove they are not on the exercised request path.

## Final Step: Redo Disassembly And Compare Roc To pico And may

This must be the last step after every checklist item above is complete.

- [ ] Rebuild all comparison binaries and disassembly output.
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
  - Completed with
    `tools/http-header-disasm-report.sh --skip-roc-build --work-dir /tmp/roc-http-header-disasm-after-sso-substring`
    after the SSO substring/trim helper changes.
    The generated files are:
    - Roc:
      `/tmp/roc-http-header-disasm-after-sso-substring/roc_http_header.s`;
    - pico:
      `/tmp/roc-http-header-disasm-after-sso-substring/picohttpparser_arm64_o3.s`;
    - may:
      `/tmp/roc-http-header-disasm-after-sso-substring/may_minihttp_roc_compare.s`.
    - These completed files are prior comparison artifacts. This item remains
      open until it is rerun after every remaining completion-checklist item has
      been finished.

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
  - Final side-by-side numbers:
    - Roc binary:
      - on disk: 233,504 bytes;
      - `__TEXT`: 196,608 bytes;
      - disassembly lines: 43,029;
      - `_main`: 3,360 code bytes, 2,384 total stack bytes;
      - `_roc__proc_ce`: 5,744 code bytes, 1,024 total stack bytes;
      - `_roc__proc_d2`: 153,228 code bytes, 3,024 total stack bytes;
      - `_roc__proc_d4`: 5,872 code bytes, 272 total stack bytes;
      - full-disassembly textual counts:
        `_roc_alloc` 0, `roc_builtins_str_equal` 0,
        `roc_builtins_str_caseless_ascii_equals` 0,
        `roc_builtins_roc_crashed` 2, `roc__proc_` 13.
    - pico object:
      - on disk: 6,224 bytes;
      - text: 4,311 bytes;
      - disassembly lines: 942;
      - `phr_parse_request`: 720 code bytes, 80 stack bytes;
      - `phr_parse_headers`: 248 code bytes, 48 stack bytes;
      - internal `parse_headers`: 468 code bytes, 80 stack bytes.
    - may binary:
      - on disk: 627,776 bytes;
      - `__TEXT`: 425,984 bytes;
      - disassembly lines: 86,453;
      - `httparse::parse_headers_iter_uninit`: 2,332 code bytes,
        112 stack bytes;
      - `httparse::Request::parse_with_config_and_uninit_headers`:
        1,792 code bytes, 160 stack bytes;
      - `may_minihttp::response::encode`: 1,936 code bytes,
        160 stack bytes;
      - comparable `generator::stack::StackBox<...>::call_once` loop:
        3,300 code bytes, 1,504 stack bytes;
      - `roc_compare::main`: 1,944 code bytes, 288 stack bytes.
    - These are prior comparison numbers. The final report must refresh every
      number after the remaining borrowed-string and cold-outlining work is
      complete.

- [ ] Analyze remaining gaps without proposing Roc API changes.
  - Success criteria:
    - The report explains which differences are due to required Roc value representation and ARC behavior,
      which are due to remaining compiler implementation overhead, and which are
      likely removable with further lowering/backend work.
    - Any new follow-up item is grounded in disassembly evidence from the final
      comparison.
  - Current gap analysis:
    - Required Roc value representation and ARC behavior: the generated parser
      still carries `Str` values from the userspace format API and must maintain
      their reference-counted lifetime until the derived record has been
      finished or the error path has cleaned up. This accounts for the
      visible incref/decref traffic and some payload-presence control flow.
    - Remaining compiler implementation overhead: `_roc__proc_db` still
      materializes a large parser-state frame and explicit parser-construction
      zeroing, even after local-slot zeroing and optional-field finish cleanup.
      pico and may/httparse keep their parser state in a much smaller set of
      direct pointer/length/header-array locals.
    - Likely removable without changing the Roc API: more aggressive generated
      parser state scalarization, cold outlining for decode-error/missing-field
      construction and ARC cleanup, and better lowering of the safe `Str` slice
      operations used by `find_first`/`trim` in the header format. Those are
      grounded in the final disassembly: the generic string equality helpers
      and textual allocator call sites are already gone, but stack traffic,
      cold blocks, crash support paths, and ordinary `Str`/ARC lifetime traffic
      remain in the generated proc.
    - This current gap analysis is an intermediate snapshot. The final analysis
      must be regenerated after the last optimized disassembly comparison.
