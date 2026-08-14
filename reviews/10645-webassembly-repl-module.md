# PR #10645 — Add dedicated WebAssembly REPL module

- **Author:** lukewilliamboswell · **Draft:** no · **Base:** `main`
- **Size:** +3,624 / −366 across 34 files

A self-contained stateful wasm REPL (`src/repl_wasm/`) with a versioned JSON
protocol, a TypeScript declaration file, a notebook-style browser demo, and two
test suites. Also refactors `ReplSession.zig` to separate presentation from
language stepping (+783/−159), and reorganizes the web build outputs.

## Verdict

Well-structured work. The protocol design is thoughtful (explicit
`protocol` version, `revision` for state invalidation, `offset_unit:
"utf8_bytes"` so embedders know what cursors mean, `capabilities` advertising
what's *not* supported), the presentation/language split in `ReplSession` is
the right refactor, and the memory ownership is correct where it counts.

**Two things I'd want changed before merge:** a new hard Node.js dependency
wired straight into CI (#1), and four more line-number-range tidy exclusions on
a mechanism that is already fragile (#2). The rest are performance findings and
nits.

---

## Findings

### 1. (Should fix) `run-test-repl-wasm` introduces a Node.js build dependency, and CI depends on it

```zig
const run_repl_cells_test = b.addSystemCommand(&.{ "node", "--test" });
run_repl_cells_test.addFileArg(b.path("test/repl-wasm-test/cells.test.mjs"));
run_test_repl_wasm_step.dependOn(&run_repl_cells_test.step);
```

`"node"` appears **nowhere else in `build.zig`** — I checked. This is the first
external-runtime dependency in the Zig build, and `.github/workflows/ci_zig.yml`
now runs `zig build run-test-repl-wasm` unconditionally.

Consequences:

- A contributor without Node on `PATH` gets an opaque `FileNotFound` from a step
  labelled "Run dedicated REPL WebAssembly tests," with no hint that the wasm
  half passed and only the JS half is missing.
- `node --test` needs Node ≥ 18 (≥ 20 for stable). Nothing pins or checks a
  version, so an older Node fails with a different opaque error.
- CI works only because the GitHub runner happens to ship Node. That's an
  undeclared dependency: if the runner image changes, this breaks with no
  obvious cause.

Options, roughly in order of preference:

- **Split the step.** Keep `run-test-repl-wasm` as the bytebox-driven Zig test
  (which needs nothing external and covers the actual wasm protocol), and put
  the JS test behind a separate `run-test-repl-cells` step that CI opts into
  explicitly and that documents the Node requirement.
- **Port `cells.test.mjs` to Zig.** The seven assertions are pure string/offset
  logic (`findCells`, `activeCell`, `utf16ToUtf8Offset`, …). The only reason it's
  in JS is that the implementation is; but a Zig port of the *expectations*
  against a Zig port of the logic would remove the dependency entirely — at the
  cost of duplicating the implementation, so probably not worth it.
- **At minimum**, declare Node in `BUILDING_FROM_SOURCE.md` and add an explicit
  `node --version` preflight with a clear error.

To be clear: the JS tests themselves are good. `findCells` recognizing "only
standalone delimiter lines" (with the `"first #%% stays"` case) and the
UTF-16↔UTF-8 offset round-trips are exactly the two places a notebook UI silently
corrupts text. I just don't want them to be the thing that makes `zig build`
unrunnable.

### 2. (Should fix) Four new line-number-range tidy exclusions

```zig
.{ .file = "inspected.zig", .start = 226,  .end = 232  },
.{ .file = "inspected.zig", .start = 2110, .end = 2116 },
.{ .file = "inspected.zig", .start = 2830, .end = 2840 },
.{ .file = "inspected_run.zig", .start = 97, .end = 103 },
```

The `CheckTypeCheckerPatternsStep` exclusion list keys on **line numbers**. This
PR adds four entries and itself changes `inspected.zig` by +120/−85 — so the
line numbers being excluded are the *post-change* ones, and the very next edit
to that file silently shifts them. When that happens you get one of two silent
failures:

- the range no longer covers the intended line → tidy fails for a reason nobody
  understands, and the fix is to bump a magic number;
- the range now covers a *different* line → a genuinely banned pattern
  (`find*`, `bytesToValue`, `eql` in checker-adjacent code) is silently
  permitted.

The second is the dangerous one, and it's exactly the failure the ban exists to
prevent. The mechanism is pre-existing — there was one entry (`cir_to_lir.zig`)
before this — but going from one to five is the point at which it should become
anchor-based instead: a `// tidy:allow-name-compare <reason>` comment on the
offending line, matched by the checker. That's a small change to `build.zig` and
it makes every existing exclusion self-documenting at its site rather than 3,500
lines away.

The four justifications themselves are all sound (module name from outside the
ident store, trailing-newline trim on presentation text, NUL-terminated dylib
path, ABI symbol dispatch at the host boundary) — this is about the mechanism,
not the calls.

### 3. (Performance) Every REPL expression is compiled twice

In `evaluate`:

```zig
.expression => |output| try results.append(arena, .{
    ...
    .value = try arenaDupe(arena, output),
    .type  = try expressionType(arena, session, statement),   // <- second full pass
```

`stepLanguageWithConfig(statement)` has already parsed, canonicalized, checked
and evaluated the expression. `expressionType` then calls
`session.inspectExpressionType(source, ...)`, which does the whole front end
again purely to recover a type the first pass already computed.

In a browser REPL where the user is typing, that's a straight 2× on the
dominant cost. The right fix is for `LanguageStepResult.expression` to carry the
rendered type alongside the value — `ReplSession` is being restructured in this
PR anyway, so this is the moment.

Same shape, worse complexity, for definitions:

```zig
fn definitionType(arena, session, name) {
    const items = try session.completionItems();   // enumerates ALL definitions
    ...
    for (items) |item| if (item.label == name) ...
}
```

`completionItems()` is called once per committed definition, and it walks the
whole session scope each time — so evaluating a cell with N definitions is
O(N²) in session size. `DefinitionCommit` already carries `name` and `kind`;
carrying the rendered type too would make this O(1).

Neither is a correctness bug, and neither will be visible at three definitions.
Both will be visible in a notebook with fifty cells, which is the use case the
demo is built around.

### 4. `@as(std.Io, undefined)` in `ensureSession`

```zig
const roc_ctx = CoreCtx.default(allocator, allocator, @as(std.Io, undefined));
```

This copies `src/playground_wasm/main.zig:1420`, so it's precedented rather than
novel — but it's still undefined behavior the instant anything reaches for I/O,
and "nothing in the REPL path does I/O" is an invariant nobody is checking. The
codebase already has the safe version of this: `src/shim_io.zig` builds vtables
from `std.Io.failing.vtable`. Passing a failing `std.Io` here converts a silent
UB jump into a clean `error.Unimplemented` at the exact call that violated the
assumption, at zero cost. Worth doing in both places.

### 5. Every unlisted error collapses to one opaque message

```zig
else => errorResponse(request.id, "internal_error", "The REPL could not complete this request."),
```

That `else` swallows `OutOfMemory`, `DefinitionTypeUnavailable`,
`ExpressionTypeUnavailable`, `ParseDiagnosticUnavailable`,
`UnexpectedEmptyResult`, `RevisionExhausted`, and every member of
`ReplStepError`. An embedder debugging why `eval` returned `internal_error`
gets no signal whatsoever, and neither will you when a bug report arrives.

`@errorName(err)` appended to the message (or as a separate `detail` field —
the protocol already has room) costs nothing and this is a developer tool, not
a service with an information-disclosure boundary.

Relatedly: `RequestError` declares `DefinitionTypeUnavailable`,
`ExpressionTypeUnavailable`, and `ParseDiagnosticUnavailable` as if they were
protocol-visible, but none of them is mapped in `processJson`. Either map them
to real codes or move them somewhere that makes clear they're internal.

## Things I verified and found correct

These are the places a reviewer should be suspicious, so I want to record that I
actually checked them rather than leaving silence:

- **Virtual module ownership.** `setModules` builds `ModuleSource` values whose
  `name`/`source` point into the **request arena**, which `processJson` frees on
  return. `VirtualModuleStore.append` (`ReplSession.zig:496-502`) dupes both
  with the session allocator, with correct `errdefer`s. No dangling. ✓
- **`replaceVirtualModules` failure atomicity.** It builds a complete
  `replacement` store and only swaps it in after every module validates, with
  `errdefer replacement.deinit`. A `DuplicateVirtualModule` on the third module
  leaves the session's existing modules untouched. ✓
- **The wasm ABI's length-prefix contract.** `storeResponse` allocates
  `len + 4`; `roc_repl_free_response` reads the prefix and frees
  `len + 4`. Matched. ✓
- **JS detached-buffer hazard.** `worker.js` constructs every `Uint8Array` view
  *after* the wasm call that could have grown memory
  (`roc_repl_alloc` → view, `roc_repl_process` → `readU32` → view). No stale
  `memory.buffer` reference is held across a growth point. ✓
- **Null-pointer failure paths.** `roc_repl_alloc`/`roc_repl_process` return `0`
  on failure and `worker.js` checks both (`if (!inputPtr) throw`,
  `if (!responsePtr) throw`). ✓
- **Revision semantics.** `clear` advances the revision only when something
  actually changed; expressions report the current revision without advancing;
  definitions advance. Consistent with the advertised
  `revision_scope: "wasm_instance"`. ✓
- **The echo output-path move** (`lib/echo.wasm` → `lib/echo/echo.wasm`) is
  complete: both `b.step` descriptions, the `build.zig` comment,
  `test/echo-wasm-test/main.zig`'s `wasm_path` **and** its "did you run…" hint
  (helpfully retargeted from `build-playground` to `build-echo-wasm`), and
  `echo_wasm_archive.zig`'s doc comment. I grepped for stragglers and found
  none. ✓
- **Default-step cost.** `b.default_step` moves from `build_playground_step` to
  `build_web_step`. Since the old playground step already pulled in
  `echo_wasm_install` and the echo www files (both removed here), the net
  addition to a plain `zig build` is just `repl.wasm`. Smaller than it looks. ✓

## Nits

- **`addImport("ReplSession.zig", ...)`** — every other module in the build is
  named without an extension (`base`, `can`, `eval`, `reporting`). A module
  named `ReplSession.zig` reads like a file path and will confuse the next
  person editing this block. Call it `repl_session`.
- **`.optimize = .ReleaseSmall` is hardcoded** for `repl_wasm`, so
  `-Doptimize=Debug` can't produce a debuggable REPL module. Given the module is
  new and will need debugging, consider honoring the user's optimize mode with
  `ReleaseSmall` as the default.
- **`complete`'s cursor validation is O(n) per keystroke.**
  `!std.unicode.utf8ValidateSlice(source[0..cursor])` re-validates the entire
  prefix on every completion request. A boundary check —
  `cursor == source.len or (source[cursor] & 0xC0) != 0x80` — is O(1) and
  actually more precise about what's being asserted.
- **`identifierPrefixStart` is ASCII-only.** It walks back over `[A-Za-z0-9_]`,
  so a non-ASCII identifier byte terminates the prefix early and completion
  starts from the wrong offset. Probably fine given Roc's identifier rules, but
  the function name promises more generality than it delivers — worth a comment
  saying identifiers are ASCII by construction.
- **`RevisionExhausted`** at `maxInt(u32)` is unreachable in any real session and
  currently reports as a generic `internal_error`. Harmless; noting only because
  it's declared in `RequestError` alongside errors that do matter.
