# PR #9923 — Add `--rpc` option for `roc repl`

- **Author:** ageron (Aurélien Geron) · **Draft:** yes · **Base:** `main`
- **Size:** +528 / −1 across 2 files (`src/cli/cli_args.zig`, `src/cli/main.zig`)

Adds `roc repl --rpc`, a line-delimited JSON server over stdin/stdout with six
methods (`repl.start`/`stop`/`reset`/`evaluate`/`autocomplete`/`inspect`) and
multi-session support.

## Verdict

Useful capability, and the session model plus the stdout-interception idea are
the right shape for an editor/notebook integration.

**But there's a genuine crash** in the stdout interception — it misreads Zig's
`splat` contract and can index out of bounds on ordinary padded output (#1) —
plus a protocol that calls itself JSON-RPC without being JSON-RPC (#2), an
unknown method that reports success (#3), a stub method advertised as working
(#4), zero tests, and ~120 lines of copy-pasted response boilerplate.

It's a draft, so most of this is "before it comes out of draft" rather than
"blocking merge."

---

## Findings

### 1. (Crash) The write-interception hooks misuse `splat`

Both `customFileWritePositional` and `customOperate` do:

```zig
buf.appendSlice(alloc, header) catch ...;
var total = header.len;
for (data[0..splat]) |slice| {
    buf.appendSlice(alloc, slice) catch ...;
    total += slice.len;
}
return total;
```

Zig 0.16's contract is different (`std/Io/Writer.zig:33-34`):

> The last element of `data` is repeated as necessary so that it is written
> `splat` number of times, which may be zero.

So `splat` is a **repeat count for the final element**, not a length of `data`.
All of `data[0 .. data.len-1]` must be written once, and `data[data.len-1]` must
be written `splat` times. The consequences of reading it as a length:

| case | correct behavior | what this code does |
|---|---|---|
| `data.len == 1`, `splat == 1` | write the one slice | ✓ accidentally right |
| `data.len > 1`, `splat == 1` | write every slice | **drops `data[1..]`** — intercepted output silently truncated |
| `splat == 0` | write `data[0..len-1]`, skip the last | **drops everything** |
| `splat > data.len` | write the last slice `splat` times | **out-of-bounds slice → panic** |

That last row is reachable in practice. `Writer.splatByteAll` / `splatBytesAll`
produce exactly `data = [one_slice]` with a large `splat` — and the diagnostic
renderer uses them constantly for padding and rules
(`splatByteAll(' ', …)`, `splatBytesAll("─", dashes)`). So a `repl.evaluate`
whose result is a *formatted diagnostic* can crash the RPC server in a safe
build.

The returned `total` is also wrong (it should count the header plus every slice
plus the repeats), which will confuse the calling `Writer`'s accounting even
when it doesn't crash.

Correct form:

```zig
try buf.appendSlice(alloc, header);
var total = header.len;
for (data[0 .. data.len - 1]) |slice| {
    try buf.appendSlice(alloc, slice);
    total += slice.len;
}
const last = data[data.len - 1];
for (0..splat) |_| {
    try buf.appendSlice(alloc, last);
    total += last.len;
}
return total;
```

(`data.len` is guaranteed nonzero by the contract.)

### 2. (Should fix) It's called JSON-RPC but isn't JSON-RPC

The help text says:

```
--rpc        Start in JSON-RPC server mode
```

The protocol has no `"jsonrpc": "2.0"` field, no `error` object, and responses
use a different ad-hoc shape per method (`{session_id, id}`, `{success, id}`,
`{result, stdout, diagnostics, id}`, `{matches, id}`, `{type, id}`,
`{status, id}`). A client written against JSON-RPC 2.0 will not interoperate.

Either implement JSON-RPC 2.0 properly — `jsonrpc`/`id`/`result`/`error`, with
the standard error codes (`-32601 Method not found`, `-32602 Invalid params`) —
or rename the flag and help text to something honest like "line-delimited JSON
protocol." The former is better: editors and notebook frontends already have
JSON-RPC clients, which is presumably the point of the feature.

Note PR #10645 defines a *different* JSON protocol for the wasm REPL
(`protocol` version field, `ok`/`error` envelope, typed error codes,
`capabilities` handshake). Two REPL JSON protocols with different shapes would
be an unfortunate outcome. Worth converging — #10645's envelope is the more
thought-through of the two, and it already ships a `protocol.d.ts`.

### 3. (Should fix) An unknown method returns success

```zig
.unknown => {
    const Response = struct { status: []const u8, id: ?std.json.Value };
    const response = Response{ .status = "ok", .id = maybe_id };
```

A client that typos `repl.evalute` gets `{"status":"ok"}` and no indication
anything went wrong. This should be an error response. Same class of problem:
`repl.evaluate` against a nonexistent session returns
`diagnostics: "Session not found"`, which is indistinguishable from a real
compile diagnostic — a client cannot tell "bad session id" from "your code
doesn't type-check."

Both go away with a proper `error` object (#2).

### 4. (Should fix) `repl.inspect` is a stub but is documented as supported

```zig
.@"repl.inspect" => {
    const Response = struct { @"type": ?[]const u8, id: ?std.json.Value };
    const response = Response{ .@"type" = null, .id = maybe_id };
```

It ignores its params entirely and always returns `null`. The PR body lists it
among the supported methods with no caveat. Either implement it (`ReplSession`
has the machinery — #10645's wasm REPL implements exactly this via
`inspectExpressionType`) or drop it from the method list until it works.

### 5. (Should fix) ~120 lines of duplicated response plumbing

This exact block appears **seven times**, verbatim:

```zig
std.json.Stringify.value(response, .{}, writer) catch |err| {
    std.debug.print("RPC Server JSON stringify error: {any}\n", .{err});
    return error.CliError;
};
writer.writeByte('\n') catch |err| { ... };
writer.flush() catch |err| { ... };
```

One `fn sendResponse(writer: *std.Io.Writer, value: anytype) !void` would remove
about a quarter of the function and make the seven handlers readable as a
dispatch table rather than a wall. This is the single highest-value cleanup here.

### 6. (Convention) ~20 uses of `std.debug.print`

Every error path logs with `std.debug.print` rather than going through
`ctx.io.stderr()` like the rest of the CLI. Two problems: it bypasses the `Io`
abstraction the whole file is built on (and which this feature itself patches for
interception), and `std.debug.print` is discouraged in this tree because it
breaks the wasm playground build.

Since these are diagnostics *about* the RPC channel, they belong on stderr — but
via the context's writer, so a caller can capture them.

### 7. (Bugs on the OOM path) Two session-lifecycle issues

**`repl.reset` can leave a dangling session in the map:**

```zig
if (sessions.get(sid)) |session| {
    session.deinit();
    session.* = try ReplSession.init(allocator, std_io, backend_kind);
```

If `init` fails, the `try` propagates and the map still holds a pointer to a
**deinited, now-uninitialized** `ReplSession`. The next `repl.evaluate` on that
id, or the cleanup loop at function exit, operates on freed memory. Remove the
entry before reinitializing, or build the new session into a temporary and swap
only on success.

**`repl.start` leaks on a failed `put`:**

```zig
const session = try allocator.create(ReplSession);
errdefer allocator.destroy(session);
session.* = try ReplSession.init(allocator, std_io, backend_kind);
try sessions.put(session_id, session);
```

The `errdefer` frees the allocation but never calls `session.deinit()`, so an
OOM in `put` leaks everything the session allocated. Add
`errdefer session.deinit();` after the `init`.

### 8. (Robustness) A too-long line kills the server

`reader.takeDelimiter('\n')` with a 1 MiB buffer returns an error for a longer
line, and the handler does `return error.CliError` — the whole server exits. For
a protocol that transports source code, that's a plausible client mistake that
should produce an error *response*, not a shutdown. Same for a JSON parse
failure on a well-formed-but-huge message.

### 9. (Design) The interception plumbing is fragile

- **Threadlocal globals** (`current_intercept_buffer`, `current_allocator`,
  `current_original_io`) plus a patched vtable is a lot of machinery to capture
  stdout. It works because the window is a single synchronous block, but it means
  the hook functions are only correct when called from inside that window.
- **`const orig = current_original_io orelse unreachable;`** — `unreachable` on a
  path whose reachability depends on a threadlocal being set. If the vtable is
  ever reached outside the window (a deferred flush, a background task), that's a
  panic in safe builds and UB in release.
- **`isStdoutOrStderr` intercepts stderr too**, and the captured bytes are
  reported to the client in a field named `stdout`. Either separate them or
  rename the field.
- **Hardcoded `file.handle == 1 or == 2`** on POSIX. Correct for the normal case;
  worth a comment saying so rather than leaving bare magic numbers next to a
  Windows branch that does look up the real handles.
- `customFileWritePositional` **ignores `offset`** and reports success as though
  a positional write occurred.

An alternative worth considering: rather than patching the vtable, give
`ReplSession` an explicit "capture output here" sink. #10645 does something like
this with `session.takeEvents()`, which is both simpler and testable.

### 10. (Missing) No tests, no documentation

Zero tests for a new user-facing protocol — not even a round-trip of
`repl.start` → `repl.evaluate` → `repl.stop`. `src/cli/test/parallel_cli_runner.zig`
is the natural home and already runs the CLI end-to-end.

And the protocol is documented only in the PR description. If this lands, the
method list, param shapes, and response shapes need to live in the repo — a
`docs/` page or a doc comment on `runRpcServer`. (#10645's `protocol.d.ts` is a
good model.)

## Smaller notes

- **`generateUuid` is correct.** Version nibble (`0x40`) and RFC 4122 variant
  bits (`0x80`) are set properly, and the hyphen positions produce the 8-4-4-4-12
  layout in exactly 36 bytes. ✓
- **`Method` enum is declared inside the `while` loop.** No runtime cost, but it
  belongs at file scope next to the other CLI types — and `fromString` is a chain
  of `mem.eql` where `std.meta.stringToEnum` would do (the enum members are
  already spelled `@"repl.start"` etc., so the mapping is mechanical).
- **`getPrefixAtCursor` is ASCII-only** (`isAlphanumeric or '_'`), same
  limitation as #10645's `identifierPrefixStart`. Consistent, at least.
- **`repl.autocomplete` only matches session definitions**, not builtins or
  imported names, and returns `[]` when the prefix is empty. Reasonable v1, but
  the PR body presents it without that caveat.
- **Merge conflict warning:** PR #10645 restructures `ReplSession` substantially
  (`initVirtual`, `CoreCtx`, a presentation/language split, `takeEvents`). This
  PR's `ReplSession.init(allocator, std_io, backend_kind)` and `session.io`
  field access will both need rework afterward. Given #10645 also defines a REPL
  JSON protocol, coordinating the two (see #2) would save doing this twice.
