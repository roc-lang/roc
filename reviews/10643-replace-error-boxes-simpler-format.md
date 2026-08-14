# PR #10643 — Replace error boxes with a simpler format

- **Author:** ageron (Aurélien Geron) · **Draft:** no · **Base:** `main` · **Head:** `1abcdf7`
- **Size:** **+20,493 / −25,025 across 437 files** (9 source files; the rest are regenerated snapshots)
- **CI: red** — 6 of 7 `zig-minici` jobs failing

Replaces the box-drawing diagnostic layout with a header-line format:

```
── ✗ type mismatch ─────────────────────────── type_mismatch_example.roc:2:30
```

The author notes up front: *"I'm not a Zig programmer, this was written by an
agent. I scrolled through the code and didn't notice anything obvious, but there
might still be issues."* That's honest and useful, and it's why this review goes
deeper on mechanics than on taste.

## Verdict

**The motivation is completely sound.** Every complaint in the PR body is real:
the pipes wreck copy/paste, the box eats horizontal room, unicode width bugs
misalign the right wall, and trailing whitespace is invisible. This is a change
worth making, and the direction is right.

**But it is not mergeable as-is.** CI is red across six jobs, and I found a
concrete reason: the one unit test of the new terminal format asserts a string
the renderer cannot produce (#1). Beyond that there's a **functional regression
that undercuts the PR's own motivation** — long-line windowing was deleted, so
wide source lines now overflow narrow terminals *worse* than the box did (#2) —
a **silent user-visible feature removal** (#3), a **path-mangling hack promoted
into user-facing output** (#4), and seven now-dead declarations.

---

## Findings

### 1. (Blocking) The only unit test of the new format asserts a string the renderer never emits

`src/reporting/test.zig:75` at the PR head:

```zig
try testing.expect(std.mem.find(u8, plain_out, "-- ❌ SYNTAX PROBLEM") != null);
```

The renderer at the same SHA disagrees on all three components:

| the test expects | `renderer.zig` emits | where |
|---|---|---|
| `--` (two ASCII hyphens) | `──` (two U+2500 box-drawing) | `:520` `writer.writeAll("── ")` |
| `❌` (U+274C) | `✗` (U+2717) | `:170-174` `getSeverityIcon` |
| `SYNTAX PROBLEM` (upper) | `syntax problem` (lower) | `:524` `writeLowercased(writer, title)` |

`std.mem.find` cannot match, so this `testing.expect` fails. That is consistent
with `zig-minici` being red on macos-core, macos-harness, macos-zig,
ubuntu-full, windows-core, and windows-zig.

The likely story is that the icon and letter-casing were iterated on after the
test was written and the test was never re-run. Worth checking whether other
assertions in the same file drifted the same way — the two neighbours
(`example.roc:1:10` and `^^`) do look correct against the current renderer.

Note the snapshots are *not* affected by this: all 437 were regenerated from the
current renderer, so they're internally consistent. It's specifically the
hand-written test — the one place the intended format is stated rather than
recorded — that's out of sync.

### 2. (Blocking) Long-line windowing was deleted, which makes the narrow-terminal problem worse

The PR body's second bullet is:

> It takes a lot of space, both vertically and horizontally. If the code is
> already 80 chars wide, it will overflow on a small terminal window.

The old renderer handled exactly this: `windowSourceLine` clipped a too-wide
source line to the available width, centred on the underlined span, with `…`
ellipses on the clipped sides, and mapped the underline's byte offsets into the
window so it still lined up.

The new renderer prints the whole line, unconditionally:

```zig
for (code_line) |ch| try writer.writeByte(if (ch == '\t') ' ' else ch);
try writer.writeByte('\n');
```

`windowSourceLine`, `byteAtDisplayCol`, and `CodeWindow` are all still *defined*
(their doc comments were even updated — "too wide for the terminal") but nothing
calls them. So a 400-column line in the source now dumps 400 columns to an
80-column terminal and hard-wraps, dragging the `^^^^` underline out of
alignment with the span it marks.

The box format at least clipped. This is a regression against the stated goal,
and it's the one thing the box did *better*.

The windowing code is still there and still correct — it should be reconnected.
The header format actually makes it easier (there's no right wall to reserve
columns for), so `avail` becomes just the terminal width.

### 3. (Should fix) `roc check` silently stopped reporting elapsed time

`finishRocCheck` previously printed, on the error path:

```zig
stderr.writeAll(" in ") catch {};
formatElapsedTimeMs(stderr, elapsed) catch {};
stderr.print(" for {s}.\n", .{args.path}) catch {};
```

The replacement `renderSummaryHeaderLine` takes no duration and prints none.
`elapsed` survives only because line 16206 still uses it on the success path, so
there's no compile error — the timing just vanishes from the failure output.

Removing a user-visible feature is fine if intended, but it isn't mentioned
anywhere in the PR body, and "how long did that take" is exactly the thing you
want when a check is slow *and* failing. Either put it back in the header line
or call the removal out.

### 4. (Should fix) A test-only path hack is now applied to user-facing output

`sanitisePathForSnapshots` carries this doc comment, unchanged by this PR:

```zig
/// TODO find a better solution this is temporary to make CI happy
```

It rewrites any path containing `/snapshots/` down to its bare filename. The PR
makes it `pub`, re-exports it from `reporting/mod.zig`, and calls it from **both**
`renderHeaderLine` and the CLI's `renderSummaryHeaderLine`.

So a user who happens to have a project directory containing `snapshots/` now
gets their real file path silently truncated to the basename in compiler errors
— in an IDE-clickable position, no less. The hack existed to keep snapshot
`PROBLEMS` sections machine-stable; widening it to every rendered path (and to
the CLI summary line, which never went through it before) exports a test
artifact into the product.

The fix is to keep the sanitisation at the snapshot-tool boundary rather than
inside the renderer — which is what "find a better solution" was pointing at.

### 5. (Should fix) Seven dead declarations left behind

Verified against the head SHA (occurrence counts include the definition itself):

| symbol | file | status |
|---|---|---|
| `windowSourceLine` | `renderer.zig:476` | never called |
| `byteAtDisplayCol` | `renderer.zig:451` | only from `windowSourceLine` |
| `CodeWindow` | `renderer.zig` | only as `windowSourceLine`'s return type |
| `padTo` | `renderer.zig:395` | never called |
| `box_underline` | `renderer.zig:157` | never read — see below |
| `expectMultilineEqual` | `test.zig:97` | both call sites replaced with `testing.expectEqualStrings` |
| `printAsMultilineString` | `test.zig:104` | only caller was the old `expectMultilineEqual` body |

Zig doesn't error on unused top-level declarations, so none of this fails the
build — but it means the file no longer describes what it does.

`box_underline` deserves a special mention: it's declared as `const box_underline = "^"`,
and the underline is written as `writer.splatBytesAll("^", ulen)` — a **literal**,
not the constant. So the named constant is decorative; changing it would do
nothing. That's the kind of thing that costs someone an hour later.

`expectMultilineEqual` is worse than merely dead: the PR **rewrote its body**
(replacing the copy-paste-ready debug dump with a plain print) while removing
both of its callers. Either restore it as the helper for the two assertions that
now use `expectEqualStrings`, or delete it and `printAsMultilineString` together.

### 6. (Design) `getSeverityIcon` dispatches on a title string

```zig
if (std.mem.eql(u8, title, "FAIL")) return .{ .icon = "✗", .color = red, .width = 1 };
return switch (severity) {
    .fatal, .runtime_error => .{ .icon = "✗", .color = red, .width = 1 },
    ...
```

Two problems:

- **It appears to be a no-op.** The `"FAIL"` branch returns byte-for-byte what
  `.fatal`/`.runtime_error` already return. It only does anything if a report
  titled `FAIL` carries severity `.warning` or `.info` — and if that's the case,
  the bug is the severity, not the icon.
- **Reaching for the title string to decide presentation** is the pattern the
  codebase spends real effort avoiding elsewhere (see the tidy bans on `eql` in
  checker-adjacent code, and AGENTS.md on consuming explicit data from earlier
  stages). The severity enum is right there.

Delete the special case, or fix the severity at the source and delete it.

### 7. (Design) The header line is implemented twice

`renderer.zig:renderHeaderLine` and `cli/main.zig:renderSummaryHeaderLine` are
independent implementations of the same visual element, sharing by copy:

- `const total_w = @min(config.getMaxLineWidth(), 120);` (same magic 120)
- `const dashes = if (total_w > prefix_w + loc_w) (total_w - prefix_w - loc_w) else 5;` (same magic 5)
- `"── "` prefix, `splatBytesAll("─", dashes)`, dim-gray/reset handling

They will drift — and the CLI one already uses `src/cli/ansi_term.zig` constants
while the renderer uses `style.zig`'s `AnsiCodes`, so two color vocabularies are
in play for one line. This should be one exported function in `reporting`
parameterized by the left-hand content.

While there: the `else 5` fallback means that when the title plus location
exceed the width, you get exactly five dashes and the line overflows anyway.
That's a silent degradation with no comment explaining the choice.

### 8. (Nit) `renderSummaryHeaderLine` signature and error handling

- `writer: anytype` with an inferred `!void`, while its immediate neighbour
  `writeDiagnosticCounts` is properly typed
  (`writer: *std.Io.Writer … std.Io.Writer.Error!void`). The `anytype` gives up
  compile-time checking for no benefit — there's one call shape.
- `std.fmt.bufPrint(&error_buf, …) catch ""` silently degrades to an empty
  string. `[32]u8` is provably large enough (`"18446744073709551615 errors"` is
  27 bytes), so `catch unreachable` states the invariant instead of hiding a
  truncation that can't happen.
- Both call sites use `renderSummaryHeaderLine(...) catch {}`, discarding write
  errors. That matches the surrounding style, so no objection — just noting the
  `!void` return is decorative.

### 9. (Nit) `writeColoredSummary` now string-compares per byte

```zig
const color_to_write = if (c.len == 0 or std.mem.eql(u8, c, palette.reset)) ... else c;
```

This runs inside the per-byte loop. It's guarded by the `cp != cur`
pointer-identity check so it only fires on color transitions, not every byte —
but comparing color codes by *content* when the surrounding code compares them
by *pointer identity* (`@intFromPtr(c.ptr)`) mixes two notions of equality in
one function. If the intent is "is this the reset color," a pointer comparison
against `palette.reset.ptr` matches the existing convention and is O(1).

### 10. (Nit) `assertValidHeadline` weakened

The debug assertion now accepts a headline ending in `:` as well as `.`, on the
grounds that a colon introduces a code block. That's reasonable, and the four
call-site edits in `Diagnostic.zig` / `ModuleEnv.zig` (`"… here."` → `"… here:"`)
are consistent with it.

Two small things: the doc comment says the headline must read as "a complete
sentence (ending in a period) or introduce a code block (ending in a colon)",
but nothing checks that a colon-ended headline is actually followed by a code
block — so in practice the invariant is now "ends in `.` or `:`". Say that.
And only four headlines were converted; if trailing-colon-introduces-the-snippet
is the new house style for region-bearing reports, the other several hundred
are inconsistent with it.

## Process notes

- **437 files is going to conflict with everything.** #10686 touches 8 of these
  snapshots, and any other PR regenerating snapshots will too. Whatever order is
  chosen, the smaller PRs should go first and this one should be rebased and
  regenerated — a merge of 20k lines of snapshot diff is not reviewable.
- **The snapshot diff is unreviewable by construction**, which is fine — the
  right thing to review is the renderer plus a handful of representative
  snapshots. Two or three "before/after" examples in the PR body (the body has
  one screenshot) covering a multi-line region, a very wide line, and a report
  with no region would let a reviewer sanity-check the layout without opening
  437 files. The wide-line case in particular would have surfaced finding #2.
- **Reports with no region** take `renderReportPlainFallback`, and the PR removed
  the trailing `writer.writeByte('\n')` on that path while `renderHeaderLine`
  moved the leading newline. Worth confirming that consecutive
  region-less reports still get sane separation; nothing in the remaining unit
  tests covers it.
