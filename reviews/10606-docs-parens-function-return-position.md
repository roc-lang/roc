# PR #10606 — Add parens to funcs in return position of docs rendered type annotations

- **Author:** jrrrp (Jonathan Paynter) · **Draft:** no · **Base:** `main`
- **Size:** +7 / −7 in one file (`src/docs/render_html.zig`)

Flips `needs_parens` from `false` to `true` at seven recursion sites: function
return type, tuple elements (multi- and single-line), type-application arguments
(both), and tag payloads (both). Fixes docs emitting `a -> b -> SqliteStmt => Try(…)`
and `Foo(Dec, Dec -> Dec)` — both of which re-parse as something other than the
type they describe.

## Verdict

**The bug is real, the diagnosis is right, and the fix works.** I traced
`needs_parens` through the renderer and it is consumed in exactly one place —
the `.function` arm (`render_html.zig:2269`/`2290`) — so setting it `true` adds
parens *only* when the child actually is a function. Nothing else is affected.

Three things before merge: **record fields have the same bug and aren't fixed**
(#1), **no test** (#2), and the redundant parens the PR body apologizes for are
avoidable with a two-line predicate rather than being an inherent cost (#3).

---

## Findings

### 1. (Should fix) Record field types have the identical bug and are untouched

Both record-field sites still pass `needs_parens = false`:

```zig
// multi-line
try frames.append(gpa, .{ .doc_type = .{
    .value = field.type,
    .needs_parens = false,      // <- unchanged
    .indent = item.indent + 1,
} });
try frames.append(gpa, .{ .html = " : " });
```

…and the same in the single-line branch. So

```roc
handler : { on_event : Dec, Dec -> Dec }
```

renders as `{ on_event : Dec, Dec -> Dec }`, which re-parses as a field
`on_event : Dec` followed by a stray `Dec -> Dec` — exactly the failure mode the
PR describes for tag payloads, in the construct users hit most often (callbacks
in config records).

This is the same one-line change as the other six. It should be in this PR;
leaving it means the next person has to rediscover the whole analysis.

`rec.ext` and `tu.ext` correctly stay `false` — an extension is a type variable
and can't be a function. No change needed there.

### 2. (Should fix) No test, and the harness is already there

`render_html.zig` has nine tests, several aimed at exactly this function —
including `test "renderDocTypeHtml includes the unit argument for zero-argument
functions"` and `test "renderDocTypeHtml expands trailing-comma collections and
their parents"`. Adding a case is cheap and there's an obvious model to copy.

Two assertions would lock the whole PR down:

- `f : a -> (b -> c)` — the headline case from the Zulip thread; assert the
  parens are present in the return position.
- `Foo(Dec, Dec -> Dec)` rendering as `Foo((Dec, Dec -> Dec))` — the payload
  case, which is the one where the *absence* of parens changes the type's
  meaning rather than just its validity.

Without a test, the next refactor of the frame walker silently reverts this.

### 3. (Worth doing) The redundant parens are avoidable, not inherent

The PR body accepts a cost:

> although it now leads to redundant parentheses in the docs with unary
> functions in tags and tuples
> ```
> Bar(
>     (a -> a), # Extra level of nested parens
> ),
> ```

That's a real regression in output quality, and it doesn't have to be paid. The
ambiguity in comma-separated positions comes entirely from a **top-level comma
in the function's argument list**. So:

| function shape | rendered | ambiguous in a comma position? |
|---|---|---|
| 0 args | `() -> r` | no — the comma-free `()` is a token |
| 1 arg | `a -> r` | no |
| ≥ 2 args | `a, b -> r` | **yes** |

The return position is different: `->` doesn't chain, so `a -> b -> c` is always
wrong and always needs parens.

That gives a precise rule:

```zig
/// A function only needs parens in a comma-separated position when its own
/// argument list contributes a top-level comma.
fn needsParensBetweenCommas(v: *const DocType) bool {
    return v.* == .function and v.function.args.len >= 2;
}
```

Use it at the six comma-position sites (tuple ×2, apply ×2, tag payload ×2, plus
record fields from #1) and keep the unconditional `true` at the function-return
site. Output becomes:

- `Foo((Dec, Dec -> Dec))` — parens, correct
- `Bar(a -> a)` — no parens, unchanged from today
- `a -> (b -> c)` — parens, correct

No redundancy, and the rule is stated once instead of being an emergent property
of seven boolean literals.

If that's more than this PR wants to take on, at minimum leave a comment at one
of the sites recording *why* `true` is unconditional, so the redundancy reads as
a deliberate trade rather than an oversight.

### 4. (Informational) `src/docs/render_type.zig` encodes the same seven decisions, with the old values

`renderTypeAnno` is a second, independent Roc-syntax type renderer with a
`needs_parens` parameter that means the same thing and is likewise consumed only
by its `.@"fn"` arm (`render_type.zig:137`/`149`). Its recursion sites:

| site | `needs_parens` | matches this PR? |
|---|---|---|
| `.@"fn"` args (`:72`… line 138) | `true` | ✓ (already correct in both) |
| `.@"fn"` **return** (`:145`) | `false` | ✗ |
| `.tag` args (`:105`) | `false` | ✗ |
| `.tuple` elems (`:117`) | `false` | ✗ |
| `.apply` args (`:72`) | `false` | ✗ |
| `.record` field types (`:128`) | `false` | ✗ (and see #1) |

I checked whether this is a live second bug: `renderTypeAnno` is only reachable
from `renderTypeHeader`/`renderTypeHeaderToString`, called from
`extract.zig:370,422,888` to render type *headers* like `Maybe(a)`, whose
arguments are type variables. So in practice a function can't appear there and
the bug can't manifest today.

Still worth flagging: two files now hold contradictory answers to the same
question, and the moment `renderTypeAnno` is used for anything other than
headers, the bug is back. Either fix it in the same pass (six one-word changes)
or add a comment on `renderTypeAnno` saying it's header-only and why its paren
handling therefore doesn't matter.

## Verification notes

- **`needs_parens` is only consumed by `.function`.** I grepped all 26
  occurrences: two are the paren emissions inside the `.function` arm, two
  forward `item.needs_parens` through the `where`-clause wrapper (correct — a
  `where` clause is transparent for parenthesization), and the rest are
  assignments at recursion sites. No other variant reads it. So this change
  cannot add parens around a record, tuple, tag union, or type reference. ✓
- **Function *argument* positions already passed `true`** (`:2284`) before this
  PR, so the fix is consistent with the pre-existing convention rather than
  inventing one. ✓
- **The `.parens` variant** in `render_type.zig` emits its own parens
  unconditionally, so an explicitly-parenthesized source annotation is
  unaffected either way.
