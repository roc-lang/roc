# One Report Renderer, Four Styles

## Problem

Diagnostics have a single document model (`DocumentElement` /
`Annotation` in `src/reporting/document.zig:152`) but four complete,
independent renderers of it: `renderElementToTerminal`
(`src/reporting/renderer.zig:1115`), `renderElementToMarkdown`
(`:1396`), `renderElementToHtml` (`:1608`), and `renderElementToLsp`
(`:1738`). Each is a full switch over the ~16 element variants, and
the `.annotated` arm additionally switches over the ~22 annotation
variants. The layout logic that should be target-independent —
wrapping, indentation, and above all the source-region
underline/caret drawing — is re-implemented per target, and has
already diverged in small ways (the terminal path preserves tabs via
`printLeadingWhitespace` in `src/reporting/source_region.zig:121`;
the markdown path re-implements its own whitespace loop inline at
`renderer.zig:1520-1532`).

Satellite duplications orbit the same model:

- `getAnnotationColor` (`renderer.zig:1368-1392`) is a byte-for-byte
  duplicate of `ColorPalette.colorForAnnotation`
  (`src/reporting/style.zig:221-245`). The terminal renderer calls
  the local copy; `style.zig`'s copy serves `wantsBacktick` and its
  own unit tests. Two exhaustive switches, one fact.
- `Annotation.semanticName` (`document.zig:123-147`) is a third full
  per-variant switch (CSS class names for HTML).
- Title uppercasing exists twice: `writeShouted`
  (`renderer.zig:331-335`) and the snapshot tool's `asciiUpperDupe`
  (`src/snapshot_tool/main.zig:376-380`), the latter commented
  "matching the box renderer" — a manual-sync contract in place of a
  shared function.
- `ColorPalette.HTML` / `HtmlColors` (`style.zig:55-81, 193-218`)
  define an HTML color scheme the HTML renderer never uses — it emits
  `class="<semanticName>"` and the real colors live in
  `src/snapshot_tool/snapshot.css` (a different palette). One side is
  dead code posing as a source of truth.

Adding one document element or annotation today means editing four
renderer switches, two color switches, `semanticName`, and possibly
the CSS — with only Zig's switch exhaustiveness (where no `else` has
crept in) as the net, and nothing at all guarding behavioral
agreement between targets.

## Background

The model/renderer split is already the right architecture: problems
from every phase are built into `Report`s once (`src/check/report.zig`,
`src/canonicalize/Diagnostic.zig`, `src/parse/AST.zig`), and
`RenderTarget` (`renderer.zig:37`) selects output. The four renderers
differ in exactly three dimensions: how an annotation opens/closes
(ANSI codes vs backticks vs tags vs nothing), how structure is
escaped/indented, and how a source region is drawn. Everything else —
element traversal order, wrapping decisions, region math — is logic
that should exist once. `source_region.zig` already contains partial
shared helpers; the markdown renderer just doesn't use all of them.

## Evidence

- The four `renderElementTo*` functions and the triple dispatch
  switches at `renderer.zig:81, 92, 1066`.
- `renderer.zig:1368` vs `style.zig:221` — identical arms, verifiable
  by diff.
- `renderer.zig:1520-1532` — markdown's inline whitespace loop beside
  the terminal path's shared helper.
- `style.zig:347-353` — tests pin only 4 of ~22 color arms.
- `grep -n 'ColorPalette.HTML' src/` — no renderer consumer.

## Solution design

1. **Delete the trivial duplicate first.** Remove
   `getAnnotationColor`; the terminal renderer calls
   `palette.colorForAnnotation`. Export one `shout` helper from
   `reporting` and delete `asciiUpperDupe`.
2. **Style vtable.** Define a per-target style description — for each
   annotation: open/close strings (or a fn for ANSI/palette lookup);
   plus escaping fn, indent unit, underline glyphs, and a
   source-region drawing hook. The four targets become four instances
   of that description.
3. **One walker.** A single `renderElement` traverses the document,
   consulting the style instance. Target-specific structure that is
   genuinely different (LSP's related-information shape, HTML's tag
   nesting) stays in per-target hooks, but the hooks receive
   already-computed layout (wrapped lines, underline spans) from
   shared code — the region math exists once in `source_region.zig`.
4. **Resolve the HTML palette.** Either delete
   `ColorPalette.HTML`/`HtmlColors`, or make it the generator for
   `snapshot.css`'s annotation classes so the palette and the CSS
   cannot disagree. Pick one; do not keep both.
5. **Behavioral parity net.** A table-driven test renders a corpus of
   reports (one exercising every element and annotation variant) to
   all four targets and asserts target-independent invariants: same
   visible text content after stripping markup, same line count for
   source regions, same underline column spans. This is the guard
   that outlives the refactor.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- Exactly one traversal of `DocumentElement` exists; per-target code
  is style data plus hooks, not switches over the model.
- `getAnnotationColor` and `asciiUpperDupe` are gone;
  `grep -rn 'fn getAnnotationColor\|asciiUpperDupe' src/` is empty.
- Adding a new annotation variant requires: one `semanticName` arm,
  one entry per style instance (compile-enforced), and nothing else.
- The HTML color story has one source (deleted or generating the CSS).
- The parity test corpus covers every variant (comptime-enumerated so
  a new variant fails the test until covered) and passes.
- `zig build snapshot` output is unchanged (`git diff test/snapshots`
  empty), and the terminal output of `roc check` on a fixture set is
  byte-identical before/after.

## How to evaluate the result

### Correctness ideal

Target divergence becomes impossible for shared concerns (wrapping,
regions, underlines) because the code exists once, and detectable for
per-target concerns because the parity suite enumerates variants from
the type. The markdown/terminal whitespace divergence class cannot
recur.

### Performance ideal

Rendering is not hot (it runs per diagnostic), but the refactor should
be allocation-neutral: the walker writes through the same
`*std.Io.Writer` interface; style lookups are comptime-resolvable
per target. Verify no regression by timing snapshot generation over
the corpus before/after (should be within noise).

## Tests to add

- The four-target parity suite (comptime-enumerated variant
  coverage).
- Byte-identity pins for a small fixture set per target, regenerated
  intentionally when output is meant to change.
- A test that `snapshot.css` contains a class for every
  `semanticName` (or is generated — then this is free).

## Related projects

- [../small/severity-and-report-collection.md](../small/severity-and-report-collection.md)
  — the classification and orchestration seams around the same report
  pipeline; independent, but lands naturally before or after this.
- [../small/silent-drift-guards.md](../small/silent-drift-guards.md)
  — the parity-suite pattern this project applies to renderers.
