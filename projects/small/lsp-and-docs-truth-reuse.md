# LSP and Docs Reuse Compiler Truth

## Problem

The LSP server and the docs generator re-derive several facts the
compiler already owns, and one of the copies has already visibly
drifted:

1. **Doc-comment gathering has forked (observable divergence).** The
   character-level predicates are shared (`src/base/doc_comment.zig`
   deliberately exposes strict `isDocCommentLine`, which excludes
   `###`, and permissive `startsWithHashHash`, which includes it) —
   but the multi-line *gathering policy* is implemented twice with
   different choices: LSP hover
   (`src/lsp/doc_comments.zig:25-89`) uses the strict predicate, so a
   `###` section header terminates the block, and additionally skips
   type-annotation lines; docs generation
   (`src/docs/extract.zig:133-190`) uses the permissive predicate, so
   `###` lines are folded into the doc text, with its own
   blank-line policy. The same source yields different documentation
   in hover versus the generated site today.
2. **Byte-offset→line/column exists three times.** Canonical:
   `findLineStarts`/`RegionInfo` (`src/base/CommonEnv.zig:60`,
   `src/base/RegionInfo.zig:23-99`), stored on `ModuleEnv`. The LSP
   has two more: `src/lsp/line_info.zig:80` (`computeLineStarts` +
   binary-search `positionFromOffset`) and
   `src/lsp/position.zig:20-46` (`buildLineOffsets` + linear-scan
   `offsetToPosition`) — and is internally inconsistent, since
   `position.zig:60`'s `positionToOffset` *does* reuse the
   `ModuleEnv` table while its neighbor rebuilds its own. Off-by-one
   fixes (EOF, `\r\n`) must land three times.
3. **The semantic-token legend is a positional pair.**
   `SemanticType` enum with explicit indices
   (`src/lsp/semantic_tokens.zig:31-44`) and the `TOKEN_TYPES` string
   array sent to clients (`src/lsp/capabilities.zig:5-18`) must agree
   in order; the only tie is a comment. A reorder mislabels every
   affected token in the editor with no error anywhere.
4. **The completion builtin-type list is a manual copy.**
   `src/lsp/completion/builtins.zig:9-38` hardcodes the builtin type
   roster that `CIR.builtin_type_specs` already owns (hash-validated
   against the compiled `Builtin` module); a new builtin type just
   silently never completes.

## Background

The LSP already consumes shared single sources where they were
offered — `Token.Tag.highlightCategory` drives its semantic-token
classification, and diagnostics go through the shared report
constructors — so these four are omissions, not architecture. For
item 1, the `base/doc_comment.zig` module's own doc comment
documents the strict/permissive split, which means the divergence is
half-intentional at the predicate level; what was never decided is
which policy is *the* policy for gathered doc blocks.

## Evidence

- `src/lsp/doc_comments.zig:61` (strict) vs `src/docs/extract.zig:164`
  (permissive) — feed both a def preceded by `## a`, `### b`, `## c`
  and observe different outputs.
- The two LSP line-table builders cited above, plus
  `position.zig:60`'s use of the canonical table.
- `semantic_tokens.zig:31` — "indices matching TOKEN_TYPES in
  capabilities.zig" as the only enforcement.
- `completion/builtins.zig:9` — no reference to the registry.

## Solution design

1. **Decide the doc-block policy once, then share the gatherer.**
   Settle the `###` question (and the blank-line and
   annotation-skip policies) as a language-level decision; implement
   one `gatherDocCommentBlock(source, offset)` in
   `src/base/doc_comment.zig`; LSP and docs both call it. If hover
   legitimately wants to stop at section headers while the site
   renders them, that is a *parameter* of the one gatherer, stated at
   the call sites — not two scanners.
2. **Delete both LSP line tables.** LSP position math uses
   `ModuleEnv.getLineStartsAll()`/`RegionInfo` throughout (it already
   has the env in hand at `position.zig:60`); `line_info.zig`'s
   builder and `position.zig`'s builder are removed. Where the LSP
   needs line info for unsaved buffer text with no `ModuleEnv`, it
   calls the base `findLineStarts` rather than a private copy.
3. **Derive the legend.** `TOKEN_TYPES` becomes a comptime map over
   `@typeInfo(SemanticType)` `@tagName`s (with the LSP-spec casing
   applied), or at minimum a comptime assert that
   `TOKEN_TYPES[@intFromEnum(t)]` matches each variant.
4. **Derive the completion roster.** Build the builtin-type completion
   list from the loaded builtin module's exposed types (or comptime
   from `builtin_type_specs`), deleting the hand list.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- One doc-block gatherer; hover and site output agree on the corpus
  (or differ only via the documented parameter), with the `###`
  decision recorded in the gatherer's doc comment.
- `grep -rn 'fn computeLineStarts\|fn buildLineOffsets' src/lsp` is
  empty.
- Reordering `SemanticType` without touching `TOKEN_TYPES` fails to
  compile.
- A type added to `builtin_type_specs` completes in the LSP with no
  LSP-side edit.

## How to evaluate the result

### Correctness ideal

Editor-visible facts (positions, token classes, docs, completions)
are projections of compiler-owned data, so they cannot contradict
what the compiler ships — the drift class this project closes is
structurally gone.

### Performance ideal

Neutral or better: one line-start table per document instead of up
to three; legend generation is comptime. Hover latency on a large
file unchanged (spot-check).

## Tests to add

- The `## / ### / ##` gathering corpus, asserted identical (or
  parameter-documented) across both consumers.
- Round-trip position tests (offset→position→offset) against the
  canonical table, including `\r\n` and EOF cases.
- The legend comptime assert (is the test).
- A completion test driven by a spec-table entry.

## Related projects

- [severity-and-report-collection.md](severity-and-report-collection.md)
  — the LSP's other re-derived classification.
- [syntax-fact-single-sourcing.md](syntax-fact-single-sourcing.md) —
  the same reuse discipline inside the parser/formatter.
