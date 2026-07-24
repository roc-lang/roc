# Syntax Fact Single-Sourcing (Keywords, Suffixes, Number Grammar)

## Problem

Surface syntax is mostly single-sourced (escape alphabet, precedence
table, highlight classification — see Background), but three facts are
still encoded more than once:

1. **Keyword spellings ×3.** The tokenizer's `keywords`
   `StaticStringMap` (`src/parse/tokenize.zig:550-585`) is the
   recognizer; the formatter re-emits keyword text as ~25 hardcoded
   string literals instead of slicing source or consulting a shared
   table (`src/fmt/fmt.zig:474` `"var"`, `:505` `"import"`, `:702`
   `"expect"`, `:713` `"for"`, `:746` `"while"`, `:764` `"crash"`,
   `:1742` `"if"`, `:1771` `"else"`, `:1811` `"match "`, `:2467`
   `"app"`, `:2525` `"platform"`, `:2566` `"module"`, `:2656`
   `"requires {"`, and more); and the test-only reverse map in
   `rebuildBufferForTesting` (`tokenize.zig:2474-2569`) spells them a
   third time. Notably the formatter already does this right for
   operators — it emits them via `pushTokenText` reading source
   (`fmt.zig:1677, 1702`) — so the keyword literals are an
   inconsistency within fmt itself.
2. **The deprecated numeric-suffix map is a hand-written
   bidirectional pair.** In `src/parse/NumericLiteral.zig`,
   `DeprecatedSuffix.oldText()` (`:64-81`, enum→text) and
   `deprecatedSuffixFromText()` (`:243-258`, text→enum as an `eql`
   chain) encode the same strings twice; a typo makes them disagree
   silently. (The tokenizer's accepted-suffix set is already
   comptime-derived from `oldText()` at `tokenize.zig:590-603` —
   the remaining gap is only between the two functions.)
3. **The number-literal grammar is scanned twice.** The tokenizer's
   `chompNumber`/`chompIntegerBase16/8/2`/`chompExponent`
   (`tokenize.zig:788-940`) and canonicalization-time
   `NumericLiteral.numberTextEnd`/`digitValue`/`parseExactInteger`
   (`NumericLiteral.zig:293-348, 357+`) each hand-encode the base
   prefixes (`0x/0o/0b` and uppercase variants), per-radix digit
   validity, underscore separators, and exponent shape. The two agree
   today but nothing shares the rules; adding a digit separator or a
   base means editing both scanners, and a mismatch means the
   tokenizer accepts a token the value parser then misreads.

## Background

The consolidation pattern is already established in this exact area:
`src/parse/escape.zig` is the documented single source for the escape
alphabet, consumed by tokenizer and canonicalizer; operator
precedence lives only in `Parser.zig`'s `bin_op_bp_table` (fmt
preserves parens as AST nodes and reads operator text from source);
`Token.Tag.highlightCategory` feeds both the HTML view and LSP
semantic tokens. This project extends that pattern to the last three
holdouts.

## Evidence

- The fmt keyword-literal sites cited above versus the operator path
  in the same file.
- `NumericLiteral.zig:64-81` vs `:243-258` — the unlinked pair.
- `tokenize.zig:788-940` vs `NumericLiteral.zig:293-357` — the two
  scanners' parallel base-prefix switches.

## Solution design

1. **Keywords.** Add `Token.Tag.keywordText() ?[]const u8` (one
   switch, adjacent to the `keywords` map, with a comptime check that
   the map and the function agree in both directions). The formatter's
   keyword literals become `keywordText(.kw_if)` etc. — or, where a
   token is at hand, `pushTokenText` like the operator path.
   `rebuildBufferForTesting`'s keyword arms derive from the same
   function.
2. **Suffixes.** Replace `deprecatedSuffixFromText`'s `eql` chain with
   a comptime loop over the enum comparing against `oldText()` — the
   inverse is then correct by construction.
3. **Number grammar.** Extract the shared micro-facts into
   `NumericLiteral` (or a small shared decl both import): base-prefix
   recognition (`x/X/o/O/b/B → radix`), `isDigitForRadix`, and the
   underscore rule. Both scanners keep their different jobs
   (span-finding vs value computation) but consult the same
   predicates. Full unification of the scanners is out of scope.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `grep -n '"if"\|"else"\|"match \|"platform"' src/fmt/fmt.zig` finds
  no keyword emission literals (structural punctuation like `{`
  remains).
- The keywords map ↔ `keywordText` bidirectional comptime check
  exists; adding a keyword in one place only is a compile error.
- `deprecatedSuffixFromText` contains no string literals.
- Base-prefix/digit predicates exist once;
  both scanners call them.
- `zig build fmt`-family tests and the tokenizer fuzz corpus are
  unchanged; snapshot corpus unchanged.

## How to evaluate the result

### Correctness ideal

A keyword, suffix, or number-grammar change is a one-site edit that
every consumer (tokenizer, formatter, value parser, test helper)
follows automatically or rejects at comptime.

### Performance ideal

Neutral: `keywordText` is a comptime-resolvable switch identical to
the literals it replaces; the shared digit predicates inline to the
same comparisons. Confirm the tokenizer benchmark (if any) and
`zig build snapshot` timing are unchanged.

## Tests to add

- The bidirectional keywords-map/`keywordText` comptime check.
- A suffix round-trip test (`fromText(oldText(s)) == s` for all
  variants — trivially true after step 2, kept as a guard).
- Cross-scanner agreement corpus: number strings (all bases,
  underscores, exponents, malformed variants) where
  tokenizer-accepts must imply value-parser-succeeds.

## Related projects

- [lsp-and-docs-truth-reuse.md](lsp-and-docs-truth-reuse.md) — the
  same reuse move for editor-facing syntax facts.
