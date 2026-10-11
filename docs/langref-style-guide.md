# Language Reference Style Guide

This is how to write pages for the [language reference](langref/README.md). Every
page should read as if the same person wrote it.

(This file lives outside `docs/langref/` because every Markdown file in that
directory gets published as a langref page.)

## Talk to the Reader

Write to "you," the way you'd explain something to a colleague who already knows
how to program. Use contractions (can't, it's, doesn't, you'd). The langref is
detailed, but it's not a spec written in legalese.

- Yes: "Another way to think of an expression is that you can always assign it to a name—so, you can always put it after an `=` sign."
- No: "An expression is any construct that may appear on the right-hand side of an assignment."

Avoid instruction-manual phrasing like "Use X to…" or "Prefer X when…" as the main
way of explaining something. Explain what X does, and the reader will know when to
use it.

## Define Terms Plainly, Where They're Introduced

Introduce a term in italics in the sentence that defines it, using everyday words.
"X is where…" and "X is something that…" are fine.

- "A _block expression_ is an expression with some optional statements before the expression."
- "_Dispatch_ is where the same call expression can result in a different function being run, depending on the types of its arguments and/or return value."

When a concept has a standard name in computer science, link the name to
Wikipedia (or to a paper) instead of re-explaining it: "Heap-allocated Roc values
are automatically [reference-counted](https://en.wikipedia.org/wiki/Reference_counting)."

Bold a word when you're defining several properties at once in a bulleted list:

- "**Structural** means that you don't have to choose a name for the type…"
- "**Extensible** means that the type can accumulate new tags based on how it's used…"

## Avoid Compiler-Internals Vocabulary

Readers know programming, not the compiler's internals. Don't use words like
unification, inhabited, materialize, lowering, backing row, monotype, solver, or
"lift" unless the page is specifically about that concept and has defined it
first. Describe what the reader can observe: what compiles, what's an error, and
what happens at runtime.

## Explain Why, Including the Tradeoff

Say why something is the way it is, and what was weighed against what. That's
most of what makes a reference worth reading instead of just trying things in the
REPL.

- "That's because it's much more useful to have `{ x }` be a block expression, for situations like `else { x }`, than syntax sugar for a single-field record like `{ x: x }`. Single-field records are much less common than blocks in conditional branches."
- "(If it did not give an error at compile time, it would either crash or loop infinitely at runtime.)"
- "By design, Roc has no way to express reference cycles, so none of these solutions are necessary."

"By design" is the phrase for a deliberate omission. Use it when something is
missing on purpose, and say what the purpose was.

## Compare to Other Languages When It Helps

Briefly, and without judging: "Like most programming languages, Roc uses strict
evaluation and does not support lazy evaluation like some non-strict languages do
(such as Haskell)." or "Other languages support reference cycles, which create
problems for reference counting systems."

## Small, Annotated Examples

Prefer several tiny examples over one big one. Lists of one-line examples work well:

- "In `x = Foo`, `Foo` is a tag."
- "In `y = Foo(4)`, `Foo` is a tag with a payload of `4`."

In code blocks, use comments to point at the interesting line:
`# This line will never be reached.` or `Purple # ERROR! to_color returns ..[] and so does not accept new tags.`

Show the error case right next to the working case: "This is allowed: … However,
this gives a shadowing error: …"

Nothing tests the langref's code blocks automatically, so check every example
(and every claim about what's an error) with a current build of `roc` before
publishing it.

## Say Who Something Is For

Different readers care about different things. Say so directly: "This is rarely
useful to application authors, but it is useful to platform authors." Caveats that
only matter to one audience go in a block quote starting with "Note that":

> Note that platform authors can choose to implement features based on memory
> addresses, since platforms have access to lower-level languages…

## Be Definitive

When a list is complete, say so: "There are no other types of expressions in the
language." When something is an error, say "This gives an error at compile time,"
not "This is generally not permitted."

Describe the language as it is. Don't describe how it used to work, and don't
describe planned features as if they exist.

## Performance Sections

Every page whose subject exists at runtime gets a section on performance. It
should explain the representation in enough detail that the reader could predict
what a piece of code costs:

- Where the bytes live (inline vs. on the heap), how big things are on 64-bit and
  32-bit targets, and whether there's a reference count.
- What an operation copies, allocates, or mutates in place, and what decides
  which one happens (usually [opportunistic mutation](langref/expressions.md#opportunistic-mutation)).
- What the compiler does for you, so the reader knows what they _don't_ need to
  hand-optimize (for example, record fields are reordered to minimize padding, so
  reordering them yourself does nothing).
- What the reader can do differently when performance matters, with an example.

Only state things the compiler actually does today. If something is planned but
not implemented, leave it out.

## Mechanics

- Em dashes have no spaces around them (`value—regardless`). Tidy enforces this.
- Headings are Title Case.
- Link other langref pages with extensionless relative links: `[records](records)`.
- Code fences use `roc`.
- Paragraphs are short: usually one to three sentences.
- Don't end every paragraph with "See X for more." Link the words where they're
  used instead.
