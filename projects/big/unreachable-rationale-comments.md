# Rationale Comments on Every `unreachable`

## Problem

`unreachable` is the most dangerous statement in the codebase. It is an
executable claim — "control flow can never arrive here" — but in
`ReleaseFast` (the mode we ship) Zig compiles it to nothing and treats
reaching it as undefined behavior: no panic, no message, just miscompiled
downstream code from an optimizer that was told this branch is dead. The
claim is only as good as the invariant that backs it, and today that
invariant almost never appears next to the statement. It lives in the
author's head.

Under `src/`, `grep` finds **1751** lines containing `unreachable` in
`.zig` files. Only **32** of them (under 2%) carry a same-line comment
explaining why the case is impossible. The rest are bare. The forms break
down as:

- **995** statement `unreachable;`
- **442** switch-prong `=> unreachable,` — of which only **22** are
  annotated
- **452** `orelse unreachable`
- **107** `catch unreachable`

(The forms overlap: `orelse unreachable;` counts in two rows.) By module,
the concentration is `backend` 516, `canonicalize` 324, `check` 227,
`builtins` 100, `cli` 96, `postcheck` 80, `compile` 83, `eval` 69.

A bare `unreachable` is a maintenance hazard in three ways. A reader
auditing the code cannot tell whether the case is genuinely impossible or
merely unhandled. A person refactoring an upstream stage cannot tell which
of their changes would turn the claim false. And when one does turn false,
the failure is not local — it is a silent miscompile far downstream. The
whole point of these annotations is the *static* reader: the justification
must be visible without running anything.

## Background

The codebase already has one strong tool for the *dynamic* side of the
same problem. The LIR interpreter's `invariantFailed`
(`src/eval/interpreter.zig:819`) prints a formatted message and asserts in
Debug builds, then falls through to `unreachable` in release. That gives a
rich runtime diagnostic when an invariant breaks under test — but it does
nothing for someone reading the source, and it exists in exactly one
subsystem. This project is the complementary static discipline for the
other ~1740 sites: a short comment stating the invariant, checkable by
eye and by lint.

The good sites already show the target shape. These are real and correct:

- `src/values/RocValue.zig:350` —
  `.erased_callable => unreachable, // Function values are not equality-comparable Roc values.`
- `src/postcheck/match_tree.zig:443` —
  `.bind, .wildcard, .as_pattern, .record, .tuple, .nominal => unreachable, // normalized away`
- `src/canonicalize/DependencyGraph.zig:1311` —
  `const w = self.stack.pop() orelse unreachable; // Stack should not be empty`
- `src/parse/AST.zig:1079` —
  `... catch unreachable; // Malformed handled above`
- `src/fmt/fmt.zig:3020` —
  `.closed => unreachable, // is_open is true`

Each names the upstream guarantee — a normalization pass, a prior
malformed-node check, a loop condition — that makes the case impossible.

The bare sites are where the invariant is invisible. Representative:

- `src/check/canonical_type_keys.zig:697` — bare `unreachable;`
- `src/check/checked_traverse.zig:279` — bare `unreachable;`
- `src/parse/NumericLiteral.zig:304` — `else => unreachable,`
- `src/postcheck/common.zig:114` — bare `unreachable;`
- `src/eval/value.zig:144` — `else => unreachable,`
- `src/collections/safe_list.zig:753` — bare `unreachable;`
- `src/postcheck/match_tree.zig:1272` —
  `return (env.locals.get(occ) orelse unreachable).value orelse unreachable;`
  (two bare claims on one line)

For some of these the invariant is knowable with a minute of reading. For
others, the honest answer is "we are not sure why this can't happen" — and
that is the most important category to surface, because a comment invented
after the fact to satisfy a lint is worse than no comment: it launders a
guess into an assertion.

## Evidence

- `grep -rn unreachable src --include='*.zig' | wc -l` → 1751; the
  annotated subset (`unreachable` followed by punctuation then `//`,
  excluding the one string-literal match) → 32.
- `grep -rnE '=> unreachable,' src --include='*.zig' | wc -l` → 442;
  with `// ` trailing → 22.
- `src/eval/interpreter.zig:819` — the `invariantFailed` model: Debug
  message + assert, release `unreachable`.
- The existing lint fleet this one joins: `CheckPanicStep`
  (`build.zig:1173`), which scans named files/dirs for `@panic`, skips
  comment lines, honors a line-level allowlist and excluded line ranges,
  and fails the build step with per-site violations.

## Solution design

### The convention

Every `unreachable` token carries a same-line trailing comment stating
**why** the case is impossible — specifically, which upstream invariant
guarantees it. Format rules, chosen so a lint can check them mechanically:

- The comment is on the **same physical line** as the `unreachable`
  token, introduced by `//`, after any trailing `,` or `;`.
- Switch-prong form: `=> unreachable, // <reason>` — the comma comes
  before the comment.
- Statement form: `unreachable; // <reason>`.
- Expression forms: `orelse unreachable // <reason>` and
  `catch unreachable // <reason>` (the `//` may follow a closing `)` or
  the terminating `;` of the enclosing statement, whichever ends the
  line).
- The reason text must be non-empty after the `//` and must not be a bare
  restatement like `unreachable` or `impossible`; it names the guarantee
  (a pass that normalized the input away, a prior validation that rejected
  the bad case, an enum whose other variants are handled above, a loop
  condition).
- When a single line holds two `unreachable` tokens (as at
  `match_tree.zig:1272`), split it so each claim gets its own line and
  comment, or the reason must cover both explicitly.

The comment states a present-tense fact about the current code — "handled
above", "normalized away", "checked by canonicalization" — never a history
of what the code used to do.

### When to annotate vs. convert

The migration is not "add a comment to all 1751 sites." For each site the
author asks: *can I name the invariant honestly?*

- **Yes, and it is a true compile-time impossibility** → add the
  rationale comment. Done.
- **No, I am guessing** → do not invent a comment. Escalate the site:
  convert it to a Debug-checked invariant in the style of
  `invariantFailed` (message + assert in Debug, `unreachable` in release),
  or, if the "impossible" case is actually a recoverable malformed-input
  path, return an error / diagnostic instead of asserting. A converted
  site either carries a message string (which the lint accepts in place of
  a comment) or is no longer an `unreachable` at all.

This is the load-bearing part of the project: the lint's real value is
that it forces this triage on every site, and forces it again on every
new site forever.

### The lint

A new check, modeled on `CheckPanicStep`, scanning all `.zig` under
`src/`:

1. **Tokenize enough to be safe.** Read each file, walk it tracking
   whether the cursor is inside a `"..."` or `\\` string, a `'...'` char,
   or a `//` comment, so an `unreachable` inside a string literal (e.g.
   the diagnostic text `"...unreachable..."`) is never counted. Only
   `unreachable` appearing as a code token is a candidate.
2. **Check the line tail.** For each candidate, a site is *compliant* if
   the remainder of its physical line contains a `//` whose text is
   non-empty and not in the banned-restatement set. Sites that read a
   Debug message argument (the `invariantFailed`/assert conversion path)
   are compliant by virtue of the message, but the simplest rule is: a
   trailing `//` rationale, or the token is not lexically `unreachable`
   at all. For expression-form sites the trailing `//` counts only when it
   is attributed to the *last* unreachable-bearing token on the line:
   close to thirty sites bury `orelse unreachable`/`catch unreachable`
   mid-expression (e.g. `(map.get(x) orelse unreachable).field;`), where a
   trailing comment could just as well pertain to the rest of the line, so
   the lint requires that the `unreachable` be the final such token before
   the `//`, or the site must be split onto its own line — otherwise an
   unrelated trailing comment could satisfy the lint without justifying
   the claim.
3. **Shrinking allowlist.** A single tracked file,
   `ci/unreachable_allowlist.txt`, lists `path:line` sites still to be
   handled, one per line, each with a trailing `# owner/reason-todo`. The
   lint passes if every non-compliant site is in the allowlist AND the
   allowlist has no stale entries (a listed site that is now compliant or
   gone is itself a failure — the file may only shrink). Migration lands
   as a sequence of PRs that each delete a block of allowlist lines by
   annotating or converting those sites. When the file reaches zero
   entries it is deleted and the lint becomes absolute.
4. **Teaching failure message.** On a bare new site the lint prints the
   `path:line`, the offending line, and a short paragraph teaching the
   convention: state which upstream invariant makes this impossible as a
   `// ` comment on the same line, or convert to a Debug-checked invariant
   if you cannot name one — plus a pointer back to this doc. The message
   must make the *right* action obvious, because the wrong action
   (inventing a comment) is always available.

### Wiring

Follow the established pattern exactly:

- Add the check as a build Step next to `CheckPanicStep` in `build.zig`,
  and a `run-check-unreachable-rationale` step declared beside the other
  `run-check-*` steps (`build.zig:2568-2585`), wired at the block near
  `build.zig:5159-5160`.
- Add one step to the `check-once` job in
  `.github/workflows/ci_zig.yml` (alongside "Check panic usage",
  ~lines 110-111): `run: zig build run-check-unreachable-rationale`.

## What success looks like

Every criterion below must hold; the project is not done until all do:

- `ci/unreachable_allowlist.txt` has **zero** entries and is deleted;
  every one of the ~1751 sites is either annotated with a same-line
  rationale or converted (to a Debug-checked invariant or a returned
  error).
- `zig build run-check-unreachable-rationale` passes and **fails** when a
  bare `unreachable` is added anywhere under `src/` — verified by adding
  one in a throwaway edit and observing the failure and its teaching
  message.
- The lint is wired into the `check-once` CI job and gates merges.
- No comment added by this project describes past code behavior; every
  rationale states a present-tense invariant.
- The escalation category is real, not cosmetic: at least the sites where
  no honest invariant could be named were converted to Debug checks or
  error returns rather than given invented comments. (Spot-check the diff:
  a PR that turned 1751 bare sites into 1751 one-liners with zero
  conversions did the wrong thing.)
- `git grep` for the banned restatement words as the entire rationale
  (`// unreachable`, `// impossible`, `// can't happen`) returns nothing.

## How to evaluate the result

### Correctness ideal

Every dead-branch claim in the compiler is auditable from the source
alone: a reader sees the invariant that backs it, and a refactorer of an
upstream stage can `grep` the downstream `unreachable` rationales that
name that stage. The class of silent `ReleaseFast` miscompiles from a
falsified `unreachable` shrinks because each such claim was forced through
honest triage, and the ones that could not survive triage became Debug
assertions that fire under test instead of UB in production. The lint
makes the discipline permanent: the experience of writing new code that
reaches for `unreachable` is that CI stops you and teaches the convention,
so the annotated fraction can only go up.

### Performance ideal

Zero runtime cost. Annotations are comments; the lint is CI-only and runs
in the `check-once` job like its siblings. Conversions to Debug-checked
invariants must keep the release path identical — message and assert
compiled out, leaving the same `unreachable` the site had before — so
`ReleaseFast` codegen is unchanged. Verify by confirming the converted
functions' release output is unaffected (the `invariantFailed` shape at
`interpreter.zig:819` already demonstrates this: release mode is a bare
`unreachable`).

## Tests to add

- Lint self-tests with fixtures: a compliant fixture (each of the four
  forms with a valid rationale) that must pass, and a non-compliant
  fixture (bare statement, bare switch prong, bare `orelse`/`catch`, an
  `unreachable` inside a string literal that must NOT be flagged, and a
  banned-restatement comment that must be flagged) that must fail — so the
  string-literal skipping and the restatement ban are both pinned.
- An allowlist-integrity test: a listed site that is actually compliant,
  and a listed `path:line` that no longer exists, each make the lint fail
  (the file may only shrink, never go stale).
- A pinned regression for one converted site: the chosen bare
  `unreachable` whose invariant could not be named honestly is now a
  Debug-checked invariant, with a test that trips it in Debug and a note
  that release codegen is unchanged.

## Related projects

- [../small/rceffect-conformance.md](../small/rceffect-conformance.md) —
  the same philosophy (a hand-written claim that nothing checks becomes a
  silent miscompile) applied to the ownership table; this doc is the
  control-flow instance.
- [one-report-renderer.md](one-report-renderer.md) — shares the
  enforcement pattern of a comptime/CI gate that a new variant fails until
  covered.
