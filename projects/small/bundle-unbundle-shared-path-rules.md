# One Path-Validation Rule Set for bundle and unbundle

## Problem

`src/bundle` (archive writer) and `src/unbundle` (archive reader) each
maintain their own copy of the archive-path safety rules — and the
copies already disagree. This is a security-relevant validator
(directory traversal, Windows reserved names) whose writer and reader
halves can drift independently:

- Two public functions **with the same name**:
  `pathHasUnbundleErr` at `src/unbundle/unbundle.zig:294` (empty,
  length, absolute, `.`/`..`, Windows reserved names, trailing
  space/period, reserved characters, all in one pass) versus
  `pathHasUnbundleErr` at `src/bundle/bundle.zig:383` (a different,
  smaller check set; the reserved-name/char checks live separately in
  `pathHasBundleErr`, `bundle.zig:313-379`). `bundle.zig` calls its
  own local version despite a comment saying it performs "the
  validation checks we'd do on unbundle."
- The 22-entry `WINDOWS_RESERVED_NAMES` array is duplicated verbatim
  (`bundle.zig:270-277`, `unbundle.zig:284-291`).
- `PathValidationReason` (10 variants), `PathValidationError`, and
  `ErrorContext` are each declared twice (`bundle.zig:280-297,
  107-110` vs `unbundle.zig:51-62, 279-282, 45-48`).
- The max path length is a named constant on one side
  (`TAR_PATH_MAX_LENGTH: usize = 255`, `bundle.zig:33`) and a bare
  `255` literal on the other (`unbundle.zig:302`).
- The reserved-character rule is encoded two different ways — an array
  consumed by `inline for` in bundle (`bundle.zig:258-267`) and an
  inline switch in unbundle (`unbundle.zig:372-391`) — and the
  encodings differ: unbundle's switch handles backslash-on-unix;
  bundle's array does not include it.
- The `UnbundleError` sets differ by ~9 members (`bundle.zig:91-104`
  vs `unbundle.zig:22-42`).

Nothing cross-checks any of this: no shared module for the rules, no
test asserting that every path bundle accepts, unbundle accepts.

## Background

The directories are already a dependency pair with a shared layer:
`src/unbundle/format.zig` holds `TAR_EXTENSION` and
`STREAM_BUFFER_SIZE`, and `bundle.zig:23-36` imports them. unbundle is
the lower, wasm-compatible layer; bundle depends on it. The
consolidation direction is therefore already established — the path
rules just never moved.

## Evidence

- The paired declarations cited above (side-by-side diff makes the
  divergences obvious).
- `bundle.zig:318` — the comment claiming unbundle-equivalence while
  calling the local, weaker function.
- No test in either directory feeds one validator's accepted paths to
  the other.

## Solution design

1. Move the single rule set into `src/unbundle` (in `format.zig` or a
   new `path_validation.zig`): `WINDOWS_RESERVED_NAMES`,
   `TAR_PATH_MAX_LENGTH`, the reserved-character predicate,
   `PathValidationReason`, `PathValidationError`, `ErrorContext`, and
   one `pathHasUnbundleErr`.
2. `bundle` imports all of it and DELETES its local copies. Bundle-side
   extra checks that genuinely only apply at write time (if any
   survive inspection) stay in `bundle.zig` but are written as
   additions on top of the shared validator, not a parallel one.
3. Reconcile the divergences deliberately while merging: the
   backslash rule, the check set split between bundle's two local
   functions, and which error-set members each side needs. Every
   difference either disappears or gets a comment stating why the
   writer is stricter than the reader (the reader may never be
   stricter than the writer).
4. Add the round-trip property test: for a corpus of valid and
   invalid path shapes (including the divergence cases above), assert
   `bundle accepts ⇒ unbundle accepts`, and that both reject the
   attack shapes (traversal, absolute, reserved names, reserved
   chars, overlong).

## What success looks like

Every criterion below must hold; the project is not done until all do:

- One `pathHasUnbundleErr` exists;
  `grep -rn 'fn pathHasUnbundleErr\|WINDOWS_RESERVED_NAMES\|PathValidationReason'
  src/bundle src/unbundle` shows single definitions, in unbundle.
- No bare `255` path-length literal; both sides use the shared
  constant.
- The writer-⊆-reader property test is in-tree and green, including
  the backslash and reserved-name cases.
- Any intentional writer-only strictness is code-commented at the
  shared-validator call site.

## How to evaluate the result

### Correctness ideal

The reader's safety rules and the writer's are the same code, so "we
never produce an archive we would refuse to extract" holds by
construction, and a future rule change lands on both sides atomically.

### Performance ideal

Neutral: same checks, one implementation. The `inline for` vs switch
encoding choice goes to whichever the merged implementation keeps;
neither is hot (per-path validation during bundling).

## Tests to add

- The writer-⊆-reader property corpus.
- Attack-shape rejections pinned on both entry points.
- A test that the shared `PathValidationReason` variants each have at
  least one corpus case (comptime-enumerated).

## Related projects

- [cache-and-identity-residuals.md](cache-and-identity-residuals.md)
  — same philosophy applied to the cache boundary's composed facts.
