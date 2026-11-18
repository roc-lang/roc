# #13100: stored nominal-record metadata differs between strategies

Issue: https://github.com/roc-lang/roc/issues/13100

The issue is assigned to `rtfeldman`. The reproduction and root-cause
sections record the pre-fix investigation.

## Reproduction

From the repository root:

```sh
zig build roc -j2
./zig-out/bin/roc test --specialize=yes --no-cache test/cli/Issue13100StoredNominalRecordList.roc
```

The test asserts that a stored empty `List(Id)` has length zero. The command
aborts with:

```text
postcheck invariant violated: stored const representation differed from its expected Monotype type
```

The stack goes through `restoreConstUseAtType` and `assertSameType` in
`src/postcheck/monotype/lower.zig`. A CLI regression entry in
`src/cli/test/parallel_cli_runner.zig` requires successful execution and
`All (1) tests passed`, with no panic or invariant message.

Current behavior differs from the issue's original CLI defaults:
`roc test --no-cache` uses Boxy and passes. Explicit `--specialize=yes` is
necessary for this reproduction on the inspected checkout.

The second reported example is retained as
`test/cli/Issue13100HoistedNominalRecordList.roc`. It passes both with the
current default and with `--specialize=yes`; it is companion coverage, not
a confirmed second failure.

## Root cause

The producer of stored types drops declaration metadata:

1. `Converter.backing` / `declaredBacking` in
   `src/postcheck/boxy/types.zig:2126-2155` deliberately reuse an ordinary structural
   backing for an unpadded nominal record. Only padding requires a distinct
   backing with a `declared` layout order. This is consistent with the
   documented layout policy.
2. `TypeWalk.declaredOrder` in `src/postcheck/boxy/const_schema.zig:430-438` reads
   only `record.declared`, returning an empty span when it is absent.
3. Monotype's `stepInstDeclaredOrder` and `instDeclaredOrderEntries`
   (`src/postcheck/monotype/lower.zig:29177-29232`) retain
   the checked nominal declaration's named fields even without padding.
4. Restoring the stored list exposes the two different element types to
   the exact-type assertion (`src/postcheck/monotype/lower.zig:49603-49605`).

The invariant is that stored type metadata preserves the nominal
declaration independently of the selected runtime layout policy. Absence
of a declared-order **layout** is not absence of declared fields.

## Implemented rule

The initial recommendation was to serialize all declared fields to match
Monotype's current contract. A whole-compiler performance review changes
the recommendation: define post-check declared-order metadata as explicit
layout-policy data, and do not carry an unpadded declaration's source-order
list in every instantiated runtime type.

Keep complete source declarations in checked modules for consumers that
need them. At post-check type production, use structural order for unpadded
records and retain ordered named/padding entries only for declarations
that explicitly opt into declared-order layout. Apply that contract
consistently to Monotype instantiation and both stored-type producers,
with the existing iterative, memoized handling of instantiated padding
and recursive types.

There is concrete evidence of unnecessary downstream work today:

- `src/postcheck/solved_lir_lower.zig:14954-14975` uses declared order only
  when it includes padding; otherwise it uses the structural backing.
- `src/postcheck/monotype/layout_key.zig:231-240` rejects backing-layout
  sharing whenever the declared-order span is nonempty, even without
  padding.
- The order spans are also copied, hashed, compared and serialized through
  the Monotype and lambda-solved type pipeline.

This is a producer-contract change, not permission for restoration or
equality to ignore disagreements. Update design.md's declared-order
contract with the implementation, preserve nominal identity, retain exact
type checks, and ensure all producers emit the same canonical metadata.
An unnamed zero-sized field still opts into declared order; the rule is
presence of the explicit marker, not whether padding occupies bytes.

The expected benefit is less work throughout compilation, rather than
merely no generated-program overhead. It remains unmeasured. Validate
field access, inspection, codecs, host layout and constant restoration,
including padded/unpadded, nominal/opaque, generic, recursive, imported,
empty-list and non-empty-list cases. Measure full compile time, peak
memory, declared-field span volume, type hashing/equality work, and
layout-key reuse before claiming a performance improvement.

Both Monotype producers now omit source-only order: `declaredOrderOps`
for closed checked types and `stepInstDeclaredOrder` for graph-local
instantiation. Boxy's existing layout-only producer already obeys the
same rule. The Solved constant producer carries the canonical Monotype
metadata unchanged; restoration and equality do not repair disagreements.
The language contract is updated in design.md.

Additional tests cover structural and explicit zero-sized-padding layouts,
generic/opaque/recursive record constants, and exact post-check metadata.
The original regression files and assertions remain unchanged.
