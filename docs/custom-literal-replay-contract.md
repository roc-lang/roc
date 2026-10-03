# Custom literal replay experiment: admission contract

Status: contract investigation only; no implementation or performance claim.
The measured seven-feature configurations are unchanged. This experiment needs
a separate default-off gate, outside the measured `perf-all` configuration.
Cache work is limited to the normal specializing pipeline; Boxy cache support
is out of scope.

## Correction and narrower alternative under review

The language reports a rejected conversion and continues code generation with
an erroneous runtime path that crashes; rejection is not a requirement to abort
compilation. Already-finalized code with the correct crash can be reused for the
same complete specialization/dependency identity. Its compile-time diagnostic
must also be preserved or replayed through the normal reporting machinery.

The current pack marker `.crash.literal_rejection` can instead describe the
conditional error arm of an unevaluated conversion. Code-cache presence therefore
does not establish prior literal finalization. The current exclusion is
conservative, not an inherent prohibition on caching finalized literal code.

Before implementing the larger obligation/inlet proposal below, compare the
smaller option: cache finalized specialization code plus any validation outcome
and diagnostic facts not already retained by checked artifacts. Re-running a
conversion on every hit is not established as necessary. The plan below remains
an unimplemented alternative for entries requiring new validation; its necessity
has not been demonstrated.

## Proposed rule and mutation inventory

Checked artifacts own deferred literal obligations. Reusing an object cannot
change the obligations activated by a concrete specialization reservation.
Packs carry derived execution plans, not proof that a literal is valid.
Unreserved generic templates activate nothing.

No solver rewrite is proposed. Permitted new state is checked obligation
publication, immutable specialization plans, declared completed-value bindings,
pack serialization, and ordinary finalization root registration. Solver
redirects, solved-graph mutations, dispatch restamping, broad higher-order
changes, and lazy rows are outside this experiment.

## Minimal producer/consumer contract

The checked producer publishes an ordered obligation list per owning procedure
template: literal kind/bytes, artifact-qualified checked expression, lexical
owner, checked result type, conversion dispatch, and evidence inputs. Nested
procedure ownership is explicit; reserving an outer procedure cannot activate a
dormant nested procedure's literals.

Reservation instantiates that list with ordinary specialization substitutions
and exact dispatch/codec evidence. It produces a complete closed plan or explicit
ineligibility before early lookup. It never discovers obligations from machine
code. Initial admission requires closed conversion types/evidence independent
of nested callable analysis. Closure-containing results remain ineligible.
Existing branch-observation and higher-order conditions remain unchanged.

Each plan entry declares a stable typed completed-value inlet: specialization,
obligation ordinal, result representation, storage/ownership contract, and link
identity. Cold bodies and warm objects reference the same inlet. Fresh program
slots and relocations bind it to the current completed value; process pointers
and local root IDs cannot persist. Literal values cannot alter specialization
decisions beneath cached code.

The warm consumer validates the derived plan against current checked authority,
registers conversion-only roots without lowering the owning body, and runs
ordinary finalization before cached code executes. Successful values populate
the inlets. Rejections and debug observations use normal evaluation. Reporting
resolves regions through the current checked expression, not old object lines.
Conversion results are not reused across programs.

Existing cache identity is preserved and extended with plan/inlet identity,
actual result type, implementation, evidence and dependency closure. Digest
lookup also requires exact contract validation. Compiler feature identity
isolates configurations; changed checked/pack formats invalidate incompatible
artifacts. With object caching disabled there is no cache-only planning,
serialization, or hashing: ordinary required literal analysis still runs.
ABI, ARC ownership and relocation assertions remain intact.

## Focused regressions

These are concrete test specifications, not implemented or passing tests.

1. Real successful warm workflow: an imported generic quote conversion returns
   a closed non-callable nominal value. Cold build publishes its specialization;
   warm build proves that exact object hit and zero owning-body lowering, while
   validation still executes. Compare output and debug observations. Exercise
   multiple literals and distinct result type/evidence instances.
2. Seeded unit rejection: supply a valid typed plan and object offer for a
   rejecting converter, reserve it, and assert root registration and ordinary
   compile-time diagnostic/current-source region without owning-body lowering,
   preserving report-and-continue code generation and the erroneous crash path.
   This is explicitly seeded, not a naturally published rejected object.
3. Warm-module negative workflow: preserve the existing
   `test/cli/literal_root_rejected` Clean/Rejecting/RejectingAtRuntime sequence.
   Clean leaves the imported generic dormant; runtime-only specialization
   rejects during compilation. A module hit is not proof of object reuse.
4. Compare cold/warm/uncached and flag-off diagnostics, counts, regions and debug
   order. Source movement and converter/dependency edits invalidate naturally;
   never override keys to force hits.
5. CallableLiteral and unresolved/captured evidence retain current eligibility
   exclusions; branch-observation proof is unchanged.

Acceptance needs cold-miss overhead and warm saving measured separately, source
and test hashes, and no extra cache-only work under `--no-cache`.

## Missing earlier producer contract

Checked literal root publication admits `direct_closed` conversions but leaves
specialization dispatch and lexical evidence to specialization. There is no
published procedure-owned deferred-obligation table.

Reservation looks up objects before body lowering. The specialized literal path
creates a fresh definition, root and `comptime_value` initializer inside
`BodyContext.literalRootRead`; its initializer is representation evidence for
the read. No stable typed inlet contract is supplied to early lookup.
`PackFile.SpecEntry` currently contains key, artifact and ARC signature only.

The missing earlier producer must establish both obligation completeness and
typed inlet representation before lookup. A pack-only manifest establishes
neither semantic fact. Recommended next step: separately review the checked
obligation table and reservation-time inlet producer before changing admission.
The safe alternative is to keep these specializations excluded. Extracting
conversion-only lowering may evaluate roots, but alone proves neither inlet ABI
nor obligation completeness. This investigation stops before compiler changes.
