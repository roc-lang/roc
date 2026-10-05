# Custom literal replay experiment: admission contract

Status: source implementation includes completion retention, exact read-origin
ownership, early certificate validation, external propagation, per-closure
certificate construction, and explicit completed-owner native publication.
Recovered-source guards and named Debug/ReleaseFast workflows pass for the
observation-free success class. Fresh same-source RF measurements show no
convincing SQL wall gain, and observed warm compilation transcripts differ
between literal-off and literal-on. Full acceptance is therefore rejected;
the experiment remains default-off. See
`compiler-performance-finalized-literal-cache.md` for exact measurements,
identities and the observer-eligibility/replay boundary. The original
converter/symbol withholding filters remain intact.
The measured seven-feature configurations are unchanged. This experiment needs
a separate default-off gate, outside the measured `perf-all` configuration.
Cache work is limited to the normal specializing pipeline; Boxy cache support
is out of scope.

## Correction and narrower alternative under review

The authoritative smaller contract is now in `design.md`, under compile-time
evaluation and static storage. Normal runtime materialization is the completed
artifact producer; separately lowered module packs remain unfinalized and
withheld. Session-owned facts retain artifact-qualified literal sources, exact
evaluated root identities, successful/rejected outcomes, report authority, and
owned debug messages alongside the existing typed frozen values.

The neutral data model and gate-isolated pack codec are defined in source.
Version 6 carries optional success-only certificates bound to specialization
and artifact identities; gate-off keeps version 4. Version 5 is rejected because
its success marker did not establish an explicit noncallable-result proof.
Initial publication requires
observation-free successful outcomes with explicit owners and the existing
portable artifact representation. Rejection/debug facts remain session-owned,
not admitted by this schema. Qualified completed direct owners now have explicit
native publication demand; existing portability filters are unchanged.

An evaluated root identity is not proof of ownership by a cacheable
specialization. Ordered publication records every retained root/owner pair,
including deduplicated roots. Each immutable read descriptor retains its exact
owner-row reference through lifting, inlining, and cloning; finalized runtime
reads publish that reference and the actual emitting procedure before folding.
No source or crash scan reconstructs ownership. Crash/expect observations without portable origin
facts remain ineligible. The completed frozen payload remains session-owned
until an existing portable artifact representation owns it.

## Source implementation lifetime and scope

The active read/write cache context is declared before Monotype preparation
and copied into private worker inputs separately from cache lookup. Gate-off
and `--no-cache` publish no owner/use rows or retained completion tables.
The evaluation counter measures actual ordinary evaluation, not offers or
debug replay. Early closed owner keys are supported; null/late owners,
rejected outcomes, and debug/failed-expect observations are ineligible.

Decoded certificates belong to the loaded pack's arena through compilation.
`SpecCacheHit` and `Result` borrow them, never serialize their pointers.
Validated early Monotype hits supply external certificates; dynamic Direct-LIR
and HOF certificate offers are declined before external/body-skip annotation.
The publication index resolves local owner/root references against the live
session and owns new certificates until encoding copies their stable fields.
Unsupported reads withhold their actual emitter closures, not unrelated code.
Existing ARC, frozen-data ownership, function-relocation, and program-symbol
checks remain authoritative.

Failed-entry attribution still needs validation. A function can contain a
rejected literal yet return successfully along another runtime branch. A
checked caller that reads the failed value owns the embedded report; one that
avoids the failed read leaves the standalone literal report. Thus a producer's
embedding flag cannot suppress a different program's report. The branch
fixtures in `test/cli/finalized_literal_cache` distinguish these cases but have
not been run yet.

Runtime artifacts do not carry the evaluator's failure hooks. Existing native
CTFE failure hooks use session-local statement/file coordinates. Reusing the
correct runtime crash does not, by itself, supply portable failed-producer
attribution to a fresh checked caller. Failed-entry CTFE admission must either
consume an explicit portable attribution contract or remain excluded. It may
not guess the failed branch or substitute an unconditional crash for the body.

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

## Larger unimplemented alternative: rule and mutation inventory

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
