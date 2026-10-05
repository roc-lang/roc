# Late callable object reuse

`check.py --roc <compiler> --expect-hits` exercises a compiler built with
`-Dperf-late-callable-cache=true`. Omit `--expect-hits` for the feature-off
oracle. Builds use `--jobs=2`, specializing dev codegen, and a fresh cache and
workspace beneath `.tmp`; the original fixture and package remain untouched.

The same imported higher-order helper receives two imported callbacks with
identical arrow signatures but different bodies. Its callbacks are invoked
with runtime input, and repeated uses retain the procedures without changing
inlining policy. The test checks exact uncached/cold/warm output, forces app
checking with an app-only comment edit, verifies the package bytes did not
change, and reverses caller order under the same cache.

On-feature acceptance requires distinct completed identities for single-target
callback specializations, actual later-stage hits, and matching identity traces
from skipped LIR bodies. A joined finite target set can share one procedure.
Reversing its member order changes the existing callable tag ABI and correctly
misses; repeated single-target build order must keep the same identity.
Aggregate module-cache hits are insufficient. The feature-off run must emit
neither late lookups nor late body skips.

Procedure identity unit coverage additionally distinguishes nested record
fields, nominal backings, returned functions, target source identities and
capture types, and checks recursive unfolding/query-order stability. A runtime
capture fixture returns the same closure code with different environment values
and verifies exact cold/warm output and late body reuse. Nested record/nominal
field fixtures exercise the unchanged SpecConstr-clone exclusion with explicit
ineligibility traces. Compile-time debug and failed expect/observed incomplete
match reports remain identical through warm checking.

Eligibility still belongs to plain finite specialization and the existing
artifact-closure, literal-conversion and observation proofs; identity coverage
alone does not establish portable artifact support.

The closed `Pack.mapped` export exercises pack production with an inlined
`List.map`: representation preflight reserves map/helper procedures without
requesting standalone bodies. Cache publication must not turn those unused
reservations into native roots. The publication unit regression separately
pins a genuine alias-before-owner code demand, retaining both keys after body
completion or object-artifact supply.
