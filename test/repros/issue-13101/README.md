# #13101: closed dispatch bypasses representation-aware specialization

Issue: https://github.com/roc-lang/roc/issues/13101

The issue is assigned to `rtfeldman`. The reproduction and root-cause
sections record the pre-fix investigation. Temporary diagnostic
instrumentation was removed.

## Reproduction

From the repository root:

```sh
zig build run-test-eval-host-effects -j2 -- --filter 'issue 13101'
```

The test in `src/eval/test/host_effects_tests.zig` requires successful
execution, a single `dbg` event containing `1`, and zero live allocations.
The harness reports one crashed test, with:

```text
postcheck invariant violated: eager procedure specialization request changed before final graph seal
```

The standalone issue reproduction fails the same way:

```sh
zig build roc -j2
./zig-out/bin/roc run --specialize=yes --no-cache test/repros/issue-13101/main.roc
```

Changing only the call to
`Counter.restart(Counter.{}, Stream.from_iter([1].iter()))` prints `1` on
the same compiler with specialization enabled.

## Root cause

In `src/postcheck/monotype/lower.zig:38493-38513`, the `.direct_closed` branch of
`stepDispatchLower` sends every ordinary `.procedure` target to
`stepClosedProcedure`. It confuses a closed checked type with a fixed
runtime representation.

`stepClosedProcedure` constructs its callable from `lowerType(plan.callable_ty)`
(`lower.zig:38872-38897`),
so this call starts with the public
`Counter, Stream(U8) -> Stream(U8)` interface. Its specialization cache is
keyed by the checked callable and evidence, not a producer-specific
representation. It eventually calls `draftFnSlotTypeNode`, which eagerly
completes the method through `completeDeferredIteratorResult` and records a
supposedly stable request snapshot.

The result-evidence path does not make the same choice:
`closedDirectGraphFreeResultType` (`lower.zig:54852`) already excludes ordinary procedures
returning iterators. It uses the graph path and carries the actual generated
`from_iter` operand representation. Expression emission then independently
takes the closed shortcut, constructing a second public-only request.

Temporary tracing confirmed all of the following:

- Operand-evidence discovery already returned a generated-private
  `from_iter` result. The producer did not fail to supply the evidence.
- The eager completion that later fails was called specifically by
  `stepClosedProcedure -> draftFnSlotTypeNode -> completeDeferredIteratorResult`.
- Its saved request's stream argument and return had
  `iterator_representation = none`, `generated = null`, and one public
  type argument.
- At final sealing, both live positions instead had
  `iterator_representation = minted`, `iterator_kind = from_iter`,
  `iterator_depth = 2`, and two type arguments.
- Comparing immediately after eager body completion did not show that
  change; later graph relations changed the retained request.

The public argument and result initially share the same type node. The
subsequent representation refinement consequently changes both. The final
assertion (`lower.zig:15831-15840`) correctly rejects publishing a specialization under a different
request from the one it was lowered against.

## Implemented rule

Give closed-dispatch optimization one authoritative representation-dependence
summary, consumed by both result-evidence queries and expression emission.
Checked closure and a known ordinary procedure target are not enough.
The full callable interface, including nested argument and result types,
must be proven unable to acquire producer-authored iterator or codec
representations before using the checked-type-only call fast path and
cache key.

The consumers need not have identical eligibility predicates. A fixed-result
query can remain cheap even when the call's arguments require
representation-aware specialization, provided it does not omit necessary
relations. Track argument and result dependence separately; do not send
every type query through call construction merely because one argument can
carry a private representation.

Route representation-dependent calls through the existing graph-aware
request construction and specialization path. That path must retain the
operand's explicit producer evidence and the callee's own result witness,
with a single coherent request used by type queries and emitted calls.
Keep requests that still participate in representation joins graph-local
until the representation relations settle.

Concretely, share that summary between `closedDirectGraphFreeResultType` and
`stepDispatchLower`, and restrict `stepClosedProcedure` and
`closed_direct_specializations` to that proven domain. Reuse/generalize
the memoized checked-type representation analysis, lazily for demanded
specialization work, rather than introducing an unconditional checker pass
or separate containment walks at each use. Its recursive closure must
cover representations nested in containers, records, nominal backings,
and callable arguments/results, not only a top-level `Stream`.

This retains the fast path for genuinely fixed interfaces, preserves
specialization and inlining for iterator producers, and avoids duplicate
public/private specialization work. Do not suppress the seal assertion,
rekey an already-lowered body, or force all streams to dynamic/Boxy
representation. Those would hide the broken request identity or give up
specialization rather than fix its producer.

Whole-compiler optimality is not established by the reproduction. Measure
end-to-end compile time, allocations, graph nodes and specialization counts
on ordinary closed calls as well as iterator/codec-heavy modules. Check
generated-program performance too: reducing compiler work by erasing
representations would be the wrong tradeoff.

The implementation reuses the existing iterative Monotype containment walk.
A sparse callable-summary cache holds only requested checked module/type
pairs, not another dense type column. Result queries compute only result
dependence. Emission computes argument dependence only if the result alone
has not already ruled out the shortcut. The per-Monotype cache remains one
byte, pinned by a unit test. No new checking or Boxy pass is introduced.
