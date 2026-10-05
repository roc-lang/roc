# Normal `.lss` artifact address determinism

For fixed source, dependencies, compiler, target, options, specialization inputs,
and immutable compilation context, normal `.lss` artifacts must persist identical
disk bytes regardless of source/static allocation addresses or machine-code
placement. This is a producer contract, not permission for a reader to infer
identity from addresses or repair persisted code heuristically.

`src/backend/dev/LirCodeGen.zig` test
`independent object producers persist identical normal lss artifact bytes`
pins the native emission → finalized extraction → `PackFile.write` boundary for
`x64linux` and `arm64linux`. Two independent allocation domains remain alive,
with independently allocated LIR string storage and readonly-data backing;
the test asserts different source, static-data, and code-buffer addresses.
The comparable closure has the same emitted body and artifact/data/spec order.
Placement-owned prefix and trailing padding differ and are excluded using the
producer's branch-island region classification.

Both producers emit an ordinary named external call, a real direct call back
to the body's entry, and a static-data address. The immutable readonly-data
fixture contains an explicit pointer relocation to byte three of its backing,
so extraction must preserve a nonzero addend and transitive data identity.
The test checks reference target/site/form/delta and relocation
name/scope/kind/offset, plus data target/addend/kind and exact backing bytes.
It then compares the complete serialized pack bytes without test-side
normalization or reordering.

On AArch64, a reduced branch-reach limit forces a detached external stub.
Different trailing placement makes the finalized BL instructions differ;
the test requires that difference before extraction and requires both
extracted calls to contain the normalized BL placeholder. This prevents a
vacuous equality check in which normalization was never exercised.
The existing changed-placement external-call tests remain intact.

## Proof boundary

This is a focused code-generator-operations fixture, not two end-to-end Roc
compilations. Its allocated source bytes establish independent ownership;
they are not parsed or lowered to produce the body. Its static exports are
explicit immutable producer inputs, not compile-time evaluator output.
The entrypoint-shaped region isolates native normalization from procedure
selection, demand scheduling, and upstream specialization identity.
Full compiler reproducibility still needs integration validation with fixed
source/dependency/compiler/target/options/spec inputs loaded independently.

`PackFile.write` preserves ordered input; it does not canonicalize arbitrary
artifact, data, relocation, or spec insertion orders. The test neither demands
that stronger property nor changes publication or cache concurrency behavior.
Boxy caching is outside this contract and this test.

## Validation

The initial backend filter selected zero tests; that apparent success was
withdrawn. Explicit test-mode references now discover the modules. Actual backend
execution now passes 5/5 tests with clean debug allocator checks, including four
codec cases and this test's `x64linux` and `arm64linux` loops.

Real compilation/execution exposed three fixture defects: an assertion needed an
explicit enum type, a certificate helper returned a stack-lifetime pointer, and
`TestLayoutState` destroyed arena-created state with a different allocator. These
are corrected, including partial-initialization cleanup. No assertion or target
was removed, and no production address-normalization change was required.
The proof remains bounded as described above.

To repeat the focused backend module test:

```sh
zig build -j2 run-test-zig-module-backend -- --test-filter "independent object producers persist identical normal lss artifact bytes"
```

The same module can run the retained placement test separately:

```sh
zig build -j2 run-test-zig-module-backend -- --test-filter "AArch64 finalized artifacts preserve external calls across changed placement"
```
