# Issue 12919: composite key codec call identity is discarded

Issue: https://github.com/roc-lang/roc/issues/12919

Reproduced with Zig 0.17.1 and the initial Debug compiler
(`debug-7c9ea425`). The original regression now passes unchanged under both
specialization strategies.

## Reproduction

`test/cli/issue_12919/Format.roc` is an encode-only counting format with no
external dependencies. Its expectation requires the tuple keys and their
values to produce `Ok(11)`. The test is neither skipped nor marked as an
expected failure.

```sh
zig build roc
zig-out/bin/roc check --no-cache test/cli/issue_12919/Format.roc
zig-out/bin/roc test --specialize=yes --no-cache test/cli/issue_12919/Format.roc
zig-out/bin/roc test --specialize=no --no-cache test/cli/issue_12919/Format.roc
```

Before the fix, checking succeeded but both test commands aborted with
SIGABRT (shell exit 134), before evaluating the expectation:

```text
postcheck invariant violated: sealed structural codec requested a format method absent from its checked contract
```

```text
Boxy lowering invariant violated: a generated codec called encode_key_start, which its checked contract does not record
```

The CLI harness registers both strategies in
`src/cli/test/parallel_cli_runner.zig:4240`, requiring successful execution
and `All (1) tests passed`:

```sh
zig build -j2 run-test-cli -- --filter 'issue 12919'
```

Initial harness result: 0 passed, 2 failed, 0 skipped; both compiler
children terminate with signal 6 at the corresponding invariant above.

Separate direct-tuple and scalar-dictionary controls both pass under
`--specialize=yes`. The tuple control uses the issue's generic
`a.encoder_for` helper; the scalar dictionary uses `encode_key_str`.

An initial eval-runner approach was stopped while compiling unrelated
deep-nesting cases, before any test result. It was replaced by the CLI
regressions above, which exercise the exact reported command and both
lowering strategies without duplicating the Roc source.

## Root cause observed before the fix

The method is present in the checked contract. Its lookup identity is
discarded by the generated encoder:

1. `Check.stepDerivedEncodeNominal` records `encode_key_start` with
   `args.key` as its subject (`src/check/Check.zig:64730`).
   `validateEncodeDictProtocolMethod` carries this through
   `finishGeneratedCodecMethodValidation` (`62617`), and
   `recordGeneratedCodecCall` retains it (`61909`).
2. Monotype preparation selects that same subject-bearing call
   (`src/postcheck/monotype/lower.zig:63423`) and indexes frozen calls by
   method and shape (`18520`). But `stepEncode` requests
   `encode_key_start` with a null subject (`59730`–`59735`).
   `formatCall` correctly distinguishes subject-free and subject-bearing
   keys (`18576`), so this lookup cannot find the prepared call.
3. Boxy has the same violation: `collectCalls` indexes the contract's
   subject (`src/postcheck/boxy/codec.zig:465`–`483`), but
   `lowerKeyThunk` requests a null subject (`1923`). Its exact lookup
   fails at `509`.

Thus this is not missing method validation or absent checker evidence.
The first incorrect step is each emitter throwing away the key subject
when addressing a call whose identity includes it.

## Implemented rule

Performance review identified a stronger producer-side representation than
preserving a separate key-shaped gate role. The hook takes encoding and state,
not a key; its role depends on the owning derivation's encoding/state/error
context, not the composite key shape.

Checking now validates and records one subject-free gate role per derivation.
A boundary-owned slot is checked before method instantiation or expected-type
allocation. Nested generated nominal derivations save and restore this slot
alongside their call-span boundary. Monotype preparation selects the same
subject-free role that both emitters already consume. No fallback, global
cache, or emitter-side contract search is added.

This shares compile-time evidence, not runtime execution: each composite key
still executes its gate, propagates a gate error, and passes the successful
state to its shape-specific encoder.

`src/check/test/issue_12919_test.zig` asserts one subject-free gate per
derivation with multiple key shapes and preserves the missing-gate diagnostic.
`test/cli/issue_12919/Gates.roc` covers tuple, record, and derived nominal keys;
nested nominal derivations; an observable state increment for every key;
gate error propagation; and scalar keys bypassing the composite gate.
The CLI harness executes both the unchanged repro and these controls with
`--specialize=yes` and `--specialize=no`.

An additional format uses `Str` state and an empty error row in the same
module; a checker test ensures another format without a gate is still
rejected after a valid gate has been checked.

See [combined validation and performance results](../record-codec-validation.md).
