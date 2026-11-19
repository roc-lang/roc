# Record and codec fixes: validation

Issues: #10886, #12919; passing regression retained for #13067.

## Correctness

Validated with Zig 0.17.1:

```sh
zig build -j2 roc
zig build -j2 run-test-zig-module-check
zig build -j2 run-test-cli -- --filter 'issue 10886' --filter 'issue 12919' --filter 'JSON'
zig build -j2 run-snapshot-tool -- --check-expected
```

- The full checker module passes, including the unchanged original
  reproductions, omission-ownership tests, recursive callback error-row
  tests, and #13067's imported-record diagnostic.
- The CLI selection passes all 61 cases. After adding the final
  independent-format/state control, the six issue-specific CLI cases
  were rerun and pass. Both `--specialize=yes` and `--specialize=no`
  execute the tests.
- All 1,409 snapshots pass EXPECTED enforcement. A subsequent regeneration
  was byte-identical, verified with SHA-256 hashes without Git inspection.
- Zig formatting and all new Roc fixtures pass their formatting checks.

Snapshot regeneration changed 53 files, predominantly generated constraint
variable IDs. Reviewed semantic changes are earlier specialization of a
string interpolation, the ordering of the two conflicting interpolation
types, and reporting a bad generic interpolation argument at its use rather
than inside the generic definition. Only that diagnostic's EXPECTED location
needed changing.

Final snapshot validation used EXPECTED enforcement and byte-stable
regeneration rather than the tracked-file-difference gate.
No full MiniCI, full Zig suite, or full eval suite is claimed.

## Performance

Performance has not been measured with an optimized ReleaseFast compiler.
No performance claim is made.
