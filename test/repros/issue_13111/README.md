# Package dependency must name a package

Reproduction for [#13111](https://github.com/roc-lang/roc/issues/13111).

From the repository root:

```sh
zig build roc
(cd test/repros/issue_13111 && ../../../zig-out/bin/roc check sub/main.roc)
zig build run-test-zig-module-compile -Dtest-filter="issue 13111" -- --test-filter "issue 13111"
```

The CLI must reject `base: "../Shared.roc"` at the dependency declaration,
not report `Shared.value does not exist`. The unit tests require
`ResolutionFailed` and an `Invalid Package Dependency` diagnostic, including
source highlighting after a sidecar round trip. Standalone module roots remain
valid, and transitive URL dependencies obey the same header-kind contract.

## Invariant

Resolution must not output an ordinary module as a dependency package.
Silently dropping such an entry later loses its shorthand and turns a header
error into misleading use-site errors, or no error for an unused import.

The edge contract belongs in resolution: ordinary dependencies require a
package header, platform-marked dependencies require a platform header.
Dependency source regions survive scanning and URL sidecars so errors point
at the declaring header without reparsing successful dependency graphs.
