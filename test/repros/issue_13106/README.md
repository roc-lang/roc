# Exposed types must have consistent lookup

Reproduction for [#13106](https://github.com/roc-lang/roc/issues/13106).

From the repository root:

```sh
zig build roc
(cd test/repros/issue_13106 && ../../../zig-out/bin/roc run --no-cache main.roc)
zig build run-test-zig-module-check -Dtest-filter="issue 13106" -- --test-filter "issue 13106"
```

The import must report `Reserved Type Name`, without a misleading
`Bool.show does not exist` or invalid `Yes` tag diagnostic.
Renaming the exposed binding to `Flag` makes the same program print `yes`
without errors (the declaration and deprecated-header warnings remain).

The focused checking tests use a modern type module to avoid the unrelated
deprecated-header warning. They require exactly the import-site error and no
secondary checking problems. They cover exposed names, aliases, nested imports,
forward uses, annotations, associated values, and nominal tags and patterns.

## Policy

Imports must not introduce builtin type names. Previously, scope binding
accepted those names while qualified lookup selected the builtin, contradicting
the shadowing warning. Rejecting the binding at its introduction keeps both
the name policy and diagnostics consistent. Uses of rejected names become
explicit errors, never implicit builtin references. Compiler-generated builtin
operations and the separate local type declaration policy remain unchanged.
