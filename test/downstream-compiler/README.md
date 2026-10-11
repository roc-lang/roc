# Downstream compiler consumer

This separate Zig executable uses only `dependency.module(...)` to import Roc's
compiler. It checks a small application, lowers its platform entrypoint to LIR,
builds and reads a LIR image, and executes the entrypoint with the interpreter,
asserting that the returned `U64` is 42.

Run from the repository root:

```sh
zig build build-test-downstream-package
zig build run-test-downstream-package
```

The build helper snapshots the compiler sources, runs `zig fetch` (which applies
Roc's `build.zig.zon` `.paths`), and generates a consumer manifest with a URL and
hash for the resulting package archive. It builds in a separate directory;
there are no relative imports of the compiler checkout. The run step only
executes the built consumer. MiniCI includes both phases.

The manifest is generated because its dependency hash must describe the current
compiler sources. In a real downstream project, pin a published commit instead:

```sh
zig fetch --save=roc 'git+https://github.com/roc-lang/roc.git#<full-commit-sha>'
```

Use this fixture's `build.zig` as an example of importing the named modules.
See `src/compile/README.md` for the embedding contract. The platform needs no
native host library because this test executes its entrypoint in-process.
