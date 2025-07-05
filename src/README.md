## Status

This table provides a summary of progress for the zig compiler re-write and should be updated with the PR that includes new features.

|                          | Str & Num * | Functions  | Modules | Collections | Records &  Tuples | Recursive  Types | Static  Dispatch |
|--------------------------|:-----------:|:----------:|:-------:|:-----------:|:-----------------:|:----------------:|:----------------:|
| **Parse**                | 🪫          | 🪫         | 🚧      | 🪫          |  🪫               |  🪫              |  🪫              |
| **Canonicalize**         | 🚧          | 🚧         |         |             |                   |                  |                  |
| **Resolve Imports**      |             |            |         |             |                   |                  |                  |
| **Check Types**          |             |            |         |             |                   |                  |                  |
| **Interpreter**          |             |            |         |             |                   |                  |                  |
| **Specialize Types**     |             |            |         |             |                   |                  |                  |
| **Lift Functions**       |             |            |         |             |                   |                  |                  |
| **Solve Functions**      |             |            |         |             |                   |                  |                  |
| **Specialize Functions** |             |            |         |             |                   |                  |                  |
| **Reference Counting**   |             |            |         |             |                   |                  |                  |
| **Lower IR**             |             |            |         |             |                   |                  |                  |
| **Gen LLVM**             |             |            |         |             |                   |                  |                  |

- N/A   Not applicable
- 🚧    Work Started
- 🪫    Tests Passing
- 🔋    Polished

## Fast Feedback Loop

The roc zig compiler can have a very fast feedback loop. We support zigs incremental compilation and watch mode.
By avoiding generating final executables, we can build and typecheck much much faster.

Try it with `zig build check-test -fincremental --watch`

Note: `check-test` is recommended to get good error coverage while avoiding duplicate errors due to multiple build points.
`check` will show errors for each entrypoint that hits the error (often leading to many duplicate errors).

:warning: `--watch` and `-fincremental` have bugs on macos currently. They may crash or otherwise break. As such, it is best to remove them for now on  macos.
An alternative is to install [entr](https://github.com/eradman/entr) and run `find src/ *.zig | entr -ccr zig build check-test`.

### Expanding to ZLS

This fast config can also be used with `zls`. Simply follow these steps:
1. run `zls --version` and make sure it is `0.14.0`.
2. run `zls env` and grab the `config_file` path.
3. Edit the config file to include
```json
{
  "enable_build_on_save": true,
  "build_on_save_args": ["check-test", "-fincremental", "--watch"]
}
```
4. Advised, also changing the cache dir, I use `"--cache-dir", ".zig-cache/zls"`.
Otherwise, zig commands run manually can lead to the lsp breaking and requiring a restart.
5. Note, I had to fully delete my `.zig-cache` to get `zls` to start.
Make sure to check the logs if you aren't getting type failures.
6. Enjoy better lsp results.

### Simply testing feedback loop

Sadly, this is not nearly as fast due to building binaries.
One day, we will get dev zig backends, and it should be fast.

Try it with `zig build test --watch`

## Overview

![Zig Dependency Graph](https://anton-4.github.io/roc-compiler-vis/zig_dependency_graph.webp)
