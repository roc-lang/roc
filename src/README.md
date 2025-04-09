## Status

This table provides a summary of progress for the zig compiler re-write and should be updated with the PR that includes new features.

|                          | Str & Num * | Functions  | Modules | Collections | Records &  Tuples | Recursive  Types | Static  Dispatch |
|--------------------------|:-----------:|:----------:|:-------:|:-----------:|:-----------------:|:----------------:|:----------------:|
| **Parse**                | 🪫          | 🪫         | 🚧      | 🪫          |  🪫               |  🪫              |  🪫              |
| **Canonicalize**         | 🚧          | 🚧         |         |             |                   |                  |                  |
| **Resolve Imports**      |             |            |         |             |                   |                  |                  |
| **Check Types**          |             |            |         |             |                   |                  |                  |
| **Intepreter**           |             |            |         |             |                   |                  |                  |
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

Try it with `zig build -Dno-bin -fincremental --watch`

### Expanding to ZLS

This fast config can also be used with `zls`. Simply follow these steps:
1. run `zls --version` and make sure it is `0.14.0`.
2. run `zls env` and grab the `config_file` path.
3. Edit the config file to include
```json
{
  "enable_build_on_save": true,
  "build_on_save_args": ["-Dno-bin", "-fincremental"]
}
```
4. Advised, also changing the cache dir, I use `--cache-dir .zig-cache/zls`.
Otherwise, zig commands run manually can lead to the lsp breaking and requiring a restart.
5. Optionally, add `-Dfuzz` above as well to get type checking of fuzz scripts as well.
6. Note, I had to fully delete my `.zig-cache` to get `zls` to start.
Make sure to check the logs if you aren't geting type failures.
7. Enjoy better lsp results.

### Simply testing feedback loop

Sadly, this is not nearly as fast due to building binaries.
One day, we will get dev zig backends, and it should be fast.

Try it with `zig build test -fincremental --watch`
