# kcov source origin

The sources and build description are from
[roc-lang/zig-kcov](https://github.com/roc-lang/zig-kcov/tree/5e1954e53ce775a6ecf65abd03ae67deee64ac3c)
at commit `5e1954e53ce775a6ecf65abd03ae67deee64ac3c` (version 42.0.1).
The licenses are retained in `COPYING`, `COPYING.externals`, and source headers.
Only the runtime sources, report assets, entitlements, and build description
are included; the upstream test assets and packaging files are omitted.

The repository had no published Zig 0.17-compatible revision when this upgrade
was prepared. The build description now declares runtime arguments with
`Run.addPassthruArgs()` instead of reading the removed `Build.args` field.
The minimum Zig version is 0.17.0. kcov remains a lazy coverage-only dependency.
