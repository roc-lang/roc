# Packages

TODO

## Shorthands

TODO

## Inspecting dependencies with `roc deps`

`roc deps main.roc` resolves the dependency graph of an app, package, or
platform and prints it as a tree without compiling anything. Every edge shows
the complete URL or path exactly as the header declares it (never its
shorthand), so it can be copied straight into `--replace-dep`:

```text
main.roc (/home/me/app/main.roc) [app]
├── https://example.com/basic-cli/0.23.0/3hT3So…4r1i.tar.zst [platform]
├── https://example.com/roc-ascii/0.5.0/5WxqRf…9Z3H.tar.zst
└── https://example.com/roc-ansi/0.13.0/JXLM47…ovL.tar.zst
    └── https://example.com/roc-ascii/0.5.0/5WxqRf…9Z3H.tar.zst [shared]

[shared]  this package was already shown above, where its dependencies are listed
```

Each shared package's subtree is expanded once; later occurrences are marked
`[shared]`. When version selection picks a different compatible release than
a header declares, the tree shows `[resolved to URL]` next to the declaration.

## Replacing dependencies with `--replace-dep`

To test a local change, try a fork, or build against a different release
without editing any header, pass `--replace-dep OLD NEW` to `roc run`,
`build`, `check`, `test`, `docs`, or `deps`. `OLD` and `NEW` are each a
complete package URL or an explicit path to a root `.roc` file; the flag can
be repeated.

```sh
# Use a local platform in place of its published URL.
roc build app.roc --replace-dep "$PLATFORM_URL" ../basic-cli/platform/main.roc

# Replace a package everywhere it is declared, including inside other packages.
roc test app.roc --replace-dep "$ASCII_URL" ../roc-ascii/main.roc
```

Matching is exact: every declaration of exactly `OLD` (including its version
and hash, or its canonical file path) loads `NEW` instead, whatever shorthand
each header gives it, and nothing else is rewritten. Different release URLs
need separate flags. A flag that matches nothing is an error, as is a
shorthand such as `ascii` in place of a full URL or path. The replacement's own
header decides its dependencies, and those declarations are subject to the
same flags. `roc deps main.roc --replace-dep …` shows the resulting graph with
each replaced declaration marked `[replaced by …]`.

Replacements last for one invocation only. They never edit source files or
touch cached packages.
