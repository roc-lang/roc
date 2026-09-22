# Packages

A _package_ is a collection of Roc modules which can be shared between projects. For example, a
package for working with JSON might expose modules named `Parser` and `Encoder`.

A package's root module is its [package module](modules#package-modules), which is usually named `main.roc`.
Its header lists which [type modules](modules#type-modules) the package exposes, along with the
package's own dependencies:

```roc
package [Parser, Encoder] {}
```

Modules in the package which aren't listed there can still be imported by the package's other modules,
but they can't be imported by anything outside the package. This lets packages have private
implementation details.

## Depending on Packages

Applications, platforms, and packages declare their package dependencies in the record at the end of
their module header. Each dependency is given a [shorthand](#shorthands) and a location:

```roc
app [main!] {
    pf: platform "https://example.com/basic-cli/1.0.0/4ZuGvfzgQ8bm8nVz7dLNYRTYHoNeuDLPdrvzeAUm7gwF.tar.zst",
    json: "https://example.com/json/2.1.0/7AfjuYkuixaofoi12pAudKZpk557rKy1aSW3Ugxr6dab.tar.zst",
    util: "../util/main.roc",
}
```

A package's location can be either a URL or a filesystem path.

### URL Packages

A URL package is a compressed `.tar.zst` bundle of the package's `.roc` files, downloaded over HTTPS.
(Plain HTTP is only allowed for `localhost`, which is useful when testing.) Downloaded packages are
cached, so each one only needs to be downloaded once.

The last part of the URL, before `.tar.zst`, must be a hash of the bundle's contents. After downloading
a bundle, Roc verifies that its contents match that hash, and refuses to use it if they don't. This means
that a given URL always refers to exactly the same package contents, even if the server hosting it gets
compromised.

The `roc bundle` command creates a bundle from a package's `.roc` files, and names the resulting file
after its hash:

```sh
roc bundle main.roc Parser.roc Encoder.roc
```

This creates a file with a name like `7AfjuYkuixaofoi12pAudKZpk557rKy1aSW3Ugxr6dab.tar.zst`, which can
then be uploaded anywhere that can serve static files over HTTPS.

### Package Versions

A package URL can include a version number in `MAJOR.MINOR.PATCH` format, such as the `2.1.0` in
`https://example.com/json/2.1.0/7Afj….tar.zst`. The version can appear anywhere in the URL before the hash,
but only once.

When the same package is depended on multiple times in a project's dependency graph (for example,
because two different packages both depend on it), Roc uses version numbers to pick a single version
for the whole project:

- Versions with different major numbers (for example, `1.4.0` and `2.0.0`) are considered completely different packages, so both are used.
- Versions with the same major number (for example, `2.1.0` and `2.3.1`) are considered compatible, so only the highest version mentioned is used.
- For versions with a major number of `0`, the minor number is treated as the major number. So `0.3.0` and `0.4.0` are considered different packages, while `0.3.0` and `0.3.5` are considered compatible.

An application's own dependencies are used exactly as written. If some other package in the dependency graph
requires a higher version of the same package than the application specifies, that's an error which shows the
chain of dependencies involved, rather than a silent upgrade.

The `roc bump` command compares a package's public API against a previous version, and reports whether
the changes require a major, minor, or patch version bump.

URLs without a version number don't participate in this process; each one is treated as a separate package.

### Path Packages

A dependency can also be a path to a package's `main.roc` on the local filesystem, relative to the module
declaring the dependency:

```roc
util: "../util/main.roc"
```

Path packages are convenient for developing multiple packages together, or for splitting a large
project into multiple packages without publishing them. They aren't downloaded, cached, or versioned;
Roc reads them directly from the filesystem.

The `expect`s in path packages are run by [`roc test`](statements#expect), because they're part of your
project. The `expect`s in URL packages are not.

## Shorthands

A package's _shorthand_ is the lowercase name its dependent gives it in the module header, such as `json`
in `json: "https://…"`. Modules use the shorthand to [import](modules#import-statements) the package's
modules:

```roc
import json.Parser
```

Shorthands follow the rules for [lowercase names](naming#lowercase-names), and they may not
include `$` or `!`. The name `roc` is reserved for [pinning a Roc version](modules#pinning-a-roc-version),
so it can't be used as a shorthand.

A shorthand is chosen by the dependent, not by the package itself, and it's only in scope within the
application, package, or platform whose header declares it. So if an application refers to a JSON package as `json`, and a package
it depends on refers to the same JSON package as `j`, both work. This also means that if two packages
happen to have similar names, each dependent can choose whatever shorthands it likes to tell them apart.

## Platforms

A [platform](platforms) is a special kind of package which provides the lower-level functionality that
an application builds on. Every application has exactly one platform, which is marked with the `platform`
keyword in the application's header. See [Platform Modules](modules#platform-modules) for details.

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
