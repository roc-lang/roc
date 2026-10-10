# Modules

Every `.roc` file is a _module_. Modules have two purposes:

- Namespacing
- Hiding

Roc has several different categories of modules, and they each hide different things:

- [Type modules](#type-modules) expose a single [type](types), including all its associated items (methods, nested types, etc.) and hide implementation details such as private helper functions called by that type's methods.
- [Package modules](#package-modules) expose one or more [type modules](#type-modules) and hide private modules that are only used behind the scenes.
- [Application modules](#application-modules) expose the entrypoints (e.g. `main`) required by the platform, and hide the implementation details which go into building those entrypoints.
- [Platform modules](#platform-modules) expose the [type modules](#type-modules) that application authors can import from the platform, and hide the configuration it uses to communicate with its lower-level [host](platforms#host) implementation.

## Type Modules

Type modules are specified by a .roc file with a capitalized name, such as `Url.roc`.

The file must contain a top-level [nominal type](types#nominal-types)
(defined with `:=`, or optionally with `::` to make it [opaque](types#opaque-nominal-types)) whose name is
the same as the filename without the `.roc` extension. Note that [type aliases](types#type-aliases)
(defined with `:`) don't satisfy this requirement.

So for example, if a type module has a filename of `Url.roc`, then it must have
something like `Url :=` or `Url ::` defined at the top level. We call that "the module's type."
For the `Url.roc` module, we'd say that its type is `Url`.

### Hiding implementation details

While modules can import the `Url` type from `Url.roc`, they can't see anything else
defined in the top level of `Url.roc`. So if `Url` defines a separate top-level nominal
type of `Foo :=` then that `Foo` type will only be visible inside `Url.roc`. Other
modules won't be able to access it. Similarly, if it defines a function or constant
named `blah =` at the top level, other modules won't be able to see that either.

The way to expose other nominal types, functions, and constants is to make them be
associated items on the `Url` type itself. For example:

```roc
Url :: { self : Str }.{
	# Other modules can access as Url.ParseErr
	ParseErr := …

	# Other modules can access as Url.from_str
	from_str : …
}
```

In this example, since `Url.ParseErr` is itself a type, you can nest other types inside it
to get something like `Url.ParseErr.Foo.blah`. The nesting can go as deep as you like, and
other modules can flatten out the nesting using `import` with the [`as` keyword](#renaming-imported-modules-with-as).

### Alias modules

If you want to make these nested modules easier to import, you can make an "alias module" whose
type is an alias of another type. For example, you could make `ParseErr.roc` and have its
type be a type alias of `Url.ParseErr` like so:

```roc
# ParseErr.roc

import Url

ParseErr : Url.ParseErr
```

Now you could import `ParseErr` directly. This isn't commonly done for things like error types,
though, because having it qualified as `Url.ParseErr` is useful; it tells you that it's
specifically a URL parsing error, which is more informative than a generic name like `ParseErr`.

Alias modules are more useful when exporting [mutually recursive types](#importing-mutually-recursive-types).

(Note: alias modules have not been implemented yet. Currently, the compiler requires a type
module's type to be nominal, and reports an error if it's a type alias.)

### "Void" modules

Although it is most common to organize modules around a single type, sometimes you just
want a collection of functions or constants. A classic example of this would be something like
`Util.roc`, which is a pattern that can be found in countless programming languages.

This is easy to do in Roc: expose a type which has no data inside it.

```roc
# Util.roc

Util :: [].{
	public_utility_function : …

	another_public_function : …
}

private_helper_fn : …
```

This is known as a _void module_ because it exposes an opaque _void type_ (namely, `[]`, which is
the [empty tag union type](tag-unions#void); the empty tag union type is known as "void" for short).

`Util` is [opaque](types#opaque-nominal-types), which prevents other modules from instantiating it,
and its backing type is `[]`, which means it can't even be instantiated inside `Util.roc`
itself. Choosing `[]` over `{}` for the backing type makes it clear that the `Util` type's
purpose is just to be a namespace, not to be a value that ever gets passed anywhere.

### Design Notes on Type Modules

Roc's "type modules" design is informed by the experiences of using modules in Elm and Rust.

#### Elm

In Elm, modules are formally decoupled from types, but there's a strong cultural norm for
modules to be built around one central type—and for the module's filename to be that type's
name followed by the `.elm` extension.

For example, `Url.elm` defines [the `Url` type](https://package.elm-lang.org/packages/elm/url/1.0.0/Url)
and lists that type among the module's publicly exposed items, along with the public functions which
operate on `Url` values. Private helpers are left out of the module's `exposing` list, and consumers
of this module typically write  `import Url exposing (Url)` to bring the `Url` type into scope.

In Elm, when you call a  function like `Url.parse`, the capitalized `Url` refers to the
_module_ `Url`, not the type. But in a type annotation, like `Url -> Bool`, the capitalized
`Url` refers to the _type_ `Url` that was imported from the `Url` module via `exposing (Url)`.
If `Url.elm` exposes a `ParseError` type, you might refer to it as `Url.ParseError` in type
annotations, where `Url` is the module name and `ParseError` is the type.

Similarly, if you have a module like `Util.elm`, you still capitalize it, and still use `Util.foo`
to call a `foo` function it exposes, but you don't have to define a `Util` type like you do in Roc.
Mutually recursive types (discussed [below](#importing-mutually-recursive-types)) work similarly in Elm to how
they work in Roc; you'd define `FooBar.elm` which exposes the mutually recursive types `Foo` and `Bar`,
and then import them using something like `import FooBar exposing (Foo, Bar)`.

Comparing Roc and Elm, the `Util` case is nicer in Elm (you don't need the void `Util` type),
and it's more obvious how to organize mutually recursive types (in Elm, you're already doing
`import ____ exposing ____` as a matter of course).

Roc optimizes for the common case at the expense of these less-common ones. You can
`import Url` instead of `import Url exposing (Url)`, you don't need to list the type(s)
and/or function(s) that `Url.roc` exposes (it's always just the type `Url` based on the
filename, and only that type's associated items are exposed), and both `Url.foo` and
`Url -> Bool` refer to the _type_ `Url`. Similarly, `Url.ParseErr` refers to a `ParseErr`
type associated with a `Url` type.

#### Rust

In Rust, it would be common to give the module a lowercase filename and then import the `Url` type
with `use crate::url::Url;`. Inside the `.rs` file (likely named either `mod.rs` or `url.rs`),
you'd find a definition of the `Url` type,  along with `impl Url { … }` where its associated items
would be found. When you call a function like `Url::parse` in Rust, the `Url` is referring to
the _type_, because the `url` _module_ is commonly lowercase in Rust.

Rust does not share Elm's strong cultural norm of organizing a module around a particular type.
This does happen, such as in the standard library's [`string` module](https://doc.rust-lang.org/stable/std/string/index.html)
being organized around the [`String` type](https://doc.rust-lang.org/stable/std/string/struct.String.html),
but you also see examples like the [`ffi` module](https://doc.rust-lang.org/stable/std/ffi/index.html) which
exposes the types [`CStr`](https://doc.rust-lang.org/stable/std/ffi/struct.CStr.html),
[`CString`](https://doc.rust-lang.org/stable/std/ffi/struct.CString.html),
[`OsStr`](https://doc.rust-lang.org/stable/std/ffi/struct.OsStr.html),
[`OsString`](https://doc.rust-lang.org/stable/std/ffi/struct.OsString.html), and doesn't particularly focus on any one of them.

The documentation for Rust's [`ffi::NulError`](https://doc.rust-lang.org/stable/std/ffi/struct.NulError.html) states:

> While Rust strings may contain nul bytes in the middle, C strings can't, as that byte would
> effectively truncate the string.
>
> This error is created by the new method on `CString`.

Both [`ffi::IntoStringError`](https://doc.rust-lang.org/stable/std/ffi/struct.IntoStringError.html) and
[`ffi::FromVecWithNulError`](https://doc.rust-lang.org/stable/std/ffi/struct.FromVecWithNulError.html)
are likewise used only in `CString` methods. Because these errors are specific to `CString`, Roc's convention
would be to nest them under the `CString` type. Something like:

```roc
# CString.roc

CString :: { … }.{
	NulError :: …

	IntoStringError :: …

	FromVecWithNulError :: …
}
```

Similarly, [`ffi::FromBytesWithNulError`](https://doc.rust-lang.org/stable/std/ffi/enum.FromBytesWithNulError.html) is only
used by a `CStr` method, so in Roc it would typically be nested under the `CStr` type.

Unlike Roc, Rust has a concept of private methods. This is not strictly necessary, as any Rust programmer could use
the same technique Roc embraces—namely, putting private helper functions at the top level, which is where they go in Elm too.
Rust already has a `pub` modifier, but Roc would have to introduce some equivalent just for allowing private methods as
a stylistic alternative to top-level helper functions (which would work the same way semantically), and it would necessarily
make Roc code more verbose.

The pitch of "increase complexity and verbosity to enable an alternative way to express something you can already express"
was not strong enough to justify adding private methods to Roc.

#### Roc

Both Elm and Roc do module-level caching, and disallow cyclic imports as a natural consequence.
Rust allows cyclic module imports because it caches at the package ("crate" in Rust parlance)
level rather than the module level. (As a similar consequence, Rust disallows packages from
cyclically depending on one another, as do Elm and Roc.) Cyclic module imports can be
convenient in Rust, but Rust's lack of module-level caching is a significant contributing
factor to Elm and Roc generally being known for much faster build times than Rust.

## `import` Statements

Roc's `import` statement brings a [type](types) into scope from a [type module](#type-modules):

```roc
import Color
import json.Parser
import pf.Stdout
```

Import statements can only appear at the top level of a module, and they can only import types.
They can't be used with any other category of module besides type modules.

### Exposing

Adding `exposing` to an import brings specific items into scope, so you can use them without
writing the type's name in front:

```roc
import pkg.Json exposing [to_str, decode]
import Http exposing [Request, Response]
```

Now `to_str` and `decode` can be called directly instead of `pkg.Json.to_str` and `pkg.Json.decode`. Types like `Request` and `Response` can be used in annotations without the `Http.` prefix.

### Renaming imported modules with `as`

Adding `as` to an import gives the imported type a different name in this module:

```roc
import Color as CC
import json.Parser as JP
```

### Modules in subdirectories

To import a module that's in a subdirectory, separate the directory names with `/`:

```roc
import Src/Widget as Widget
import Internal/Http/Client exposing [send]
```

These load `Src/Widget.roc` and `Internal/Http/Client.roc`, relative to the directory of the file
doing the importing. You can also start the path with `./` (which means the same thing), `../` to go
up a directory, or `/` to start at the package's root directory:

```roc
import Helper
import ./Internal/Parser
import ../Shared/Codec
import /Public/Api
```

Note the difference between `/` and `.` here. A `/` means "look in this directory," whereas a `.`
means "the type nested inside this one." So `import Url/ParseErr` loads the file `Url/ParseErr.roc`,
whereas `import Url.ParseErr` loads `Url.roc` and imports the `ParseErr` type nested inside `Url`.

A package can expose a module that's in a subdirectory by importing it with `as` and listing that
name in the package header:

```roc
package [Widget] {}

import Src/Widget as Widget
```

Modules outside the package then import it as `Widget` (for example, `import ui.Widget`). They
never see the `Src/` part, so the package can move the file around without breaking anyone.

When importing from another package, write the package's [shorthand](packages#shorthands), then
`.`, then the module's name. Any more `.`s after that refer to nested types:

```roc
import json.Parser
import json.Parser.ParseErr as PE
```

### Importing types from packages

Packages contain a collection of modules that are imported by applications, platforms or packages. Package dependencies are specified in the module header:

```roc
app [main!] { pf: platform "https://...", json: "https://..." }
```

This defines two package aliases: `pf` for the platform package and `json` for a JSON package. Use these aliases as prefixes when importing types from those packages:

```roc
import pf.Stdout
import json.Parser
```

### Importing constants

Since `import` only imports types, constants (and functions) are imported by way of the type they're
associated with. For example, given this type module:

```roc
# Color.roc
Color := [Red, Green, Blue].{
    all : List(Color)
    all = [Red, Green, Blue]

    to_str : Color -> Str
    to_str = |color| match color {
        Red => "red"
        Green => "green"
        Blue => "blue"
    }
}
```

...another module can `import Color` and then refer to `Color.all` and `Color.to_str`. To use them without
the `Color.` prefix, use [`exposing`](#exposing):

```roc
import Color exposing [all, to_str]

names = all.map(to_str)
```

If you have some constants that don't naturally belong to any particular type, you can associate them
with a [void module](#void-modules)'s type instead.

### Importing mutually recursive types

Occasionally, you may want to define two types in terms of each other. For example:

```roc
Foo := [BarVal(Bar), Nothing]

Bar := [FooVal(Foo), Nothing]
```

These [mutually recursive types](types#mutually-recursive) do not come up often, but when
they do, there's a helpful technique you can use to make them easier to import.

Since type modules expose a single type, you can't expose both `Foo` and `Bar` from the
same `.roc` file. However, you can wrap them both in a [void module](#void-modules) named something like `FooBar.roc`:

```roc
FooBar :: [].{
	Foo := [BarVal(Bar), Nothing]

	Bar := [FooVal(Foo), Nothing]
}
```

At this point you can `import FooBar` and then reference `FooBar.Foo` and `FooBar.Bar`,
or you could `import FooBar.Foo` and `import FooBar.Bar` to bring `Foo` and `Bar` into
scope unqualified.

You could also make separate [alias modules](#alias-modules) for `Foo` and `Bar`:

```roc
# Foo.roc

Foo : FooBar.Foo
```

```roc
# Bar.roc

Bar : FooBar.Bar
```

This would let you `import Foo` and `import Bar` even though they were defined in a single
module for purposes of referencing each other. This technique can be especially useful in
[package modules](#package-modules), which can choose to expose `Foo` and `Bar` but not
`FooBar`, such that end users don't even see the `FooBar` wrapper type.

(Note: as mentioned in the [alias modules](#alias-modules) section, alias modules have not
been implemented yet, so this technique doesn't work yet either.)

### Design Notes on Imports

Obviously, mutually recursive types take more effort to work with than other types.

This was an intentional design decision based on how rarely mutually recursive types
come up in practice. The cost of making the rare case nicer was making the common case more
complex, which seemed like the wrong tradeoff to make. As such, the rare case (mutually
recursive types) is now more work, which is the accepted drawback of this design.

Another important factor in this choice was build times. Roc is designed to make each
individual module cacheable, so that the compiler doesn't need to redo work when there
are no relevant changes to modules.

Some languages allow modules to import each other, forming _import cycles_. When module
imports form a cycle, then changing one module requires all the others in the cycle to be
rebuilt too. This makes cyclic imports a footgun for build times; it becomes very easy to
accidentally create a cycle, get no feedback that you have done this, and silently lose a
huge amount of caching. Worse, you can do this when a code base is small and not notice that
the compiler's ability to cache things has been decimated because even scratch-builds are
fast when a code base is small.

Roc intentionally disallows import cycles in order to prevent this from happening. If you
want to have modules reference each other, you have to put them in the same `.roc` file. This
adds friction (imports get more verbose, and the antidote for that is to create alias modules,
which is also extra effort), and that friction is how the language naturally pushes back on a code
organization strategy which unavoidably harms build times.

Having a large module cycle is easy to do by accident when cyclic imports are allowed,
but it is very difficult to do accidentally when doing so requires putting everything in one
giant `.roc` file. Putting things into one file also makes it more obvious that the compiler
can't benefit from module-level caching when doing this, since everything is in one big file.

In summary, mutually recursive types (and module cycles) inherently slow down builds by
precluding caching. Roc's design naturally leads to faster builds by disallowing cyclic
imports in favor of putting everything involved in a cycle into a single module, which makes
the unavoidable build time cost of doing so more obvious.

## Module Headers

[Type modules](#type-modules) specify which type they expose by choosing a filename that
matches it. Package modules, platform modules, and application modules all specify what
they expose or hide using a _module header,_ which is a section at the top of the file
that includes other information besides what's hidden and what's exposed.

Exactly what information goes in which headers will be discussed below.

## Package Modules

A [package](packages) is a collection of modules that can be shared between projects. Its
_package module_ (usually named `main.roc`) has a header that lists which type modules the package
exposes, along with the package's own dependencies:

```roc
package [
    Parser,
    Encoder,
    Decoder,
] { json: "..." }
```

A platform-specific package marks its platform dependency with the same
`platform` keyword used by an application:

```roc
package [FxHttp] {
    pf: platform "https://example.com/platform/1.0.0/content-hash.tar.zst",
    json: "https://example.com/json/2.0.0/content-hash.tar.zst",
}

import pf.Http
```

A package like this can use everything the platform exposes, including its effectful functions.
It doesn't provide anything for the platform's `requires` section, though; only the application
does that.

When an application uses a package like this, every platform mentioned anywhere in the dependency
graph has to be exactly the same platform. For URLs, that means the same version and the same hash.
(The usual [version selection](packages#package-versions) doesn't apply to platforms, since an
application can only have one.) For paths, they all have to point to the same file.

### Package Shorthands

The record at the end of a package module's header gives a [shorthand](packages#shorthands) to each of the
package's dependencies. Any module in the package can then use that shorthand to import types from the
dependency, as in `import json.Parser`. See [Packages](packages) for how dependencies are located and versioned.

## Platform Modules

A _platform module_ is the root module of a [platform](platforms). Its header describes how
applications and the platform's [host](platforms#host) connect to each other:

```roc
platform "my-platform"
    requires { main : Str -> Str }
    exposes [Http, File]
    packages { json: "../json/main.roc" }
    provides { "roc__entrypoint": main }
    targets: { … }
```

### requires

The `requires` section says what the application has to provide to the platform:

```roc
requires { main : Str -> Str }
```

If the platform needs the application to choose a type, use a `for` clause:

```roc
requires {
    [Model : model] for main : {
        init : model,
        update : model, Event -> model,
        render : model -> Str
    }
}
```

Here, the application defines a type named `Model`, and the platform refers to it as the type
variable `model`. That way, each application can choose its own `Model` type, and the platform
code works with all of them without knowing what's inside.

### exposes

The `exposes` section lists the type modules that applications (and packages) can import from the platform:

```roc
exposes [Stdout, Stderr, File, Http]
```

### packages

The `packages` section lists the platform's own package dependencies, with their
[shorthands](packages#shorthands):

```roc
packages { json: "../json/main.roc" }
```

### provides

The `provides` section lists the Roc functions the host can call, and the name of the symbol each
one will have when the host links against it:

```roc
provides { "roc__entrypoint": main }
```

### targets

The `targets` section lists which targets the platform supports, what gets linked together for
each one, and what kind of file each one produces:

```roc
targets: {
    inputs_dir: "targets/",
    x64linux: { inputs: ["crt1.o", "host.o", app] },
    arm64mac: { inputs: ["host.o", app] },
    wasm32: { inputs: ["host.wasm", app], output: Shared, exports: ["run"] },
}
```

- `inputs_dir` is the directory (inside the platform's bundle) that contains the files for each target.
- Each target lists its `inputs`, which are the files that get linked together, and optionally an `output`.
- These paths have to stay inside the platform's directory. An absolute path (like `/usr/lib/foo.a`
  or `C:\foo.lib`), or a path with a `..` in it, gives an "Invalid Target Path" error. Otherwise,
  a platform you downloaded could make `roc build` link in any file on your computer.
- WebAssembly targets that get linked must list the functions the final module exports to the
  outside world, using `exports`. (`exports: []` exports none.)

The `output` field says what kind of file the target produces:

- `Exe` (the default): a linked executable. For wasm32, a command module with an entry point.
- `Shared`: a shared library (`.so`, `.dylib`, `.dll`). For wasm32, a reactor module: no entry point, with the `provides` entrypoints exported.
- `Archive`: a static archive (`.a`, `.lib`) containing the host inputs, the compiled app, and the builtins, for linking in another build.

The platform decides what kind of file gets built, so application authors never need to tell
`roc build` that.

`app` in the `inputs` list stands for the compiled Roc application. The order of the inputs matters,
because that's the order they get passed to the linker.

Running `roc build` without a `--target` flag builds for the first target in this list that's
compatible with the machine running the build.

Linking takes nothing from a toolchain installed on the machine running the build: no system C runtime, SDK, or default library is searched for. On Linux, BSD, and Windows targets the link uses only the listed `inputs` and Roc's own objects. A macOS target additionally links `libSystem`, and the frameworks in a `macos-sysroot` directory the platform provides, from that sysroot or the minimal one bundled with Roc. A Windows target (`x64win`, `arm64win`, `x64mingw`, `arm64mingw`) therefore lists everything a Windows link needs:

- the entry point the linker infers, `mainCRTStartup` (or `_DllMainCRTStartup` for a `Shared` output), from a startup object or archive;
- whatever C runtime the host calls into;
- import libraries for `kernel32` and `ntdll`, which Roc's runtime imports from, and for any other DLL the host uses.

```roc
x64win: { inputs: ["host.lib", app, "startup.lib", "ucrtbase.lib", "kernel32.lib", "ntdll.lib"] },
```

An import library can be generated from a module-definition file with `zig dlltool -m i386:x86-64 -d kernel32.def -l kernel32.lib`. Because no input comes from an installed toolchain, a Windows target builds the same on a machine without Visual Studio and when cross-compiling.

### Hosted type modules

A platform's type modules can declare _hosted_ functions, which are implemented by the platform's
[host](platforms#host) rather than in Roc. A hosted function is declared as an associated item with a type
annotation but no implementation:

```roc
# Stdout.roc
Stdout := [].{
    line! : Str => {}
}
```

The platform module's header then lists each hosted function in its `hosted` section, along with the
name of the symbol the host uses to implement it:

```roc
platform ""
    requires { main! : List(Str) => Try({}, [Exit(I8), ..]) }
    exposes [Stdout]
    packages {}
    provides { "roc_main": main_for_host! }
    hosted { "roc_stdout_line": Stdout.line! }
```

Here, the host must provide a function named `roc_stdout_line`, and whenever Roc code calls `Stdout.line!`,
that host function will be called.

Hosted functions have a few restrictions:

- They must be [effectful functions](functions#effectful-functions) (with `=>` in their types), because the compiler can't run host code during [compile-time evaluation](compile-time).
- Every hosted function declaration must be listed in the platform's `hosted` section; otherwise, there would be no host function for calls to it to reach.
- Type variables in a hosted function's type can only appear inside a `Box`. A `Box` is always a pointer at runtime no matter what it contains, so this lets a single host function work for every type the variable could be.

## Application Modules

An _application module_ is the root module of a Roc program. It provides whatever the platform's
[requires](#requires) section asks for:

```roc
app [main!] { pf: platform "https://..." }

import pf.Stdout

main! = |_| {
    Stdout.line!("Hello!")
}
```

The application header has two parts:

- **The list** (`[main!]` here) names the things the application provides to the platform.
- **The record** (`{ pf: platform "…" }` here) lists the application's dependencies, with their [shorthands](packages#shorthands).

Exactly one of the dependencies has to be a platform, marked with the `platform` keyword:

```roc
app [main!] {
    pf: platform "../basic-cli/main.roc",
    json: "../json/main.roc"
}
```

Packages the application uses may also depend on a platform, but it has to be exactly the same
platform the application uses. Only the application provides what the platform requires.

### Pinning a Roc version

An application, package, or platform can say which version of the Roc compiler it was written
for, using a `roc` entry in its dependencies:

```roc
app [main!] {
    pf: platform "../basic-cli/main.roc",
    roc: "nightly-2026-08-05-24f0b47"
}
```

This is optional. If it's there, it has to be a version in the format that `roc version` prints:
either a nightly like `nightly-2026-08-05-24f0b47`, or a release like `0.1.0`. (That's also why
`roc` can't be used as the shorthand for a package.)

If you compile it with a different version of the compiler, you get a warning, but the build
still goes ahead.

`roc fmt` keeps a nightly version up to date: if the compiler running `roc fmt` is a nightly that's
at least as new as the one written in the header, it updates the header to say that compiler's
version. It leaves release versions alone, since writing a release version is a deliberate choice,
whereas a nightly version usually just means "whatever nightly was current when I wrote this."
Since this is part of formatting, `roc fmt --check` reports an out-of-date nightly version as
needing to be formatted.

### Nominal type identity across packages

Two [nominal types](types#nominal-types) from different places are the same type if they have the
same name, and the modules that declare them have exactly the same contents (including the
contents of every module they import, and every module those import, and so on).

This matters when the same module shows up more than once in a project's dependencies. For
example, two different versions of a package might both include a module that didn't change
between those versions, or you might have downloaded the same package from two different URLs.
In those cases, the types that module declares are the same type, no matter where they came
from, so you can pass a value from one to code expecting the other. On the other hand, if the
module (or anything it imports) changed at all, even by one byte, then its types are different
types, even if they have the same names and look the same.

The exception is `hosted` functions and `provides` entries in platforms. Those are identified by
the symbol names in the platform header, not by the module's contents, so two hosted functions
with different symbol names are always different functions.

### Headerless Application Modules

To facilitate tutorials, Roc permits application modules to omit the header entirely.
When this is done, the application automatically receives the built-in "Echo Platform"
which exposes a single function—`echo!`—that prints to stdout when compiled to machine code,
and to an externed wasm function (which might be wired up to either `console.log` or to
a UI for displaying printed output) in WebAssembly. This `echo!` function is automatically
imported unqualified into the application's scope, so that a complete Hello World in Roc can be:

```roc
main! = |_args| echo!("Hello, World!")
```

The `main!` function the Echo Platform receives will get command-line arguments, if applicable,
as a `List(Str)`. (In WebAssembly, these won't be _command-line_ arguments, but rather arbitrary
arguments from the outside world.)

An application that needs package dependencies can still use the Echo Platform, by writing an
`app` header that names no platform:

```roc
app [main!] {
    unicode: "https://github.com/roc-lang/unicode/releases/download/4.0.0/3DGC3M4b2pxaRLg4i8cmxWkm2E2WbCPCLntQzf2mkbUV.tar.zst",
}

import unicode.Grapheme

main! = |_| {
    echo!(Grapheme.owned("café") |> Str.inspect)
    Ok({})
}
```

Such an application gets the Echo Platform and its unqualified `echo!` exactly as a headerless
one does; the header is there only to declare the packages it imports from. Writing the header
without any packages (`app [main!] {}`) is the same thing as writing no header at all.

The Echo Platform is intentionally limited to this one effectful function because that's all
that is needed to teach a wide variety of beginner Roc concepts—expressions, defining and calling functions,
looping over inputs that get evaluated at runtime (as opposed to compile-time, as user-defined constants would be),
type annotations, effectfulness, and so on. Once the learner has gotten the desired amount of experience,
the tutorial can introduce the `app` module header and move on to a more featureful platform.

The Echo Platform is explicitly intended to be too bare-bones for production use cases. The reason for this
is partly to avoid excessive favoritism in platforms (reputation alone creates plenty of bias towards some
platforms over others; not even having to separately download certain blessed platforms would discourage
competition and innovation), but also to prevent needing to version the platform, document it (the tutorial can
cover the handful of facts there are to know about it), and so on.
