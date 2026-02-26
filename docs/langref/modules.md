# Modules

Every `.roc` file is a _module_. Modules have two purposes:

- Namespacing
- Hiding

Roc has several different categories of modules, and they each hide different things:

- [Type modules](#type-modules) expose a single [type](types), including all its associated items (methods, nested types, etc.) and hide implementation details such as private helper functions called by that type's methods.
- [Package modules](#package-modules) expose one or more [type modules](#type-modules) and hide private modules that are only used behind the scenes.
- [Application modules](#app-modules) expose the entrypoints (e.g. `main`) required by the platform, and hide the implementation details which go into building those entrypoints.
- [Platform modules](#platform-modules) expose the [type modules](#type-modules) that application authors can import from the platform, and hide the configuration it uses to communicate with its lower-level [host](platforms#host) implementation.

## Type Modules

Type modules are specified by a .roc file with a capitalized name, such as `Url.roc`.

The file must contain a top-level [nominal type](types#nominal-types)
(defined with `:=`, or optionally with `::` to make it [opaque](types#opaque-types)) whose name is
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
other modules can flatten out the nesting using `import` with the [`as` keyword](#import-as).

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

Alias modules are more useful when exporting [mutually recursive types](#mutually-recursive-type-modules).

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

`Util` is [opaque](types#opaque-types), which prevents other modules from instantiating it,
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
Mutually recursive types (discussed [below](#mutually-recursive-types)) work similarly in Elm to how
they work in Roc; you'd define `FooBar.elm` which exposes the mutually recursive types `Foo` and `Bar`,
and then import them using something like `import FooBar exposing (Foo, Bar)`.

Comparing Roc and Elm, the `Util` case is nicer in Elm (you don't the void `Util` type),
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
module being organized around the [`String` type](https://doc.rust-lang.org/stable/std/string/struct.String.html),
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
factor to Elm and Roc being generally being known for much faster build times than Rust.

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

Use `exposing` to bring specific items into scope without a package qualifier or module prefix:

```roc
import pkg.Json exposing [encode, decode]
import Http exposing [Request, Response]
```

Now `encode` and `decode` can be called directly instead of `pkg.Json.encode` and `pkg.Json.decode`. Types like `Request` and `Response` can be used in annotations without the `Http.` prefix.

### Renaming imported modules with `as`

Use `as` to give an import a different name:

```roc
import Color as CC
import json.Parser as JP
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

### Importing mutually recursive types

Occasionally, you may want to define two types in terms of each other. For example:

```roc
Foo := [BarVal(Bar), Nothing]

Bar := [FooVal(Foo), Nothing]
```

These [mutually recursive types](types#mutually-recursive) do not come up often, but when
they do, there's a helpful technique you can use to make them easier to import.

Since type modules expose a single type, you can't expose both `Foo` and `Bar` from the
same `.roc` file. However, you can wrap them both in a [void module](#void-module) named something like `FooBar.roc`:

```roc
FooBar :: {}.{
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
which is also extra effort), and that friction is the language naturally pushes back on a code
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

Packages are collections of types that can depend on other packages.

They have their own (lowercase) namespaces, so for example:

```roc
package [] {

} depends [
# TODO what's the new syntax for this? introduce package shorthands here, since app modules are down below
]
```

### Package Shorthands

## Package Modules

A _package module_ provides types to be shared with packages, applications and platforms. The module header specifies which types are exposed, and also includes package aliases for importing other packages:

```roc
package [
    Parser,
    Encoder,
    Decoder,
] { json: "..." }
```

## Platform Modules

A _platform module_ defines the interface between a Roc application and the host program.

```roc
platform "my-platform"
    requires { main : Str -> Str }
    exposes [Http, File]
    packages { json: "../json/main.roc" }
    provides { entrypoint: "roc__entrypoint" }
    targets { ... }
```

### requires

The `requires` section declares what the application must provide to the platform:

```roc
requires { main : Str -> Str }
```

For an app to provide a type to the platform, use a `for` clause:

```roc
requires {
    [Model : model] for main : {
        init : model,
        update : model, Event -> model,
        render : model -> Str
    }
}
```

The `[Model : model]` syntax maps an uppercase type alias (`Model`) to a lowercase rigid type variable (`model`). This allows the app to provide a `Model` type which remains opaque to the platform.

### exposes

The `exposes` section lists the types the platform provides to the application:

```roc
exposes [Stdout, Stderr, File, Http]
```

### packages

The `packages` section specifies package dependencies and aliases these with package qualifiers, e.g. `json.`:

```roc
packages { json: "../json/main.roc" }
```

### provides

The `provides` section maps function identifiers to the symbols names Roc will link with the platform host:

```roc
provides { entrypoint: "roc__entrypoint" }
```

### targets

The `targets` section specifies the supported build targets and default linking behaviour:

```roc
targets : {
    files: "targets/",
    exe: {
        x64linux: ["crt1.o", "host.o", app],
        arm64mac: ["host.o", app]
    },
    static_lib: {
        x64: ["lib.a", app]
    }
}
```

- `files`: The directory containing target-specific files within a package `.tar.zst` bundle.
- `exe`: Executable build targets
- `static_lib`: Static library build targets

The `app` placeholder represents the compiled Roc application. The order files are specified in each of the build targets is important for linking correctly.

The default behaviour for `roc build` without a `--target` flag is the first compatible target in the `exe` or `static_lib` sections.

### Hosted type modules

## Application Modules

An _application module_ is the entry point for a Roc program. The app provides implementations that satisfy the platform's [requires](#requires) section:

```roc
app [main!] { pf: platform "https://..." }

import pf.Stdout

main! = |_| {
    Stdout.line!("Hello!")
}
```

The application header has two parts:
- **Exposed list** `[main!]`: Implementations the app provides to satisfy the platform's requirements
- **Packages record** `{ pf: platform "..." }`: Specifies the platform and package dependencies for the application

The app must have a platform package which is marked using the `platform` keyword:

```roc
app [main!] {
    pf: platform "../basic-cli/main.roc",
    json: "../json/main.roc"
}
```

### Headerless Application Modules

To facilitate tutorials, Roc permits application modules to omit the header entirely.
When this is done, the application automatically receives the built-in "Echo Platform"
which exposes a single function—`echo!`—that prints to stdout when compiled to machine code,
and to an externed wasm function (which might be wired up to either `console.log` or to
a UI for displaying printed output) in WebAssembly. This `echo!` function is automatically
imported unqualified into the application's scope, so that a complete Hello World in Roc can be:

```roc
main! = |_args| echo! "Hello, World!"
```

The `main!` function the Echo Platform receives will get command-line arguments, if applicable,
as a `List(Str)`. (In WebAssembly, these won't be _command-line_ arguments, but rather arbitrary
arguments from the outside world.)

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
