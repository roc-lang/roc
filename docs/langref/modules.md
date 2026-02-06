# Modules

A _module_ is a single `.roc` file. There are four kinds of modules:

- **Type modules** define a type with associated functions
- **App modules** are the entry point for a Roc program
- **Package modules** provide types to be shared with other packages, applications and platforms
- **Platform modules** interface between the Roc application and the platform Host

## Type Modules

A _type module_ is a module that defines a [type](./types.md). The module filename without the `.roc` extension must match the name of the type being defined (e.g., `Color.roc` defines a module named `Color`).

A type module must contain a [nominal type](./types.md#nominal-types) (`:=`) or [opaque nominal type](./types.md#opaque-nominal-types) (`::`) matching the filename. [Type aliases](./types.md#type-aliases) (`:`) are not valid for type modules. 

A type can also define [associated functions](./static-dispatch.md#methods) (also referred to as methods) which can be called using [static dispatch](./static-dispatch.md). 

```roc
Color := [Red, Green, Blue].{
    to_str : Color -> Str
    to_str = |color|
        match color {
            Red -> "red"
            Green -> "green"
            Blue -> "blue"
        }
}
```

This file would be named `Color.roc`. The nominal type `Color` is defined with `:=`, and `to_str` is an associated function.


## Imports

The `import` statement brings a module into scope:

```roc
import Color
import json.Parser
import pf.Stdout
```

Import statements can only appear at the top level of a module.

### Exposing

Use `exposing` to bring specific items into scope without a package qualifier or module prefix:

```roc
import pkg.Json exposing [encode, decode]
import Http exposing [Request, Response]
```

Now `encode` and `decode` can be called directly instead of `pkg.Json.encode` and `pkg.Json.decode`. Types like `Request` and `Response` can be used in annotations without the `Http.` prefix.

### Renaming with `as`

Use `as` to give an import a different name:

```roc
import Color as CC
import json.Parser as JP
```

### Directory Imports

Modules can be organized in directories. Use dot notation to import from subdirectories:

```roc
import Utils.Helpers
import Models.User as User
```

This imports `Helpers.roc` from the `Utils/` directory and `User.roc` from the `Models/` directory. Directory names must start with an uppercase letter (lowercase would indicate a package qualifier). The module name after the last dot becomes the identifier in scope.

### Package Imports

Packages contain a collection of modules that are imported by applications, platforms or packages. Package dependencies are specified in the module header:

```roc
app [main!] { pf: platform "https://...", json: "https://..." }
```

This defines two package aliases: `pf` for the platform package and `json` for a JSON package. Use these aliases as prefixes when importing modules from those packages:

```roc
import pf.Stdout
import json.Parser
```

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
