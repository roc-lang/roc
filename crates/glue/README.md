# Glue

Glue is a bit of tooling built into Roc to help with platform development. Roc platforms are written in a different language than Roc, and it requires some finesse to let other languages read and write Roc types like records and unions in a way that's compatible with how Roc uses those types.

The `roc glue` command generates code in other languages for interacting with the Roc types used by your platform. It takes three arguments:

1. A 'glue spec', this is a Roc file specifying how to output type helpers for a particular language. You can find some examples in the src/ subdirectory:

    - **RustGlue.roc:** Generates Roc bindings for rust platforms.
    - **ZigGlue.roc:** Generates Roc bindings for zig platforms (out of date).
    - **DescribeGlue.roc:** Does not generate Roc bindings, but outputs some information about the types that assist writing compatible types in other languages by hand.

2. A 'glue dir', specifying where glue should place generated files. Pass any directory you want here.

3. A .roc file exposing some types that glue should generate code for. You can extend the template below.


```roc
platform "glue-types"
    requires {} { main : _ }
    exposes []
    packages {}
    imports []
    provides [main_for_host]

GlueTypes : {
    a : SomeType,
    b : AnotherType,
}

main_for_host : GlueTypes
main_for_host = main
```
