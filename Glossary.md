Here you can find definitions for words that are commonly used in the **compiler** along
with links to the codebase. Check https://www.roc-lang.org/tutorial if you want to know 
about general Roc terms. Feel free to ask for a term to be added or add one yourself!

Contributor note: definitons should be roughly ordered like in a tutorial, e.g.
Parser should be explained before Canonicalization. 

## CLI

Command Line Interface. The entrypoint of the compiler that brings together all
functionality in the Roc toolset and makes it accessible to the user through the
terminal, e.g. `roc build main.roc`.

[Implementation (new compiler)](src/main.zig)
[Implementation (old compiler)](crates/cli/src/main.rs)

## Module

## Identifier

## AST

## Parser

## Symbol

## Closure

## Canonicalization

## Lambda Set

## Type Inference

## Monomorphization

(mono, specialization)

Monomorphization, also known as type specialization, is the process of creating a distinct copy
of each instance of a generic function or value based on all specific usages in a program.
For example; a function with the type `Num a -> Num a` may only be called in the program with a
`U64` and a `I64`. Specialization will then create two functions with the types `U64 -> U64` and
`I64 -> I64`.
This trades off some compile time for a much better runtime performance, since we don't need to
look up which implementation to call at runtime (AKA dynamic dispatch).

Related Files:
- new compiler:
  - [specialize_functions.zig](src/build/specialize_functions.zig)
  - [specialize_functions folder](src/build/specialize_functions)
  - [specialize_types.zig](src/build/specialize_types.zig)
  - [specialize types folder](src/build/specialize_types)
  
- old compiler:
  - [mono folder](crates/compiler/mono)
  - [mono tests](crates/compiler/test_mono)
  - [mono macro tests](crates/compiler/test_mono_macros)

## Type Checking

## Reference Count

## Alias Analysis

## Code Gen

## Host

## Linking

### Surgical Linker

### Legacy Linker

## Glue

## WASM
