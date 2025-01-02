# Compiler Design

The current Roc compiler is designed as a pipelining compiler parallelizable
across Roc modules.

Roc's compilation pipeline consists of a few major components, which form the
table of contents for this document.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Parsing](#parsing)
- [Canonicalization](#canonicalization)
  - [Symbol Resolution](#symbol-resolution)
  - [Type-alias normalization](#type-alias-normalization)
  - [Closure naming](#closure-naming)
- [Constraint Generation](#constraint-generation)
  - [(Mutually-)recursive definitions](#mutually-recursive-definitions)
- [Type Solving](#type-solving)
  - [Unification](#unification)
  - [Type Inference](#type-inference)
  - [Recursive Types](#recursive-types)
  - [Lambda Sets](#lambda-sets)
  - [Ability Collection](#ability-collection)
  - [Ability Specialization](#ability-specialization)
  - [Ability Derivation](#ability-derivation)
  - [Exhaustiveness Checking](#exhaustiveness-checking)
  - [Debugging](#debugging)
- [IR Generation](#ir-generation)
  - [Memory Layouts](#memory-layouts)
  - [Compiling Calls](#compiling-calls)
  - [Decision Trees](#decision-trees)
  - [Tail-call Optimization](#tail-call-optimization)
  - [Reference-count insertion](#reference-count-insertion)
  - [Reusing Memory Allocations](#reusing-memory-allocations)
  - [Debugging](#debugging-1)
- [LLVM Code Generator](#llvm-code-generator)
  - [Morphic Analysis](#morphic-analysis)
  - [C ABI](#c-abi)
  - [Test Harness](#test-harness)
  - [Debugging](#debugging-2)
- [WASM Code Generator](#wasm-code-generator)
  - [WASM Interpreter](#wasm-interpreter)
  - [Debugging](#debugging-3)
- [Dev Code Generator](#dev-code-generator)
  - [Debugging](#debugging-4)
- [Builtins](#builtins)
- [Compiler Driver](#compiler-driver)
  - [Caching types](#caching-types)
- [Repl](#repl)
- [`test` and `dbg`](#test-and-dbg)
- [Formatter](#formatter)
- [Glue](#glue)
- [Active areas of research / help wanted](#active-areas-of-research--help-wanted)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Parsing

Roc's parsers are designed as [combinators](https://en.wikipedia.org/wiki/Parser_combinator).
A list of Roc's parse AST and combinators can be found in [the root parse
file](./parse/src/parser.rs).

Combinators enable parsing to compose as functions would - for example, the
`one_of` combinator supports attempting multiple parsing strategies, and
succeeding on the first one; the `and_then` combinator chains two parsers
together, failing if either parser in the sequence fails.

Since Roc is an indentation-sensitive language, parsing must be cognizant and
deligent about handling indentation and de-indentation levels. Most parsing
functions take a `min_indent` parameter that specifies the minimum indentation
of the scope an expression should be parsed in. Generally, failing to reach
`min_indent` indicates that an expression has ended (but perhaps too early).

## Canonicalization

After parsing a Roc program into an AST, the AST is transformed into a [canonical
form](./can/src/expr.rs) AST. This may seem a bit redundant - why build another
tree, when we already have the AST? Canonicalization performs a few analyses
to catch user errors, and sets up the state necessary to solve the types in a
program. Among other things, canonicalization

- Uniquely identifies names (think variable and function names). Along the way,
    canonicalization builds a graph of all variables' references, and catches
    unused definitions, undefined definitions, and shadowed definitions.
- Resolves type signatures, including aliases, into a form suitable for type
    solving.
- Determines the order definitions are used in, if they are defined
    out-of-order.
- Eliminates syntax sugar (for example, renaming `+` to the function call `add`).
- Collects declared abilities, and ability implementations defined for opaque
    types. Derived abilities for opaque types are elaborated during
    canonicalization.

### Symbol Resolution

Identifiers, like variable names, are resolved to [Symbol](./module/src/symbol.rs)s.

Currently, a symbol is a 64-bit value with
- the bottom 32 bits defining the [ModuleId](./module/src/ident.rs) the symbol
    is defined in
- the top 32 bits defining the [IdentId](./module/src/ident.rs) of the symbol
    in the module

A symbol is unique per identifier name and the scope
that the identifier has been declared in. Symbols are how the rest of the
compiler refers to value definitions - since the unique scope and identifier
name is disambiguated when symbols are created, referencing symbols requires no
further name resolution.

As symbols are constructed, canonicalization also keeps track of all references
to a given symbol. This simplifies catching unused definitions, undefined
definitions, and shadowing, to an index into an array.

### Type-alias normalization

### Closure naming

## Constraint Generation

### (Mutually-)recursive definitions

## Type Solving

### Unification

### Type Inference

### Recursive Types

### Lambda Sets

### Ability Collection

### Ability Specialization

### Ability Derivation

### Exhaustiveness Checking

### Debugging

## IR Generation

### Memory Layouts

### Compiling Calls

### Decision Trees

### Tail-call Optimization

### Reference-count insertion

### Reusing Memory Allocations

### Debugging

## LLVM Code Generator

### Morphic Analysis

### C ABI

### Test Harness

### Debugging

## WASM Code Generator

### WASM Interpreter

### Debugging

## Dev Code Generator

### Debugging

## Builtins

## Compiler Driver

### Caching types

## Repl

## `test` and `dbg`

## Formatter

## Glue

## Active areas of research / help wanted
