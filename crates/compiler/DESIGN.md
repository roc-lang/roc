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



### Symbol Resolution

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
