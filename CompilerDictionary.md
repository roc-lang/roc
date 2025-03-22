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
