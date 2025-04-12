Here you can find definitions for words that are commonly used in the **compiler** along
with links to the codebase. Check https://www.roc-lang.org/tutorial if you want to know 
about general Roc terms. Feel free to ask for a term to be added or add one yourself!

Contributor note: definitons should be roughly ordered like in a tutorial, e.g.
Parser should be explained before Canonicalization. 

## CLI

Command Line Interface. The entrypoint of the compiler that brings together all
functionality in the Roc toolset and makes it accessible to the user through the
terminal, e.g. `roc build main.roc`.

- new compiler: [src/main.zig](src/main.zig)
- old compiler: [crates/cli/src/main.rs](crates/cli/src/main.rs)

## Module

A .roc file forms one module.

Types of modules:
- app [(example)](https://github.com/roc-lang/examples/blob/main/examples/HelloWorld/main.roc): Applications are combined with a platform and compiled into an executable.
- module [(example)](https://github.com/roc-lang/examples/blob/main/examples/MultipleRocFiles/Hello.roc): Provide types and functions which can be imported into other modules.
- package [(example)](https://github.com/lukewilliamboswell/roc-json/blob/main/package/main.roc): Organises modules to share functionality across applications and platforms.
- platform [(example)](https://github.com/roc-lang/basic-cli/blob/main/platform/main.roc): Provides memory management and effects like writing to files, network communication,... to interface with the outside world. [Detailed explanation](https://www.roc-lang.org/platforms).
- hosted [(example)](https://github.com/roc-lang/basic-cli/blob/main/platform/Host.roc): Lists all Roc types and functions provided by the platform.

Implementation:
- new compiler:
  - [processing of modules](src/coordinate.zig)
  - [folder with lots of module related things](src/base)
- old compiler:
  - [module folder](crates/compiler/module)

## Identifier

## Keyword

A specific word that has a predefined meaning in the language, like `crash`, `if`, `when`, ... .
Many keywords can not be used as a variable name.
We have an [overview of all Roc keywords](https://www.roc-lang.org/tutorial#reserved-keywords).

Keywords in the compiler:
- [new compiler](src/check/parse/tokenize.zig)
- [old compiler](crates/compiler/parse/src/keyword.rs)

## Operator

An operator is a [symbol](#symbol) or [keyword](#keyword) that performs a specific operation on one or more operands (values or variables) to produce a result.
Some examples: `+`, `=`, `==`, `>`. [A table of all operators in Roc](https://www.roc-lang.org/tutorial#operator-desugaring-table).
`+` is an example of binary operator because it works with two operands, e.g. `1 + 1`. Similarly `!` (e.g. `!Bool.false`) is a unary operator.

Operators in the compiler:
- New compiler: search `Op` in [tokenize.zig](src/check/parse/tokenize.zig)
- Old compiler: search `operator_help` in [expr.rs](crates/compiler/parse/src/expr.rs)

## Syntax

## Syntactic Sugar

[Syntax](#syntax) within a programming language that is designed to make things easier to read or express.
It allows developers to write code in a more concise, readable, or convenient way without adding new functionality
to the language itself.

Desugaring converts syntax sugar (like `x + 1`) into more fundamental operations (like `Num.add(x, 1)`).

[A table of all operators in Roc and what they desugar to](https://www.roc-lang.org/tutorial#operator-desugaring-table)

Desugaring in the compiler:
- New compiler: [canonicalize.zig (WIP)](src/check/canonicalize.zig)
- Old compiler: [desugar.rs](crates/check/can_solo/src/desugar.rs)

## Compiler Phase

A compiler phase is a distinct stage in the process the compiler goes through to translate high-level source code into machine code that a computer can execute. Compilers don’t just do this in one big step, they break it down into several phases, each handling a specific task. Some examples of phases: [tokenization](#tokenization), [parsing](#parsing), [code generation](#code-gen),... .

## Compiler Pass

## Tokenization

The process of breaking down source code into smaller units called tokens. These tokens are the basic building blocks of a programming language, such as [keywords](#Keyword), [identifiers](#identifier), [operators](#operator), and [symbols](#symbol). The input code is scanned character by character and is grouped into meaningful sequences based on the language's syntax rules.
This step makes [parsing](#parsing) simpler.

Example source code:
```roc
module []

foo : U64
```
Corresponding tokens:
```
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:10),Newline(1:1-1:1)
```

New compiler:
- [tokenize.zig](src/check/parse/tokenize.zig)

Old compiler:
- We did not do a separate tokenization step, everything happened in the [parser](crates/compiler/parse/src/parser.rs).

## AST

(Abstract Syntax Tree)

An AST organizes and represents the source code as a tree-like structure.
So for the code below:
```roc
module []

foo : U64
```

The AST is:
```
(file
    (module (1:1-1:10))
    (type_anno (3:1-4:4)
        "foo"
        (tag (3:7-3:10) "U64")))
```

It captures the meaning of the code, while ignoring purely syntactic details like parentheses, commas, semicolons,... .
Compared to raw source code, this structured format is much easier to analyze and manipulate programmatically by the next compiler phase.

The AST is created by the [parser](#parsing).

New compiler:
- See the `Node` struct in [this file](src/check/parse/IR.zig).
- You can see examples of ASTs in the .txt files in [this folder](src/snapshots).

Old compiler:
- See `FullAst` [here](crates/compiler/parse/src/ast.rs)
- [Some tests](crates/compiler/parse/tests/test_parse.rs)
- [Many snapshot tests](crates/compiler/test_syntax/tests/snapshots)

## Parsing

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
