Here you can find definitions for words that are commonly used in the **compiler** along
with links to the codebase. Check https://www.roc-lang.org/tutorial if you want to know 
about general Roc terms. Feel free to ask for a term to be added or add one yourself!

Contributor note: definitions should be roughly ordered like in a tutorial, e.g.
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

## IR

(Intermediate Representation)

An abstract code format that sits between the high-level source code and the low-level machine code.
It is generated after the source code is parsed and before target code is produced. IR makes it easier for the compiler to analyze and optimize programs.

Example for:
```roc
module []

foo : U64
```
[Token](#tokenization) IR:
```
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpColon(3:5-3:6),UpperIdent(3:7-3:10),Newline(1:1-1:1)
```
[AST](#ast) IR:
```
(file
    (module (1:1-1:10))
    (type_anno (3:1-4:4)
        "foo"
        (tag (3:7-3:10) "U64")))
```


## Interning

A memory optimization technique where only one copy of each distinct value is stored in memory, regardless of how many times it appears in a program or [IR](#ir). For example, a function named `foo` may be called many times in a Roc file, but we store `foo` once and use an index to refer to `foo` at the call sites. 

Uses of interning:
- new compiler: [collections/SmallStringInterner.zig](src/collections/SmallStringInterner.zig), [ident.zig](src/base/Ident.zig), [ModuleEnv.zig](src/base/ModuleEnv.zig), [tokenize.zig](src/check/parse/tokenize.zig), ...
- old compiler: [small_string_interner.rs](crates/compiler/collections/src/small_string_interner.rs), [mono_module.rs](crates/build/specialize_types/src/mono_module.rs), [format.rs](crates/cli/src/format.rs), ...
- There are many more uses of interning, I recommend searching for "interner" (case-insensitive).

## Identifier

Any text in a Roc source file that has significant content, but is not a Roc Str like "Hello".
Used for variable names, record field names, type names, etc. .

During [tokenization](#tokenization) all identifiers are put into a deduplicated collection and given an ID.
That ID is used in [IRs](#ir) instead of the actual text to save memory.

Identifier in the compiler:
- new compiler:
    - [Ident](src/base/Ident.zig)
    - [Ident tokenization](src/check/parse/tokenize.zig): check the functions `chompIdentLower` and `chompIdentGeneral`, and their uses.
    - [Ident parsing](src/check/parse/Parser.zig): search `Ident`
- old compiler:
    - [IdentStr](crates/compiler/ident/src/lib.rs)
    - [module/ident.rs](crates/compiler/module/src/ident.rs)
    - [parsing](crates/compiler/parse/src/expr.rs): search "identifier" (case-insensitive)

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

The set of rules that define the correct structure and format of statements, expressions, and code blocks. It specifies how code should be written so that it can be interpreted and executed correctly. In other words, syntax determines how symbols, keywords, and punctuation must be arranged to form valid source code.

Syntax in the compiler:
- New compiler: determined by the [tokenizer and parser](src/check/parse).
- Old compiler: determined by the [parser](crates/compiler/parse).

## Syntactic Sugar

[Syntax](#syntax) within a programming language that is designed to make things easier to read or express.
It allows developers to write code in a more concise, readable, or convenient way without adding new functionality
to the language itself.

Desugaring converts syntax sugar (like `x + 1`) into more fundamental operations (like `Num.add(x, 1)`).

[A table of all operators in Roc and what they desugar to](https://www.roc-lang.org/tutorial#operator-desugaring-table)

Desugaring in the compiler:
- New compiler: [canonicalize.zig (WIP)](src/check/canonicalize.zig)
- Old compiler: [desugar.rs](crates/check/can_solo/src/desugar.rs)

## Type Signature

Specifies the type of a variable. For example, the type signature of `Str.concat` is:
```roc
concat : Str, Str -> Str
```
Here it specifies `concat` takes two strings as input and produces one as output.

In the compiler, the type signature specified in the source code has priority over the type found by [type inference](#type-inference), although both need to match for your code to compile completely.

Type annotations are basically the same thing as type signatures and both terms are used interchangeably throughout the compiler.

Type signature in the code base:
- New compiler: [Parser.zig](src/check/parse/Parser.zig) (search signature)
- Old compiler: [ast.rs](crates/compiler/parse/src/ast.rs) (search TypeAnnotation)

## Type Alias

A way to give a new name to an existing type to make code more readable or meaningful. It doesn't create a new type, just an alternative name for an existing one. For example:
```roc
Person : { first_name : Str, last_name : Str }

# Using Person:
register : Person => Result {} [RegistrationFailed]
register = |person|
    ... 
```

Note: the term "alias" in the code base does not always refer to a type alias, it can also refer to an import alias using `as` or [alias analysis](#alias-analysis) etc. .

## Type Variable

TODO

## Compiler Phase

A compiler phase is a distinct stage in the process the compiler goes through to translate high-level source code into machine code that a computer can execute. Compilers don’t just do this in one big step, they break it down into several phases, each handling a specific task. Some examples of phases: [tokenization](#tokenization), [parsing](#parsing), [code generation](#code-gen),... .

## Compiler Pass

A single traversal through a program's [IR](#ir) that performs a specific transformation or analysis. [`insert_reset_reuse_operations`](https://github.com/roc-lang/roc/blob/1a9b4255407e7bec33c81d80057c5a8c2f70ead5/crates/compiler/mono/src/reset_reuse.rs#L29) is an example of a compiler pass, it analyzes and modifies the [IR](#IR) to detect when memory is no longer used and allows us to re-use that memory for other things.

Note the difference with a compiler phase; a phase is a larger logical unit of compilation that may include multiple passes.

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

The step where the compiler checks if the source code follows the correct structure or “grammar” of the programming language. It takes the tokens produced by [tokenization](#tokenization) and organizes them to see if they make sense together, like checking the structure of sentences in a language. If the code is correct, the parser builds a tree-like structure ([AST](#ast)) that shows how the code is organized. If not, it reports errors.

Parser implementation:
- new compiler: [src/check/parse](src/check/parse)
- old compiler: [crates/compiler/parse](crates/compiler/parse) (tokenization is not a separate step here)

## Symbol

A symbol points to a specific [identifier](#identifier) with an `ident_id` and a `module_id` ([see module](#module)).

Symbol implementation:
- new compiler: Not yet implemented.
- old compiler: [symbol.rs](crates/compiler/module/src/symbol.rs)

## Closure

A function that remembers and can access variables from its outer (enclosing) scope, even after that scope has finished executing. This means the function "closes over" its environment, retaining access to the variables it was created with.

Example of a closure:
```roc
# A function that returns a closure
makeCounter : I64 -> (I64 -> I64)
makeCounter = |start|
    # This inner function is a closure - it "closes over" the start variable
    |increment| 
        start + increment

# Test the closure with expect
expect
    counter = makeCounter(10)
    result = counter(5)
    result == 15
```

Closure implementation:
- new compiler: Not yet implemented
- old compiler: `ClosureData` in [expr.rs](crates/compiler/can/src/expr.rs). Closures are used all over the place, just search "Closure" (match case).

## Type Inference

The process of automatically determining the types of expressions without explicit [type annotations](#type-signature) from the programmer.
The compiler analyzes how values are used in code to deduce their types.

Type inference implementation:
- new compiler: Not yet implemented
- old compiler: Type inference is spread over multiple crates: [solve](crates/compiler/solve), [late-solve](crates/compiler/solve),[unify](crates/compiler/unify), [constrain](crates/compiler/constrain), ...

## Type Solving

TODO

## Canonicalization

After parsing a Roc program, the obtained [IR](#ir) is transformed into a
canonical form called CanIR.

Canonicalization performs analysis to catch user errors, and sets up the state necessary to figure out
the types in a program. Among other things, canonicalization;
- Uniquely identifies names (think variable and function names). Along the way,
  canonicalization builds a [graph](https://en.wikipedia.org/wiki/Graph_(discrete_mathematics))
  of all variables' references, and catches unused definitions, undefined definitions, and shadowed definitions.
- Resolves [type signatures](#type-signature), including [aliases](#type-alias), into a form suitable for [type solving](#type-solving).
- Determines the order definitions are used in, if they are defined out-of-order.
- Eliminates syntax sugar (for example, turning `+` into the function call `add`).

Canonicalization occurs on a single module (file) in isolation, so the work can be easily parallelized and cached.
If the source code for a [module](#module) has not changed, the CanIR can simply be loaded from disk and used immediately.

Implementation of Canonicalization:
- new compiler: [canonicalize.zig](src/check/canonicalize.zig), [canonicalize folder](https://github.com/roc-lang/roc/tree/main/src/check/canonicalize)
- old compiler: [can folder](crates/compiler/can)

## Lambda Set

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
