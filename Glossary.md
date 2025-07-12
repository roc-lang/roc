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

A placeholder for a type. It allows generic programming that can make your code work with a collection of types.
For example:
```roc
reverse : List a -> List a
```
`reverse` can be done on any kind of list, regardless of the type of the element, so we indicate this with the type variable `a`.

Type variables can also be required to have certain [abilities](https://www.roc-lang.org/abilities), for example:
```roc
Graph a := Dict a (List a) where a implements Eq
```

Type variables don't have to be a single letter, they just have to start with a lowercase letter.

Parsing of type vars:
- new compiler: search `ty_var` in [Parser.zig](src/check/parse/Parser.zig)
- old compiler: search `parse_type_variable` in [type_annotation.rs](crates/compiler/parse/src/type_annotation.rs)

## Builtin

A function or type that is natively provided by Roc, for example `Dict.empty`, `List.map`, `Result`, ... .
You don't need to import any of these to use them.

[Builtin Docs](https://www.roc-lang.org/builtins)

Implementation of builtins:
- new compiler: [src/builtins](src/builtins) (work in progress)
- old compiler: [crates/compiler/builtins](crates/compiler/builtins). Note: some builtin functions are implemented in zig, like `Num.f64_to_bits`, see [num.zig](crates/compiler/builtins/bitcode/src/num.zig).

Interesting fact: our builtins are integrated into the compiler, there is no typical separate standard library.

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
- See the `Node` struct in [this file](src/check/parse/AST.zig).
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
The compiler analyzes how values are used in code to deduce their types. For example:
```roc
foo = |bar|
    Num.to_str(bar)
```
`foo` has no type annotation so we don't immediately know the type of bar. Later in the function, `Num.to_str` is called on `bar` and we know the type of `to_str` is `Num * -> Str`, so that means `bar` must be a `Num *`! We have now inferred the type of `bar` :tada:

Type inference implementation:
- new compiler: Not yet implemented
- old compiler: Type inference is spread over multiple crates: [solve](crates/compiler/solve), [late-solve](crates/compiler/solve),[unify](crates/compiler/unify), [constrain](crates/compiler/constrain), ...

## Type constraint

A requirement or restriction that limits what types can be used in a particular context.
Type constraints express relationships between types that must be satisfied for a program to be well-typed.
Type constraints can take several forms:
- Equality constraint: requires types to be the same:
    + In the expression `a + b`, both `a` and `b` need to have the same type.
- Try target constraint: requires the expression you use `?` on to be a `Result`:
    + `Str.from_utf8(byte_list)?`
- Effect suffix constraint: Your function should have a `!` suffix if it calls an effectful function.
- See `pub enum Constraint` in [crates/compiler/can/src/constraint.rs](crates/compiler/can/src/constraint.rs) for an overview of all constraints.

Type constraint implementation:
- new compiler: Not yet implemented
- old compiler:
    + Definition:  [can/src/constraint.rs](crates/compiler/can/src/constraint.rs)
    + [Type solving](#type-solving) using constraints: [crates/compiler/solve/src/solve.rs](crates/compiler/solve/src/solve.rs)

## Type Solving

TODO

## Unification

TODO

## Structural Typing

A type system where type equivalence is based on the shape (the set of members and their types) of the values, not the names by which the types were declared.
Example:
```roc
Person : { first_name : Str, last_name : Str }

User : { first_name : Str, last_name : Str }

register_person! : Person => Result {} [InvalidName]
register_person! = |person|
    ...

user : User
user = { first_name: "Bob", last_name: "Foo"}

# This works even though `register_person!` expects a `Person`, the types have the same structure so they are compatible!
register_person!(user)
```

## Nominal Typing

A type system where type equivalence is based on explicit names or declarations, not just structure. Two types are considered the same, only if they have the same name or originate from the same type declaration.

## Let

Roc is inspired by [Elm](https://elm-lang.org), in Elm, variables are defined using `let`:
```elm
double arg =
    let
        two = 2
    in
        arg * two
```
This makes the scope of `two` very clear.

In Roc, you can just define variables and use them without `let ... in`, we add `let` behind the scenes.
See `Stmt::Let` in [crates/compiler/mono/src/ir.rs](crates/compiler/mono/src/ir.rs) (old compiler).

## Generalized

Say we have the following code:
```roc
my_record =
    id = |x| x

    { a: id(1), b: id("foo") }
```
If we would infer the type of `id` to be `Int -> Int`, that would lead to a type error at the next call site `id("foo")`.
So, we **generalize** the type of `id` to `a -> a`. This allows `id` to be called with any type.

## Rank

In general, the rank tracks the number of [let-bindings](#let) a variable is "under". Top-level definitions
have rank 1. A [let](#let) inside a top-level definition gets rank 2, and so on.

An example:
```roc 
foo = 3

plus_five = |arg|
    x = 5
    arg + x
```
Here the rank of `foo` is 1 because it is at the top level and the rank of `x` is 2 because it is under or inside `plus_five`.

Imported variables get rank 2.

Rank 0 is special, it is used for variables that are [generalized](#generalized). 

Keeping track of ranks makes type inference faster. You can see how ranks are used [here](crates/compiler/solve/src/solve.rs) (old compiler).

## Rigid vs Flexible

An identifier is rigid if it has a fixed, concrete name that's written in Roc code. On the other hand, a flexible identifier is one created by the compiler during type inference.
`a` is rigid below, I defined it. The compiler should use `a` in the error message if there is something wrong with the function's type.
```roc
take_first : List a, U64 -> List a
take_first = |list, output_length|
    List.sublist(list, { start: 0, len: output_length })
```
If I make a type error in the definition:
```roc
take_first : U64
take_first = |list, output_length|
    List.sublist(list, { start: 0, len: output_length })
```
The compiler's error message will say:
```
The body is an anonymous function of type:

    List elem, Int Unsigned64 -> List elem

But the type annotation on take_first says it should be:

    U64
```
`elem` is a flexible [type variable](#type-variable), the compiler chose that name.

Related definitions in the compiler:
- old compiler: search "pub enum Content" in [types/src/subs.rs](crates/compiler/types/src/subs.rs)
- new compiler: search "pub const Content" in [check/canonicalize/CIR.zig](src/check/canonicalize/CIR.zig)

## Flat Type

Represents types without indirection, it's the concrete form that types take after
resolving [variables](#type-variable) and [aliases](#type-alias).

definitions in the compiler:
- old compiler: search "pub enum FlatType" in [types/src/subs.rs](crates/compiler/types/src/subs.rs)
- new compiler: search "pub const FlatType" in [types/types.zig](src/types/types.zig)

## Canonicalization

(can)

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

TODO

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

- old compiler:
  - [mono folder](crates/compiler/mono)
  - [mono tests](crates/compiler/test_mono)
  - [mono macro tests](crates/compiler/test_mono_macros)

## Type Checking

TODO

## Reference Count

(refcount)

A memory management technique where each thing in memory has an associated counter that tracks how many references are pointing to that thing.

How it works:
- Every time a new reference to something is created, their reference counter is incremented.
- Every time a reference is deleted or goes out of scope, the counter is decremented.
- When the reference count reaches zero, it means no references are pointing to the thing, so the memory occupied by it can be safely freed.

Roc uses automatic reference counting because it avoids the significant pauses that can happen with traditional [garbage collection](https://en.wikipedia.org/wiki/Garbage_collection_(computer_science)).
These pauses can be annoying in games for example, because it can result in a noticeable drop in framerate.

Another approach, manual memory management, would allow you to produce the fastest program but it is also more tedious to write code that way.

Reference counting implementation:
- Old compiler: [Mono folder](crates/compiler/mono/src) (search ref)

## Mutate in place

TODO

## Alias Analysis

TODO

## Backend

TODO

## Code Gen

(code generation)

The [phase](#compiler-phase) where the compiler translates intermediate representation (IR) of a program into target code, usually assembly code.
This assembly code is not yet ready for execution, it needs to be [linked](#linking) first.

For the old compiler we have three code gen backends, see [#backend](#backend) for more explanation.

Code Gen implementation:
- New compiler: Not done yet
- Old compiler:
  - [crates/compiler/gen_llvm](crates/compiler/gen_llvm) generates LLVM IR
  - [crates/compiler/gen_dev](crates/compiler/gen_dev) generates assembly fast for quick build times
  - [crates/compiler/gen_wasm](crates/compiler/gen_wasm) generates webassembly (binary) for typical [wasm](#wasm) usecases.
  - [code gen tests](crates/compiler/test_gen)

## Host

TODO

## Heap

TODO

## Stack

TODO

## Boxing

Wrapping a value or function in a generic, opaque representation (box) that can easily be passed to the platform.
A boxed value is allocated on the [heap](#heap).
You can box something in Roc with the [builtin](#builtin) [Box.box](https://www.roc-lang.org/builtins/Box#box) and unbox it with [Box.unbox](https://www.roc-lang.org/builtins/Box#unbox).

[Example handling of boxes in basic-cli](https://github.com/search?q=repo%3Aroc-lang%2Fbasic-cli%20Box&type=code).

See also [std::boxed::Box](https://doc.rust-lang.org/std/boxed/struct.Box.html) in Rust.

## Linking

TODO

### Surgical Linker

TODO

### Legacy Linker

TODO

## Glue

TODO

## WASM

TODO

## lhs & rhs

Left & Right Hand Side: for example in `1 + 2`, `1` is on the left hand side and `2` is on the right hand side.

## Span

TODO

## Joinpoint

TODO

## Mutate in Place

TODO

## SExpr

TODO

TODO
