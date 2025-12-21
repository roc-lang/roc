# Expressions

An expression is something that evaluates to a [value](#values). 

You can wrap expressions in parentheses without changing what they do. Non-expressions
can't be wrapped in parentheses without causing an error. Some examples:

- `x` is a valid expression. It evaluates to a value. `(x)` is valid.
- `foo(1)` is a valid expression. It evaluates to a value. `(foo(1))` is valid.
- `1` is a valid expression. It's already a value `(1)` is valid.
- `import Foo` is a [statement](statements) not an expression. `(import Foo)` is invalid.
- `# Something` is a [comment](comments), not an expression. `(# Something)` is invalid.
- `package […]` is a [module heaader](modules#headers), not an expression. `(package […])` is invalid.

Another way to think of an expression is that you can always assign it to a name—so, you
can always put it after an `=` sign.

## [Types of Expressions](#literal-expressions) {#literal-expression}

Here are all the different types of expressions in Roc:

- String literals, e.g. `"foo"` or `"Hello, ${name}!"`
- Number literals, e.g. `1` or `2.34` or `0.123e4`
- List literals, e.g. `[1, 2]` or `[]` or `["foo"]`
- Record literals, e.g. `{ x: 1, y: 2 }` or `{}` or `{ x, y, ..other_record }`
- Tag literals, e.g. `Foo` or `Foo(bar)`
- Tuple literals, e.g. `(a, b, "foo")`
- Function literals (aka "lambdas"), e.g. `|a, b| a + b` or `|| c + d`
- Lookups, e.g. `blah` or `(blah)` or `$blah` or `($blah)` or `blah!` or `(blah!)`
- Calls, e.g. `blah(arg)` or `foo.bar(baz)`
- Operator applications, e.g. `a + b` or `!x`, which [desugar to calls](operators#desugaring)
- [Block expressions](#block-expressions), e.g. `{ foo() }`

There are no other types of expressions in the language.

## [Values](#values) {#values}

A Roc value is a semantically immutable piece of data. 

### [Value Identity](#value-identity) {#value-identity}

Since values take up memory at runtime, each value has a [memory address](https://en.wikipedia.org/wiki/Memory_address). 
Roc treats memory addresses as behind-the-scenes implementation details that should not affect
program behavior, and by design exposes no language-level way to access or compare addresses.

This implies that Roc has no concept of [value identity](https://en.wikipedia.org/wiki/Identity_(object-oriented_programming)), reference equality (also known as [physical equality](https://ocaml.org/manual/5.4/api/Repr.html#VALphys_equal)), or [pointers](https://en.wikipedia.org/wiki/Pointer_(computer_programming)), all of which are semantic concepts based on memory addresses. 

> Note that platform authors can choose to implement features based on memory addresses,
> since platforms have access to lower-level languages which can naturally see the addresses
> of any Roc value the platform receives. This means it's up to a platform author to decide 
> whether it's a good idea for users of their platform to start needing to think about memory
> addresses, when the rest of the language is designed to keep them behind the scenes.

### [Reference Counting](#reference-counting) {#reference-counting}

Heap-allocated Roc values are automatically [reference-counted](https://en.wikipedia.org/wiki/Reference_counting) ([atomically](https://en.wikipedia.org/wiki/Linearizability#Primitive_atomic_instructions), for thread-safety). 

Heap-allocated values include as strings, lists, boxes, and recursive tag unions. Numbers,
records, tuples, and non-recursive tag unions are stack-allocated, and so are not reference counted.

### [Reference Cycles](#reference-cycles) {#reference-cycles}

Other languages support [reference cycles](https://en.wikipedia.org/wiki/Reference_counting#Dealing_with_reference_cycles), which create problems for reference counting systems. Solutions to these problems include runtime tracing garbage collectors for cycles, or a concept of [weak references](https://en.wikipedia.org/wiki/Weak_reference). By design, Roc has no way to express reference cycles, so none of these solutions are necessary.

### [Opportunistic Mutation](#opportunistic-mutation) {#opportunistic-mutation}

Roc's compiler does _opportunistic mutation_ using the [Perceus](https://www.microsoft.com/en-us/research/wp-content/uploads/2020/11/perceus-tr-v4.pdf) "functional-but-in-place" reference counting system. The way this works is:

- Builtin operations on reference-counted values will update them in place when their reference counts are 1
- When their reference counts are greater than 1, they will be shallowly cloned first, and then the clone will be updated and returned.

For example, when [`List.set`](../builtins/List#set) is passed a unique list (reference count is 1), then that list will have the given element replaced. When it's given a shared list (reference count is not 1), it will first shallowly clone the list, and then replace the given element in the clone. Either way, the modified list will be returned—regardless of whether the clone or the original was the one modified.

## [Block Expressions](#block-expressions) {#block-expressions}

A _block expression_ is an expression with some optional [statements](statements) 
before it. It has its own scope, so anything assigned in it can't be accessed
outsdie the block.

The statements are optional, so `{ x }` is a valid block expression. This is useful 
stylistically in situations like conditional branches:

```roc
x = if foo {
    …
} else {
    x
}
```

> Note that `{ x, y }` is a [record](records) with two fields (it's syntax sugar for `{ x: x, y: y }`),
> but `{ x }` is always a block expression. That's because it's much more useful to have `{ x }` 
> be a block expression, for situations like `else { x }`, than syntax sugar for a single-field 
> record like `{ x: x }`. Single-field records are much less common than blocks in 
> conditional branches.

## [Evaluation](#evaluation) {#evaluation}

Evaluation is the process of an expression becoming a value.

Like most programming languages, Roc uses [strict evaluation](https://en.wikipedia.org/wiki/Strict_programming_language) and does not support [lazy evaluation](https://en.wikipedia.org/wiki/Strict_programming_language) like some non-strict languages do (such as [Haskell](https://www.haskell.org)).

Expressions that are already values (such as `4`, `"foo"`, etc.) evaluate to themselves.

More complex expressions, such as function calls or [block expressions](#block-expressions)
may require multiple steps to evaluate to a value. 

### [Side Effects during evaluation](#side-effects-during-evaluation) {#side-effects-during-evaluation}

Normally, only evaluating [effectful functions](functions#effectful-functions) can 
cause [side effects](functions#side-effects), but evaluating any other type of expression cannot. 

One exception to this rule is [`dbg` statements](statements#dbg), which perform the side effect of 
logging a value for debugging purposes. This is intended to be an interface for debugging, much 
like a [step debugger](https://en.wikipedia.org/wiki/Debugger), not part of a program's semantics, 
and so the side effect is allowed outside effectful functions (just like step debugging works on
any expression, not just calling effectful functions).

The only other exception to the rule is [`expect` statements](statements#expect), 

> Note that platform authors are in charge of what happens when memory gets allocated and
> deallocated, and can therefore decide to perform side effects during memory allocation
> and deallocation. This can easily cause surprising behavior for Roc applicationa authors,
> and should not be relied upon because Roc's optimizer assumes memory allocation and deallocation
> has no observable effect on the program (unless allocation fails), which means these side
> effects may be optimized away differently between patch releases of the compiler.

### [Compile-Time Evaluation](#compile-time-evaluation) {#compile-time-evaluation}

When possible, Roc's compiler will evaluate expressions at compile-time instead of at runtime.

This is only possible for expressions that depend only on values known at compile-time. The
following are not known at compile time:

- Values returned from effectful function calls (which can vary based on runtime state)
- Values provided by the platform

If a [`crash`](statements#crash) is encountered during compile-time evaluation,
it will be reported at compile time just like any other compilation errors (such as
syntax errors, naming errors, and type mismatches).
