# Lambdamono Method Resolution Design

## Goal

Method calls must remain unresolved until `lambdamono`.

That means:

- before `lambdamono`, the compiler has **not** chosen a concrete method implementation symbol for any ordinary method call
- inside `lambdamono`, method resolution happens exactly once, from the already-specialized monomorphic receiver or dispatcher type
- by the end of `lambdamono`, there are no ordinary `method_call` or `type_method_call` operations left anywhere in the lowered program

This keeps the pipeline aligned with the general architecture:

- earlier stages do type checking and type specialization
- `lambdamono` is the stage that still has both
  - expression structure
  - fully specialized solved types
- later stages only lower already-decided executable calls

## What “method resolution” means here

In this document, “resolve a method call” means:

- take syntax like `x.foo(y, z)`
- determine the exact top-level method implementation symbol it refers to
- rewrite the call to an ordinary direct call to that symbol, with the receiver passed explicitly

Example:

```roc
Id := U64
Id.is_eq = |a, b| Num.rem(a, 10) == Num.rem(b, 10)

eq = |x, y| x.is_eq(y)
```

After specialization, when `x` and `y` are monomorphic `Id`, method resolution means turning:

- `x.is_eq(y)`

into the exact ordinary call:

- `Id.is_eq(x, y)`

No earlier stage should have tried to choose `Id.is_eq`.

## Core rules

### 1. No ordinary method target resolution before `lambdamono`

The following stages must **not** choose a concrete method implementation symbol:

- checker
- `typed_cir`
- `monotype`
- `monotype_lifted`
- `lambdasolved`

Those stages may:

- parse and preserve method syntax
- type-check method usage
- record the method name
- record the module/namespace context needed for later lookup
- specialize the receiver type and argument types normally

But they must not:

- enumerate possible method implementation symbols for a call site
- store “resolved targets”
- build lambda sets that stand in for unresolved ordinary methods
- collapse a method call to one exact symbol

### 2. `lambdamono` resolves methods exactly once

`lambdamono` is the first stage allowed to turn an ordinary method call into a concrete symbol.

It does this only after:

- the surrounding function specialization has happened
- the specialization-local solved world is fixed
- the receiver or dispatcher type at the call site is monomorphic

### 3. By the end of `lambdamono`, no ordinary method calls remain

After `lambdamono` finishes, executable lowering must contain:

- ordinary direct calls
- lambda-set defunctionalized dispatch
- erased-call lowering where explicitly required
- explicit structural-equality lowering
- explicit inspect lowering

It must not contain:

- unresolved ordinary method names
- method lookup logic
- attached-method tables
- method-call AST nodes waiting for later stages to interpret

## Stage-by-stage design

## Checker

The checker is responsible for legality, not target selection.

For ordinary methods, it must:

- verify that the program’s method constraints are valid
- verify arity and method-call shape
- verify any relevant `where`-clause obligations
- record the method name and method-call origin/module context

It must **not**:

- create a list of possible implementation symbols
- decide which nominal method definition this call will use later

### Structural equality

Structural equality is not an ordinary method by the time it leaves the checker.

If the user writes:

- `x == y`
- or `x.is_eq(y)`

then the checker chooses one of two meanings:

1. ordinary resolved-by-name method semantics
2. structural equality semantics

If it is structural equality, the checker rewrites it immediately to an explicit structural-equality node.

That means:

- user-defined nominal `is_eq` stays an ordinary method call
- implicit structural equality does not stay a method call

### `Str.inspect`

`Str.inspect` follows the same general principle:

- surface syntax may look method-like or builtin-like
- later stages should not have to decide what it means
- it should already be an explicit dedicated operation before `lambdamono`

## `typed_cir`

`typed_cir` should expose method calls structurally, but still unresolved.

An ordinary method-call node should carry only the information needed for later exact lookup:

- receiver expression
- method name
- explicit argument expressions
- the checked result type
- the module/namespace context needed to interpret the method name correctly

It must not carry:

- resolved target symbol lists
- “candidate methods”
- exact chosen callee symbols

For type-directed method calls, it should likewise carry:

- dispatcher type
- method name
- explicit argument expressions
- lookup namespace context

Again: unresolved, but explicit.

## `monotype`

`monotype` stays structural.

It lowers:

- ordinary method-call nodes
- type-method-call nodes
- structural-equality nodes
- inspect nodes

But it does not resolve ordinary methods.

The type-level work here is ordinary specialization work only:

- receiver types
- argument types
- result types

The important consequence is:

- no method-specific target lists
- no exact attached-method symbol payloads
- no fake lambda sets for unresolved methods

If `monotype` stores a type alongside a method call, that type is only the specialized signature of the call shape. It is not a resolved callee identity.

## `monotype_lifted`

No special method logic belongs here.

This stage preserves the unresolved method-call node shape and its specialized types.

## `lambdasolved`

`lambdasolved` still does not resolve ordinary methods.

Its job is:

- solve ordinary types
- specialize the receiver/dispatcher type normally
- carry the method-call node forward

By the time a method call reaches `lambdamono`, the important thing is not that the method target is already known. The important thing is that:

- the receiver or dispatcher type is now fully specialized in the local solved world

That is enough.

`lambdasolved` must not:

- enumerate method targets
- encode unresolved ordinary methods as lambda sets
- choose the exact symbol

## `lambdamono`

This is the first and only stage that resolves ordinary methods.

It already traverses expressions and statements. That makes it the right place, because it still has:

- full expression structure
- full specialized solved types
- access to the symbol/type environment needed to specialize ordinary direct calls

### Resolution timing

Method resolution happens when `lambdamono` visits:

- `method_call`
- `type_method_call`

and only there.

At that point:

- the surrounding specialization-local solved world already exists
- the receiver or dispatcher type is monomorphic
- normal function specialization machinery is available

### Resolution algorithm for ordinary method calls

For an expression:

- `receiver.method_name(arg1, arg2, ...)`

`lambdamono` must do this:

1. Read the already-specialized solved receiver type.
2. Normalize it enough to determine the exact owning nominal/type constructor identity.
3. Use:
   - method name
   - lookup namespace/module context
   - resolved receiver nominal identity
   to look up the exact attached method symbol.
4. If that lookup does not return exactly one symbol, this is a compiler bug or an earlier-stage checking bug.
5. Rewrite the operation to the ordinary exact-call path:
   - callee = that one exact symbol
   - args = `[receiver] ++ explicit_args`
6. Specialize that exact symbol using the ordinary direct-call specialization machinery.

After step 5, there is nothing method-specific left about the call.

### Resolution algorithm for type method calls

For a type-dispatched method call, `lambdamono` does the same thing, except the lookup key comes from the already-specialized dispatcher type rather than a receiver expression.

Algorithm:

1. Read the already-specialized dispatcher type.
2. Determine the exact nominal/type constructor identity from that monomorphic type.
3. Look up the exact method symbol by:
   - dispatcher nominal/type identity
   - method name
   - lookup namespace/module context
4. Rewrite to the ordinary exact-call path.

### What if the receiver type is structural?

That should not happen for ordinary method calls.

By the time `lambdamono` sees a structural-equality case or another structural builtin-like case, it should already be represented by its own explicit node, not as an ordinary method call.

So for ordinary method resolution:

- nominal/type-method case: resolve exactly
- structural case: compiler bug

### No lambda-set machinery for unresolved methods

Ordinary unresolved methods are not lambda sets.

Lambda sets are for higher-order function values whose possible callees are part of the solved callable semantics.

Ordinary attached-method syntax is different:

- before `lambdamono`, it is unresolved method syntax plus specialized receiver/dispatcher type
- inside `lambdamono`, it becomes one exact direct call

So this design intentionally does **not** create singleton or multi-member lambda sets to represent unresolved ordinary methods before `lambdamono`.

### Postcondition

After `lambdamono`, there must be no ordinary:

- `method_call`
- `type_method_call`

remaining anywhere in the lowered executable program.

Everything has been converted to:

- ordinary exact calls
- explicit structural equality lowering
- explicit inspect lowering
- other already-lowered executable forms

## Data that earlier stages may carry

Before `lambdamono`, ordinary method calls may carry:

- method name
- namespace/module context needed for lookup
- receiver expression or dispatcher type
- explicit argument expressions
- specialized argument/result types

They may not carry:

- exact target symbol
- candidate symbol lists
- resolved target lists
- unresolved ordinary methods encoded as lambda sets

This is the key boundary.

## Why this design is better

### It avoids huge target lists entirely

The compiler never materializes “all possible methods named `foo`” for a call site.

There is no candidate-list transport mechanism at all.

### It keeps type specialization normal

Earlier stages specialize:

- receiver types
- argument types
- result types

using ordinary machinery.

They do not do special target-resolution work.

### It resolves methods at the first stage that actually has enough information

Earlier than `lambdamono`:

- the receiver may still be polymorphic

Later than `lambdamono`:

- we would be pushing source-level semantic work into stages that should already be dumb lowering

So `lambdamono` is the right boundary.

### It keeps later stages dumb

`ir/lower`, `FromIr`, and the backends should never need to know:

- method names
- nominal method tables
- lookup namespaces
- how attached methods work

That semantic work is complete before they run.

## Example walkthrough

```roc
Id := U64
Id.is_eq = |a, b| Num.rem(a, 10) == Num.rem(b, 10)

eq = |x, y| x.is_eq(y)

main = eq(@Id 12, @Id 22)
```

### Checker

The checker sees:

- `x.is_eq(y)`

It verifies:

- valid method-call shape
- the relevant method constraint is legal

It does **not** choose `Id.is_eq`.

### `monotype` / `lambdasolved`

Ordinary type specialization eventually makes:

- `x : Id`
- `y : Id`

The node is still an unresolved ordinary method call.

### `lambdamono`

Now the receiver type is monomorphic `Id`.

`lambdamono`:

1. sees ordinary `method_call`
2. reads the specialized receiver type `Id`
3. looks up `(Id, is_eq, namespace)`
4. gets the one exact symbol `Id.is_eq`
5. rewrites the call to ordinary exact call:
   - `Id.is_eq(x, y)`
6. lowers it through the normal exact-call specialization path

After that, the executable program no longer contains method syntax there at all.

## Required deletions from the current implementation

The end-state design requires deleting any mechanism that resolves ordinary methods before `lambdamono`, including:

- resolved target lists in checked/CIR transport
- any “candidate method” data structure
- any method-call type construction that bakes in exact method symbols before `lambdamono`
- any lambdasolved-time or monotype-time exact method target collapse

The only method-specific work before `lambdamono` should be:

- checking legality
- preserving the unresolved method name and lookup context
- specializing receiver/dispatcher/arg/result types normally

## Final invariant

The invariant we want is:

- before `lambdamono`, ordinary methods are unresolved
- inside `lambdamono`, ordinary methods are resolved exactly once from monomorphic receiver/dispatcher types
- after `lambdamono`, ordinary methods no longer exist as a distinct operation

That is the full design target.
