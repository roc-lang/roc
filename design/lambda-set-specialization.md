# Lambda Set Specialization

Lambda set specialization is Roc's approach to compiling higher-order functions.
It is a form of **defunctionalization**: converting function values into data values
so that all calls in the compiled program are direct (no function pointers).

The core invariant is:

> **By codegen time, a closure is a tagged union.**
> The tag identifies which procedure to call.
> The payload carries the captured environment.
> There is no difference in memory between a closure and any other tagged union.

---

## 1. Function Types and Lambda Sets

Conceptually, a function type has three components:

```
Fn(input, lambda_set, output)
```

- **input**: the argument type
- **output**: the return type
- **lambda_set**: the set of closures that could appear at this position

The lambda set may be tracked as a separate data structure alongside the type
rather than embedded directly in the function type. What matters is that by the
time we reach monomorphization, every function-typed value has a known, concrete
lambda set.

A lambda set is a set of `(lambda_identity, captures)` pairs, where:
- `lambda_identity` is a globally unique name for the closure
- `captures` is a record of the free variables the closure captures from its environment

### How lambda sets arise

Every lambda literal introduces a **singleton** lambda set containing just itself:

```roc
add1 = |x| x + 1
# add1 has type: I64 -[add1 {}]-> I64
# Lambda set = {add1} with no captures

make_adder = |n| |x| x + n
# The inner lambda has type: I64 -[inner {n: I64}]-> I64
# Lambda set = {inner} capturing n
```

### How lambda sets grow

When multiple closures flow to the same call site, their lambda sets **unify**.
Unification of two lambda sets works as follows:

1. Lambdas that appear in **only one** of the two sets are kept as-is (union)
2. Lambdas that appear in **both** sets must have compatible captures — the same
   capture names, and the capture types are recursively unified
3. The result is the union of all lambdas, with shared members having unified captures

```roc
apply_twice = |f, x| f(f(x))

apply_twice(|x| x + 1, 10)
apply_twice(|x| x * 2, 10)
```

At the call site `f(...)` inside `apply_twice`, the parameter `f` could be either
closure. Type inference unifies them into a single lambda set:

```
f : I64 -[add1 {}, mul2 {}]-> I64
```

---

## 2. Pipeline

### Stage 1: Lambda Set Inference

The compiler collects all closures across all modules and assigns each a globally
unique identity (e.g. `ModuleName.#3_inner_fn`). It then tracks which closures
flow to which higher-order function parameters and builds lambda sets by unifying
at each call site.

The output is a concrete lambda set for every function-typed value, listing exactly
which closures could appear at that position.

### Stage 2: Closure Transformation

Each closure is transformed into a tagged union value, and each closure call site
is transformed into a dispatch switch.

Three transformations happen:

#### A. Closure construction becomes tag construction

When a closure value is created, it becomes a tagged union value — the tag
identifies the closure, the payload carries the captures:

```roc
# Source
make_adder = |n| |x| x + n
add5 = make_adder(5)
```

After transformation, the inner closure `|x| x + n` with `n = 5` becomes:

```
# Zero captures:
Tag(Add1)

# With captures:
Tag(Inner, {n: 5})
```

Each lambda identity maps to a tag name. The captures become a record payload.

#### B. Closure calls become dispatch switches

When a closure-typed value is called, the call becomes a `match` that switches on
the tag to determine which procedure to invoke:

```roc
# Source
result = f(arg)
```

```
# After transformation (f's lambda set = {add1, mul2}):
result = match f {
    Add1 => add1_proc(arg)
    Mul2 => mul2_proc(arg)
}
```

With captures:

```
result = match f {
    AddN(captures) => add_n_proc(arg, captures)
    MulN(captures) => mul_n_proc(arg, captures)
}
```

Every call to a closure-typed value generates a dispatch switch, regardless of
the lambda set size. Even singleton lambda sets produce a one-branch switch at
this stage. (Optimizations may simplify these later — see Section 4.)

#### C. Lambda lifting — specialized procedures with captures as a parameter

Each closure's body is extracted to a top-level procedure. If the closure has
captures, the procedure takes a captures record as an additional parameter
and extracts the captured variables from it:

```roc
# Source: |x| x + n   (captures n)
```

```
# Specialized procedure (with captures):
inner_proc = |x, captures: {n: I64}| -> I64
    n = captures.n
    x + n

# Specialized procedure (no captures):
add1_proc = |x| -> I64
    x + 1
```

### Stage 3: Representation Selection

During mono lowering, each lambda set's tagged union is assigned a concrete
memory representation. The representation is selected based on the lambda set
size and capture structure:

| Lambda Set Size | Captures     | Representation      | Runtime Layout               |
|-----------------|--------------|----------------------|------------------------------|
| 1               | 0            | `direct_call`        | Zero-size (erasable)         |
| 1               | 1            | `unwrapped_capture`  | The capture value itself     |
| 1               | N            | `struct_captures`    | Struct of capture values     |
| N               | all 0        | `enum_dispatch`      | Tag byte                     |
| N               | any non-zero | `union_repr`         | Tag + max(payload sizes)     |

These are all specializations of the general tagged union layout. In each case,
the representation is isomorphic to the corresponding tag union representation —
single-variant unions can omit the tag, multi-variant unions store a discriminant
followed by the largest payload, etc.

The representation is recorded in the mono IR alongside the closure expression.

### Stage 4: Layout

Closure layouts are tag union layouts. There is no special "closure header" or
metadata in the runtime representation:

- **`direct_call`**: Zero bytes. No runtime data.
- **`unwrapped_capture`**: Same layout as the single capture value.
- **`struct_captures`**: A struct (tuple) of the capture values, laid out
  sequentially with appropriate alignment.
- **`enum_dispatch`**: A single tag byte (u8). No payload.
- **`union_repr`**: A discriminant tag followed by a payload region sized to the
  largest variant. Standard discriminated union layout.

These are the same layout rules used for all tag unions in Roc.

### Stage 5: Codegen

By the time codegen runs, closures are ordinary data values. The codegen handles
them using the same paths as any tag union:

#### Constructing a closure

Materializing a closure means writing tag + capture bytes to the stack (or a
register for small values). The result is a regular value location — just bytes.

#### Calling a closure

When the codegen encounters a dispatch switch for a closure call:

1. Load the tag discriminant from the runtime value
2. Emit a switch on the discriminant
3. For each branch: extract captures from the payload, call the corresponding
   specialized procedure with `(args..., captures)`
4. For branches with no captures: call the procedure with just `(args...)`

The dispatch information — which procedures exist and what captures each
expects — comes from the mono IR and layout metadata. It is **never** stored
in the runtime value.

#### Returning a closure from a function

Since closures are just bytes (like any tag union), returning them from a function
uses the same calling convention as returning any struct or union:

- Small closures (fits in registers): return in general-purpose or float registers
- Large closures: caller provides a return pointer, callee writes the bytes to it

#### Passing a closure as an argument

Same as passing any tag union argument — by value in registers for small closures,
by pointer for large ones.

---

## 3. Codegen Optimizations

The core design always produces tagged unions and dispatch switches. The
following optimizations may be applied:

- **Singleton direct call**: When a lambda set has exactly one member with zero
  captures, the tag union is zero-size and the switch has one branch. The codegen
  can skip materializing the closure entirely and emit a direct call.

- **Unwrapped capture**: When a lambda set has exactly one member with one
  capture, the tag can be omitted and the payload stored unwrapped — just the
  capture value itself.

- **Struct captures**: When a lambda set has exactly one member with multiple
  captures, the tag can be omitted and only the capture struct is stored.

- **Enum dispatch**: When a lambda set has multiple members but all have zero
  captures, the tag union is just a tag byte with no payload.

These do not change semantics — only memory layout and dispatch code.

---

## 4. Worked Examples

### Example 1: No captures — singleton

```roc
identity = |x| x
identity(42)
```

Lambda set for `identity`: `{identity {}}` — one function, zero captures.

Lowered type: `[Identity]` — a tag union with one zero-payload variant.

Lowered code:
```
identity_proc(x: I64) -> I64 = x

# Construction:
closure = Tag(Identity)

# Dispatch:
result = match closure {
    Identity => identity_proc(42)
}
```

### Example 2: Single capture, returned from function

```roc
make_adder = |n| |x| x + n
add5 = make_adder(5)
result = add5(10)    # 15
```

The inner lambda `|x| x + n` has lambda set `{inner {n: I64}}` — one function,
one capture.

Lowered type: `[Inner {n: I64}]` — a tag union with one variant carrying a record.

Lowered code:
```
# make_adder compiles to a proc that returns a tag union:
make_adder_proc(n: I64) -> [Inner {n: I64}] =
    Tag(Inner, {n: n})

# add5 = make_adder_proc(5)  ->  add5 = Tag(Inner, {n: 5})

inner_proc(x: I64, captures: {n: I64}) -> I64 =
    n = captures.n
    x + n

# Dispatch:
result = match add5 {
    Inner(captures) => inner_proc(10, captures)
}
```

The closure is returned from `make_adder_proc` as a tag union through the normal
return convention.

### Example 3: Multiple captures

```roc
make_linear = |m, b| |x| m * x + b
f = make_linear(3, 7)
f(10)    # 37
```

Inner lambda set: `{inner {m: I64, b: I64}}`. One function, two captures.

Lowered type: `[Inner {m: I64, b: I64}]`.

Lowered code:
```
make_linear_proc(m: I64, b: I64) -> [Inner {m: I64, b: I64}] =
    Tag(Inner, {m: m, b: b})

inner_proc(x: I64, captures: {m: I64, b: I64}) -> I64 =
    m = captures.m
    b = captures.b
    m * x + b

result = match f {
    Inner(captures) => inner_proc(10, captures)
}
```

### Example 4: Multiple functions in a lambda set

```roc
f = |t|
    match t {
        A => |w| w
        B(y) => |w| w + y
        C(y, z) => |w| w + y + z
    }

result = f(B(10))(12)    # 22
```

The three inner lambdas flow to the same return position, producing lambda set:
`{clos {}, clos1 {y: I64}, clos2 {y: I64, z: I64}}`.

Lowered type: `[Clos, Clos1 {y: I64}, Clos2 {y: I64, z: I64}]` — a multi-variant
tagged union.

Lowered code:
```
f_proc(t: [A, B I64, C I64 I64]) -> [Clos, Clos1 {y: I64}, Clos2 {y: I64, z: I64}] =
    match t {
        A => Tag(Clos)
        B(y) => Tag(Clos1, {y: y})
        C(y, z) => Tag(Clos2, {y: y, z: z})
    }

clos_proc(w: I64) -> I64 = w

clos1_proc(w: I64, captures: {y: I64}) -> I64 =
    y = captures.y
    w + y

clos2_proc(w: I64, captures: {y: I64, z: I64}) -> I64 =
    y = captures.y
    z = captures.z
    w + y + z

# Dispatch:
closure = f_proc(B(10))
result = match closure {
    Clos => clos_proc(12)
    Clos1(captures) => clos1_proc(12, captures)
    Clos2(captures) => clos2_proc(12, captures)
}
```

### Example 5: Nested closures — closure returned from a function

```roc
result = (|a| |b| a * b)(5)(10)    # 50
```

Step by step:

1. The outer lambda `|a| |b| a * b` takes `a` and returns a closure.

2. The inner lambda `|b| a * b` has lambda set `{inner {a: I64}}` — one function,
   one capture.

3. The outer lambda's return type is the closure's tag union type: `[Inner {a: I64}]`.

Lowered code:
```
outer_proc(a: I64) -> [Inner {a: I64}] =
    Tag(Inner, {a: a})

inner_proc(b: I64, captures: {a: I64}) -> I64 =
    a = captures.a
    a * b

# Execution:
closure = outer_proc(5)          # Tag(Inner, {a: 5})
result = match closure {
    Inner(captures) => inner_proc(10, captures)    # 5 * 10 = 50
}
```

The outer proc returns a tag union through the normal return convention. The caller
receives it and dispatches.

### Example 6: Higher-order function with multiple callers

```roc
apply_twice = |f, x| f(f(x))

result1 = apply_twice(|x| x + 1, 10)    # 12
result2 = apply_twice(|x| x * 2, 10)    # 40
```

Lambda set for `f`: `{add1 {}, mul2 {}}`. Two functions, zero captures each.

Lowered type: `[Add1, Mul2]` — a tag union with two zero-payload variants.

Lowered code:
```
add1_proc(x: I64) -> I64 = x + 1
mul2_proc(x: I64) -> I64 = x * 2

apply_twice_proc(f: [Add1, Mul2], x: I64) -> I64 =
    inner = match f {
        Add1 => add1_proc(x)
        Mul2 => mul2_proc(x)
    }
    match f {
        Add1 => add1_proc(inner)
        Mul2 => mul2_proc(inner)
    }

# Call sites:
result1 = apply_twice_proc(Tag(Add1), 10)
result2 = apply_twice_proc(Tag(Mul2), 10)
```

---

## 5. Key Invariants

1. **Closures are tagged unions.** By codegen time, there is no difference in memory
   layout between a closure and a regular tag union. The same instructions that
   construct, return, pass, and destructure tag unions work for closures.

2. **No compile-time metadata in runtime values.** The runtime representation of a
   closure contains only the tag (which procedure) and the payload (captures). It
   does not contain IR references, representation descriptors, or any other
   compile-time data.

3. **Dispatch info is static.** When generating code for a closure call, the codegen
   reads the lambda set membership from the IR/layout. It knows at compile time which
   procedures could be called and what captures each expects. Only the tag value is
   read at runtime.

4. **Every call dispatches.** Every call to a closure-typed value generates a
   dispatch switch, even for singleton lambda sets. Optimizing away the switch for
   singletons is a valid codegen optimization (see Section 3) but not part of the
   core algorithm.

5. **Closures are first-class data.** Because closures are just bytes, they can be:
   - Returned from functions
   - Stored in records, lists, or other data structures
   - Passed as arguments to other functions

   None of these operations require special handling beyond what already exists for
   tag unions.
