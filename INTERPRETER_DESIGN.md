# Roc Interpreter Design: Type-Carrying Architecture

## Table of Contents
1. [Design Goals](#design-goals)
2. [The Fundamental Challenge](#the-fundamental-challenge)
3. [Current Design Problems](#current-design-problems)
4. [Proposed Architecture: Type-Carrying Interpreter](#proposed-architecture-type-carrying-interpreter)
5. [Implementation Details](#implementation-details)
6. [Examples and Walkthroughs](#examples-and-walkthroughs)
7. [Performance Considerations](#performance-considerations)
8. [Migration Path](#migration-path)

## Design Goals

The Roc interpreter must satisfy several competing requirements:

1. **Unboxed Values**: For performance, we need compact runtime representations. An `i16` should be 2 bytes on the stack, not a pointer to a heap-allocated object.

2. **Runtime Polymorphism**: Support polymorphic functions without compile-time monomorphization. The function `id = \x -> x` should have one runtime representation that works for all types.

3. **Cross-Module Polymorphism**: Polymorphic functions must work correctly across module boundaries, preserving type relationships.

4. **Lazy Evaluation**: The interpreter should do work lazily, deferring type specialization until necessary (unlike ahead-of-time monomorphization for optimized builds).

5. **Zero-Cost for Monomorphic Code**: Code that doesn't use polymorphism shouldn't pay any runtime penalty.

## The Fundamental Challenge

The core tension in our design comes from wanting both:
- **Concrete, unboxed values** for performance
- **Abstract type information** for polymorphism

Consider this example:
```roc
# Module A
id : a -> a
id = \x -> x

# Module B
result = A.id(42)  # Should return 42 : i64
```

The challenge: How does the interpreter know that `A.id` should return an `i64` when called with `42`? The function itself is polymorphic - it works for any type. We need to track that "the return type equals the argument type" at runtime.

## Current Design Problems

The current interpreter converts types to "layouts" (memory representations) too early:

```zig
// Current design
pub const Layout = union(enum) {
    i64_layout,
    f64_layout,
    record_layout: RecordData,
    // ... etc
};

pub const StackValue = struct {
    ptr: ?*anyopaque,
    layout: Layout,  // <-- Problem: too concrete!
};
```

### Why Layouts Are Too Concrete

When we convert a type variable to a layout, we lose critical information:

```roc
# Type information:
# id : Var(1) -> Var(1)
# (same variable used twice = input type equals output type)

# After conversion to layouts:
# id : ? -> ?
# (lost the connection between input and output!)
```

Layouts tell us "how many bytes" and "what memory representation" but not "what type relationships exist". This is why cross-module polymorphic functions fail - we can't propagate type constraints through layouts.

### The Band-Aid Approach

The current interpreter tries to work around this with ad-hoc solutions:
- `TypeScope` for tracking some type variable mappings
- `LayoutWitness` for carrying extra type information
- Special-case handling for certain patterns

These band-aids don't compose well and fail for complex cross-module scenarios.

## Proposed Architecture: Type-Carrying Interpreter

### Core Concept: Parallel Stacks

Instead of each value carrying a layout, we maintain **two parallel stacks**:

```zig
pub const Interpreter = struct {
    // Stack 1: Raw, unboxed values (just bytes)
    value_stack: []u8,

    // Stack 2: Type information (as type variables)
    type_stack: []types.Var,

    // Runtime type store (separate from compile-time types)
    runtime_types: *types.Store,

    // ... other fields
};
```

**Key Invariant**: For every value on the value stack, there's a corresponding type variable at the same logical index on the type stack.

### How Values and Types Stay Synchronized

```zig
// Pushing a value:
fn pushValue(self: *Interpreter, bytes: []const u8, type_var: types.Var) !void {
    const value_index = self.value_stack.len;
    const type_index = self.type_stack.len;

    // These indices must stay synchronized!
    assert(value_index == type_index);

    self.value_stack.appendSlice(bytes);
    self.type_stack.append(type_var);
}

// Getting a value's type:
fn getValueType(self: *Interpreter, value_index: usize) types.Var {
    return self.type_stack[value_index];
}
```

### Type Variables Instead of Layouts

The crucial difference is that we keep type variables (which preserve relationships) instead of layouts (which don't):

```zig
// OLD: Lost polymorphic relationships
struct OldStackValue {
    ptr: ?*anyopaque,
    layout: Layout,  // "this is 8 bytes, interpreted as i64"
}

// NEW: Preserves polymorphic relationships
struct NewRuntimeValue {
    ptr: ?*anyopaque,
    type_var: types.Var,  // "this is type variable #42, which might be related to other type variables"
}
```

## Implementation Details

### The Runtime Type Store

Each interpreter instance has its own runtime type store, separate from the compile-time type store:

```zig
pub const RuntimeTypeContext = struct {
    // The runtime type store - separate from compile-time types
    types: *types.Store,

    // Stack of type scopes for nested polymorphic contexts
    scopes: []TypeScope,

    // Maps polymorphic type variables to their concrete instantiations
    instantiation_map: types.VarMap,

    // Cache of type variable -> layout conversions
    layout_cache: std.AutoHashMap(types.Var, Layout),
};
```

### When We Convert Types to Layouts

We only convert from type variables to layouts when we actually need memory information:

```zig
fn getLayoutWhenNeeded(self: *Interpreter, type_var: types.Var) !Layout {
    // Check cache first
    if (self.layout_cache.get(type_var)) |cached_layout| {
        return cached_layout;
    }

    // Resolve the type variable
    const resolved = self.runtime_types.resolveVar(type_var);

    // Convert to layout
    const layout = try self.typeToLayout(resolved.desc.content);

    // Cache for next time
    try self.layout_cache.put(type_var, layout);

    return layout;
}
```

Operations that need layouts:
- **Memory allocation**: How many bytes to reserve?
- **Field access**: What offset for this field?
- **Value copying**: How many bytes to copy?
- **FFI calls**: C functions need concrete types

Operations that DON'T need layouts:
- **Function calls**: Just pass type variables through
- **Local bindings**: Just reference stack positions
- **Returns**: Type variable goes back to caller
- **Pattern matching**: Compare type variables directly

### Polymorphic Function Calls

Here's how a polymorphic function call works in detail:

```zig
fn handlePolymorphicCall(self: *Interpreter,
                        func_type_var: types.Var,
                        arg_type_vars: []types.Var) !types.Var {
    // 1. Get the function's type
    const func_resolved = self.runtime_types.resolveVar(func_type_var);
    const func_type = func_resolved.desc.content.structure.func;

    // 2. Create fresh type variables for this instantiation
    const fresh_vars = try self.instantiatePolyVars(func_type);

    // 3. Unify argument types with parameter types
    for (arg_type_vars, func_type.param_vars) |arg_var, param_var| {
        try self.unifyTypes(param_var, arg_var);
    }

    // 4. The return type is now correctly constrained!
    return func_type.return_var;
}
```

### Type Unification at Runtime

We reuse the compile-time unification algorithm, but on the runtime type store:

```zig
fn unifyTypes(self: *Interpreter, var1: types.Var, var2: types.Var) !void {
    // Use the same unification algorithm as compile-time
    const result = try unify.unifyWithContext(
        self.module_env,
        self.runtime_types,  // Runtime types, not compile-time!
        self.unify_scratch,
        var1,
        var2,
        false
    );

    if (!result.isOk()) {
        return error.RuntimeTypeError;
    }
}
```

## Examples and Walkthroughs

### Example 1: Simple Polymorphic Identity

```roc
id = \x -> x
result = id(42)
```

**Step-by-step execution:**

1. **Define `id`**:
   - Type: `Var(1) -> Var(1)` (input and output share same type variable)
   - No execution yet (lazy)

2. **Call `id(42)`**:
   - Push `42` onto value stack (as raw bytes: `0x0000002A`)
   - Push `Var(50)` onto type stack (where `Var(50) = i64`)
   - Call `id`

3. **Inside `id`**:
   - Parameter expects `Var(1)`
   - Unify: `Var(1) := Var(50)` (now `Var(1)` means `i64`)
   - Body just returns the parameter
   - Return type is `Var(1)`, which now resolves to `i64`

4. **Return**:
   - Value stack: still has `0x0000002A`
   - Type stack: `Var(1)` (which resolves to `i64`)

### Example 2: Cross-Module Polymorphism

```roc
# Module A
getFirst : {first: a, second: b} -> a
getFirst = \record -> record.first

# Module B
point = {first: 42, second: "hello"}
result = A.getFirst(point)  # Should return 42
```

**Step-by-step execution:**

1. **Module B creates `point`**:
   - Value stack: `[42, ptr_to_"hello"]`
   - Type stack: `[Var(100), Var(101)]` where:
     - `Var(100) = i64`
     - `Var(101) = Str`
   - Record type: `Var(102) = {first: Var(100), second: Var(101)}`

2. **Call `A.getFirst(point)`**:
   - Push record onto stack
   - Push `Var(102)` onto type stack

3. **Inside `A.getFirst`**:
   - Parameter expects `Var(200) = {first: Var(201), second: Var(202)}`
   - Unify `Var(200)` with `Var(102)`:
     - `Var(201) := Var(100)` (first field is `i64`)
     - `Var(202) := Var(101)` (second field is `Str`)
   - Return type is `Var(201)`, which resolves to `i64`

4. **Field access `record.first`**:
   - Only NOW do we need a layout (for field offset)
   - Convert `Var(102)` to layout: `{first: offset=0, second: offset=8}`
   - Read 8 bytes at offset 0
   - Return with type `Var(201)` (resolves to `i64`)

### Example 3: Higher-Order Functions

```roc
# Module A
apply : (a -> b), a -> b
apply = \f, x -> f(x)

# Module B
double = \n -> n * 2
result = A.apply(double, 21)  # Should return 42
```

**Type flow:**

1. `double` has type: `Var(300) -> Var(301)` with constraint `Var(300) = Var(301) = Num *`
2. `A.apply` expects: `(Var(400) -> Var(401)), Var(400) -> Var(401)`
3. Unify at call site:
   - `(Var(400) -> Var(401))` with `(Var(300) -> Var(301))`
   - `Var(400)` with `i64` (from literal `21`)
4. Constraints propagate:
   - `Var(400) = Var(300) = i64`
   - `Var(401) = Var(301) = i64`
5. Return type `Var(401)` resolves to `i64`

## Performance Considerations

### Memory Overhead

The type-carrying design adds:
- **Type stack**: One `Var` (4 bytes) per value on the stack
- **Runtime type store**: Grows with program complexity
- **Layout cache**: Bounded by number of unique types

For typical programs, this overhead is minimal compared to the values themselves.

### Performance Optimizations

1. **Type Erasure for Primitives**:
   ```zig
   // Don't create new type vars for known primitives
   if (isLiteralI64(expr)) {
       return self.cached_i64_var;  // Reuse same var
   }
   ```

2. **Inline Monomorphic Calls**:
   ```zig
   // Fast path for non-polymorphic functions
   if (!func_type.isPolymorphic()) {
       return self.callMonomorphic(func, args);  // Skip unification
   }
   ```

3. **Layout Cache**:
   - Cache type variable → layout conversions
   - Most types repeat, so cache hit rate is high

4. **Lazy Unification**:
   - Only unify when types actually interact
   - Many type variables never need resolution

### Comparison with Current Design

| Aspect | Current Design | Type-Carrying Design |
|--------|---------------|---------------------|
| Value representation | Unboxed ✓ | Unboxed ✓ |
| Memory per value | ptr + layout | ptr + type var |
| Polymorphic calls | Often broken ✗ | Always correct ✓ |
| Cross-module | Fails ✗ | Works ✓ |
| Type resolution | At call boundaries | On-demand |
| Complexity | Ad-hoc workarounds | Systematic approach |

## Critical Design Issue: Cross-Module Type Variables

### The Problem

Type variables are just indices into a type store. When Module B calls a function from Module A:
- Module A's function has type `Var(42) -> Var(42)` in Module A's type store
- Module B can't just use `Var(42)` - that refers to something different in Module B's store!

### Solution: Single Runtime Type Store

The key insight is that the **runtime has ONE unified type store**, not per-module stores:

```zig
pub const Interpreter = struct {
    // ONE runtime type store for ALL modules
    runtime_types: *types.Store,

    // Maps (module_id, compile_time_var) -> runtime_var
    var_translation: std.AutoHashMap(VarKey, types.Var),
};

const VarKey = struct {
    module_id: ModuleId,
    compile_var: types.Var,
};
```

### Type Variable Translation

When we encounter a type from any module, we translate it to the runtime store:

```zig
fn translateTypeVar(self: *Interpreter,
                   module: *ModuleEnv,
                   compile_var: types.Var) !types.Var {
    const key = VarKey{
        .module_id = module.id,
        .compile_var = compile_var
    };

    // Check if we've already translated this var
    if (self.var_translation.get(key)) |runtime_var| {
        return runtime_var;
    }

    // Copy the type structure from module's store to runtime store
    const compile_resolved = module.types.resolveVar(compile_var);
    const runtime_var = try self.copyTypeToRuntime(compile_resolved.desc.content);

    // Cache the translation
    try self.var_translation.put(key, runtime_var);

    return runtime_var;
}
```

### Deep Copying Type Structure

When translating types, we must recursively translate all referenced type variables:

```zig
fn copyTypeToRuntime(self: *Interpreter,
                     module: *ModuleEnv,
                     content: types.Content) !types.Var {
    switch (content) {
        .structure => |s| switch (s) {
            .func => |func| {
                // Recursively translate parameter and return types
                const runtime_params = try self.translateVarList(module, func.params);
                const runtime_return = try self.translateTypeVar(module, func.return_var);

                // Create new function type in runtime store
                return self.runtime_types.mkFunc(runtime_params, runtime_return);
            },
            .record => |record| {
                // Translate all field types
                var runtime_fields = ArrayList(RecordField).init(self.allocator);
                for (record.fields) |field| {
                    const runtime_field_type = try self.translateTypeVar(module, field.type_var);
                    try runtime_fields.append(.{
                        .name = field.name,
                        .type_var = runtime_field_type,
                    });
                }
                return self.runtime_types.mkRecord(runtime_fields.items);
            },
            // ... handle other type structures
        },
        .flex_var => {
            // Create a fresh type variable in runtime store
            return self.runtime_types.fresh();
        },
        // ... handle other content types
    }
}
```

### Example: Cross-Module Function Call

```roc
# Module A (compile-time type store)
id : Var(42) -> Var(42)  # in Module A's store

# Module B calling A.id(100)
```

**What happens:**

1. **Module B prepares to call A.id**:
   ```zig
   // Translate A.id's type to runtime store
   const a_id_type = module_a.getFunctionType("id");  // Var(42) -> Var(42)
   const runtime_type = translateTypeVar(module_a, a_id_type);
   // Returns fresh Var(1000) -> Var(1000) in runtime store
   ```

2. **Push argument with its runtime type**:
   ```zig
   value_stack.push(100);  // The actual value
   type_stack.push(Var(2000));  // Fresh var in runtime store for i64
   ```

3. **Unification happens in runtime store**:
   ```zig
   // Unify Var(1000) (parameter) with Var(2000) (argument)
   runtime_types.unify(Var(1000), Var(2000));
   // Now Var(1000) = i64 in runtime store
   ```

4. **Return type is correctly resolved**:
   ```zig
   // Return type is Var(1000), which now resolves to i64
   ```

### Alternative Design: Module-Local Translation

Another approach would be to keep per-module type stores but maintain translation tables:

```zig
pub const InterModuleTypeMap = struct {
    // Maps (from_module, from_var, to_module) -> to_var
    translations: HashMap(TranslationKey, types.Var),

    fn translateVar(self: *@This(),
                   from: *ModuleEnv,
                   to: *ModuleEnv,
                   var: types.Var) !types.Var {
        // ... translation logic
    }
};
```

**Why we prefer the single runtime store**:
- Simpler conceptually (one source of truth)
- No need to translate between every module pair
- Unification naturally works across all modules
- Easier to debug (can dump one store to see all types)

### Performance Implications

The translation overhead is minimal:
- **Translation happens once per type** (cached)
- **Most types are monomorphic** (direct mapping)
- **Polymorphic types are rare** (small translation table)

The var_translation cache ensures we don't repeatedly translate the same types.

## Migration Path

### Phase 1: Add Type Stack
1. Add `type_stack` alongside existing `value_stack`
2. Maintain both layouts and type vars temporarily
3. Verify type stack stays synchronized with value stack

### Phase 2: Runtime Type Store
1. Create separate runtime type store
2. Populate it during execution
3. Add unification for polymorphic calls

### Phase 3: Replace Layouts with Type Vars
1. Change `StackValue` to use type vars instead of layouts
2. Convert to layouts only when needed
3. Remove `LayoutWitness` and other workarounds

### Phase 4: Optimization
1. Add layout cache
2. Implement fast paths for monomorphic code
3. Profile and optimize hot paths

## Alternative Approaches Considered

### 1. Full Monomorphization
**Idea**: Generate specialized versions of polymorphic functions for each type.

**Pros**:
- Simple implementation
- Fast execution

**Cons**:
- Code bloat
- Slow compilation
- Not lazy (against our design goals)

### 2. Boxing Everything
**Idea**: Make all values pointers to heap objects with type tags.

**Pros**:
- Simple polymorphism (like OCaml)
- Type info always available

**Cons**:
- Performance hit from boxing
- Heap allocation pressure
- Against our "unboxed values" goal

### 3. Monomorphization Cache
**Idea**: Keep current design but cache (function, arg_types) → return_type.

**Pros**:
- Minimal changes to current code
- Could work for simple cases

**Cons**:
- Still doesn't handle complex type relationships
- Cache misses still fail
- Feels like another band-aid

## Conclusion

The type-carrying interpreter design solves our polymorphism problems systematically:

1. **Values remain unboxed** for performance
2. **Type relationships are preserved** via type variables
3. **Cross-module polymorphism works** through runtime unification
4. **Layouts are computed on-demand** only when needed
5. **The design is principled** rather than ad-hoc

This architecture mirrors how advanced functional language runtimes work internally (OCaml, Haskell) but adapted for our unboxed value requirement. While more complex than the current design, it's complex in a systematic way that actually solves the problem rather than working around it.

The key insight: **Keep type information as long as possible, convert to concrete representations only when absolutely necessary.**