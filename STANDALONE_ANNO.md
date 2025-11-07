# Standalone Type Annotations in the Roc Compiler

## Overview

A **standalone type annotation** is a type declaration without an accompanying implementation. For example:

```roc
is_empty : Str -> Bool
```

When this annotation is not followed by a definition like `is_empty = ...`, the compiler treats it as a standalone annotation.

## Internal Representation: The `e_anno_only` Expression

When the compiler encounters a standalone annotation, it creates a special expression variant called **`e_anno_only`** to serve as a "fake body" for the definition. This is a simple empty struct:

```zig
// From src/canonicalize/Expression.zig:367
e_anno_only: struct {},
```

## Compiler Pipeline

### 1. Canonicalization (Can.zig)

When canonicalizing statements, if a type annotation is not followed by a matching definition, the compiler:

1. Creates a pattern for the identifier (e.g., `is_empty`)
2. Introduces the name into scope
3. **Creates an `e_anno_only` expression as the "body"** (Can.zig:6921, 6966)
4. Creates a declaration statement with:
   - The pattern
   - The `e_anno_only` expression as the body
   - The type annotation

```zig
// Create the e_anno_only expression
const anno_only_expr = try self.env.addExpr(Expr{ .e_anno_only = .{} }, region);

// Add the decl as a statement with the e_anno_only body
const stmt_idx = try self.env.addStatement(Statement{ .s_decl = .{
    .pattern = pattern_idx,
    .expr = anno_only_expr,
    .anno = annotation_idx,
} }, region);
```

This means every standalone annotation still creates a "def" in the canonical IR—it just has `e_anno_only` as its body instead of a real implementation.

### 2. Type Checking (Check.zig)

The type checker handles `e_anno_only` specially:

```zig
// From src/check/Check.zig:2978
.e_anno_only => {
    // For annotation-only expressions, the type comes from the annotation.
    // The expr_var will be unified with the annotation var
}
```

The expression gets its type directly from the annotation. Since standalone annotations are always created with an annotation (enforced during canonicalization), the type is always available.

### 3. Dependency Analysis (DependencyGraph.zig)

The `e_anno_only` expression is treated like a literal with **no dependencies**:

```zig
// From src/canonicalize/DependencyGraph.zig:242
.e_num, .e_frac_f32, ..., .e_anno_only => {},
```

This makes sense because there's no implementation to analyze for dependencies.

### 4. Interpreter (interpreter.zig)

The interpreter creates different runtime behavior depending on whether the standalone annotation is for a function or a value:

#### Function Types (interpreter.zig:1379-1409)

For function-typed annotations like `foo : Str -> Bool`:

1. The interpreter creates a **closure-like value**
2. The closure's `body_idx` points to the `e_anno_only` expression itself (as a marker)
3. When the function is **called** (interpreter.zig:1579-1584):
   ```zig
   // Check if this is an annotation-only function
   const body_expr = self.env.store.getExpr(header.body_idx);
   if (body_expr == .e_anno_only) {
       self.triggerCrash("This function has no implementation. It is only a type annotation for now.", false, roc_ops);
       return error.Crash;
   }
   ```

**Result**: The function can be passed around, but crashes with a helpful error message when called.

#### Non-Function Types (interpreter.zig:1858-1868)

For non-function annotations like `bar : Str`:

1. The interpreter creates a **placeholder value** with the appropriate layout
2. When the value is **looked up** (accessed):
   ```zig
   const binding_expr = self.env.store.getExpr(b.expr_idx);
   if (binding_expr == .e_anno_only and b.value.layout.tag != .closure) {
       self.triggerCrash("This value has no implementation. It is only a type annotation for now.", false, roc_ops);
       return error.Crash;
   }
   ```

**Result**: The value crashes with a helpful error message when accessed.

## Summary

| Stage | Behavior |
|-------|----------|
| **Canonicalization** | Creates a def with `e_anno_only` as the "fake body" |
| **Type Checking** | Gets type from the annotation, no special checking |
| **Dependency Analysis** | Treated as having no dependencies (like a literal) |
| **Interpreter (function type)** | Creates a closure that crashes when **called** |
| **Interpreter (non-function type)** | Creates a placeholder that crashes when **looked up** |

## Example Behavior

```roc
# Standalone function annotation
is_empty : Str -> Bool

# Can reference it (creates a closure)
f = is_empty

# But calling it crashes
result = is_empty "hello"  # Error: "This function has no implementation..."
```

```roc
# Standalone value annotation
some_string : Str

# Accessing it crashes immediately
x = some_string  # Error: "This value has no implementation..."
```

## Implementation Files

- **Definition**: `src/canonicalize/Expression.zig:367`
- **Canonicalization**: `src/canonicalize/Can.zig:6896-6982`
- **Type Checking**: `src/check/Check.zig:2978-2990`
- **Dependency Analysis**: `src/canonicalize/DependencyGraph.zig:242`
- **Interpreter Evaluation**: `src/eval/interpreter.zig:1379-1409`
- **Interpreter Function Call Check**: `src/eval/interpreter.zig:1579-1584`
- **Interpreter Lookup Check**: `src/eval/interpreter.zig:1858-1868`
- **Tests**:
  - `src/canonicalize/test/anno_only_test.zig`
  - `src/eval/test/anno_only_interp_test.zig`
