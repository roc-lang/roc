# Implementation Snippet: Extract Fields from Type Annotation

This file shows how to implement field extraction from type annotations to fix record completion.

## Location
`src/lsp/syntax.zig` - in `addRecordFieldCompletions` function around line 3305

## Current Code (Incomplete)

```zig
if (def.annotation) |anno_idx| {
    std.debug.print("addRecordFieldCompletions: def has annotation, trying to get type from it\n", .{});
    const annotation = module_env.store.getAnnotation(anno_idx);
    const type_anno = module_env.store.getTypeAnno(annotation.anno);
    std.debug.print("addRecordFieldCompletions: type_anno={any}\n", .{type_anno});
    
    // Try to get the type var from the annotation
    // Type annotations should have been checked and have associated type vars
    // For now, fall through to using pattern's type var
}
```

## Needed Implementation

```zig
if (def.annotation) |anno_idx| {
    const annotation = module_env.store.getAnnotation(anno_idx);
    const type_anno = module_env.store.getTypeAnno(annotation.anno);
    
    // If this is a record type annotation, extract fields from it
    switch (type_anno) {
        .record => |record_anno| {
            // Extract fields from the type annotation AST
            try self.addFieldsFromTypeAnnoRecord(items, module_env, record_anno);
            return;
        },
        .lookup => {
            // This might be a type alias that resolves to a record
            // For now, fall through to using pattern's type var
            // TODO: Resolve the alias and check if it's a record
        },
        else => {
            // Not a record type, fall through to pattern's type var
        },
    }
}

// Fall back to getting type from pattern (current behavior)
const type_var = ModuleEnv.varFrom(def.pattern);
try self.addFieldsFromTypeVar(items, module_env, type_var);
```

## New Helper Function Needed

```zig
/// Extract and add record fields from a type annotation AST (not from inferred type)
/// This is more accurate than using the inferred type variable, which may have been
/// specialized by row polymorphism to only include accessed fields.
fn addFieldsFromTypeAnnoRecord(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
    record_anno: TypeAnno.RecordAnno, // TODO: Determine actual type from TypeAnnotation.zig
) !void {
    // TODO: Understand the structure of TypeAnno.RecordAnno
    // The debug output shows: .record = .{ .fields = .{ .span = ... }, .ext = null }
    
    // Steps needed:
    // 1. Get the fields span from record_anno.fields
    // 2. Iterate over the span to get each field
    // 3. For each field:
    //    - Extract field name (identifier)
    //    - Extract field type (TypeAnno)
    //    - Convert type to string for detail (or leave null)
    //    - Add completion item
    
    // Pseudo-code:
    // const fields_span = record_anno.fields;
    // for (fields_span) |field| {
    //     const field_name = module_env.getIdentText(field.name);
    //     const field_type_str = try self.formatTypeAnno(module_env, field.type_anno);
    //     
    //     try items.append(self.allocator, .{
    //         .label = field_name,
    //         .kind = @intFromEnum(completion_handler.CompletionItemKind.field),
    //         .detail = field_type_str,
    //     });
    // }
    
    _ = self;
    _ = items;
    _ = module_env;
    _ = record_anno;
    
    // For now, just mark as TODO
    std.debug.print("TODO: addFieldsFromTypeAnnoRecord not implemented\n", .{});
}

/// Format a type annotation as a string for completion details
/// This is separate from type variable formatting since we're working with AST, not resolved types
fn formatTypeAnno(
    self: *SyntaxChecker,
    module_env: *ModuleEnv,
    type_anno: TypeAnno,
) !?[]const u8 {
    _ = self;
    _ = module_env;
    _ = type_anno;
    
    // TODO: Implement type annotation formatting
    // Could be simple like "Str", "I64", "List a", etc.
    // Or just return null for now (no detail)
    
    return null;
}
```

## Research Needed

To implement this, you need to answer these questions by studying `src/canonicalize/TypeAnnotation.zig`:

1. **What is the exact type of `TypeAnno.record`?**
   ```zig
   // Is it:
   .record => |record_anno| { ... }
   // Where record_anno is what type exactly?
   ```

2. **How to iterate over record fields?**
   ```zig
   // The debug output shows: .fields = .{ .span = ... }
   // How to get the span and iterate:
   const fields_span = record_anno.fields; // What type?
   // Is it a Span that needs to be resolved via store?
   // Or can we iterate directly?
   ```

3. **What is the structure of a field in the annotation?**
   ```zig
   // For each field, what information is available?
   // - Field name (Ident.Idx?)
   // - Field type (TypeAnno?)
   // - Optional/required flag?
   ```

4. **How to handle record extensions?**
   ```zig
   // For: { foo : Str | rest }
   // The debug output shows: .ext = null for our test case
   // What does .ext contain when present?
   // Should we ignore it for completion or try to resolve it?
   ```

## Testing

Once implemented, these tests should pass:

```bash
zig build test-lsp
```

Specifically:
- `test.record field completion works for modules` - should suggest both `foo` and `bar`
- `test.record completion uses snapshot env when builds fail` - should work with snapshot
- `test.record field completion with partial field name` - should filter correctly

## Example TypeAnnotation.zig Study Points

Look for these patterns in `src/canonicalize/TypeAnnotation.zig`:

```zig
// Find the TypeAnno union definition
pub const TypeAnno = union(enum) {
    record: RecordAnno,
    lookup: LookupAnno,
    // ... other variants
};

// Find RecordAnno structure
pub const RecordAnno = struct {
    fields: FieldsSpan,  // Or similar
    ext: ?Extension,     // Or similar
};

// Find how fields are stored
pub const FieldsSpan = struct {
    // ...
};

// Look for existing code that iterates over record fields in type annotations
// Search for: "TypeAnno" + "record" + "fields"
// Search for: iteration over type annotation fields
```

## Alternative: Simple String-Based Extraction

If TypeAnno parsing is too complex, you could try a simpler approach:

1. Get the source text range for the type annotation
2. Parse it as a string to extract field names
3. No type details, just field names

This is less robust but might work for common cases:

```zig
fn addFieldsFromTypeAnnoSimple(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
    def: CIR.Def,
) !void {
    // Try to get the source location of the type annotation
    // Parse the text: "{ foo : Str, bar : I64 }"
    // Extract field names: ["foo", "bar"]
    // Add completion items with no type details
    
    // This is hacky but might be easier than understanding TypeAnno API
}
```

## Debug Commands

To see TypeAnno structure:

```zig
// Add more detailed debug output:
std.debug.print("type_anno full: {}\n", .{type_anno});

// Or use a debugger with breakpoint at line 3310 in syntax.zig
// Inspect the type_anno variable
```

## Related Code to Study

Files to read for understanding:

1. `src/canonicalize/TypeAnnotation.zig` - TypeAnno definition and utilities
2. `src/canonicalize/CIR.zig` - Def, Annotation, and their relationships  
3. `src/check/` - How type checker processes annotations (might show field iteration)
4. `src/reporting/` - Error messages might format TypeAnnos (shows how to extract info)

Search patterns:
```bash
# Find examples of TypeAnno.record usage
rg "TypeAnno.*record" src/

# Find examples of iterating fields
rg "fields.*span" src/canonicalize/

# Find existing field extraction code
rg "getRecordFields" src/
```

## Success Criteria

When this is implemented correctly:

1. Completion after `my_record.` suggests ALL fields from type annotation
2. Both `foo` and `bar` appear in suggestions (not just one)
3. Tests pass: `zig build test-lsp`
4. Works for:
   - Simple records: `{ foo : Str, bar : I64 }`
   - Records with extensions: `{ foo : Str | rest }`  (if possible)
   - Type aliases: `MyRecord : { foo : Str, bar : I64 }; val : MyRecord` (if possible)

## Notes

- This fix is LSP-specific and doesn't change the type system
- The type system limitation (only storing accessed fields) remains
- Other tools might need similar fixes
- Consider filing an issue about the type system behavior for future improvement
