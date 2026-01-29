# LSP Completion Module Refactoring Analysis

This document catalogs the public API of the completion modules and identifies functions in syntax.zig that should be refactored to use CompletionBuilder.

---

## Completion Module Public API

### builder.zig - CompletionBuilder Struct

#### Lifecycle Methods

- **`init(allocator, items)`** - Initialize a new CompletionBuilder
- **`deinit(self)`** - Clean up resources used by the builder
- **`addItem(self, item)`** - Add a completion item with deduplication (returns bool)

#### Module Name Completions

- **`addModuleNameCompletionsFromEnv(self, env)`** - Add module names from all loaded packages in BuildEnv
- **`addModuleLikeNameCompletionsFromModuleEnv(self, module_env)`** - Add module-like names (e.g., "Basic" from "Basic.to_str")
- **`addModuleNameCompletions(self, module_env)`** - Add module names from current module (imports and local)

#### Module Member Completions

- **`addModuleMemberCompletions(self, env, module_name, module_env_opt)`** - Add members of a specific module (e.g., Str.concat)
- **`addModuleMemberCompletionsFromModuleEnv(self, module_env, module_name)`** - Add module members from specific ModuleEnv
- **`addLocalModuleMemberCompletions(self, module_env, module_name)`** - Add local module members (qualified identifiers)

#### Type Completions

- **`addTypeCompletions(self, module_env)`** - Add type completions for type annotation context
- **`addTypeCompletionsFromEnv(self, env)`** - Add type completions from all modules in BuildEnv

#### Local Completions

- **`addLocalCompletions(self, module_env, cursor_offset)`** - Add local variables and functions in scope

#### Record Field Completions

- **`addRecordFieldCompletions(self, module_env, variable_name, variable_start)`** - Add record field completions for "myRecord."
- **`addFieldsFromTypeVar(self, module_env, type_var)`** - Extract and add record fields from a type variable

#### Method Completions

- **`addMethodCompletions(self, module_env, variable_name, variable_start)`** - Add method completions for static dispatch "value.method()"
- **`addMethodsFromTypeVar(self, module_env, type_var)`** - Extract methods from a type variable

### context.zig - Public Functions

- **`detectCompletionContext(source, line, character)`** - Detect completion context (after_module_dot, after_record_dot, after_colon, expression)
- **`computeOffset(source, line, character)`** - Convert LSP position to byte offset

### builtins.zig - Public Functions

- **`isBuiltinType(type_name)`** - Check if a type name is a builtin
- **`BUILTIN_TYPES`** - Constant array of builtin type names

---

## Functions in syntax.zig That Should Use CompletionBuilder

### Completion Functions to Replace (18 functions)

These functions exist in syntax.zig but have equivalent functionality in CompletionBuilder:

1. **`addModuleNameCompletionsFromEnv`** (Line 2464)
   - ✅ **Replace with**: `CompletionBuilder.addModuleNameCompletionsFromEnv()`

2. **`addModuleLikeNameCompletionsFromModuleEnv`** (Line 2493)
   - ✅ **Replace with**: `CompletionBuilder.addModuleLikeNameCompletionsFromModuleEnv()`

3. **`addTypeNamesFromModuleEnv`** (Line 2523)
   - ⚠️ **Note**: Private helper in builder, called by `addTypeCompletions()`

4. **`addTypeCompletionsFromEnv`** (Line 2558)
   - ✅ **Replace with**: `CompletionBuilder.addTypeCompletionsFromEnv()`

5. **`addModuleMemberCompletions`** (Line 2600)
   - ✅ **Replace with**: `CompletionBuilder.addModuleMemberCompletions()`

6. **`addModuleMemberCompletionsFromModuleEnv`** (Line 2673)
   - ✅ **Replace with**: `CompletionBuilder.addModuleMemberCompletionsFromModuleEnv()`

7. **`addLocalModuleMemberCompletions`** (Line 2726)
   - ✅ **Replace with**: `CompletionBuilder.addLocalModuleMemberCompletions()`

8. **`addTypeCompletions`** (Line 2815)
   - ✅ **Replace with**: `CompletionBuilder.addTypeCompletions()`

9. **`addRecordFieldCompletions`** (Line 2821)
   - ✅ **Replace with**: `CompletionBuilder.addRecordFieldCompletions()`

10. **`addFieldsFromTypeVar`** (Line 2930)
    - ✅ **Replace with**: `CompletionBuilder.addFieldsFromTypeVar()`

11. **`addFieldsFromContent`** (Line 2957)
    - ⚠️ **Note**: Private helper in builder

12. **`addFieldsFromRecord`** (Line 2986)
    - ⚠️ **Note**: Private helper in builder

13. **`addLocalCompletions`** (Line 3023)
    - ✅ **Replace with**: `CompletionBuilder.addLocalCompletions()`

14. **`addModuleNameCompletions`** (Line 3139)
    - ✅ **Replace with**: `CompletionBuilder.addModuleNameCompletions()`

15. **`addMethodCompletions`** (Line 3363)
    - ✅ **Replace with**: `CompletionBuilder.addMethodCompletions()`

16. **`addMethodsFromTypeVar`** (Line 3445)
    - ✅ **Replace with**: `CompletionBuilder.addMethodsFromTypeVar()`

17. **`addMethodsFromConstraints`** (Line 3492)
    - ⚠️ **Note**: Private helper in builder

18. **`addMethodsForTypeIdent`** (Line 3563)
    - ⚠️ **Note**: Private helper in builder

### Helper Functions Referenced in Completions

These are also in builder.zig:

- **`stripModulePrefix`** (Line 2571) - ⚠️ In builder.zig as private helper
- **`firstSegment`** (Line 2588) - ⚠️ In builder.zig as private helper
- **`lastSegment`** (Line 2593) - ⚠️ In builder.zig as private helper
- **`getStatementParts`** - Already moved to module_lookup.zig

### Context Functions

- **`findDotReceiverTypeVar`** (Line 3219) - Keep in syntax.zig (internal traversal logic)
- **`findDotReceiverTypeVarInExpr`** (Line 3239) - Keep in syntax.zig (internal traversal)
- **`findExprTypeForPattern`** (Line 3533) - Keep in syntax.zig (internal lookup)
- **`findMethodType`** (Line 4010) - Keep in syntax.zig (internal lookup)

---

## Refactoring Strategy

### Phase 1: Update getCompletionsAtPosition

The main function `getCompletionsAtPosition` should:

1. Create a `CompletionBuilder` instance
2. Call appropriate builder methods based on completion context
3. Use the builder's deduplication automatically

### Phase 2: Remove Duplicate Functions

After replacing calls to the 14 main completion functions with CompletionBuilder methods, the duplicate implementations in syntax.zig should be removed.

### Phase 3: Keep Internal Helpers

Keep syntax.zig-specific functions like:
- `findDotReceiverTypeVar` (cursor position analysis)
- `findDotReceiverTypeVarInExpr` (expression traversal)
- `findExprTypeForPattern` (pattern type lookup)
- `findMethodType` (method type lookup)

These are internal to syntax.zig's analysis and not exposed in the completion API.

---

## Example Refactoring Pattern

### Before:
```zig
try self.addModuleMemberCompletions(items, env, module_name, module_env);
```

### After:
```zig
var builder = completion_builder.CompletionBuilder.init(self.allocator, &items);
defer builder.deinit();
try builder.addModuleMemberCompletions(env, module_name, module_env);
```

---

## Files Modified

- ✅ `/home/eli/Code/roc/roc/src/lsp/syntax.zig` - Remove duplicate completion functions, use CompletionBuilder
- ✅ `/home/eli/Code/roc/roc/src/lsp/completion/builder.zig` - Already contains all builder methods
- ✅ `/home/eli/Code/roc/roc/src/lsp/completion/context.zig` - Already contains context detection
- ✅ `/home/eli/Code/roc/roc/src/lsp/completion/builtins.zig` - Already contains builtin type checks

---

Generated: 2026-01-22
