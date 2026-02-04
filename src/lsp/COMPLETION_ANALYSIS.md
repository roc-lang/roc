# Completion System Analysis Report

This document provides a comprehensive analysis of the completion-related code in `/home/eli/Code/roc/roc/src/lsp/syntax.zig` to inform the design of a `completion/` subdirectory.

## 1. Context Detection

### `CompletionContext` Type Definition (Lines 2617-2633)

```zig
pub const CompletionContext = union(enum) {
    /// After a dot with a module prefix: "Str." or "List."
    after_module_dot: []const u8,
    /// After a dot with a record variable: "myRecord."
    after_record_dot: struct {
        /// The variable name before the dot
        variable_name: []const u8,
        /// Byte offset of the start of the variable name
        variable_start: u32,
    },
    /// After a colon (type annotation context)
    after_colon,
    /// General expression context
    expression,
};
```

**Variants:**
1. `after_module_dot` - Module member access (e.g., `Str.`, `List.`)
   - Payload: module name (`[]const u8`)
2. `after_record_dot` - Record field or method access (e.g., `myRecord.`)
   - Payload: struct with `variable_name` and `variable_start` offset
3. `after_colon` - Type annotation position (e.g., `foo : `)
4. `expression` - General expression context (default)

---

### `detectCompletionContext` Function (Lines 2636-2718)

**Purpose:** Analyze source text at a given position to determine what type of completion is needed.

**Signature:**
```zig
pub fn detectCompletionContext(source: []const u8, line: u32, character: u32) CompletionContext
```

**Algorithm:**
1. Convert line/character to byte offset
2. Skip backwards past any partial identifier being typed
3. Skip whitespace
4. Check the character before:
   - `.` → Extract identifier before dot
     - Uppercase first char → `after_module_dot`
     - Lowercase first char → `after_record_dot`
   - `:` → `after_colon`
   - Otherwise → `expression`

**Dependencies:** None (pure text analysis)

---

### `computeOffset` Function (Lines 2721-2733)

**Purpose:** Convert LSP line/character position to byte offset.

**Signature:**
```zig
fn computeOffset(source: []const u8, line: u32, character: u32) u32
```

**Algorithm:**
1. Iterate through source, counting newlines
2. When target line is reached, record line start
3. Return `line_start + character` (clamped to source length)

**Dependencies:** None (pure text analysis)

---

## 2. Builtin Data

### `BUILTIN_TYPES` Constant (Line 978)

```zig
const BUILTIN_TYPES = [_][]const u8{
    "Str", "List", "Bool", "Try", "Dict", "Set", "Box",
    "U8", "U16", "U32", "U64", "U128",
    "I8", "I16", "I32", "I64", "I128",
    "F32", "F64", "Dec", "Num"
};
```

**Usage:** Used by `isBuiltinType()` and `findModuleByName()` to:
- Detect when navigating to a builtin type definition
- Redirect to the embedded `Builtin.roc` source file

---

### `isBuiltinType` Function (Lines 986-991)

**Signature:**
```zig
fn isBuiltinType(type_name: []const u8) bool
```

**Purpose:** Check if a type name is a known builtin type.

---

### Note: No `BUILTIN_MODULES` Constant

Currently there is no hardcoded list of builtin modules. Module names are discovered dynamically from:
- `BuildEnv.schedulers` (all loaded packages/modules)
- `ModuleEnv.common.exposed_items` (exports from each module)
- Import statements in the current module

---

## 3. Completion Item Building Functions

### Main Entry Point: `getCompletionsAtPosition` (Lines 2738-2842)

**Signature:**
```zig
pub fn getCompletionsAtPosition(
    self: *SyntaxChecker,
    uri: []const u8,
    override_text: ?[]const u8,
    line: u32,
    character: u32,
) !?completion_handler.CompletionResult
```

**Workflow:**
1. Acquire mutex, create fresh BuildEnv
2. Build the module via `BuildSession`
3. Detect completion context from source
4. Try to get ModuleEnv (prefers snapshot, falls back to previous/current build)
5. Dispatch to appropriate completion function based on context
6. Return `CompletionResult` with items list

**Dependencies:**
- `SyntaxChecker` (self)
- `BuildEnv` / `BuildSession`
- `ModuleEnv` (from snapshot or current build)
- `completion_handler.CompletionResult` / `CompletionItem`

---

### Context-Specific Functions

#### `addModuleMemberCompletions` (Lines 2916-2998)

**Line:** 2916  
**Purpose:** Add completions for members of a specific module (e.g., `Str.concat`, `List.map`)

**Signature:**
```zig
fn addModuleMemberCompletions(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    env: *BuildEnv,
    module_name: []const u8,
    module_env_opt: ?*ModuleEnv,
) !void
```

**Dependencies:**
- `BuildEnv` - to iterate schedulers/modules
- `ModuleEnv` - for exposed_items, idents, type writer
- `base.Ident.Idx` - for identifier lookup

**What it adds:**
- All exposed items from the named module
- Items from exposed_items that match `ModuleName.xxx` pattern
- Detail: type signature if available

---

#### `addModuleMemberCompletionsFromModuleEnv` (Lines 3000-3056)

**Line:** 3000  
**Purpose:** Helper to extract module members from a specific ModuleEnv

---

#### `addLocalModuleMemberCompletions` (Lines 3058-3146)

**Line:** 3058  
**Purpose:** Add members defined as qualified identifiers (e.g., `Basic.to_str`)

---

#### `addTypeCompletions` (Lines 3149-3152)

**Line:** 3149  
**Purpose:** Add type completions for type annotation context

**Signature:**
```zig
fn addTypeCompletions(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
) !void
```

**What it adds:**
- Type names from `s_alias_decl` and `s_nominal_decl` statements
- Kind: `class`

---

#### `addRecordFieldCompletions` (Lines 3157-3245)

**Line:** 3157  
**Purpose:** Add record field completions for `myRecord.` access

**Signature:**
```zig
fn addRecordFieldCompletions(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
    variable_name: []const u8,
    variable_start: u32,
) !void
```

**Algorithm:**
1. Build scope map to find variable binding
2. Look up in top-level defs/statements by name
3. Get type variable for the binding
4. Call `addFieldsFromTypeVar`

**Dependencies:**
- `scope_map.ScopeMap` - for local variable lookup
- `ModuleEnv` - for type information
- `types.Var` - for type resolution

---

#### `addFieldsFromTypeVar` (Lines 3248-3280)

**Line:** 3248  
**Purpose:** Extract and add record fields from a type variable

**Signature:**
```zig
fn addFieldsFromTypeVar(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
    type_var: types.Var,
) !void
```

**Algorithm:**
1. Resolve type variable
2. Unwrap aliases (up to 8 levels)
3. If record type found, call `addFieldsFromRecord`

**Dependencies:**
- `types.Content` - for type content analysis
- `types.Record` - for record field extraction

---

#### `addFieldsFromContent` (Lines 3283-3319)

**Line:** 3283  
**Purpose:** Recursively extract fields from type content, unwrapping aliases

---

#### `addFieldsFromRecord` (Lines 3322-3361)

**Line:** 3322  
**Purpose:** Add completion items for fields in a record type

**Signature:**
```zig
fn addFieldsFromRecord(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
    record: types.Record,
) !void
```

**What it adds:**
- Each field name with kind `field`
- Detail: field type signature

**Dependencies:**
- `types.Record` - for record fields
- `ModuleEnv.initTypeWriter()` - for type formatting

---

#### `addLocalCompletions` (Lines 3364-3499)

**Line:** 3364  
**Purpose:** Add local definition completions (variables, functions in scope)

**Signature:**
```zig
fn addLocalCompletions(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
    cursor_offset: u32,
) !void
```

**What it adds:**
- Local variables in scope at cursor position (from `ScopeMap`)
- Top-level definitions from `all_defs`
- Top-level statements from `all_statements`
- Kind: `variable` or `function` based on expression type
- Detail: type signature

**Dependencies:**
- `scope_map.ScopeMap` - for scope reconstruction
- `ModuleEnv` - for definitions and type info

---

#### `addModuleNameCompletions` (Lines 3502-3592)

**Line:** 3502  
**Purpose:** Add module name completions (for qualified access)

**Signature:**
```zig
fn addModuleNameCompletions(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
) !void
```

**What it adds:**
- Module names from import statements
- Module-like names from qualified definitions (e.g., `Basic` from `Basic.to_str`)
- Kind: `module`

---

#### `addModuleNameCompletionsFromEnv` (Lines 2854-2886)

**Line:** 2854  
**Purpose:** Add module names from all loaded packages/modules in BuildEnv

---

#### `addTypeCompletionsFromEnv` (Lines 2910-2915)

**Line:** 2910  
**Purpose:** Add type completions from all modules in BuildEnv

---

### Method Completion Functions

#### `addMethodCompletions` (Lines 4079-4147)

**Line:** 4079  
**Purpose:** Add method completions for static dispatch (e.g., `value.method()`)

**Signature:**
```zig
fn addMethodCompletions(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
    variable_name: []const u8,
    variable_start: u32,
) !void
```

**Algorithm:**
1. Build scope map to find variable binding
2. Look up in top-level defs/statements
3. Get type variable and call `addMethodsFromTypeVar`

---

#### `addMethodsFromTypeVar` (Lines 4150-4195)

**Line:** 4150  
**Purpose:** Extract methods available for a type variable

**Signature:**
```zig
fn addMethodsFromTypeVar(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
    type_var: types.Var,
) !void
```

**Algorithm:**
1. Resolve type variable
2. Extract type identifier from alias or nominal
3. Call `addMethodsForTypeIdent` if identifier found
4. For flex/rigid types, call `addMethodsFromConstraints`

---

#### `addMethodsFromConstraints` (Lines 4198-4245)

**Line:** 4198  
**Purpose:** Extract method names from static dispatch constraints

**Signature:**
```zig
fn addMethodsFromConstraints(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
    constraints: types.StaticDispatchConstraint.SafeList.Range,
) !void
```

**What it adds:**
- Method names from constraint list
- Kind: `method`
- Detail: function type signature

---

#### `addMethodsForTypeIdent` (Lines 4262-4315)

**Line:** 4262  
**Purpose:** Add methods for a specific type identifier by searching `method_idents`

**Signature:**
```zig
fn addMethodsForTypeIdent(
    self: *SyntaxChecker,
    items: *std.ArrayList(completion_handler.CompletionItem),
    module_env: *ModuleEnv,
    type_ident: base.Ident.Idx,
) !void
```

**Algorithm:**
1. Iterate through `module_env.method_idents.entries`
2. For each entry where `type_ident` matches, add the method name
3. Look up method type via `findMethodType`

---

#### `findMethodType` (Lines 4318-4352)

**Line:** 4318  
**Purpose:** Find the type of a method definition by its qualified identifier

---

### Helper Functions for Dot Access

#### `findDotReceiverTypeVar` (Lines 3594-3619)

**Line:** 3594  
**Purpose:** Find the type variable of the receiver in a dot access expression

---

#### `findDotReceiverTypeVarInExpr` (Lines 3621-3770)

**Line:** 3621  
**Purpose:** Recursively search for e_dot_access at cursor position

---

## 4. Completion Item Structure

### `CompletionItem` (from handlers/completion.zig, Lines 155-162)

```zig
pub const CompletionItem = struct {
    label: []const u8,
    kind: ?u32 = null,
    detail: ?[]const u8 = null,
    documentation: ?[]const u8 = null,
    sortText: ?[]const u8 = null,
    insertText: ?[]const u8 = null,
};
```

### `CompletionItemKind` Enum (Lines 8-30)

```zig
pub const CompletionItemKind = enum(u32) {
    text = 1,
    method = 2,
    function = 3,
    constructor = 4,
    field = 5,
    variable = 6,
    class = 7,
    interface = 8,
    module = 9,
    property = 10,
    unit = 11,
    value = 12,
    @"enum" = 13,
    keyword = 14,
    snippet = 15,
    // ... (20 total)
};
```

### `CompletionResult` (Lines 164-168)

```zig
pub const CompletionResult = struct {
    items: []const CompletionItem,
    is_incomplete: bool,
};
```

### JSON Format (LSP Response)

```json
{
  "isIncomplete": false,
  "items": [
    {
      "label": "concat",
      "kind": 3,
      "detail": "Str, Str -> Str"
    },
    {
      "label": "map",
      "kind": 3,
      "detail": "List(a), (a -> b) -> List(b)"
    }
  ]
}
```

---

## 5. Dependencies Summary

### External Imports Required

```zig
const std = @import("std");
const base = @import("base");           // Ident, Region
const can = @import("can");             // CIR, ModuleEnv
const types = @import("types");         // Var, Content, Record
const compile = @import("compile");     // BuildEnv
```

### Internal Imports Required

```zig
const scope_map = @import("scope_map.zig");          // ScopeMap, Binding
const completion_handler = @import("handlers/completion.zig");  // CompletionItem, etc.
```

### Key Type Dependencies

| Type | Module | Used For |
|------|--------|----------|
| `ModuleEnv` | `can` | CIR store, type store, identifiers |
| `BuildEnv` | `compile` | Package/module schedulers |
| `types.Var` | `types` | Type variable representation |
| `types.Content` | `types` | Resolved type content |
| `types.Record` | `types` | Record type with fields |
| `CIR.Pattern.Idx` | `can` | Pattern indices for binding lookup |
| `CIR.Expr.Idx` | `can` | Expression indices |
| `base.Ident.Idx` | `base` | Identifier indices |
| `ScopeMap` | local | Local variable scope reconstruction |

---

## 6. Suggested File Structure for completion/ Subdirectory

```
completion/
├── mod.zig                    # Public interface, re-exports
├── context.zig                # CompletionContext enum + detectCompletionContext
├── items.zig                  # CompletionItem building utilities
├── builtins.zig               # BUILTIN_TYPES + isBuiltinType
├── local.zig                  # addLocalCompletions (uses scope_map)
├── modules.zig                # addModuleNameCompletions, addModuleMemberCompletions
├── types.zig                  # addTypeCompletions, addTypeCompletionsFromEnv
├── fields.zig                 # addRecordFieldCompletions, addFieldsFromTypeVar, addFieldsFromRecord
├── methods.zig                # addMethodCompletions, addMethodsFromTypeVar, addMethodsForTypeIdent
└── provider.zig               # Main CompletionProvider with getCompletionsAtPosition
```

### Detailed File Contents

#### `mod.zig`
```zig
pub const CompletionContext = @import("context.zig").CompletionContext;
pub const detectCompletionContext = @import("context.zig").detectCompletionContext;
pub const computeOffset = @import("context.zig").computeOffset;
pub const CompletionProvider = @import("provider.zig").CompletionProvider;
pub const builtins = @import("builtins.zig");
```

#### `context.zig`
- `CompletionContext` union type
- `detectCompletionContext(source, line, character)`
- `computeOffset(source, line, character)`

#### `builtins.zig`
- `BUILTIN_TYPES: []const []const u8`
- `isBuiltinType(type_name: []const u8) bool`
- Future: `BUILTIN_MODULES` if needed

#### `items.zig`
- Helper functions for creating `CompletionItem`
- Deduplication logic
- Type formatting utilities

#### `local.zig`
- `addLocalCompletions(items, module_env, cursor_offset)`
- Integration with `scope_map.ScopeMap`

#### `modules.zig`
- `addModuleNameCompletions(items, module_env)`
- `addModuleNameCompletionsFromEnv(items, env)`
- `addModuleMemberCompletions(items, env, module_name, module_env_opt)`
- `addModuleMemberCompletionsFromModuleEnv(...)`
- `addLocalModuleMemberCompletions(...)`

#### `types.zig`
- `addTypeCompletions(items, module_env)`
- `addTypeCompletionsFromEnv(items, env)`
- `addTypeNamesFromModuleEnv(items, module_env)`

#### `fields.zig`
- `addRecordFieldCompletions(items, module_env, variable_name, variable_start)`
- `addFieldsFromTypeVar(items, module_env, type_var)`
- `addFieldsFromContent(items, module_env, content, depth)`
- `addFieldsFromRecord(items, module_env, record)`

#### `methods.zig`
- `addMethodCompletions(items, module_env, variable_name, variable_start)`
- `addMethodsFromTypeVar(items, module_env, type_var)`
- `addMethodsFromConstraints(items, module_env, constraints)`
- `addMethodsForTypeIdent(items, module_env, type_ident)`
- `findMethodType(module_env, qualified_ident)`
- `findDotReceiverTypeVar(module_env, cursor_offset)` (helper)
- `findDotReceiverTypeVarInExpr(...)` (CIR traversal)

#### `provider.zig`
```zig
pub const CompletionProvider = struct {
    allocator: std.mem.Allocator,
    
    pub fn getCompletionsAtPosition(
        self: *CompletionProvider,
        module_env: ?*ModuleEnv,
        build_env: *BuildEnv,
        source: []const u8,
        line: u32,
        character: u32,
    ) !?CompletionResult {
        // Main dispatch logic
    }
};
```

---

## 7. Function Summary Table

| Function | Line | Context | Adds | Kind |
|----------|------|---------|------|------|
| `detectCompletionContext` | 2636 | - | - | Context detection |
| `computeOffset` | 2721 | - | - | Helper |
| `getCompletionsAtPosition` | 2738 | - | - | Main entry |
| `addModuleNameCompletions` | 3502 | expression | Module names | `module` |
| `addModuleNameCompletionsFromEnv` | 2854 | expression | Module names | `module` |
| `addLocalCompletions` | 3364 | expression | Variables, functions | `variable`/`function` |
| `addTypeCompletions` | 3149 | after_colon | Type names | `class` |
| `addTypeCompletionsFromEnv` | 2910 | after_colon | Type names | `class` |
| `addModuleMemberCompletions` | 2916 | after_module_dot | Exports | `function`/`class` |
| `addRecordFieldCompletions` | 3157 | after_record_dot | Fields | `field` |
| `addFieldsFromTypeVar` | 3248 | after_record_dot | Fields | `field` |
| `addFieldsFromRecord` | 3322 | after_record_dot | Fields | `field` |
| `addMethodCompletions` | 4079 | after_record_dot | Methods | `method` |
| `addMethodsFromTypeVar` | 4150 | after_record_dot | Methods | `method` |
| `addMethodsFromConstraints` | 4198 | after_record_dot | Methods | `method` |
| `addMethodsForTypeIdent` | 4262 | after_record_dot | Methods | `method` |

---

## 8. Key Implementation Notes

1. **Snapshot preference:** Completions prefer snapshot ModuleEnv over current build because typing usually produces incomplete code that fails to parse.

2. **Deduplication:** All `add*` functions check for duplicates before adding items.

3. **Type resolution:** Uses `type_store.resolveVar()` and unwraps aliases up to 8 levels deep.

4. **Method lookup:** Two sources:
   - `method_idents` table in ModuleEnv
   - Static dispatch constraints in flex/rigid type vars

5. **Scope reconstruction:** Uses `scope_map.ScopeMap` which traverses CIR to rebuild scope information that doesn't persist after canonicalization.

6. **Type formatting:** Uses `ModuleEnv.initTypeWriter()` for consistent type signature formatting.
