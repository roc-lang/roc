# Refactoring Plan: `syntax.zig` Modularization

## Overview

Decompose the monolithic 4600-line `syntax.zig` into focused modules with shared abstractions for CIR traversal, build session management, and completion handling. This eliminates ~60% of duplicated code while improving testability and maintainability.

**Target**: ~4600 lines → ~2000 lines across 8 focused modules

## Current Problems

### 1. Repeated Build Environment Setup (~40 lines × 6 occurrences)
Every public function repeats:
- Convert URI to path
- Create fresh BuildEnv
- Set up OverrideProvider  
- Change working directory
- Build and drain reports
- Find scheduler and module

**Affected functions**: `check`, `getTypeAtPosition`, `getDefinitionAtPosition`, `getHighlightsAtPosition`, `getCompletionsAtPosition`

### 2. Repeated CIR Traversal (~300 lines × 5 occurrences)
Nearly identical switch statements handling `e_lambda`, `e_closure`, `e_block`, `e_if`, `e_match`, `e_call`, etc.

**Affected functions**: `findNestedTypeAtOffset`, `findLookupAtOffset`, `collectLookupsInExpr`, `findTypeAnnoInExpr`, `findDotReceiverTypeVarInExpr`

### 3. Repeated Definition/Statement Search (~50 lines × 10+ occurrences)
Multiple functions iterate through `module_env.all_defs` and `module_env.all_statements` with similar pattern extraction logic.

### 4. Scattered Completion Logic (~800 lines)
Completion handling mixed with query logic, builtin lists hardcoded inline.

---

## Target File Structure

```
lsp/
├── syntax.zig              # ~500 lines - Main SyntaxChecker, delegates to modules
├── build_session.zig       # ~200 lines - Build environment management  
├── cir_visitor.zig         # ~300 lines - Generic CIR traversal
├── cir_queries.zig         # ~400 lines - Offset-based queries (hover, definition, highlights)
├── type_utils.zig          # ~150 lines - Type manipulation utilities
├── module_lookup.zig       # ~200 lines - Definition/pattern lookup utilities
├── completion/
│   ├── mod.zig             # ~100 lines - Main completion entry point
│   ├── context.zig         # ~150 lines - Context detection
│   ├── builder.zig         # ~400 lines - Building completion items
│   └── builtins.zig        # ~200 lines - Builtin completions data
└── (existing files unchanged)
```

---

## Implementation Steps

### Step 1: Create `build_session.zig`
**Status**: [ ] Not Started

Extract the repeated build environment setup pattern into a reusable `BuildSession` struct.

#### Sub-tasks:
- [ ] 1.1 Create `BuildSession` struct with fields: `checker`, `env`, `module_env`, `absolute_path`, `build_succeeded`
- [ ] 1.2 Implement `BuildSession.init()` - URI conversion, OverrideProvider setup, directory change, build
- [ ] 1.3 Implement `BuildSession.deinit()` - Restore directory, cleanup
- [ ] 1.4 Implement `BuildSession.getModuleEnv()` - Snapshot preference logic
- [ ] 1.5 Add unit tests for `BuildSession`
- [ ] 1.6 Refactor `check()` to use `BuildSession`
- [ ] 1.7 Refactor `getTypeAtPosition()` to use `BuildSession`
- [ ] 1.8 Refactor `getDefinitionAtPosition()` to use `BuildSession`
- [ ] 1.9 Refactor `getHighlightsAtPosition()` to use `BuildSession`
- [ ] 1.10 Refactor `getCompletionsAtPosition()` to use `BuildSession`

#### Interface Design:
```zig
pub const BuildSession = struct {
    checker: *SyntaxChecker,
    env: *BuildEnv,
    module_env: ?*ModuleEnv,
    absolute_path: []const u8,
    build_succeeded: bool,
    drained_reports: ?[]BuildEnv.DrainedModuleReports,
    prev_cwd: ?[]const u8,
    
    pub fn init(
        checker: *SyntaxChecker, 
        uri: []const u8, 
        override_text: ?[]const u8
    ) !BuildSession;
    
    pub fn deinit(self: *BuildSession) void;
    
    /// Get module env, preferring snapshot for incomplete code (completions)
    pub fn getModuleEnvPreferSnapshot(self: *BuildSession) ?*ModuleEnv;
    
    /// Get module env from current build only
    pub fn getModuleEnvFromBuild(self: *BuildSession) ?*ModuleEnv;
};
```

---

### Step 2: Create `cir_visitor.zig`
**Status**: [ ] Not Started

Implement a generic visitor pattern for CIR traversal to eliminate duplicated switch statements.

#### Sub-tasks:
- [ ] 2.1 Design `CirVisitor` comptime generic interface
- [ ] 2.2 Implement expression traversal with callback hooks
- [ ] 2.3 Implement statement traversal with callback hooks
- [ ] 2.4 Implement pattern traversal with callback hooks
- [ ] 2.5 Implement type annotation traversal with callback hooks
- [ ] 2.6 Add depth limiting to prevent stack overflow
- [ ] 2.7 Add unit tests with mock callbacks
- [ ] 2.8 Document usage patterns

#### Interface Design:
```zig
pub fn CirVisitor(comptime Context: type, comptime Result: type) type {
    return struct {
        const Self = @This();
        
        context: *Context,
        module_env: *ModuleEnv,
        max_depth: usize = 512,
        
        // User-provided callbacks (optional)
        onExpr: ?*const fn(*Self, CIR.Expr.Idx, CIR.Expr) ?Result = null,
        onPattern: ?*const fn(*Self, CIR.Pattern.Idx, CIR.Pattern) ?Result = null,
        onStatement: ?*const fn(*Self, CIR.Statement.Idx, CIR.Statement) ?Result = null,
        onTypeAnno: ?*const fn(*Self, CIR.TypeAnno.Idx, CIR.TypeAnno) ?Result = null,
        
        // Whether to continue traversing children after callback
        continueAfterMatch: bool = true,
        
        pub fn walkModule(self: *Self) ?Result;
        pub fn walkExpr(self: *Self, expr_idx: CIR.Expr.Idx, depth: usize) ?Result;
        pub fn walkStatement(self: *Self, stmt_idx: CIR.Statement.Idx, depth: usize) ?Result;
        pub fn walkPattern(self: *Self, pattern_idx: CIR.Pattern.Idx, depth: usize) ?Result;
        pub fn walkTypeAnno(self: *Self, anno_idx: CIR.TypeAnno.Idx, depth: usize) ?Result;
    };
}
```

---

### Step 3: Create `cir_queries.zig`
**Status**: [ ] Not Started

Move offset-based query logic using the new `CirVisitor`.

#### Sub-tasks:
- [ ] 3.1 Create `OffsetQuery` struct for narrowest-match tracking
- [ ] 3.2 Implement `findTypeAtOffset()` using `CirVisitor`
- [ ] 3.3 Implement `findDefinitionAtOffset()` using `CirVisitor`
- [ ] 3.4 Implement `findPatternAtOffset()` using `CirVisitor`
- [ ] 3.5 Implement `findLookupAtOffset()` using `CirVisitor`
- [ ] 3.6 Move `regionContainsOffset()` and `regionToRange()` helpers
- [ ] 3.7 Add unit tests for each query function
- [ ] 3.8 Update `syntax.zig` to use `cir_queries`

#### Interface Design:
```zig
pub const OffsetQuery = struct {
    target_offset: u32,
    best_size: u32 = std.math.maxInt(u32),
    
    pub fn checkRegion(self: *OffsetQuery, region: Region) bool {
        if (!regionContainsOffset(region, self.target_offset)) return false;
        const size = region.end.offset - region.start.offset;
        if (size >= self.best_size) return false;
        self.best_size = size;
        return true;
    }
};

pub fn findTypeAtOffset(module_env: *ModuleEnv, offset: u32) ?TypeAtOffsetResult;
pub fn findDefinitionAtOffset(module_env: *ModuleEnv, offset: u32, current_uri: []const u8) ?DefinitionResult;
pub fn findPatternAtOffset(module_env: *ModuleEnv, offset: u32) ?CIR.Pattern.Idx;
pub fn collectLookupReferences(module_env: *ModuleEnv, target_pattern: CIR.Pattern.Idx, allocator: Allocator) ![]LspRange;
```

---

### Step 4: Create `completion/` Subdirectory
**Status**: [ ] Not Started

Split completion logic into focused modules.

#### 4a: Create `completion/context.zig`
- [ ] 4a.1 Move `CompletionContext` union type
- [ ] 4a.2 Move `detectCompletionContext()` function
- [ ] 4a.3 Move `computeOffset()` helper
- [ ] 4a.4 Add unit tests for context detection

#### 4b: Create `completion/builtins.zig`
- [ ] 4b.1 Move `BUILTIN_MODULES` constant
- [ ] 4b.2 Move `BUILTIN_TYPES` constant  
- [ ] 4b.3 Move `isBuiltinType()` function
- [ ] 4b.4 Create `getBuiltinModuleMembers()` - move hardcoded member lists
- [ ] 4b.5 Add `addBuiltinTypeCompletions()` 
- [ ] 4b.6 Add `addBuiltinModuleNameCompletions()`

#### 4c: Create `completion/builder.zig`
- [ ] 4c.1 Create `CompletionBuilder` struct
- [ ] 4c.2 Move `addFieldsFromTypeVar()`
- [ ] 4c.3 Move `addFieldsFromRecord()`
- [ ] 4c.4 Move `addMethodsFromTypeVar()`
- [ ] 4c.5 Move `addMethodsFromConstraints()`
- [ ] 4c.6 Move `addMethodsForTypeIdent()`
- [ ] 4c.7 Move `addLocalCompletions()`
- [ ] 4c.8 Move `addModuleNameCompletions()`
- [ ] 4c.9 Move `addModuleMemberCompletions()`
- [ ] 4c.10 Move `addTypeCompletions()`
- [ ] 4c.11 Add deduplication logic to builder

#### 4d: Create `completion/mod.zig`
- [ ] 4d.1 Re-export public types
- [ ] 4d.2 Implement main `getCompletions()` using `BuildSession`, context, and builder
- [ ] 4d.3 Update `syntax.zig` to delegate to completion module

---

### Step 5: Create `type_utils.zig`
**Status**: [ ] Not Started

Extract type manipulation helpers.

#### Sub-tasks:
- [ ] 5.1 Implement `unwrapAliases()` - iteratively resolve alias backing vars
- [ ] 5.2 Implement `getRecordFields()` - extract field names/types from record
- [ ] 5.3 Implement `formatTypeVar()` - TypeWriter initialization and formatting
- [ ] 5.4 Implement `extractBaseTypeName()` - get base type from complex type string
- [ ] 5.5 Add unit tests
- [ ] 5.6 Update completion builder to use type utils

#### Interface Design:
```zig
pub const TypeUtils = struct {
    pub fn unwrapAliases(type_store: *TypeStore, type_var: types.Var, max_depth: usize) types.Content;
    
    pub fn getRecordFields(type_store: *TypeStore, content: types.Content) ?RecordFieldsInfo;
    
    pub fn formatTypeVar(module_env: *ModuleEnv, type_var: types.Var, allocator: Allocator) !?[]const u8;
    
    pub fn extractBaseTypeName(type_str: []const u8) []const u8;
};

pub const RecordFieldsInfo = struct {
    names: []const Ident.Idx,
    vars: []const types.Var,
};
```

---

### Step 6: Create `module_lookup.zig`
**Status**: [ ] Not Started

Consolidate definition search patterns.

#### Sub-tasks:
- [ ] 6.1 Implement `extractIdentFromPattern()` - handle `.assign` and `.as` cases
- [ ] 6.2 Implement `findDefinitionByName()` - search defs and statements
- [ ] 6.3 Implement `findModuleByName()` - search schedulers
- [ ] 6.4 Implement `getTypeVarForPattern()` 
- [ ] 6.5 Move `getStatementParts()` helper
- [ ] 6.6 Add unit tests
- [ ] 6.7 Update all callers in `syntax.zig` and `cir_queries.zig`

#### Interface Design:
```zig
pub const DefinitionInfo = struct {
    pattern_idx: CIR.Pattern.Idx,
    expr_idx: ?CIR.Expr.Idx,
    ident_idx: Ident.Idx,
};

pub fn extractIdentFromPattern(store: *CIR.Store, pattern_idx: CIR.Pattern.Idx) ?Ident.Idx;

pub fn findDefinitionByName(module_env: *ModuleEnv, name: []const u8) ?DefinitionInfo;

pub fn findModuleByName(env: *BuildEnv, module_name: []const u8) ?ModuleInfo;

pub fn getStatementParts(stmt: CIR.Statement) StatementParts;
```

---

### Step 7: Refactor `syntax.zig`
**Status**: [ ] Not Started

Slim down to orchestration layer.

#### Sub-tasks:
- [ ] 7.1 Remove functions moved to other modules
- [ ] 7.2 Keep `SyntaxChecker` struct with snapshot management
- [ ] 7.3 Keep `OverrideProvider` (used by `BuildSession`)
- [ ] 7.4 Keep dependency graph update logic
- [ ] 7.5 Update all public methods to delegate to modules
- [ ] 7.6 Update imports
- [ ] 7.7 Verify all tests pass

---

### Step 8: Update `mod.zig` and Finalize
**Status**: [ ] Not Started

#### Sub-tasks:
- [ ] 8.1 Update `lsp/mod.zig` to re-export new modules if needed
- [ ] 8.2 Verify all handler imports still work
- [ ] 8.3 Run full test suite
- [ ] 8.4 Update any documentation
- [ ] 8.5 Remove this plan file or move to docs/

---

## Risk Mitigation

### Testing Strategy
1. **Before each step**: Ensure existing tests pass
2. **During refactoring**: Keep old and new code paths temporarily, compare results
3. **After each step**: Run full test suite before proceeding

### Rollback Points
- Each step is independently valuable
- Can stop after any step and still have improved code
- Git commits at each step boundary

### Backward Compatibility
- Keep `SyntaxChecker` public API identical
- New modules are internal implementation details
- Handler imports remain unchanged

---

## Success Metrics

| Metric | Before | After |
|--------|--------|-------|
| Lines in `syntax.zig` | ~4600 | ~500 |
| Total lines (all modules) | ~4600 | ~2000 |
| Duplicated switch statements | 5 | 1 (in visitor) |
| Build setup code copies | 6 | 1 |
| Unit testable modules | 0 | 6 |

---

## Open Questions

1. **Should `CirVisitor` use comptime or runtime callbacks?** Comptime gives better optimization but less flexibility.

2. **Should completion builder own allocations or use arena?** Arena simplifies cleanup but may retain memory longer.

3. **Should we add a `CirIterator` alternative to visitor?** Iterator pattern might be simpler for some use cases.
