# Refactored Modules Analysis

## module_lookup.zig Public API

### Pattern/Definition Functions
- `extractIdentFromPattern(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) -> ?Ident.Idx`: Extract the identifier from a pattern, handling .assign and .as cases
- `extractIdentFromPatternRecursive(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) -> ?Ident.Idx`: Extract the identifier from a pattern, recursively following .as patterns to find the innermost identifier
- `findDefinitionByName(module_env: *ModuleEnv, name: []const u8) -> ?DefinitionInfo`: Find a definition by name, searching through all_defs and all_statements
- `findDefinitionsWithPrefix(module_env: *ModuleEnv, prefix: []const u8, allocator: std.mem.Allocator) -> !std.ArrayList(DefinitionInfo)`: Find all definitions in a module that match a given prefix
- `getStatementParts(stmt: CIR.Statement) -> StatementParts`: Extract the common parts from a statement (pattern, expression(s))
- `findBindingByName(module_env: *ModuleEnv, name: []const u8, offset: u32) -> ?BindingInfo`: Find a binding by name that is in scope at the given offset
- `iterateDefinitions(module_env: *ModuleEnv) -> DefinitionIterator`: Create an iterator over all definitions in a module

### Module Functions
- `findModuleByName(build_env: *BuildEnv, module_name: []const u8) -> ?ModuleInfo`: Find a module by name in the build environment's schedulers
- `findModuleByNameWithBuiltinCheck(build_env: *BuildEnv, module_name: []const u8, builtin_types: []const []const u8) -> ?ModuleInfo`: Find a module by name, optionally checking if it's a builtin type first

### Type Functions
- `getTypeVarForPattern(pattern_idx: CIR.Pattern.Idx) -> TypeVar`: Get the type variable for a pattern from the type store
- `getTypeVarForExpr(expr_idx: CIR.Expr.Idx) -> TypeVar`: Get the type variable for an expression from the type store

## cir_visitor.zig Public API

### Visitor Type (Generic)
- `CirVisitor(comptime Context: type) -> type`: Returns a comptime-generic CIR visitor struct that walks expression, statement, pattern, and type annotation trees

### CirVisitor Methods (for instances)
- `init(ctx: *Context, callbacks: Callbacks) -> Self`: Initialize a visitor with the given context and callbacks
- `walkExpr(self: *Self, store: *const NodeStore, expr_idx: CIR.Expr.Idx) -> void`: Walk an expression tree, invoking callbacks at each node
- `walkStatement(self: *Self, store: *const NodeStore, stmt_idx: CIR.Statement.Idx) -> void`: Walk a statement tree, invoking callbacks at each node
- `walkPattern(self: *Self, store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) -> void`: Walk a pattern tree, invoking callbacks at each node
- `walkTypeAnno(self: *Self, store: *const NodeStore, anno_idx: CIR.TypeAnno.Idx) -> void`: Walk a type annotation tree, invoking callbacks at each node
- `walkModule(self: *Self, store: *const NodeStore, stmts: CIR.Statement.Span) -> void`: Walk all top-level statements in a module
- `walkDefs(self: *Self, store: *const NodeStore, defs: []const CIR.Def) -> void`: Walk all definitions in a module

### Region Helper Functions
- `regionContainsOffset(region: Region, offset: u32) -> bool`: Check if a region contains a given byte offset
- `regionSize(region: Region) -> u32`: Calculate the size (span) of a region in bytes
- `getExprRegion(store: *const NodeStore, expr_idx: CIR.Expr.Idx) -> Region`: Get the region for an expression
- `getPatternRegion(store: *const NodeStore, pattern_idx: CIR.Pattern.Idx) -> Region`: Get the region for a pattern
- `getStatementRegion(store: *const NodeStore, stmt_idx: CIR.Statement.Idx) -> Region`: Get the region for a statement
- `getTypeAnnoRegion(store: *const NodeStore, anno_idx: CIR.TypeAnno.Idx) -> Region`: Get the region for a type annotation

## cir_queries.zig Public API

### Region Functions
- `regionContainsOffset(region: Region, offset: u32) -> bool`: Check if a region contains a given byte offset (inclusive on both ends)
- `regionSize(region: Region) -> u32`: Calculate the size (span) of a region in bytes
- `regionToRange(module_env: *const ModuleEnv, region: Region) -> ?LspRange`: Convert a Region to an LspRange using line starts from ModuleEnv

### Lookup/Type Query Functions
- `findTypeAtOffset(module_env: *ModuleEnv, offset: u32) -> ?TypeAtOffsetResult`: Find the type of the narrowest expression or pattern containing the target offset
- `findLookupAtOffset(module_env: *ModuleEnv, offset: u32) -> ?CIR.Expr.Idx`: Find a variable lookup (local or external) at the given offset
- `collectLookupReferences(module_env: *ModuleEnv, target_pattern: CIR.Pattern.Idx, allocator: std.mem.Allocator) -> std.ArrayList(LspRange)`: Collect all references to a specific pattern (variable binding)
- `findPatternAtOffset(module_env: *ModuleEnv, offset: u32) -> ?CIR.Pattern.Idx`: Find a pattern at the given offset
- `findDotReceiverTypeVar(module_env: *ModuleEnv, offset: u32) -> ?types.Var`: Find the type variable of a dot access receiver at the given offset

---

## All Functions in syntax.zig (Current State)

### Public API Functions
- Line 0045: `pub fn init(allocator, debug, log_file) -> SyntaxChecker`
- Line 0054: `pub fn deinit(self: *SyntaxChecker) -> void`
- Line 0069: `pub fn check(self, uri, override_text, workspace_root) -> ![]Diagnostics.PublishDiagnostics`
- Line 0253: `pub fn getModuleLookupEnv(self) -> ?*BuildEnv`
- Line 0270: `pub fn getModuleEnvByPath(self, path) -> ?*ModuleEnv`
- Line 0294: `pub fn getImportedModuleEnvs(self, module_path) -> !?[]*ModuleEnv`
- Line 0573: `pub fn getTypeAtPosition(self, uri, override_text, line, character) -> !?HoverResult`
- Line 0620: `pub fn getDefinitionAtPosition(self, uri, override_text, line, character) -> !?DefinitionResult`
- Line 1257: `pub fn getHighlightsAtPosition(self, uri, override_text, line, character) -> !?HighlightResult`
- Line 1300: `pub fn getDocumentSymbols(self, allocator, uri, source) -> ![]document_symbol_handler.SymbolInformation`
- Line 1368: `pub fn getCompletionsAtPosition(self, uri, override_text, line, character) -> !?completion_handler.CompletionResult`

### Internal Helper Functions
- Line 0131: `fn createFreshBuildEnv(self) -> !*BuildEnv`
- Line 0169: `fn shouldSnapshotBuild(...) -> bool`
- Line 0191: `fn storeSnapshotEnv(...) -> void`
- Line 0208: `fn retainSnapshotEnv(...) -> void`
- Line 0215: `fn releaseSnapshotEnv(...) -> void`
- Line 0235: `fn clearSnapshots(self) -> void`
- Line 0244: `fn isEnvReferencedInSnapshots(...) -> bool`
- Line 0276: `fn getModuleEnvByPathInEnv(...) -> ?*ModuleEnv`
- Line 0336: `fn freeDrainedEx(...) -> void`
- Line 0351: `fn freeDrained(...) -> void`
- Line 0356: `fn freeDrainedWithReports(...) -> void`
- Line 0361: `fn updateDependencyGraph(...) -> void`
- Line 0423: `fn reportToDiagnostic(...) -> !Diagnostics.Diagnostic`
- Line 0445: `fn rangeFromReport(...) -> Diagnostics.Range`
- Line 0478: `fn saturatingMinusOne(value: u32) -> u32`
- Line 0482: `fn logDebug(...) -> void`
- Line 0496: `fn shouldSuppressReport(...) -> bool`
- Line 0502: `fn reportContainsAny(...) -> bool`
- Line 0511: `fn elementContainsAny(...) -> bool`
- Line 0539: `fn textHasAny(...) -> bool`
- Line 0547: `fn read(ctx, path, gpa) -> !?[]u8`

### Definition/Navigation Functions (Kept - syntax-specific logic)
- Line 0648: `fn positionToOffset(module_env, line, character) -> ?u32` - LSP position conversion
- Line 0659: `fn findDefinitionAtOffset(self, module_env, target_offset, current_uri) -> ?DefinitionResult` - Complex navigation with builtin handling
- Line 0808: `fn findModuleByName(self, module_name) -> ?DefinitionResult` - NOT DUPLICATE: Handles builtin types, URI creation, different return type
- Line 0872: `fn extractBaseTypeName(type_str: []const u8) -> []const u8` - String utility
- Line 0891: `fn findTypeAnnoAtOffset(...) -> ?DefinitionResult` - Type annotation navigation
- Line 1018: `fn findTypeAnnoInExpr(...) -> ?DefinitionResult` - Recursive type annotation search
- Line 1115: `fn checkExprAndRecurse(...) -> ?TypeAtOffsetResult` - Kept for legacy reasons
- Line 1140: `fn checkPatternAndRecurse(...) -> ?TypeAtOffsetResult` - Kept for legacy reasons
- Line 1487: `fn findMethodType(...) -> ?types.Var` - Completion-specific helper

### Utility Functions (Kept)
- Line 1526: `fn buildLineOffsets(source) -> LineOffsets`
- Line 1546: `fn offsetToPosition(offset, line_offsets) -> document_symbol_handler.Position`
- Line 1558: `fn extractSymbolFromDecl(...) -> ?document_symbol_handler.SymbolInformation`

---

## Refactoring Results

### Functions Removed (8 total, 1026 lines)
1. ✅ `findTypeAtOffset` (Line 1241) - REPLACED with `cir_queries.findTypeAtOffset`
2. ✅ `findNestedTypeAtOffset` (Line 1492) - Dead code, only called by removed findTypeAtOffset
3. ✅ `findPatternAtOffset` (Line 2065) - REPLACED with `cir_queries.findPatternAtOffset`
4. ✅ `collectLookupReferences` (Line 2148) - REPLACED with `cir_queries.collectLookupReferences`
5. ✅ `collectLookupsInExpr` (Line ~1530) - Dead code, only called by removed collectLookupReferences
6. ✅ `findDotReceiverTypeVar` (Line 2395) - REPLACED with `cir_queries.findDotReceiverTypeVar`
7. ✅ `findDotReceiverTypeVarInExpr` (Line 2425) - Dead code, only called by removed findDotReceiverTypeVar
8. ✅ All completion functions (Phase 2, completed earlier) - REPLACED with `completion_builder.CompletionBuilder`

### File Size Reduction
- **Before Phase 2**: 4144 lines
- **After Phase 2**: 2893 lines (removed 1251 lines of completion code)
- **After Phase 3**: 1867 lines (removed 1026 lines of query code)
- **Total Reduction**: 2277 lines (55% reduction)

### Functions to Keep (Not Duplicates)
- `findModuleByName` - Handles builtin types, creates URIs, returns DefinitionResult (different from module_lookup version)
- `findMethodType` - Completion-specific helper  
- `extractBaseTypeName` - String utility
- `buildLineOffsets`, `offsetToPosition`, `extractSymbolFromDecl` - Symbol/position utilities
- Build environment management functions
- Diagnostic/reporting functions
- `checkExprAndRecurse`, `checkPatternAndRecurse` - Kept for potential future use
