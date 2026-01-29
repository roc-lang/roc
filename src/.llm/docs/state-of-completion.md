

  LSP Completion Research Summary

  1. Analysis Caching - Yes, Last Successful Analysis is Kept

  The LSP maintains dual BuildEnv pointers in src/lsp/syntax.zig:38-41:
  - build_env: Current build environment
  - previous_build_env: Last successful build (kept for lookups when current fails)

  The getModuleLookupEnv() function (lines 230-239) prioritizes current, falls back to previous. This means completions can work even when code doesn't compile.

  2. Getting Exposed Symbols from a Module - Yes

  ExposedItems (src/collections/ExposedItems.zig:24-195):
  - iterator() (lines 192-194): Returns all exposed (ident_idx, node_idx) pairs
  - containsById() / getNodeIndexById(): Lookup specific exports

  ModuleEnv (src/canonicalize/ModuleEnv.zig):
  - Has common: CommonEnv containing exposed_items
  - Can access imported module environments via getImportedModuleEnvs() in syntax.zig:265-304

  3. Cursor Context - Yes, Infrastructure Exists

  Position utilities in src/lsp/syntax.zig:
  - positionToOffset() (lines 722-732): LSP position → byte offset
  - findLookupAtOffset() (lines 1044-1240): Finds narrowest lookup expression at cursor
  - findDefinitionAtOffset() (lines 767-943): Determines if cursor is on identifier/type

  4. Data Structures for Completion Candidates
  ┌───────────────────────────────────┬────────────────────────────────────────────────────────────────────────┐
  │          Completion Type          │                                 Source                                 │
  ├───────────────────────────────────┼────────────────────────────────────────────────────────────────────────┤
  │ Local variables                   │ Scope.idents - AutoHashMap<Ident.Idx, Pattern.Idx> (Scope.zig:56-91)   │
  ├───────────────────────────────────┼────────────────────────────────────────────────────────────────────────┤
  │ Local top-levels                  │ ExposedItems in the current module's CommonEnv                         │
  ├───────────────────────────────────┼────────────────────────────────────────────────────────────────────────┤
  │ Imported symbols                  │ Scope.imported_modules, plus ExposedItems from each imported ModuleEnv │
  ├───────────────────────────────────┼────────────────────────────────────────────────────────────────────────┤
  │ Module names                      │ Scope.module_aliases maps alias → module info                          │
  ├───────────────────────────────────┼────────────────────────────────────────────────────────────────────────┤
  │ Module exports (e.g., String.map) │ Look up module by alias, then iterate its ExposedItems                 │
  ├───────────────────────────────────┼────────────────────────────────────────────────────────────────────────┤
  │ Record fields                     │ Need to query type info - getTypeAtPosition() exists (lines 552-646)   │
  ├───────────────────────────────────┼────────────────────────────────────────────────────────────────────────┤
  │ Type aliases                      │ Scope.aliases, Scope.type_bindings                                     │
  └───────────────────────────────────┴────────────────────────────────────────────────────────────────────────┘
  5. Key Files for Implementation
  ┌──────────────────────────────────┬──────────────────────────────────────────────────┐
  │               File               │                     Purpose                      │
  ├──────────────────────────────────┼──────────────────────────────────────────────────┤
  │ src/lsp/server.zig:54-65         │ Add completion handler to request_handlers       │
  ├──────────────────────────────────┼──────────────────────────────────────────────────┤
  │ src/lsp/syntax.zig               │ Core analysis queries, position utilities        │
  ├──────────────────────────────────┼──────────────────────────────────────────────────┤
  │ src/canonicalize/Scope.zig:56-91 │ Scope bindings iteration                         │
  ├──────────────────────────────────┼──────────────────────────────────────────────────┤
  │ src/collections/ExposedItems.zig │ Module export iteration                          │
  ├──────────────────────────────────┼──────────────────────────────────────────────────┤
  │ src/base/CommonEnv.zig:149-162   │ Identifier store for getting text from Ident.Idx │
  └──────────────────────────────────┴──────────────────────────────────────────────────┘
  6. What's Missing for Completion

  1. No completion handler - needs to be added following existing handler patterns
  2. No partial match search - identifier store exists but may need prefix matching
  3. No completion context detection - determining if completing:
    - After . (record field or module access)
    - After : (type annotation)
    - At expression start (local/imported values)
  4. Scope at cursor position - existing code finds definitions at cursor, but getting the enclosing scope for locals may need work
