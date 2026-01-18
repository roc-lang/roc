# Cross-Module Semantic Tokens for Roc LSP

## Summary

This plan describes how to add cross-module semantic tokens to the Roc LSP, enabling proper highlighting of imported functions and types. The design leverages existing `BuildEnv` infrastructure with a persistent build state.

## Problem

The LSP currently uses single-file canonicalization for semantic tokens. This means:

```roc
# Current highlighting (WRONG):
Str.to_uppercase text     # "to_uppercase" → property (same as record.field)
List.join_map [1,2,3] fn  # "join_map" → property

# Expected highlighting (CORRECT):
Str.to_uppercase text     # "to_uppercase" → function
List.join_map [1,2,3] fn  # "join_map" → function
```

The tokenizer sees `.DotLowerIdent` and assigns `property` type. Without cross-module context, it cannot distinguish `Str.to_uppercase` (function call) from `record.field` (field access).

## Current Architecture

### Semantic Tokens (`src/lsp/semantic_tokens.zig`)

The handler receives only the file text and builds a fresh `ModuleEnv`:

```zig
// Current: isolated single-file canonicalization
var module_env = ModuleEnv.init(allocator, source);
can.canonicalizeFile(&module_env, &parse_ast);  // No imported module context
```

Token type mapping is purely syntactic:
- `.DotLowerIdent` → `property` (line 101-109)
- `.UpperIdent` → `type`

### LSP Server (`src/lsp/syntax.zig`)

Each request resets the BuildEnv:

```zig
fn resetBuildEnv(self: *SyntaxChecker) !*BuildEnv {
    if (self.build_env != null) {
        env_ptr.deinit();  // Destroys previous build
    }
    env_ptr.* = try BuildEnv.init(...);  // Fresh start
}
```

No build state persists between requests.

### Cache Manager (`src/compile/cache_manager.zig`)

Cache keys use only source content + compiler version:

```zig
pub fn generateCacheKey(source: []const u8, compiler_version: []const u8) [32]u8 {
    var hasher = std.crypto.hash.Blake3.init(.{});
    hasher.update(compiler_version);
    hasher.update(source);
    return hasher.finalResult();
}
```

Dependencies are not tracked in cache keys.

### Build Infrastructure (`src/compile/compile_build.zig`, `compile_package.zig`)

`ModuleState` already tracks dependencies during builds:

```zig
const ModuleState = struct {
    imports: ArrayList(ModuleId),           // Local imports
    external_imports: ArrayList([]const u8), // Cross-package imports
    dependents: ArrayList(ModuleId),         // Reverse dependencies
    depth: u32,                              // Distance from root
};
```

This information exists but is discarded after each build.

## Proposed Design

### Core Change: Persistent BuildEnv

Keep the `BuildEnv` alive between LSP requests instead of recreating it:

```zig
pub const SyntaxChecker = struct {
    build_env: ?*BuildEnv,        // Persistent (currently reset each time)
    module_envs: ModuleEnvCache,  // NEW: URI → ModuleEnv mapping
    dependency_graph: DependencyGraph,  // NEW: tracks what depends on what
};
```

### Semantic Token Enhancement

Pass imported module environments to the token extractor:

```zig
pub fn extractSemanticTokensWithImports(
    module_env: *ModuleEnv,
    imported_envs: []const *ModuleEnv,  // NEW: cross-module context
) ![]SemanticToken {
    // Build lookup for imported names
    var import_map = std.StringHashMap(SemanticType).init(allocator);
    for (imported_envs) |imp_env| {
        for (imp_env.exports.functions) |func| {
            try import_map.put(func.name, .function);
        }
    }
    // Use import_map when processing DotLowerIdent tokens
}
```

### Dependency-Aware Cache Keys

Include dependency hashes in cache validation:

```zig
pub fn generateModuleCacheKey(
    source: []const u8,
    compiler_version: []const u8,
    dep_exports_hashes: []const [32]u8,  // NEW
) [32]u8 {
    var hasher = std.crypto.hash.Blake3.init(.{});
    hasher.update(compiler_version);
    hasher.update(source);
    for (dep_exports_hashes) |h| hasher.update(&h);
    return hasher.finalResult();
}
```

## Implementation Phases

### Phase 1: Persistent BuildEnv

**Goal**: Keep BuildEnv alive between requests

**Changes**:
- Remove `env_ptr.deinit()` from `resetBuildEnv()` in `syntax.zig`
- Add `build_env` field to `Server` struct (or keep in `SyntaxChecker`)
- Initialize BuildEnv on workspace open
- Add mutex for thread safety (LSP handles concurrent requests)

**Files**:
- `src/lsp/syntax.zig` - Stop resetting BuildEnv
- `src/lsp/server.zig` - Workspace initialization

**Verification**: Open workspace, make edits, verify subsequent builds are faster (cache hits).

### Phase 2: Module Environment Cache

**Goal**: Maintain URI → ModuleEnv mapping for quick lookup

**Changes**:
- Create `ModuleEnvCache` struct
- After successful builds, store ModuleEnv references by URI
- Provide `getModuleEnv(uri)` and `getImportedEnvs(uri)` APIs

**Files**:
- `src/lsp/module_env_cache.zig` (new)
- `src/lsp/syntax.zig` - Populate cache after builds

**Verification**: Request semantic tokens, verify module lookup succeeds.

### Phase 3: Cross-Module Semantic Tokens

**Goal**: Use imported module context for richer highlighting

**Changes**:
- Modify `extractSemanticTokensWithCIR` to accept `imported_envs` parameter
- Build import name → semantic type lookup table
- When encountering `DotLowerIdent` after an `UpperIdent`, check if the UpperIdent is a known module and the LowerIdent is an exported function
- Fall back to current behavior if imports unavailable

**Files**:
- `src/lsp/semantic_tokens.zig` - Accept imported_envs, build lookup
- `src/lsp/handlers/semantic_tokens.zig` - Pass imported_envs from cache

**Verification**: Open file that imports `Str`, verify `Str.to_uppercase` highlights `to_uppercase` as function (6), not property (4).

### Phase 4: Dependency Graph

**Goal**: Track what modules depend on what for invalidation

**Changes**:
- Create `DependencyGraph` struct with:
  - `modules: StringHashMap(ModuleNode)`
  - Each node tracks: path, content_hash, imports, dependents
- After each build, extract dependency info from `PackageEnv.modules`
- Implement `invalidate(path)` to mark transitive dependents as stale

**Files**:
- `src/lsp/dependency_graph.zig` (new)
- `src/lsp/syntax.zig` - Build graph after compilation

**Verification**: Log dependency graph, verify it matches import structure.

### Phase 5: Incremental Invalidation

**Goal**: Only rebuild changed modules and their dependents

**Changes**:
- On document change, compute content hash
- If unchanged (common for focus/blur events), skip rebuild
- If changed, use DependencyGraph to find stale modules
- Rebuild only stale modules in topological order

**Files**:
- `src/lsp/syntax.zig` - Selective rebuild logic
- `src/compile/compile_build.zig` - Add `rebuildModules(stale_set)` API if needed

**Verification**: Edit function body, verify only that module rebuilds. Edit export signature, verify dependents rebuild.

### Phase 6: Dependency-Aware Caching (Optional)

**Goal**: Cache validation includes dependency state

**Changes**:
- Compute `exports_hash` from sorted export names (stable across builds)
- Include dependency exports_hashes in cache key
- Cache automatically invalidates when dependencies change

**Files**:
- `src/compile/cache_manager.zig` - New key generation function
- `src/lsp/dependency_graph.zig` - Compute exports_hash

**Verification**: Change an export in module A, verify module B (which imports A) cache misses.

## Phase Summary

| Phase | Goal | Complexity |
|-------|------|------------|
| 1 | Persistent BuildEnv | Medium |
| 2 | Module Environment Cache | Low |
| 3 | Cross-Module Semantic Tokens | Medium |
| 4 | Dependency Graph | Medium |
| 5 | Incremental Invalidation | Medium |
| 6 | Dependency-Aware Caching | Low |

Phases 1-3 deliver the user-facing feature (correct highlighting). Phases 4-6 are performance optimizations.

## Key Files

| Component | File |
|-----------|------|
| LSP Server | `src/lsp/server.zig` |
| Syntax Checker | `src/lsp/syntax.zig` |
| Semantic Tokens | `src/lsp/semantic_tokens.zig` |
| Token Handler | `src/lsp/handlers/semantic_tokens.zig` |
| BuildEnv | `src/compile/compile_build.zig` |
| PackageEnv/ModuleState | `src/compile/compile_package.zig` |
| CacheManager | `src/compile/cache_manager.zig` |

## Open Questions

1. **Workspace discovery**: How to find project root when opening a single file?
   - Walk up to find `main.roc` or `package.roc`

2. **Memory management**: What's the eviction strategy for large projects?
   - LRU eviction of least-recently-used modules
   - Fall back to single-file mode if memory constrained

3. **Startup time**: Full initial build may take seconds
   - Background build on workspace open
   - Return partial/token-only results until build completes

## Notes

- Roc forbids import cycles (detected during canonicalization), so topological ordering is always possible
- Export hashes should use **names only**, not internal indices, for stability across builds
- The existing `ModuleState.imports/dependents` tracking can be reused for the dependency graph
