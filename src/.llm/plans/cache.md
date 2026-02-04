
Plan: Persistent Semantic Snapshot for Completions
Goal
Ensure completions use the last successful, error‑free semantic analysis, even when the current buffer doesn’t compile.
---
Phase 1: Define the snapshot model
1) Add SemanticSnapshot struct (new file or in lsp/syntax.zig)
   - Fields:
     - module_env: *ModuleEnv (or a compact clone of needed data)
     - scope_map: ScopeMap (prebuilt)
     - module_path: []const u8
     - content_hash: u64 (hash of source used to create snapshot)
     - deps_hash: u64 (optional, hash of imports/build state)
     - timestamp: i64 (optional)
2) Decide ownership/lifetime strategy
   - Option A: store an owned deep copy (safe, memory heavy)
   - Option B: store the BuildEnv + ModuleEnv pointer and keep env alive (simpler, but lifetime coupled)
   - Recommendation: Start with Option B using a retained BuildEnv, then evolve to a compact snapshot if memory becomes an issue.
Clarification: do you prefer a full copy of semantic data, or keeping a BuildEnv around per module?
---
Phase 2: Capture the snapshot on successful build
3) Wire into SyntaxChecker.createFreshBuildEnv / build path
   - After env.build(...), drain reports.
   - If there are zero reports for the module path, update snapshot.
   - Otherwise: do not overwrite the snapshot.
4) Where to wire
   - In SyntaxChecker.getCompletionsAtPosition
     - after build succeeds and reports are drained
     - call updateSemanticSnapshot(absolute_path, env, module_env)
   - Also consider wiring into other entry points (hover, definitions) if they use fresh builds.
5) Implement updateSemanticSnapshot
   - Input: absolute_path, env, module_env
   - Store snapshot in a new map:
     - semantic_snapshots: std.AutoHashMapUnmanaged([]const u8, SemanticSnapshot)
   - Ensure path keys are owned (dup path string).
---
Phase 3: Use snapshot for completions
6) Select module env for completion
   - Prefer snapshot first:
     - snapshot = semantic_snapshots.get(absolute_path)
   - If snapshot exists: use snapshot.module_env and snapshot.scope_map
   - Only use current build env when:
     - build succeeded
     - reports for this module are empty
     - and snapshot is missing (or still acceptable)
7) Wire into completion logic
   - getCompletionsAtPosition
     - replace current module_env_opt selection with:
       - module_env_opt = snapshot.module_env if exists
       - else fall back to current build env if no reports
   - addRecordFieldCompletions / addMethodCompletions
     - accept ScopeMap from snapshot if available
     - avoid rebuilding scope_map if stored in snapshot
---
Phase 4: Scope map caching
8) Build scope map once per snapshot
   - After successful build:
     - ScopeMap.build(module_env)
     - store in snapshot
   - In completion:
     - reuse snapshot.scope_map for variable resolution
---
Phase 5: Cleanup and lifecycle
9) Snapshot eviction policy
   - Simple: replace on new successful build for same path.
   - Optional: limit memory (LRU) if needed.
10) Ensure BuildEnv lifetime
   - If storing pointers to ModuleEnv, ensure BuildEnv remains alive.
   - That means snapshot should retain or own a BuildEnv or otherwise copy required data.
---
Phase 6: Testing & verification
11) Add targeted tests
   - A completion test where the buffer has a syntax error (record_test.) but uses a prior snapshot.
   - Ensure completions still show val1.
12) Manual test
   - In editor, type record_test. with a broken line nearby and verify fields still appear.
---
Where to wire in (summary)
- src/lsp/syntax.zig
  - SyntaxChecker struct: add semantic_snapshots
  - getCompletionsAtPosition: snapshot lookup + update on clean build
  - Add updateSemanticSnapshot helper
- src/lsp/scope_map.zig
  - No change unless we want a clone/reset method
- src/lsp/handlers/completion.zig
  - No change required unless we want snapshot‑based debug info
---
keep a full copy in the cache rather than some subset, for simplicity
