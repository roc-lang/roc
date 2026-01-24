# Fix Cache Deserialization for Fast Path

## Goal

Correctly implement cache deserialization so the fast path works without crashes or memory leaks. The fast path logic is already implemented - the issue is memory management of deserialized ModuleEnv data.

## Current Status

### What Works
- Cache files are written correctly after type-checking
- Metadata cache tracks imports for fast path validation
- Fast path detection works (cache hits are identified)
- Cache stats are printed (`Cache: 4/6 modules cached (66.7% hit rate)`)

### What's Broken
- **Warm cache crashes**: "switch on corrupt value" in worker thread, or stack overflow during deinit
- **Memory leaks**: Hash maps allocated during deserialization aren't freed
- Cold cache works fine; crashes only happen on warm cache (when loading cached modules)

## Key Insight: The Relocation Strategy

Roc uses a unique **in-place deserialization** strategy. Understanding this is critical:

### How Serialization Works (`src/collections/CompactWriter.zig`)
```
Serialized Format:
┌─────────────────────────────────────────────────┐
│ Header (magic, version_hash, data_size, etc.)   │
├─────────────────────────────────────────────────┤
│ ModuleEnv.Serialized struct                      │
│   - Fields with OFFSETS instead of pointers     │
│   - offset = ptr - base_addr (during serialize) │
├─────────────────────────────────────────────────┤
│ String data, arrays, interner bytes, etc.       │
│ (referenced by offsets above)                   │
└─────────────────────────────────────────────────┘
```

### How Deserialization Works (`ModuleEnv.Serialized.deserialize`)
1. The cache buffer is loaded into memory (via `readFromFileMapped`)
2. `ModuleEnv.Serialized` at the start of the buffer is cast to `*ModuleEnv`
3. **In-place pointer fixup**: For each field, `ptr = base_addr + offset`
4. The result is a `*ModuleEnv` that points INTO the cache buffer

### Mixed Memory Ownership After Deserialization

| Data Type | Location | Owner | Action on Cleanup |
|-----------|----------|-------|-------------------|
| String data (source, idents) | Cache buffer | Cache buffer | Don't free |
| Arrays (nodes, regions) | Cache buffer | Cache buffer | Don't free |
| SmallStringInterner bytes | Cache buffer | Cache buffer | Don't free (has `supports_inserts=false` guard) |
| Hash map **storage** | Heap (newly allocated via `ensureTotalCapacity`) | Allocator | **Must free** |
| Hash map **keys** | Cache buffer | Cache buffer | Don't free |

This is the core problem: **some things need freeing, others don't**.

## Files Involved

| File | Role |
|------|------|
| `src/compile/cache_module.zig` | Low-level cache read/write, `CacheData` union |
| `src/compile/cache_manager.zig` | High-level cache API, `CacheResult` with buffer ownership |
| `src/compile/coordinator.zig` | Fast path check in `executeParse`, `handleCacheHit`, `cache_buffers` list |
| `src/compile/compile_package.zig` | Scheduler's `ModuleState` with `was_from_cache` flag, deinit logic |
| `src/compile/compile_build.zig` | Transfer from coordinator to scheduler, `was_cache_hit` → `was_from_cache` |
| `src/canonicalize/ModuleEnv.zig` | `Serialized.deserialize()`, `deinit()` |
| `src/base/SmallStringInterner.zig` | `supports_inserts` guard, changed panic to no-op |

## Current Code Flow

### Cache Hit Path
```
1. executeParse() reads source, computes hash
2. Checks metadata cache → finds cached imports
3. Calls loadFromCacheByKey() → returns CacheResult.hit with:
   - module_env: *ModuleEnv (points into cache buffer)
   - cache_data: CacheData (owns the buffer)
4. Returns WorkerResult.cache_hit with cache_data
5. handleCacheHit() stores cache_data in coordinator.cache_buffers
6. Module marked as Done, was_cache_hit = true
7. Transfer to scheduler: was_from_cache = coord_mod.was_cache_hit
8. Scheduler deinit: skips env.deinit() if was_from_cache
9. Coordinator deinit: frees cache_buffers
```

### The Problems

**Problem 1: Calling `env.deinit()` crashes**
- `ModuleEnv.deinit()` calls `deinit()` on all sub-structures
- These structures have backing arrays in the cache buffer
- Trying to free memory from cache buffer → crash/stack overflow

**Problem 2: NOT calling `env.deinit()` leaks hash maps**
- Hash maps like `imports.map` were allocated during deserialization
- Without `env.deinit()`, these leak

**Problem 3: Race condition causing "corrupt value"**
- Only happens on warm cache path
- Worker thread sees corrupt task discriminant
- Likely related to cache buffer ownership or task queue corruption

## Investigation Needed

Before implementing a fix, we need to understand:

### 1. What exactly allocates during deserialization?

Look at `ModuleEnv.Serialized.deserialize()` and trace every `allocator` call:
- Which hash maps call `ensureTotalCapacity`?
- Are there other heap allocations?

### 2. What does `deinit()` try to free?

Trace through `ModuleEnv.deinit()` and all sub-deinits:
- Which calls are safe for deserialized data?
- Which calls try to free cache buffer memory?

### 3. Why does the "corrupt value" error happen?

This is a race condition specific to warm cache:
- Is it related to `cache_buffers` ArrayList reallocating?
- Is it a task queue issue?
- Is the cache_data being freed too early?

### 4. How do unit tests handle this?

Look at `src/canonicalize/test/module_env_test.zig` and `import_store_test.zig`:
- How do tests clean up deserialized data?
- What's the expected lifetime model?

## Potential Solutions

### Option A: Selective Deinit (`deinitCachedModule`) - RECOMMENDED

Add a new method that only frees heap-allocated hash maps:

```zig
pub fn deinitCachedModule(self: *Self) void {
    // Only free hash maps that were allocated during deserialization
    self.imports.map.deinit(self.gpa);  // Free hash storage, not keys
    // ... other hash maps ...
    // Do NOT call common.deinit(), store.deinit(), etc.
}
```

**Pros**: Explicit, no leaks
**Cons**: Must track exactly what was allocated

### Option B: Track Deserialization State

Add `is_deserialized: bool` to ModuleEnv and check in deinit:

```zig
pub fn deinit(self: *Self) void {
    if (self.is_deserialized) {
        // Only free hash map storage
        self.imports.map.deinit(self.gpa);
        return;
    }
    // Normal full deinit
    ...
}
```

**Pros**: Single deinit path
**Cons**: Extra field, must remember to set it

### Option C: Make All Sub-Structures Deserialization-Aware

Like `SmallStringInterner.supports_inserts`, add guards to all structures:

```zig
// In MultiArrayList or similar
pub fn deinit(self: *Self, allocator: Allocator) void {
    if (self.is_external_memory) return;  // No-op for deserialized
    allocator.free(self.items);
}
```

**Pros**: Each type knows its ownership
**Cons**: Many files to modify, invasive

### Option D: Accept Leaks for Now

Skip `env.deinit()` entirely for cached modules. Hash maps leak but:
- Memory is freed when process exits
- Cache hits make compilation fast, so process doesn't run long

**Pros**: Simple, no crashes
**Cons**: Leaks, not a real fix

## Recommended Approach

1. **First**: Fix the "corrupt value" race condition (this is blocking testing)
2. **Then**: Implement Option A (selective deinit) for proper cleanup
3. **Finally**: Re-enable mmap in `cache_module.zig` for better performance

## Debug Commands

```bash
# Build with trace
zig build -Doptimize=Debug -Dtrace-cache

# Test cold cache
rm -rf ~/.cache/roc && ./zig-out/bin/roc check --verbose test/fx/app.roc

# Test warm cache
./zig-out/bin/roc check --verbose test/fx/app.roc

# Test without cache (should always work)
./zig-out/bin/roc check --no-cache test/fx/app.roc

# Run tests
zig build minici
```

## Implementation Steps

1. Launch Explore agents to:
   - Trace what allocates during `ModuleEnv.Serialized.deserialize()`
   - Trace what `ModuleEnv.deinit()` tries to free
   - Understand how unit tests manage deserialized data lifetime
   - Investigate the "corrupt value" race condition

2. Based on findings, implement the fix

3. Test thoroughly with warm/cold cache cycles

4. Run full test suite

## Related Git Branch

Working branch: `fix-sibling-modules`

## Files Modified (Uncommitted)

From `git status`:
- `src/base/SmallStringInterner.zig` - Modified
- `src/cli/main.zig` - Modified
- `src/compile/cache_manager.zig` - Modified
- `src/compile/cache_module.zig` - Modified
- `src/compile/compile_build.zig` - Modified
- `src/compile/compile_package.zig` - Modified
- `src/compile/coordinator.zig` - Modified
- `src/compile/messages.zig` - Modified

## Session Transcript

Full conversation history available at:
`/home/lbw/.claude/projects/-home-lbw-Documents-Github-roc/6f640f56-4d5e-4f9f-ae59-7620f579f36c.jsonl`
