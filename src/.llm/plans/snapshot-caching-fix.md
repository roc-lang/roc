# LSP Snapshot Caching Issue - Analysis and Plan

## Problem Statement

Static dispatch completion doesn't work when:
1. User has a working file that compiles successfully
2. User adds incomplete code (e.g., `result = val.`)
3. User requests completion

Even though we have a caching system (snapshots), completions return nothing.

## Root Cause Analysis

### Issue 1: Snapshots Not Created for Clean Builds

**Location**: `lsp/syntax.zig:shouldSnapshotBuild` (lines 239-253)

**Current Logic**:
```zig
fn shouldSnapshotBuild(_: *SyntaxChecker, absolute_path: []const u8, drained: []BuildEnv.DrainedModuleReports) bool {
    var saw_entry = false;
    for (drained) |entry| {
        if (!std.mem.eql(u8, entry.abs_path, absolute_path)) continue;
        saw_entry = true;
        if (entry.reports.len > 0) return false;
    }
    return saw_entry;  // Returns FALSE if no entry exists!
}
```

**Problem**: The function only returns `true` if:
1. There's a drained entry for the file path
2. That entry has 0 reports

But `drainReports()` only returns entries for modules that produced reports (errors or warnings).
**Clean builds produce no entries**, so `saw_entry` remains `false` and no snapshot is created!

**Evidence** (from test logs):
```
shouldSnapshotBuild: checking .../static_dispatch.roc, drained.len=0
  -> saw_entry=false, returning false
After check: snapshot_envs.count = 0
```

### Issue 2: No Fallback When Snapshot Missing

When no snapshot exists:
- `getCompletionsAtPosition` tries to get module_env from snapshot
- Snapshot doesn't exist → `module_env_opt = null`
- No module_env → `completion: NO module_env for record/method completions`

## Proposed Fix

### Fix 1: Update Snapshot Creation Logic

Change `shouldSnapshotBuild` to snapshot when:
1. Build succeeded (no exception thrown)
2. No ERROR-level reports for the file (warnings are OK)
3. Module was actually processed (has ModuleEnv)

**New Logic**:
```zig
fn shouldSnapshotBuild(self: *SyntaxChecker, env: *BuildEnv, absolute_path: []const u8, drained: []BuildEnv.DrainedModuleReports) bool {
    // First check if the module was actually processed
    const module_env = self.getModuleEnvByPathInEnv(env, absolute_path);
    if (module_env == null) {
        // Module wasn't processed, don't snapshot
        return false;
    }

    // Check for ERROR-level reports (warnings are OK)
    for (drained) |entry| {
        if (!std.mem.eql(u8, entry.abs_path, absolute_path)) continue;
        for (entry.reports) |report| {
            if (report.severity == .runtime_error or report.severity == .fatal) {
                return false;  // Has errors, don't snapshot
            }
        }
    }

    // Module processed successfully with no errors → snapshot
    return true;
}
```

### Fix 2: Update Call Site

Update the call in `check()` to pass `env`:
```zig
if (self.shouldSnapshotBuild(env, absolute_path, drained)) {
    self.storeSnapshotEnv(env, absolute_path);
}
```

### Alternative Simpler Fix

If we don't want to check severity, a simpler fix is:
```zig
fn shouldSnapshotBuild(self: *SyntaxChecker, env: *BuildEnv, absolute_path: []const u8, drained: []BuildEnv.DrainedModuleReports) bool {
    // Check if module was processed
    if (self.getModuleEnvByPathInEnv(env, absolute_path) == null) {
        return false;
    }

    // Check for any reports for this file
    for (drained) |entry| {
        if (std.mem.eql(u8, entry.abs_path, absolute_path) and entry.reports.len > 0) {
            return false;  // Has reports, don't snapshot
        }
    }

    // Module processed with no reports → snapshot
    return true;
}
```

This is simpler but still requires the module to have been processed successfully.

## Implementation Steps

1. Update `shouldSnapshotBuild` signature to accept `env: *BuildEnv`
2. Add check for ModuleEnv existence
3. Update the call site in `check()` to pass the env
4. Remove debug logging after fix is verified
5. Run tests to verify fix

## Files to Modify

1. `lsp/syntax.zig`:
   - `shouldSnapshotBuild` function (lines 239-253)
   - `check` function call site (line 161)

## Test Case

The existing test `static dispatch completion for nominal type methods` will verify this:
1. First `check` with clean code → should create snapshot
2. `getCompletionsAtPosition` with incomplete code → should use snapshot and return completions
