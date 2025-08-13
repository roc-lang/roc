# Snapshots

Snapshot tests that validate compiler behavior by capturing the output of each compilation stage for specific Roc code examples.

Snapshot tests provide comprehensive validation of the compilation pipeline by showing how source code is transformed through each stage: tokenization, parsing, canonicalization, and type checking etc.

Each snapshot file contains the expected output and helps us to detect regressions when compiler behavior changes unexpectedly.

## Usage

- **Generate all snapshots**: `zig build snapshot`
- **Update specific snapshot**: `zig build snapshot -- <file_path>`
- **Update expected from problems**: `zig build update-expected -- <file_path>`
- **Debug REPL evaluation with trace**: `zig build snapshot -- <repl_snapshot.md> --trace-eval`

### Trace Debugging

The `--trace-eval` flag enables detailed interpreter tracing for debugging REPL snapshots:

```bash
# Debug build (trace support enabled by default)
zig build snapshot -- src/snapshots/repl/repl_record_field_access.md --trace-eval
```

**Requirements:**
- Only works with REPL snapshots (`type=repl`)
- Can only be used with a single snapshot file
- Trace output is automatically enabled in debug builds
- For release builds, use `-Dtrace-eval=true` to enable tracing
