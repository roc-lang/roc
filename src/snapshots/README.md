# Snapshots

Snapshot tests that validate compiler behavior by capturing the output of each compilation stage for specific Roc code examples.

Snapshot tests provide comprehensive validation of the compilation pipeline by showing how source code is transformed through each stage: tokenization, parsing, canonicalization, and type checking etc. 

Each snapshot file contains the expected output and helps us to detect regressions when compiler behavior changes unexpectedly.

## Usage

- **Generate all snapshots**: `zig build snapshot`
- **Update specific snapshot**: `zig build snapshot -- <file_path>`
- **Update expected from problems**: `zig build update-expected -- <file_path>`
