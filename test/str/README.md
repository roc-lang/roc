# Test Platform

This directory contains a primitive test platform for Roc and demonstrates passing a string to and from the host.

- **Function signature**: `Str -> Str`
- **Description**: Takes a string from the host and returns a processed string

```bash
zig build -Dllvm

# Run (ignore cached files)
./zig-out/bin/roc --no-cache test/str/app.roc
```
