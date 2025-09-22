# Test Platform

This directory contains a primitive test platform for Roc and demonstrates how to pass multiple arguments from the host.

- **Function signature**: `I64, I64 -> I64`
- **Description**: Takes two random integers from the host and returns their product

```bash
zig build

# Run (ignore cached files)
./zig-out/bin/roc --no-cache test/int/app.roc
```
