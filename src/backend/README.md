# Backend Development Guide

This directory contains the code generation backends for Roc.

## Running Tests with Filters

To run a specific eval test by name filter:

```bash
# Run a single test
zig build test --filter "list refcount function"
```

## Debugging Generated Code

When debugging code generation issues (calling conventions, register allocation, etc.),
you can enable hex dumping of the generated machine code.

### Enable Hex Dump

Use a targeted debug print in the backend code path you are investigating and
re-run the relevant eval test. The inspect-only eval harness no longer keeps a
test-helper-specific generated-code dump switch.

### Disassembling the Output

**On Windows**, save the hex bytes to a file and use:
```bash
# Using dumpbin (from Visual Studio)
dumpbin /disasm code.bin

# Or using objdump (from MinGW/MSYS2)
objdump -D -b binary -m i386:x86-64 code.bin
```

**On Linux/macOS**:
```bash
objdump -D -b binary -m i386:x86-64 code.bin
```

**Using Python** to convert hex dump to binary:
```python
import re
hex_dump = """
55 48 89 E5 53 41 54 48  81 EC 00 04 00 00 48 89
CB 49 89 D4 ...
"""
hex_values = re.findall(r'[0-9A-Fa-f]{2}', hex_dump)
code_bytes = bytes(int(h, 16) for h in hex_values)
with open('code.bin', 'wb') as f:
    f.write(code_bytes)
```
