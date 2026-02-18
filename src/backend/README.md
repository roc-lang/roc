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

In `src/eval/test/helpers.zig`, set the `dump_generated_code_hex` constant to `true`:

```zig
const dump_generated_code_hex = true;  // Set to true to enable
```

This will print the raw bytes of generated code when tests run:

```
=== Generated Code (93 bytes, entry_offset=0) ===
0000: 55 48 89 E5 53 41 54 48  81 EC 00 04 00 00 48 89  |UH..SATH......H.|
0010: CB 49 89 D4 4C 89 A5 D8  FF FF FF 48 B9 30 00 00  |.I..L......H.0..|
...
=== End Generated Code ===
```

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