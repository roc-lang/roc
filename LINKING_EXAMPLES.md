# Roc LLD Linking Examples

This document provides comprehensive examples and documentation for using the `roc link` command to link object files into executables using LLD (LLVM Linker).

## Prerequisites

- Roc must be built with LLVM support: `zig build roc -Dllvm=true`
- Object files must be compiled for the target platform
- LLD libraries must be available (included with LLVM)

## Basic Usage

```bash
roc link [OPTIONS] <OBJECT_FILES>... -o <OUTPUT>
```

### Options

- `-o <OUTPUT>`: Output executable path (required)
- `-h, --help`: Print help message

### Simple Example

Link two object files into an executable:

```bash
roc link main.o utils.o -o my_program
```

## Complete Example

Here's a step-by-step example showing how to create and link a multi-file C program:

### Step 1: Create Source Files

**main.c:**
```c
#include <stdio.h>

// Function declarations from utils.c
void print_separator();
int add_numbers(int a, int b);
void print_result(int result);
void print_info();

int main() {
    print_separator();
    printf("Hello from LLD-linked executable!\n");
    printf("This program was linked using Roc's LLD integration.\n");
    print_separator();
    
    print_info();
    print_separator();
    
    int result = add_numbers(42, 13);
    printf("Adding 42 + 13: ");
    print_result(result);
    
    print_separator();
    printf("Successfully demonstrated multi-file linking with LLD!\n");
    
    return 0;
}
```

**utils.c:**
```c
#include <stdio.h>

void print_separator() {
    printf("========================================\n");
}

int add_numbers(int a, int b) {
    return a + b;
}

void print_result(int result) {
    printf("Result: %d\n", result);
}

void print_info() {
    printf("LLD (LLVM Linker) successfully linked multiple object files!\n");
    printf("This function is from utils.c\n");
}
```

### Step 2: Compile to Object Files

```bash
clang -c main.c -o main.o
clang -c utils.c -o utils.o
```

### Step 3: Link with Roc

```bash
roc link main.o utils.o -o my_program
```

### Step 4: Run the Executable

```bash
./my_program
```

Expected output:
```
========================================
Hello from LLD-linked executable!
This program was linked using Roc's LLD integration.
========================================
LLD (LLVM Linker) successfully linked multiple object files!
This function is from utils.c
========================================
Adding 42 + 13: Result: 55
========================================
Successfully demonstrated multi-file linking with LLD!
```

## Platform-Specific Behavior

The `roc link` command automatically adds platform-specific flags:

### macOS
- Adds `-arch` flag (arm64 or x86_64)
- Sets platform version to match system
- Links against system SDK
- Links with `-lc` for C library

### Linux
- Adds dynamic linker path
- Links with `-lc` for C library

### Windows
- Adds console subsystem
- Links with `kernel32.lib` and `msvcrt.lib`

## Advanced Usage

### Custom Linker Flags

You can pass additional flags to the linker:

```bash
roc link main.o utils.o -o my_program --strip-all
```

### Multiple Object Files

Link many object files at once:

```bash
roc link file1.o file2.o file3.o file4.o -o large_program
```

### Different File Types

The linker can handle various object file formats depending on the target:

- **ELF** (Linux): `.o` files
- **Mach-O** (macOS): `.o` files  
- **COFF** (Windows): `.obj` files
- **WebAssembly**: `.wasm` object files

## Error Handling

### Common Errors

1. **LLVM Not Available**
   ```
   Error: Linking requires LLVM support. Please rebuild with -Dllvm=true
   ```
   Solution: Rebuild Roc with LLVM support enabled.

2. **Missing Output Flag**
   ```
   Error: no value was supplied for -o
   ```
   Solution: Always specify an output file with `-o`.

3. **Link Failures**
   ```
   Error: Failed to link object files
   ```
   Solution: Check that all object files are valid and compatible.

4. **Missing Object Files**
   ```
   Error: roc link received an unexpected argument: `no object files provided`
   ```
   Solution: Provide at least one object file to link.

### Debugging Tips

1. **Check Object File Compatibility**: Ensure all object files are compiled for the same target architecture.

2. **Verify Dependencies**: Make sure all required libraries are available on the system.

3. **Use Verbose Mode**: While not currently implemented, you can add debugging by examining the generated linker command.

## Implementation Details

The `roc link` command:

1. Automatically detects the target platform
2. Adds appropriate platform-specific flags
3. Converts arguments to null-terminated strings for the C API
4. Calls the appropriate LLD function (ELF, COFF, Mach-O, or WebAssembly)
5. Reports timing information and success/failure

### Target Format Detection

The linker automatically chooses the appropriate format:
- **macOS/iOS**: Mach-O format
- **Windows**: COFF format  
- **WebAssembly**: WASM format
- **Everything else**: ELF format

## Performance

The linking process is typically very fast:

```bash
Successfully linked 2 object files to my_program in 23.9 ms
```

For larger projects with many object files, LLD's performance characteristics make it suitable for:
- Incremental linking
- Large codebases
- Fast development cycles

## Integration with Build Systems

The `roc link` command can be integrated into various build systems:

### Makefile Example
```makefile
OBJECTS = main.o utils.o helper.o
EXECUTABLE = my_program

$(EXECUTABLE): $(OBJECTS)
	roc link $(OBJECTS) -o $(EXECUTABLE)

%.o: %.c
	clang -c $< -o $@

clean:
	rm -f $(OBJECTS) $(EXECUTABLE)
```

### Build Script Example
```bash
#!/bin/bash
set -e

echo "Compiling source files..."
clang -c main.c -o main.o
clang -c utils.c -o utils.o

echo "Linking executable..."
roc link main.o utils.o -o my_program

echo "Build complete!"
```

## Troubleshooting

### Common Issues

1. **Architecture Mismatches**: Ensure all object files are compiled for the same architecture.

2. **Missing System Libraries**: The linker automatically links system libraries, but custom libraries may need additional flags.

3. **Undefined Symbols**: Check that all function declarations have corresponding definitions in the linked object files.

4. **Permission Errors**: Ensure you have write permissions in the output directory.

### Getting Help

For additional help:

```bash
roc link --help
```

Or check the Roc documentation for more advanced linking scenarios.