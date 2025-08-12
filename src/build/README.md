# Build

This directory contains helpers for building the Roc compiler and its associated tools.

The scripts and utilities here are responsible for:

- **Module Management**: Managing the compilation of Zig modules through `modules.zig`, which defines dependencies and module relationships
- **Tracy Profiler Integration**: Setting up integration with the [Tracy profiler](https://github.com/wolfpld/tracy) for performance analysis through `tracy.zig`
- **LLVM Integration**: C++ bindings for LLVM integration through `zig_llvm.cpp` and `zig_llvm.h`
- **Build System**: Coordinating the build process across all compiler modules and their dependencies
