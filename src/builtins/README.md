# Builtins

Built-in functions, types, and runtime support for the Roc language.

## Overview

The builtins module provides the core runtime functionality that every Roc program depends on. It includes fundamental operations, data types, and the interface between Roc code and the host platform.

## Purpose

This module provides:
- **Core Data Types**: Built-in types like strings, numbers, and basic collections
- **Runtime Operations**: Fundamental operations like memory allocation, string manipulation, and arithmetic
- **Host Platform Interface**: The bridge between Roc code and the underlying platform (ABI, system calls, etc.)
- **Standard Library**: Essential functions that are always available in Roc programs

The builtins module is essential for both the compiler (during type checking and code generation) and the runtime (during program execution).
