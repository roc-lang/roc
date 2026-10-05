# Types

Core type system implementation for the Roc language.

## Overview

The types module provides the foundational type system that underpins the entire Roc compiler. It defines the representation of all Roc types, including primitive types, algebraic data types, functions, and type variables used during type inference.

## Purpose

This module serves as the backbone for:
- **Type Representation**: Defining how all Roc types are stored and manipulated in memory
- **Type Operations**: Providing utilities for type comparison, substitution, and manipulation
- **Type Variables**: Managing type variables used during Hindley-Milner type inference
- **Built-in Types**: Implementing the core Roc type system (numbers, strings, lists, etc.)

The types module is used extensively by the canonicalize, check, and eval stages of the compiler to ensure type safety and provide the necessary type information for compilation and execution.
