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

`instantiate.NominalOpening` supports explicitly demanded schema roots with
one opening-owned substitution map. It is not a persisted type or an implicit
flex-row promise. `nominal_rows` holds Store-owned persistent schema, opening
and exact-fragment data so publication can retain undemanded payloads without
borrowing an instantiator. Logical template edges and owned solver references
are distinct. Semantic consumers must include latent edges before the solver
installs delayed descriptors; the storage API alone does not activate admission.
