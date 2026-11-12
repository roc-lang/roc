# Collections

The collections module provides efficient data structures that are tailored for use throughout the Roc compiler. These collections are designed to handle the specific needs of compiler operations like symbol tables, dependency graphs, and intermediate representations.

## Purpose

This module provides:
- **Specialized Data Structures**: Collections optimized for compiler-specific use cases
- **Memory Efficiency**: Data structures designed to minimize memory overhead during compilation
- **Performance**: Fast access patterns for common compiler operations
- **Type Safety**: Safe, generic collections that work with Roc's type system

`RekeyingHashMap` keeps lookup cost independent of deletion history for indexes
whose keys are repeatedly removed and reinserted as compiler identities evolve.


## Table storage lifecycles

The lifecycle-specific collections keep initialization and ownership rules at
the storage boundary:

- `WriteFirstSlice` checks write-before-read and complete handover in
  runtime-safety builds without adding release storage.
- `OptionalIndexColumn` stores optional typed indices in one word.
- `EpochSet` and `EpochColumn` retain scratch across resets; natural epochs
  must increase, while internally counted epochs clear their stamps on wrap.
- `ReservedList` and `ReservedColumns` separate logical contents from capacity,
  and distinguish owned storage from borrowed serialized views.
- `ScratchArena` retains peak live demand across passes, including nested
  marked scopes, rather than retaining cumulative allocation volume.
- CSR and sorted groups preserve insertion order within a group.

Fresh zero pages never justify reading retained capacity after rollback or
recycling. Every appended value is written explicitly. Density controls huge-page
advice, not the logical contents or publication of shared tables.


Optional-index encoding follows the owning ID domain. Shared zero-based IDs
use `OptionalIndexColumn`, which biases only the stored value. A private
domain that deliberately leaves physical row zero unused can use
`ZeroSentinelIndexColumn` without encoding arithmetic. The latter does not
authorize renumbering another stage's IDs, and its unused row must be excluded
from every semantic iteration.

`zeroed_pages.SparseSlice` owns small filled allocations or large fresh sparse
mappings. Atomic elements retain their existing publication protocol; zero
initialization never establishes ownership or synchronization.
