# The Roc Surgical Linker

This linker has the goal of being extremely slim lined and fast.
It is focused on the scope of only linking platforms to Roc applications.
This restriction enables ignoring most of linking.

## General Overview

This linker is run in 2 phases: preprocessing and surigical linking.

### Platform Preprocessor

1. Dynamically link the platform to a dummy Roc application dynamic library
1. Create metadata related to Roc dynamically linked functions
   - Symbols that need to be redefined
   - Call locations that need to be modified for each symbol
   - Locations of special roc functions (roc_alloc, roc_dealloc, builtins, etc)
1. Modify the main executable to no longer be dynamically link
   - Delete dependency on dynamic library
   - Remove symbols from the dynamic table (maybe add them to the regular table?)
   - Delete GOT and PLT entries
   - Remove relocations from the dynamic table
   - Add extra header information about new text and data section at end of file

### Surgical Linker

1. Copy over preprocessed platform as base
1. Append text and data of application, noting offset
   - This could potentially have extra complication around section locations and updating the header
1. Surgically update all call locations in the platform
1. Surgically update call information in the application (also dealing with other relocations for builtins)
