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

1. Build off of preprocessed platform
1. Append text and data of application, dealing with app relocations
1. Surgically update all call locations in the platform
1. Surgically update call information in the application (also dealing with other relocations for builtins)

## TODO (In a lightly prioritized order)

- Add Macho support
  - Honestly should be almost exactly the same code.
    This means we likely need to do a lot of refactoring to minimize the duplicate code.
    The fun of almost but not quite the same.
- Add PE support
  - As a prereq, we need roc building on Windows (I'm not sure it does currently).
  - Definitely a solid bit different than elf, but hopefully after refactoring for Macho, won't be that crazy to add.
- Look at enabling completely in memory linking that could be used with `roc run` and/or `roc repl`
- Look more into rust hosts and keeping certain functions. Currently I just disabled linker garbage collection.
  This works but adds 1.2MB (40%) to even a tiny app. It may be a size issue for large rust hosts.
  Roc, for reference, adds 13MB (20%) when linked without garbage collection.
- Add a feature to the compiler to make this linker optional.
