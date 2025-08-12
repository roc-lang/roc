# Compile

This directory holds the core logic for compiling Roc modules. It takes Roc source code and processes it through various stages to produce an executable or library.

Key responsibilities include:

- **Compilation Pipeline**: Managing the flow of a module through the compiler stages:
  - Tokenizing
  - Parsing
  - Canonicalizing
  - Type Checking
  - Code Generation (Not yet implemented)
- **Module Caching**: Managing the cache of compiled modules to speed up subsequent builds
- **Package Building**: Building complete packages
- **Build Environment**: Managing build contexts and environments

The compile module serves as the orchestrator that coordinates between the different compiler stages and manages the build process efficiently.
