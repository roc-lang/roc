# Roc's Compiler Architecture

## Front-End

The _front-end_ of the compiler is the steps that happen when you run `roc check`,
with the exception of error reporting and compile-time evaluation of constants.
(Errors are _gathered_ by the front-end, they just aren't _reported_ by it.)

### Modules

Modules are the fundamental unit of compilation in Roc. We process and cache things
at a module level, and when a module needs to refer to something from another module,
an orchestrator outside the front-end will take care of copying all the relevant
information from that module (names, inferred types, etc.) into this module's local
data before proceeding to work on this module.

This architecture allows for module-level caching, and incremental compilation where
we only need to rebuild a minimum of one module (and possibly modules that depend on it).
It also means that we can use 32-bit identifiers for everything without worrying about
scaling issues, since even huge modules won't have billions of AST nodes (or types),
and even huge packages and applications won't have billions of modules inside them.

If cyclic module dependencies were allowed in this architecture:
* 32-bit identifiers might be too small on huge code bases, because module cycles would need to be combined into "mega-modules" behind the scenes and compiled as one unit, all sharing one giant 32-bit type ID space
* It would be easy to accidentally organize large Roc programs in a way that made module-level caching impossible (as large cycles would all have to be cached as one unit), potentially leading to bad compile times despite Roc's caching capabilities.
* There is obviously a convenience downside to disallowing module cycles, but at the same time, cyclic module errors sometimes reveal unintended coupling between two modules that results in a positive code architecural change even when build times aren't considered.

### Front-End Compilation Steps

The compilation steps in the front-end are:

1. Hashing: Source bytes -> BLAKE3 hash
  * Use BLAKE3 to get a 256-bit hash of the raw source bytes (we use this later)
  * Use SIMD to validate that the source file is UTF-8 (emit errors for invalid UTF-8)
2. Lexing: Source bytes -> Flat stream of tokens (with associated source regions)
  * Intern strings
  * Compact numbers
  * Emit errors for invalid tokens (e.g. `0xZZ`)
3. Parsing: Tokens -> Tree (specifically AST)
  * Resolve operator precedence (using Pratt parsing)
  * Emit errors for invalid token sequences (e.g. `a + / ! b`)
4. Canonicalization: AST -> CIR
  * Recategorize AST nodes as expressions, statements, patterns, or types
  * Resolve lookups to the correct pattern (or else emit an error) using scoping rules
5. Inference: CIR -> CIR + Types
  * Populate a database of types, where each CIR has a corresponding db entry
  * Set initial types based on CIR nodes, as well as "symlinking" some types to others
  * Use type unification to resolve all types based on symlinks, instantiation, etc.
  * Run "occurs" checks to give errors for cyclic types instead of causing infinite loops

Once this process is complete, we have:
* BLAKE3 hash of the source bytes
* CIR tree representing the structure of the module
* Types database representing the types of each CIR node

### Caching Front-End Work on Disk

TODO: describe how caching works

## Interpreter

Once we've finished the front-end work, we can run an interpreter on the CIR + types,
either to run the entire program (e.g. in an unoptimized debug build) or just when
doing compile-time evaluation of constants within the Roc program itself, as part of
`roc check`.

TODO: describe how the interpreter works, especially with polymorphic function calls

### Running the interpreter in a host

TODO: describe how elaborate dance we do to get the shared memory in without polluting env vars, while dodging macOS security countermeasures.

## Optimizing Back-End

IMPORTANT NOTE: The following is the *plan* for the optimizing back-end, but it is not
yet implemented!

### Compilation Steps for Optimizing Back-End

The compilation steps in the optimizing back-end are: (or rather, are planned to be once we implement it)

1. Monomorphization: CIR -> MIR
  * Convert polymorphic function calls into calls to monomorphic specializations of functions
  * Insert reference counting instructions where appropriate
2. Lambda Set Inference: MIR -> MIR + Lambda Set Types
3. Lambda Set Monomorphization: MIR -> MIR + Monomorphic Lambda Set Types
4. LLVM IR Generation: MIR + Monomorphic Lambda Set Types -> LLVM IR
5. Machine Code Generation: LLVM IR -> Machine Code

At some point we plan to introduce our own optimization in here, on top of the ones LLVM
does. However, we're not yet sure where in these steps it will go, so I haven't
included it.
