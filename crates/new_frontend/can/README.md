# Canonicalization

Once the canonical IR has been constructed, it should have these properties:

-   All LookupNonFn nodes occur after a corresponding DeclareNonFn node with the same name.
    -   In other words, lookups should never fail from here on. (If there was a failed lookup, it should have been reported as an error and then replaced with an Erroneous node.)
-   All LookupFn and TailCall nodes should occur after a node declaring a function with the same name.
    -   There are multiple nodes that can declare a function, depending on whether it's mutually recursive etc.
-   All type aliases and opaque types should be categorized as either self-recursive, mutually recursive (in a group), or non-recursive.
-   Non-recursive type aliases and opaque types should all be ordered such that they only refer to type names that were already in scope prior to their declaration.
-   Self-recursive type aliases and opaque types are allowed, but only behind a tag union, List, Set, or Dict (after expanding out type aliases but _not_ opaque types).
    -   Opaque types may be self-recursive but type aliases may not. Self-recursive type aliases
-
