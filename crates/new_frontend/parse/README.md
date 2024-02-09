# Parsing

The main job of the parsing step is to translate UTF-8 source code bytes into an in-memory tree of nodes that represent all the relevant semantic information about the module, as well as the original source code line and column numbers where they came from. (Line and column numbers are used later in things like reporting error messages and adding debug information to the final binary.)

This process breaks down into three steps: tokenization, parsing, and desugaring. These are all grouped together because they can be implemented either as distinct phases or, potentially more efficiently, by running each step incrementally on different chunks of the source code.

## Tokenization

This process breaks UTF-8 source code bytes into relevant "tokens" that will be easier to work with in future steps. Some tokens correspond to one UTF-8 byte, such as parentheses, while others correspond to multiple bytes, such as language keywords. Some correspond to a variable number of UTF-8 bytes, such as variable names, and others vary based on the context of the source code that came before them—such as tokens representing indents and outdents.

## Parsing

This is also where string _interning_ takes place, because we already have the string bytes in cache. We generally never need to get the interned strings back out again (because any time we would display those strings to a user, it's better to get them from the source code line/column information because we'll be showing surrounding source code context anyway, meaning those will be in cache—unlike the interned strings), so with one exception, we can discard all of those after parsing and rely on the interned keys from then on. The one exception is interned strings that were exposed for other modules to import. Those need to remain alive as long as other modules might still want to import this one.

## Desugaring

This step involves doing things like:

-   Translating operators into named function calls (nested appropriately based on operator precedence and associativity)
-   Translating destructures (e.g. `{ foo, bar: baz } = …` or `(@Email email) = ...`) into things like generated variables and field accesses
-   Translating string interpolation and `dbg` into nested calls (e.g. to `Inspect` in the case of `dbg`)
