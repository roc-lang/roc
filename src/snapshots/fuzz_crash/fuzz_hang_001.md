# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0 (
~~~
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header.

For example:
        module [main]
or for an app:
        app [main!] { pf: platform "../basic-cli/platform.roc" }
Here is the problematic code:
**fuzz_hang_001.md:1:1:1:4:**
```roc
0 (
```


**PARSE ERROR**
A parsing error occurred: `expected_expr_close_round_or_comma`
This is an unexpected parsing error. Please check your syntax.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
Int(1:1-1:2),OpenRound(1:3-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(file (1:1-1:4)
	(malformed_header (1:1-1:4) "missing_header")
	(statements (malformed_expr (1:4-1:4) "expected_expr_close_round_or_comma")))
~~~
# FORMATTED
~~~roc

~~~
# CANONICALIZE
~~~clojure
(can_ir "empty")
~~~
# TYPES
~~~clojure
(inferred_types (defs) (expressions))
~~~