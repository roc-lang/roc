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
Roc files must start with a module header like 'module [main]' or 'app [main] { pf: platform "..." }'.

**PARSE ERROR**
A parsing error occurred.

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
	(malformed_header (1:1-1:2) "missing_header")
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