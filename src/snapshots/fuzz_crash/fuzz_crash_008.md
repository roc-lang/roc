# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
||1
~~~
# PROBLEMS
**ASCII CONTROL CHARACTER**
ASCII control characters are not allowed in Roc source code.

**MISSING HEADER**
Roc files must start with a module header like 'module [main]' or 'app [main] { pf: platform "..." }'.

**PARSE ERROR**
A parsing error occurred.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
OpBar(1:1-1:2),OpBar(1:3-1:4),Int(1:4-1:5),EndOfFile(1:5-1:5),
~~~
# PARSE
~~~clojure
(file (1:1-1:5)
	(malformed_header (1:1-1:2) "missing_header")
	(statements (malformed_expr (1:5-1:5) "expected_expr_bar")))
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