# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
ff8.8.d
~~~
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header like 'module [main]' or 'app [main] { pf: platform "..." }'.

**UNEXPECTED TOKEN IN EXPRESSION**
This token is not expected in an expression.

**UNEXPECTED TOKEN IN EXPRESSION**
This token is not expected in an expression.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
LowerIdent(1:1-1:4),NoSpaceDotInt(1:4-1:6),NoSpaceDotLowerIdent(1:6-1:8),EndOfFile(1:8-1:8),
~~~
# PARSE
~~~clojure
(file (1:1-1:8)
	(malformed_header (1:1-1:4) "missing_header")
	(statements
		(malformed_expr (1:4-1:6) "expr_unexpected_token")
		(malformed_expr (1:6-1:8) "expr_unexpected_token")))
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