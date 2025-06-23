# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
0b.0
0bu22
0u22
~~~
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header like 'module [main]' or 'app [main] { pf: platform "..." }'.

**UNEXPECTED TOKEN IN EXPRESSION**
This token is not expected in an expression.

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

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
MalformedNumberNoDigits(1:1-1:3),NoSpaceDotInt(1:3-1:5),Newline(1:1-1:1),
MalformedNumberNoDigits(2:1-2:6),Newline(1:1-1:1),
MalformedNumberBadSuffix(3:1-3:5),EndOfFile(3:5-3:5),
~~~
# PARSE
~~~clojure
(file (1:1-3:5)
	(malformed_header (1:1-1:3) "missing_header")
	(statements
		(malformed_expr (1:3-1:5) "expr_unexpected_token")
		(malformed_expr (2:1-2:6) "expr_unexpected_token")
		(malformed_expr (3:1-3:5) "expr_unexpected_token")))
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