# META
~~~ini
description=fuzz crash
type=file
~~~
# SOURCE
~~~roc
me = "luc"
foo = "hello ${namF
~~~
# PROBLEMS
**MISSING HEADER**
Roc files must start with a module header like 'module [main]' or 'app [main] { pf: platform "..." }'.

**UNEXPECTED TOKEN IN EXPRESSION**
This token is not expected in an expression.

**PARSE ERROR**
A parsing error occurred.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**UNKNOWN OPERATOR**
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.

# TOKENS
~~~zig
LowerIdent(1:1-1:3),OpAssign(1:4-1:5),StringStart(1:6-1:7),StringPart(1:7-1:10),StringEnd(1:10-1:11),Newline(1:1-1:1),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),StringStart(2:7-2:8),StringPart(2:8-2:14),OpenStringInterpolation(2:14-2:16),LowerIdent(2:16-2:20),EndOfFile(2:20-2:20),
~~~
# PARSE
~~~clojure
(file (1:1-2:20)
	(malformed_header (1:1-1:3) "missing_header")
	(statements
		(malformed_expr (1:4-1:5) "expr_unexpected_token")
		(string (1:6-1:11) (string_part (1:7-1:10) "luc"))
		(decl (2:1-2:20)
			(ident (2:1-2:4) "foo")
			(malformed_expr (2:7-2:20) "string_expected_close_interpolation"))))
~~~
# FORMATTED
~~~roc
"luc"
foo = 
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (2:1-2:4)
				(pid 14)
				(ident "foo")))
		(def_expr (e_runtime_error (2:7-2:20) "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "foo" 17 (type "Error")))
	(expressions
		(expr (2:7-2:20) 16 (type "Error"))))
~~~