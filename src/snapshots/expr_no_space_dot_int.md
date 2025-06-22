# META
~~~ini
description=
type=file
~~~
# SOURCE
~~~roc
module []

foo = asd.0
~~~
# PROBLEMS
~~~txt
PARSER: expr_no_space_dot_int
UNKNOWN OPERATOR
This looks like an operator, but it's not one I recognize!
Check the spelling and make sure you're using a valid Roc operator.
~~~
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:4),OpAssign(3:5-3:6),LowerIdent(3:7-3:10),NoSpaceDotInt(3:10-3:12),EndOfFile(3:12-3:12),
~~~
# PARSE
~~~clojure
(file (1:1-3:12)
	(module (1:1-1:10) (exposes (1:8-1:10)))
	(statements
		(decl (3:1-3:12)
			(ident (3:1-3:4) "foo")
			(malformed_expr (3:10-3:12) "expr_no_space_dot_int"))))
~~~
# FORMATTED
~~~roc
module []

foo =
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (3:1-3:4)
				(pid 12)
				(ident "foo")))
		(def_expr (e_runtime_error (3:10-3:12) "expr_not_canonicalized"))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "foo" 15 (type "Error")))
	(expressions
		(expr (3:10-3:12) 14 (type "Error"))))
~~~