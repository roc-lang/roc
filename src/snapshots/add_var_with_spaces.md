# META
~~~ini
description=Add a variable with spaces
type=file
~~~
# SOURCE
~~~roc
module [add2]

add2 = x +      2
~~~
# PROBLEMS
~~~txt
UNDEFINED VARIABLE
Nothing is named `x` in this scope.
Is there an import or exposing missing up-top?
~~~
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:5),OpAssign(3:6-3:7),LowerIdent(3:8-3:9),OpPlus(3:10-3:11),Int(3:17-3:18),EndOfFile(3:18-3:18),
~~~
# PARSE
~~~clojure
(file (1:1-3:18)
	(module (1:1-1:14)
		(exposes (1:8-1:14) (exposed_item (lower_ident "add2"))))
	(statements
		(decl (3:1-3:18)
			(ident (3:1-3:5) "add2")
			(binop (3:8-3:18)
				"+"
				(ident (3:8-3:9) "" "x")
				(int (3:17-3:18) "2")))))
~~~
# FORMATTED
~~~roc
module [add2]

add2 = x + 2
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (3:1-3:5)
				(pid 12)
				(ident "add2")))
		(def_expr
			(e_binop (3:8-3:18)
				"add"
				(e_runtime_error (3:8-3:9) "ident_not_in_scope")
				(e_int (3:17-3:18)
					(int_var 16)
					(precision_var 15)
					(literal "2")
					(value "TODO")
					(bound "u8"))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "add2" 19 (type "*")))
	(expressions
		(expr (3:8-3:18) 18 (type "*"))))
~~~