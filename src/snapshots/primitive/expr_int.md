# META
~~~ini
description=A primitive
type=file
~~~
# SOURCE
~~~roc
module [foo]
foo = 42
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),LowerIdent(1:9-1:12),CloseSquare(1:12-1:13),Newline(1:1-1:1),
LowerIdent(2:1-2:4),OpAssign(2:5-2:6),Int(2:7-2:9),EndOfFile(2:9-2:9),
~~~
# PARSE
~~~clojure
(file (1:1-2:9)
	(module (1:1-1:13)
		(exposes (1:8-1:13) (exposed_item (lower_ident "foo"))))
	(statements
		(decl (2:1-2:9)
			(ident (2:1-2:4) "foo")
			(int (2:7-2:9) "42"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (2:1-2:4)
				(pid 12)
				(ident "foo")))
		(def_expr
			(e_int (2:7-2:9)
				(int_var 15)
				(requirements (sign_needed "false") (bits_needed "types.types.Num.Int.BitsNeeded.7"))
				(value "42")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "foo" 16 (type "Num(Int(*))")))
	(expressions
		(expr (2:7-2:9) 15 (type "Num(Int(*))"))))
~~~