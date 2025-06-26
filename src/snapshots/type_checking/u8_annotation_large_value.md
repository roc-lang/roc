# META
~~~ini
description=U8 type annotation with value exceeding U8 range
type=file
~~~
# SOURCE
~~~roc
module []

x : U8
x = 500
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:7),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Int(4:5-4:8),EndOfFile(4:8-4:8),
~~~
# PARSE
~~~clojure
(file @1-1-4-8
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-type-anno @3-1-4-2 (name "x")
			(ty (name "U8")))
		(s-decl @4-1-4-8
			(p-ident @4-1-4-2 (raw "x"))
			(e-int @4-5-4-8 (raw "500")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 80)
		(p-assign @4-1-4-2 (ident "x") (id 73))
		(e-int @4-5-4-8 (num-var 76) (sign-needed "false") (bits-needed "9_to_15") (value "500") (id 76))
		(annotation @4-1-4-2 (signature 78) (id 79)
			(declared-type
				(ty @3-5-3-7 (name "U8"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "x") (type "Num(Int(Unsigned8))")))
	(expressions
		(expr @4-5-4-8 (type "Num(Int(Unsigned8))"))))
~~~