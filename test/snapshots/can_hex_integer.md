# META
~~~ini
description=Hexadecimal integer literal type inference
type=snippet
~~~
# SOURCE
~~~roc
x = 0xFF
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),Int(1:5-1:9),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(file @1.1-1.9
	(type-module @1.1-1.2)
	(statements
		(s-decl @1.1-1.9
			(p-ident @1.1-1.2 (raw "x"))
			(e-int @1.5-1.9 (raw "0xFF")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @1.1-1.2 (ident "x"))
		(e-num @1.5-1.9 (value "255"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @1.1-1.2 (type "Num(Int(_size))")))
	(expressions
		(expr @1.5-1.9 (type "Num(Int(_size))"))))
~~~
