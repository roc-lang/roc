# META
~~~ini
description=Hexadecimal integer literal type inference
type=file
~~~
# SOURCE
~~~roc
module []

x = 0xFF
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Int(3:5-3:9),EndOfFile(3:9-3:9),
~~~
# PARSE
~~~clojure
(file @1.1-3.9
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-decl @3.1-3.9
			(p-ident @3.1-3.2 (raw "x"))
			(e-int @3.5-3.9 (raw "0xFF")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @3.1-3.2 (ident "x"))
		(e-int @3.5-3.9 (value "255"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "Int(a)")))
	(expressions
		(expr @3.5-3.9 (type "Int(a)"))))
~~~
