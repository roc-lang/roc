# META
~~~ini
description=Dec type annotation
type=file
~~~
# SOURCE
~~~roc
module []

x : Dec
x = 123.456
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:8),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Float(4:5-4:12),EndOfFile(4:12-4:12),
~~~
# PARSE
~~~clojure
(file @1.1-4.12
	(module @1.1-1.10
		(exposes @1.8-1.10))
	(statements
		(s-type-anno @1.1-1.1 (name "x")
			(ty @3.5-3.8 (name "Dec")))
		(s-decl @4.1-4.12
			(p-ident @4.1-4.2 (raw "x"))
			(e-frac @4.5-4.12 (raw "123.456")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.2 (ident "x"))
		(e-frac-dec @4.5-4.12 (value "123.456"))
		(annotation @4.1-4.2
			(declared-type
				(ty-type @3.5-3.8 (name "Dec"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.12 (type "Dec")))
	(expressions
		(expr (type "Dec"))))
~~~
