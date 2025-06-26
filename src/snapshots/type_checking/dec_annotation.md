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
(file @1-1-4-12
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-type-anno @3-1-4-2 (name "x")
			(ty (name "Dec")))
		(s-decl @4-1-4-12
			(p-ident @4-1-4-2 (raw "x"))
			(e-frac @4-5-4-12 (raw "123.456")))))
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
		(e-frac-dec @4-5-4-12 (frac-var 76) (fits-in-f32 "true") (fits-in-dec "true") (value "123.456") (id 76))
		(annotation @4-1-4-2 (signature 78) (id 79)
			(declared-type
				(ty @3-5-3-8 (name "Dec"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "x") (type "Num(FloatingPoint(Decimal))")))
	(expressions
		(expr @4-5-4-12 (type "Num(FloatingPoint(Decimal))"))))
~~~