# META
~~~ini
description=Dec type annotation
type=snippet
~~~
# SOURCE
~~~roc
x : Dec
x = 123.456
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),UpperIdent(1:5-1:8),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),Float(2:5-2:12),
EndOfFile(3:1-3:1),
~~~
# PARSE
~~~clojure
(file @1.1-2.12
	(type-module @1.1-1.2)
	(statements
		(s-type-anno @1.1-1.8 (name "x")
			(ty @1.5-1.8 (name "Dec")))
		(s-decl @2.1-2.12
			(p-ident @2.1-2.2 (raw "x"))
			(e-frac @2.5-2.12 (raw "123.456")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.2 (ident "x"))
		(e-frac-dec @2.5-2.12 (value "123.456"))
		(annotation @2.1-2.2
			(declared-type
				(ty-lookup @1.5-1.8 (name "Dec") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.2 (type "Num(Frac(Decimal))")))
	(expressions
		(expr @2.5-2.12 (type "Num(Frac(Decimal))"))))
~~~
