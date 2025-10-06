# META
~~~ini
description=Dec type annotation
type=file:DecAnnotation.roc
~~~
# SOURCE
~~~roc
DecAnnotation := {}

x : Dec
x = 123.456
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:14),OpColonEqual(1:15-1:17),OpenCurly(1:18-1:19),CloseCurly(1:19-1:20),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:8),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Float(4:5-4:12),
EndOfFile(5:1-5:1),
~~~
# PARSE
~~~clojure
(file @1.1-4.12
	(type-module @1.1-1.14)
	(statements
		(s-type-decl @1.1-1.20
			(header @1.1-1.14 (name "DecAnnotation")
				(args))
			(ty-record @1.18-1.20))
		(s-type-anno @3.1-3.8 (name "x")
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
				(ty-lookup @3.5-3.8 (name "Dec") (builtin)))))
	(s-nominal-decl @1.1-1.20
		(ty-header @1.1-1.14 (name "DecAnnotation"))
		(ty-record @1.18-1.20)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.2 (type "Num(Frac(Decimal))")))
	(type_decls
		(nominal @1.1-1.20 (type "DecAnnotation")
			(ty-header @1.1-1.14 (name "DecAnnotation"))))
	(expressions
		(expr @4.5-4.12 (type "Num(Frac(Decimal))"))))
~~~
