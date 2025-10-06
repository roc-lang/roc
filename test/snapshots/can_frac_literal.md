# META
~~~ini
description=Float literal type inference
type=file:CanFracLiteral.roc
~~~
# SOURCE
~~~roc
CanFracLiteral := {}

x = 3.14
y = 1.23e45
z = 0.5
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:15),OpColonEqual(1:16-1:18),OpenCurly(1:19-1:20),CloseCurly(1:20-1:21),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Float(3:5-3:9),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Float(4:5-4:12),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),Float(5:5-5:8),
EndOfFile(6:1-6:1),
~~~
# PARSE
~~~clojure
(file @1.1-5.8
	(type-module @1.1-1.15)
	(statements
		(s-type-decl @1.1-1.21
			(header @1.1-1.15 (name "CanFracLiteral")
				(args))
			(ty-record @1.19-1.21))
		(s-decl @3.1-3.9
			(p-ident @3.1-3.2 (raw "x"))
			(e-frac @3.5-3.9 (raw "3.14")))
		(s-decl @4.1-4.12
			(p-ident @4.1-4.2 (raw "y"))
			(e-frac @4.5-4.12 (raw "1.23e45")))
		(s-decl @5.1-5.8
			(p-ident @5.1-5.2 (raw "z"))
			(e-frac @5.5-5.8 (raw "0.5")))))
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
		(e-dec-small @3.5-3.9 (numerator "314") (denominator-power-of-ten "2") (value "3.14")))
	(d-let
		(p-assign @4.1-4.2 (ident "y"))
		(e-frac-f64 @4.5-4.12 (value "1.23e45")))
	(d-let
		(p-assign @5.1-5.2 (ident "z"))
		(e-dec-small @5.5-5.8 (numerator "5") (denominator-power-of-ten "1") (value "0.5")))
	(s-nominal-decl @1.1-1.21
		(ty-header @1.1-1.15 (name "CanFracLiteral"))
		(ty-record @1.19-1.21)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @3.1-3.2 (type "Num(Frac(_size))"))
		(patt @4.1-4.2 (type "Num(Frac(_size))"))
		(patt @5.1-5.2 (type "Num(Frac(_size))")))
	(type_decls
		(nominal @1.1-1.21 (type "CanFracLiteral")
			(ty-header @1.1-1.15 (name "CanFracLiteral"))))
	(expressions
		(expr @3.5-3.9 (type "Num(Frac(_size))"))
		(expr @4.5-4.12 (type "Num(Frac(_size))"))
		(expr @5.5-5.8 (type "Num(Frac(_size))"))))
~~~
