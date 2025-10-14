# META
~~~ini
description=All fractional type annotations
type=snippet
~~~
# SOURCE
~~~roc
a : F32
a = 3.14

b : F64
b = 2.71828

c : Dec
c = 123.456
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:3-1:4),UpperIdent(1:5-1:8),
LowerIdent(2:1-2:2),OpAssign(2:3-2:4),Float(2:5-2:9),
LowerIdent(4:1-4:2),OpColon(4:3-4:4),UpperIdent(4:5-4:8),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),Float(5:5-5:12),
LowerIdent(7:1-7:2),OpColon(7:3-7:4),UpperIdent(7:5-7:8),
LowerIdent(8:1-8:2),OpAssign(8:3-8:4),Float(8:5-8:12),
EndOfFile(9:1-9:1),
~~~
# PARSE
~~~clojure
(file @1.1-8.12
	(type-module @1.1-1.2)
	(statements
		(s-type-anno @1.1-1.8 (name "a")
			(ty @1.5-1.8 (name "F32")))
		(s-decl @2.1-2.9
			(p-ident @2.1-2.2 (raw "a"))
			(e-frac @2.5-2.9 (raw "3.14")))
		(s-type-anno @4.1-4.8 (name "b")
			(ty @4.5-4.8 (name "F64")))
		(s-decl @5.1-5.12
			(p-ident @5.1-5.2 (raw "b"))
			(e-frac @5.5-5.12 (raw "2.71828")))
		(s-type-anno @7.1-7.8 (name "c")
			(ty @7.5-7.8 (name "Dec")))
		(s-decl @8.1-8.12
			(p-ident @8.1-8.2 (raw "c"))
			(e-frac @8.5-8.12 (raw "123.456")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @2.1-2.2 (ident "a"))
		(e-dec-small @2.5-2.9 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
		(annotation @2.1-2.2
			(declared-type
				(ty-lookup @1.5-1.8 (name "F32") (builtin)))))
	(d-let
		(p-assign @5.1-5.2 (ident "b"))
		(e-frac-dec @5.5-5.12 (value "2.71828"))
		(annotation @5.1-5.2
			(declared-type
				(ty-lookup @4.5-4.8 (name "F64") (builtin)))))
	(d-let
		(p-assign @8.1-8.2 (ident "c"))
		(e-frac-dec @8.5-8.12 (value "123.456"))
		(annotation @8.1-8.2
			(declared-type
				(ty-lookup @7.5-7.8 (name "Dec") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @2.1-2.2 (type "Num(Frac(Float32))"))
		(patt @5.1-5.2 (type "Num(Frac(Float64))"))
		(patt @8.1-8.2 (type "Num(Frac(Decimal))")))
	(expressions
		(expr @2.5-2.9 (type "Num(Frac(Float32))"))
		(expr @5.5-5.12 (type "Num(Frac(Float64))"))
		(expr @8.5-8.12 (type "Num(Frac(Decimal))"))))
~~~
