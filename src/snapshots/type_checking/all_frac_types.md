# META
~~~ini
description=All fractional type annotations
type=file
~~~
# SOURCE
~~~roc
module []

a : F32
a = 3.14

b : F64
b = 2.71828

c : Dec
c = 123.456
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpColon(3:3-3:4),UpperIdent(3:5-3:8),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Float(4:5-4:9),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(6:1-6:2),OpColon(6:3-6:4),UpperIdent(6:5-6:8),Newline(1:1-1:1),
LowerIdent(7:1-7:2),OpAssign(7:3-7:4),Float(7:5-7:12),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(9:1-9:2),OpColon(9:3-9:4),UpperIdent(9:5-9:8),Newline(1:1-1:1),
LowerIdent(10:1-10:2),OpAssign(10:3-10:4),Float(10:5-10:12),EndOfFile(10:12-10:12),
~~~
# PARSE
~~~clojure
(file @1-1-10-12
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-type-anno @3-1-4-2 (name "a")
			(ty (name "F32")))
		(s-decl @4-1-4-9
			(p-ident @4-1-4-2 (raw "a"))
			(e-frac @4-5-4-9 (raw "3.14")))
		(s-type-anno @6-1-7-2 (name "b")
			(ty (name "F64")))
		(s-decl @7-1-7-12
			(p-ident @7-1-7-2 (raw "b"))
			(e-frac @7-5-7-12 (raw "2.71828")))
		(s-type-anno @9-1-10-2 (name "c")
			(ty (name "Dec")))
		(s-decl @10-1-10-12
			(p-ident @10-1-10-2 (raw "c"))
			(e-frac @10-5-10-12 (raw "123.456")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 80)
		(p-assign @4-1-4-2 (ident "a") (id 73))
		(e-dec-small @4-5-4-9 (num-var 76) (fits-in-f32 "true") (fits-in-dec "true") (numerator "314") (denominator-power-of-ten "2") (value "3.14") (id 76))
		(annotation @4-1-4-2 (signature 78) (id 79)
			(declared-type
				(ty @3-5-3-8 (name "F32")))))
	(d-let (id 89)
		(p-assign @7-1-7-2 (ident "b") (id 82))
		(e-frac-dec @7-5-7-12 (frac-var 85) (fits-in-f32 "true") (fits-in-dec "true") (value "2.71828") (id 85))
		(annotation @7-1-7-2 (signature 87) (id 88)
			(declared-type
				(ty @6-5-6-8 (name "F64")))))
	(d-let (id 98)
		(p-assign @10-1-10-2 (ident "c") (id 91))
		(e-frac-dec @10-5-10-12 (frac-var 94) (fits-in-f32 "true") (fits-in-dec "true") (value "123.456") (id 94))
		(annotation @10-1-10-2 (signature 96) (id 97)
			(declared-type
				(ty @9-5-9-8 (name "Dec"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(def (name "a") (type "Num(Fraction(Binary32))"))
		(def (name "b") (type "Num(Fraction(Binary64))"))
		(def (name "c") (type "Num(Fraction(Decimal))")))
	(expressions
		(expr @4-5-4-9 (type "Num(Fraction(Binary32))"))
		(expr @7-5-7-12 (type "Num(Fraction(Binary64))"))
		(expr @10-5-10-12 (type "Num(Fraction(Decimal))"))))
~~~