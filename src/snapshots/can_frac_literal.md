# META
~~~ini
description=Float literal type inference
type=file
~~~
# SOURCE
~~~roc
module []

x = 3.14
y = 1.23e45
z = 0.5
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),CloseSquare(1:9-1:10),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(3:1-3:2),OpAssign(3:3-3:4),Float(3:5-3:9),Newline(1:1-1:1),
LowerIdent(4:1-4:2),OpAssign(4:3-4:4),Float(4:5-4:12),Newline(1:1-1:1),
LowerIdent(5:1-5:2),OpAssign(5:3-5:4),Float(5:5-5:8),EndOfFile(5:8-5:8),
~~~
# PARSE
~~~clojure
(file @1-1-5-8
	(module @1-1-1-10
		(exposes @1-8-1-10))
	(statements
		(s-decl @3-1-3-9
			(p-ident @3-1-3-2 (raw "x"))
			(e-frac @3-5-3-9 (raw "3.14")))
		(s-decl @4-1-4-12
			(p-ident @4-1-4-2 (raw "y"))
			(e-frac @4-5-4-12 (raw "1.23e45")))
		(s-decl @5-1-5-8
			(p-ident @5-1-5-2 (raw "z"))
			(e-frac @5-5-5-8 (raw "0.5")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let (id 75)
		(p-assign @3-1-3-2 (ident "x") (id 72))
		(e-dec-small @3-5-3-9 (numerator "314") (denominator-power-of-ten "2") (value "3.14") (id 74)))
	(d-let (id 79)
		(p-assign @4-1-4-2 (ident "y") (id 76))
		(e-frac-f64 @4-5-4-12 (value "1.23e45") (id 78)))
	(d-let (id 83)
		(p-assign @5-1-5-2 (ident "z") (id 80))
		(e-dec-small @5-5-5-8 (numerator "5") (denominator-power-of-ten "1") (value "0.5") (id 82))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d_assign (name "x") (def_var 75) (type "Frac(*)"))
		(d_assign (name "y") (def_var 79) (type "Frac(*)"))
		(d_assign (name "z") (def_var 83) (type "Frac(*)")))
	(expressions
		(expr @3-5-3-9 (type "Frac(*)"))
		(expr @4-5-4-12 (type "Frac(*)"))
		(expr @5-5-5-8 (type "Frac(*)"))))
~~~
