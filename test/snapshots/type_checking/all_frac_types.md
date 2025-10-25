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
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "a")
			(ty (name "F32")))
		(s-decl
			(p-ident (raw "a"))
			(e-frac (raw "3.14")))
		(s-type-anno (name "b")
			(ty (name "F64")))
		(s-decl
			(p-ident (raw "b"))
			(e-frac (raw "2.71828")))
		(s-type-anno (name "c")
			(ty (name "Dec")))
		(s-decl
			(p-ident (raw "c"))
			(e-frac (raw "123.456")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-dec-small (numerator "314") (denominator-power-of-ten "2") (value "3.14"))
		(annotation
			(ty-lookup (name "F32") (builtin))))
	(d-let
		(p-assign (ident "b"))
		(e-frac-dec (value "2.71828"))
		(annotation
			(ty-lookup (name "F64") (builtin))))
	(d-let
		(p-assign (ident "c"))
		(e-frac-dec (value "123.456"))
		(annotation
			(ty-lookup (name "Dec") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Frac(Float32))"))
		(patt (type "Num(Frac(Float64))"))
		(patt (type "Num(Frac(Decimal))")))
	(expressions
		(expr (type "Num(Frac(Float32))"))
		(expr (type "Num(Frac(Float64))"))
		(expr (type "Num(Frac(Decimal))"))))
~~~
