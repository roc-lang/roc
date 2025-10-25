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
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Float,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "x")
			(ty (name "Dec")))
		(s-decl
			(p-ident (raw "x"))
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
		(p-assign (ident "x"))
		(e-frac-dec (value "123.456"))
		(annotation
			(ty-lookup (name "Dec") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Frac(Decimal))")))
	(expressions
		(expr (type "Num(Frac(Decimal))"))))
~~~
