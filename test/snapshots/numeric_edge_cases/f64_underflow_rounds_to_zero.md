# META
~~~ini
description=F64 literal below the smallest subnormal rounds to zero without a diagnostic
type=snippet
~~~
# SOURCE
~~~roc
value : F64
value = 1e-400
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
	(type-mod)
	(statements
		(s-type-anno (name "value")
			(ty (name "F64")))
		(s-decl
			(p-ident (raw "value"))
			(e-frac (raw "1e-400")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "value"))
		(e-num-from-numeral)
		(annotation
			(ty-lookup (name "F64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "F64")))
	(expressions
		(expr (type "F64"))))
~~~
