# META
~~~ini
description=Mono test: arithmetic expression at top-level
type=mono
~~~
# SOURCE
~~~roc
sum = 1 + 2
~~~
# MONO
~~~roc
sum : Dec
sum = 3
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,OpPlus,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "sum"))
			(e-binop (op "+")
				(e-int (raw "1"))
				(e-int (raw "2"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "sum"))
		(e-num (value "3"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "Bool"))))
~~~
