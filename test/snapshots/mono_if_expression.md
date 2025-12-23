# META
~~~ini
description=Mono test: if expression at top-level
type=mono
~~~
# SOURCE
~~~roc
result = if True 1 else 2
~~~
# MONO
~~~roc
result : []
result = 1
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
LowerIdent,OpAssign,KwIf,UpperIdent,Int,KwElse,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "result"))
			(e-if-then-else
				(e-tag (raw "True"))
				(e-int (raw "1"))
				(e-int (raw "2"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "result"))
		(e-num (value "1"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "[]"))))
~~~
