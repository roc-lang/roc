# META
~~~ini
description=Dot access expression
type=expr
~~~
# SOURCE
~~~roc
list.map(fn)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-field-access
	(e-ident (raw "list"))
	(e-apply
		(e-ident (raw "map"))
		(e-ident (raw "fn"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-dot-access (field "map")
	(receiver
		(e-lookup-local
			(p-assign (ident "list"))))
	(args
		(e-lookup-local
			(p-assign (ident "fn")))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
