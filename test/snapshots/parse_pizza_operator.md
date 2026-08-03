# META
~~~ini
description=Pipe operator (|>) parsing
type=expr
~~~
# SOURCE
~~~roc
1 |> add(2) |> mul(3)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,OpPizza,LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpPizza,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-arrow-call
	(e-arrow-call
		(e-int (raw "1"))
		(e-apply
			(e-ident (raw "add"))
			(e-int (raw "2"))))
	(e-apply
		(e-ident (raw "mul"))
		(e-int (raw "3"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-call
		(e-runtime-error (tag "ident_not_in_scope"))
		(e-num (value "1"))
		(e-num (value "2")))
	(e-num (value "3")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
