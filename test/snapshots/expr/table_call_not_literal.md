# META
~~~ini
description=table(x) remains an ordinary call, not a table literal
type=expr
~~~
# SOURCE
~~~roc
table(x)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-ident (raw "table"))
	(e-ident (raw "x")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
