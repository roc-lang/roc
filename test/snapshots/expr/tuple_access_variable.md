# META
~~~ini
description=Tuple access on a variable
type=expr
~~~
# SOURCE
~~~roc
t.0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple-access
	(e-ident (raw "t"))
	".0")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple-access (index "0")
	(e-lookup-local
		(p-assign (ident "t"))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
