# META
~~~ini
description=When is old syntax use match instead (should error)
type=expr
~~~
# SOURCE
~~~roc
when x is
 1 -> 2
 3 -> 4
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,LowerIdent,LowerIdent,
Int,OpArrow,Int,
Int,OpArrow,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-ident (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-lookup-local
	(p-assign (ident "when")))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
