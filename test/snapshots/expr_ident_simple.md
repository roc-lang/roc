# META
~~~ini
description=Simple identifier lookup canonicalization
type=expr
~~~
# SOURCE
~~~roc
foo
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-ident (raw "foo"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lookup-local
	(p-assign (ident "foo")))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
