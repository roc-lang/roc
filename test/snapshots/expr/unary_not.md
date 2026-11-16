# META
~~~ini
description=unary_not
type=expr
~~~
# SOURCE
~~~roc
!blah
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBang,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(unary "!"
	(e-ident (raw "blah")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-unary-not
	(e-runtime-error (tag "ident_not_in_scope")))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
