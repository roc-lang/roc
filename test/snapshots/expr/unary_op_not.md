# META
~~~ini
description=Unary not operation expression
type=expr
~~~
# SOURCE
~~~roc
!isValid
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
	(e-ident (raw "isValid")))
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
