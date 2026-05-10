# META
~~~ini
description=Method call syntax with .inspect() should produce e_field_access with args
type=expr
~~~
# SOURCE
~~~roc
x.inspect()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-method-call (method ".inspect")
	(receiver
		(e-ident (raw "x")))
	(args))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-method-call (method "inspect")
	(receiver
		(e-runtime-error (tag "ident_not_in_scope")))
	(args))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
