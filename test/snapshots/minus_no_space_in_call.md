# META
~~~ini
description=Minus without spaces in function call
type=expr
~~~
# SOURCE
~~~roc
foo(x-1)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,LowerIdent,OpBinaryMinus,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-ident (raw "foo"))
	(e-binop (op "-")
		(e-ident (raw "x"))
		(e-int (raw "1"))))
~~~
# FORMATTED
~~~roc
foo(x - 1)
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-runtime-error (tag "ident_not_in_scope"))
	(e-dispatch-call (method "minus") (constraint-fn-var 46)
		(receiver
			(e-runtime-error (tag "ident_not_in_scope")))
		(args
			(e-num (value "1")))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
