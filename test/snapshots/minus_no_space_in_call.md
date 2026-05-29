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
UNDEFINED VARIABLE - minus_no_space_in_call.md:1:1:1:4
UNDEFINED VARIABLE - minus_no_space_in_call.md:1:5:1:6
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
	(e-lookup-local
		(p-assign (ident "foo")))
	(e-binop (op "sub")
		(e-lookup-local
			(p-assign (ident "x")))
		(e-num (value "1"))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
