# META
~~~ini
description=Function application expression
type=expr
~~~
# SOURCE
~~~roc
foo(42, "hello")
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-ident (raw "foo"))
	(e-int (raw "42"))
	(e-string
		(e-string-part (raw "hello"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-lookup-local
		(p-assign (ident "foo")))
	(e-num (value "42"))
	(e-string
		(e-literal (string "hello"))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
