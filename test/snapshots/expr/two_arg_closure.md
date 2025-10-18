# META
~~~ini
description=two_arg_closure
type=expr
~~~
# SOURCE
~~~roc
|_, _| 42
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,Underscore,Comma,Underscore,OpBar,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-underscore)
		(p-underscore))
	(e-int (raw "42")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-underscore)
		(p-underscore))
	(e-num (value "42")))
~~~
# TYPES
~~~clojure
(expr (type "_arg, _arg2 -> Num(_size)"))
~~~
