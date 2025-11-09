# META
~~~ini
description=If expression with numeric comparison
type=expr
~~~
# SOURCE
~~~roc
if 5 > 3 1 else 2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwIf,Int,OpGreaterThan,Int,Int,KwElse,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-if-then-else
	(e-binop (op ">")
		(e-int (raw "5"))
		(e-int (raw "3")))
	(e-int (raw "1"))
	(e-int (raw "2")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-if
	(if-branches
		(if-branch
			(e-binop (op "gt")
				(e-num (value "5"))
				(e-num (value "3")))
			(e-num (value "1"))))
	(if-else
		(e-num (value "2"))))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
