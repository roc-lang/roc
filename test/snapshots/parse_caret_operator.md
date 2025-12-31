# META
~~~ini
description=Caret operator (^) parsing
type=expr
~~~
# SOURCE
~~~roc
2 ^ 3 ^ 4
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int,OpCaret,Int,OpCaret,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-binop (op "^")
	(e-int (raw "2"))
	(e-binop (op "^")
		(e-int (raw "3"))
		(e-int (raw "4"))))
~~~
