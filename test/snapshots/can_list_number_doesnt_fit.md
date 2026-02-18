# META
~~~ini
description=List with number literal that doesn't fit in inferred type
type=expr
~~~
# SOURCE
~~~roc
[1.U8, 2.U8, 300]
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,Comma,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-typed-int (raw "1") (type ".U8"))
	(e-typed-int (raw "2") (type ".U8"))
	(e-int (raw "300")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-typed-int (value "1") (type "U8"))
		(e-typed-int (value "2") (type "U8"))
		(e-num (value "300"))))
~~~
# TYPES
~~~clojure
(expr (type "List(U8)"))
~~~
