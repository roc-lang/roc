# META
~~~ini
description=Mono test: list with elements
type=mono
~~~
# SOURCE
~~~roc
[1, 2, 3]
~~~
# MONO
~~~roc
[1, 2, 3] : List(a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-int (raw "1"))
	(e-int (raw "2"))
	(e-int (raw "3")))
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-num (value "1"))
		(e-num (value "2"))
		(e-num (value "3"))))
~~~
# TYPES
~~~clojure
(expr (type "List(a) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
