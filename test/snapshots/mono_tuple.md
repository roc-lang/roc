# META
~~~ini
description=Mono test: tuple
type=mono
~~~
# SOURCE
~~~roc
(1, 2)
~~~
# MONO
~~~roc
(1, 2) : (a, b) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]
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
OpenRound,Int,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-int (raw "1"))
	(e-int (raw "2")))
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-num (value "1"))
		(e-num (value "2"))))
~~~
# TYPES
~~~clojure
(expr (type "(a, b) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]"))
~~~
