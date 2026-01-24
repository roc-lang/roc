# META
~~~ini
description=Test nested tuple expressions
type=expr
~~~
# SOURCE
~~~roc
((1, 2), (3, 4))
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,NoSpaceOpenRound,Int,Comma,Int,CloseRound,Comma,OpenRound,Int,Comma,Int,CloseRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-tuple
		(e-int (raw "1"))
		(e-int (raw "2")))
	(e-tuple
		(e-int (raw "3"))
		(e-int (raw "4"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-tuple
			(elems
				(e-num (value "1"))
				(e-num (value "2"))))
		(e-tuple
			(elems
				(e-num (value "3"))
				(e-num (value "4"))))))
~~~
# TYPES
~~~clojure
(expr (type "((a, b), (c, d)) where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
~~~
