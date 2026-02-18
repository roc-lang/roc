# META
~~~ini
description=Simple tuple element access
type=expr
~~~
# SOURCE
~~~roc
("a", "b").0
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,StringStart,StringPart,StringEnd,Comma,StringStart,StringPart,StringEnd,CloseRound,NoSpaceDotInt,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple-access
	(e-tuple
		(e-string
			(e-string-part (raw "a")))
		(e-string
			(e-string-part (raw "b"))))
	".0")
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple-access (index "0")
	(e-tuple
		(elems
			(e-string
				(e-literal (string "a")))
			(e-string
				(e-literal (string "b"))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
