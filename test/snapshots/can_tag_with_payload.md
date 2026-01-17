# META
~~~ini
description=Test tag with payload
type=expr
~~~
# SOURCE
~~~roc
Ok(42)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tag (raw "Ok"))
	(e-int (raw "42")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tag (name "Ok")
	(args
		(e-num (value "42"))))
~~~
# TYPES
~~~clojure
(expr (type "[Ok(a), .._others] where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
~~~
