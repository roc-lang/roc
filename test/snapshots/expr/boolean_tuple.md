# META
~~~ini
description=Test that True and False resolve to Bool type in a tuple
type=expr
~~~
# SOURCE
~~~roc
(True, False)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-tuple
	(e-tag (raw "True"))
	(e-tag (raw "False")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple
	(elems
		(e-nominal (nominal "Bool")
			(e-tag (name "True")))
		(e-nominal (nominal "Bool")
			(e-tag (name "False")))))
~~~
# TYPES
~~~clojure
(expr (type "(Bool, Bool)"))
~~~
