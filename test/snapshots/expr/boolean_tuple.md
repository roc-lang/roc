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
OpenRound(1:1-1:2),UpperIdent(1:2-1:6),Comma(1:6-1:7),UpperIdent(1:8-1:13),CloseRound(1:13-1:14),EndOfFile(1:14-1:14),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.14
	(e-tag @1.2-1.6 (raw "True"))
	(e-tag @1.8-1.13 (raw "False")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-1.14
	(elems
		(e-nominal @1.2-1.6 (nominal "Bool")
			(e-tag @1.2-1.6 (name "True")))
		(e-nominal @1.8-1.13 (nominal "Bool")
			(e-tag @1.8-1.13 (name "False")))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.14 (type "(Bool, Bool)"))
~~~
