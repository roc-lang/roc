# META
~~~ini
description=Tuple expression
type=expr
~~~
# SOURCE
~~~roc
(1, "hello", True)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Int(1:2-1:3),Comma(1:3-1:4),StringStart(1:5-1:6),StringPart(1:6-1:11),StringEnd(1:11-1:12),Comma(1:12-1:13),UpperIdent(1:14-1:18),CloseRound(1:18-1:19),EndOfFile(1:19-1:19),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.19
	(e-int @1.2-1.3 (raw "1"))
	(e-string @1.5-1.12
		(e-string-part @1.6-1.11 (raw "hello")))
	(e-tag @1.14-1.18 (raw "True")))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-1.19
	(elems
		(e-int @1.2-1.3 (value "1"))
		(e-string @1.5-1.12
			(e-literal @1.6-1.11 (string "hello")))
		(e-tag @1.14-1.18 (name "True"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.19 (type "(Num(size), Str, [True]a)"))
~~~
