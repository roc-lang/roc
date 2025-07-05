# META
~~~ini
description=Tag with payload
type=expr
~~~
# SOURCE
~~~roc
Some(42)
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:5),NoSpaceOpenRound(1:5-1:6),Int(1:6-1:8),CloseRound(1:8-1:9),Newline(1:1-1:1),
MalformedUnknownToken(2:1-2:2),MalformedUnknownToken(2:2-2:3),MalformedUnknownToken(2:3-2:4),EndOfFile(2:4-2:4),
~~~
# PARSE
~~~clojure
(e-apply @1.1-1.9
	(e-tag @1.1-1.5 (raw "Some"))
	(e-int @1.6-1.8 (raw "42")))
~~~
# FORMATTED
~~~roc
Some(42)
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.9 (name "Some")
	(args
		(e-int @1.6-1.8 (value "42"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.9 (type "*"))
~~~
