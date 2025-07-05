# META
~~~ini
description=newline_after_paren
type=expr
~~~
# SOURCE
~~~roc
(
A)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
UpperIdent(2:1-2:2),CloseRound(2:2-2:3),EndOfFile(2:3-2:3),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-2.3
	(e-tag @2.1-2.2 (raw "A")))
~~~
# FORMATTED
~~~roc
(
	A,
)
~~~
# CANONICALIZE
~~~clojure
(e-tag @2.1-2.2 (name "A"))
~~~
# TYPES
~~~clojure
(expr @2.1-2.2 (type "[A]*"))
~~~
