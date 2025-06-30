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
(e-tuple @1.1-2.3 (id 75)
	(elems
		(e-tag @2.1-2.2 (ext-var 73) (name "A") (args "TODO"))))
~~~
# TYPES
~~~clojure
(expr (id 75) (type "([A]*)"))
~~~
