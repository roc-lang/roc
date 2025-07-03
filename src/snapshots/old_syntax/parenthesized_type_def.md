# META
~~~ini
description=parenthesized_type_def
type=expr
~~~
# SOURCE
~~~roc
(D):b
a
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),UpperIdent(1:2-1:3),CloseRound(1:3-1:4),OpColon(1:4-1:5),LowerIdent(1:5-1:6),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-1.4
	(e-tag @1.2-1.3 (raw "D")))
~~~
# FORMATTED
~~~roc
(D)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-1.4
	(elems
		(e-tag @1.2-1.3 (name "D") (args "TODO"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "([D]a)"))
~~~
