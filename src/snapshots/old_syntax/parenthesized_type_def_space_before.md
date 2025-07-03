# META
~~~ini
description=parenthesized_type_def_space_before
type=expr
~~~
# SOURCE
~~~roc
(
A):b
a
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
UpperIdent(2:1-2:2),CloseRound(2:2-2:3),OpColon(2:3-2:4),LowerIdent(2:4-2:5),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
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
(e-tag @2.1-2.2 (name "A") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @2.1-2.2 (type "[A]*"))
~~~
