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
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
UpperIdent(2:1-2:2),CloseRound(2:2-2:3),OpColon(2:3-2:4),LowerIdent(2:4-2:5),Newline(1:1-1:1),
LowerIdent(3:1-3:2),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
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
