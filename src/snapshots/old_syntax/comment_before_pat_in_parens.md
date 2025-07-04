# META
~~~ini
description=comment_before_pat_in_parens
type=expr
~~~
# SOURCE
~~~roc
(
#
6):s
h
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
Newline(2:2-2:2),
Int(3:1-3:2),CloseRound(3:2-3:3),OpColon(3:3-3:4),LowerIdent(3:4-3:5),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-3.3
	(e-int @3.1-3.2 (raw "6")))
~~~
# FORMATTED
~~~roc
(

	6,
)
~~~
# CANONICALIZE
~~~clojure
(e-int @3.1-3.2 (value "6"))
~~~
# TYPES
~~~clojure
(expr @3.1-3.2 (type "Num(*)"))
~~~
