# META
~~~ini
description=extra_newline_in_parens
type=expr
~~~
# SOURCE
~~~roc
B:{}

(
a)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),OpenCurly(1:3-1:4),CloseCurly(1:4-1:5),Newline(1:1-1:1),
Newline(1:1-1:1),
OpenRound(3:1-3:2),Newline(1:1-1:1),
LowerIdent(4:1-4:2),CloseRound(4:2-4:3),EndOfFile(4:3-4:3),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "B"))
~~~
# FORMATTED
~~~roc
B
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "B") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[B]*"))
~~~
