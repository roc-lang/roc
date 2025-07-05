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
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),OpenCurly(1:3-1:4),CloseCurly(1:4-1:5),Newline(1:1-1:1),
Newline(1:1-1:1),
OpenRound(3:1-3:2),Newline(1:1-1:1),
LowerIdent(4:1-4:2),CloseRound(4:2-4:3),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
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
(e-tag @1.1-1.2 (name "B"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[B]*"))
~~~
