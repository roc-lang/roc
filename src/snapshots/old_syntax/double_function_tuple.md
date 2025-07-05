# META
~~~ini
description=double_function_tuple
type=expr
~~~
# SOURCE
~~~roc
1:(w->p,
w->p)
h
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),OpArrow(1:5-1:7),LowerIdent(1:7-1:8),Comma(1:8-1:9),Newline(1:1-1:1),
LowerIdent(2:1-2:2),OpArrow(2:2-2:4),LowerIdent(2:4-2:5),CloseRound(2:5-2:6),Newline(1:1-1:1),
LowerIdent(3:1-3:2),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "1"))
~~~
# FORMATTED
~~~roc
1
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "1"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(*)"))
~~~
