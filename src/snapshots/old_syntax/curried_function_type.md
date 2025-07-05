# META
~~~ini
description=curried_function_type
type=expr
~~~
# SOURCE
~~~roc
1:(w->w->p)
h
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),OpArrow(1:5-1:7),LowerIdent(1:7-1:8),OpArrow(1:8-1:10),LowerIdent(1:10-1:11),CloseRound(1:11-1:12),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
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
