# META
~~~ini
description=comment_before_comma_in_tuple_type_with_func
type=expr
~~~
# SOURCE
~~~roc
1:(M,b#,
,h->g)e
h
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),UpperIdent(1:4-1:5),Comma(1:5-1:6),LowerIdent(1:6-1:7),Newline(1:8-1:9),
Comma(2:1-2:2),LowerIdent(2:2-2:3),OpArrow(2:3-2:5),LowerIdent(2:5-2:6),CloseRound(2:6-2:7),LowerIdent(2:7-2:8),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
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
(e-int @1.1-1.2 (value "1") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Num(*)"))
~~~
