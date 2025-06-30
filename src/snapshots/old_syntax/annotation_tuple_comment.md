# META
~~~ini
description=annotation_tuple_comment
type=expr
~~~
# SOURCE
~~~roc
3:(#
)n->n 
0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Newline(1:5-1:5),
CloseRound(2:1-2:2),LowerIdent(2:2-2:3),OpArrow(2:3-2:5),LowerIdent(2:5-2:6),Newline(1:1-1:1),
Int(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-int @1.1-1.2 (raw "3"))
~~~
# FORMATTED
~~~roc
3
~~~
# CANONICALIZE
~~~clojure
(e-int @1.1-1.2 (value "3") (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "Num(*)"))
~~~
