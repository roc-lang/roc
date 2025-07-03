# META
~~~ini
description=type_ann_tag_union_parens_applies
type=expr
~~~
# SOURCE
~~~roc
1:[N(H(S
))]
_
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),OpenSquare(1:3-1:4),UpperIdent(1:4-1:5),NoSpaceOpenRound(1:5-1:6),UpperIdent(1:6-1:7),NoSpaceOpenRound(1:7-1:8),UpperIdent(1:8-1:9),Newline(1:1-1:1),
CloseRound(2:1-2:2),CloseRound(2:2-2:3),CloseSquare(2:3-2:4),Newline(1:1-1:1),
Underscore(3:1-3:2),EndOfFile(3:2-3:2),
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
(expr @1.1-1.2 (type "Num(a)"))
~~~
