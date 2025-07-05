# META
~~~ini
description=tag_union_ann_with_as
type=expr
~~~
# SOURCE
~~~roc
1:[N(*as(S))]
_
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),OpenSquare(1:3-1:4),UpperIdent(1:4-1:5),NoSpaceOpenRound(1:5-1:6),OpStar(1:6-1:7),KwAs(1:7-1:9),NoSpaceOpenRound(1:9-1:10),UpperIdent(1:10-1:11),CloseRound(1:11-1:12),CloseRound(1:12-1:13),CloseSquare(1:13-1:14),Newline(1:1-1:1),
Underscore(2:1-2:2),Newline(1:1-1:1),
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
