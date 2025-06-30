# META
~~~ini
description=parens_newlines_before_as
type=expr
~~~
# SOURCE
~~~roc
1:(*#
)as J
l
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
Int(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),OpStar(1:4-1:5),Newline(1:6-1:6),
CloseRound(2:1-2:2),KwAs(2:2-2:4),UpperIdent(2:5-2:6),Newline(1:1-1:1),
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
(e-int @1.1-1.2 (value "1"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Num(*)"))
~~~
