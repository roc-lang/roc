# META
~~~ini
description=ann_pattern_comment_before_body
type=expr
~~~
# SOURCE
~~~roc
H:p
(#
s)=p
d#
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),LowerIdent(1:3-1:4),Newline(1:1-1:1),
OpenRound(2:1-2:2),Newline(2:3-2:3),
LowerIdent(3:1-3:2),CloseRound(3:2-3:3),OpAssign(3:3-3:4),LowerIdent(3:4-3:5),Newline(1:1-1:1),
LowerIdent(4:1-4:2),Newline(4:3-4:3),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "H"))
~~~
# FORMATTED
~~~roc
H
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "H"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[H]*"))
~~~
