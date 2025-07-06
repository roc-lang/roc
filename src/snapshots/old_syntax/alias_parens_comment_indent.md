# META
~~~ini
description=alias_parens_comment_indent
type=expr
~~~
# SOURCE
~~~roc
O:O(z
#
)
b#
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),NoSpaceOpenRound(1:4-1:5),LowerIdent(1:5-1:6),Newline(1:1-1:1),
Newline(2:2-2:2),
CloseRound(3:1-3:2),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:3-4:3),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "O"))
~~~
# FORMATTED
~~~roc
O
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "O"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[O]*"))
~~~
