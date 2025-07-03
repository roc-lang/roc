# META
~~~ini
description=parens_comment_in_ty_annotation malformed
type=expr
~~~
# SOURCE
~~~roc
Zx (e#
)f:i
s
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:3),OpenRound(1:4-1:5),LowerIdent(1:5-1:6),Newline(1:7-1:7),
CloseRound(2:1-2:2),LowerIdent(2:2-2:3),OpColon(2:3-2:4),LowerIdent(2:4-2:5),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.3 (raw "Zx"))
~~~
# FORMATTED
~~~roc
Zx
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.3 (name "Zx") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "[Zx]a"))
~~~
