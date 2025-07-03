# META
~~~ini
description=alias_parens_comment
type=expr
~~~
# SOURCE
~~~roc
K:(#
s)
K
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Newline(1:5-1:5),
LowerIdent(2:1-2:2),CloseRound(2:2-2:3),Newline(1:1-1:1),
UpperIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "K"))
~~~
# FORMATTED
~~~roc
K
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "K") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[K]a"))
~~~
