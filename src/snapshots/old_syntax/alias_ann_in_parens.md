# META
~~~ini
description=alias_ann_in_parens
type=expr
~~~
# SOURCE
~~~roc
M:(
r)
h
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Newline(1:1-1:1),
LowerIdent(2:1-2:2),CloseRound(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "M"))
~~~
# FORMATTED
~~~roc
M
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "M") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[M]*"))
~~~
