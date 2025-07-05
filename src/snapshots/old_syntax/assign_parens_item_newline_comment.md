# META
~~~ini
description=assign_parens_item_newline_comment
type=expr
~~~
# SOURCE
~~~roc
a=(
i
#
)
r
~~~
# EXPECTED
UNDEFINED VARIABLE - assign_parens_item_newline_comment.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
Newline(3:2-3:2),
CloseRound(4:1-4:2),Newline(1:1-1:1),
LowerIdent(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "a"))
~~~
# FORMATTED
~~~roc
a
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
