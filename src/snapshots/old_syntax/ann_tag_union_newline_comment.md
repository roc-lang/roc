# META
~~~ini
description=ann_tag_union_newline_comment
type=expr
~~~
# SOURCE
~~~roc
k:
[T,
]m#
D
~~~
# EXPECTED
UNDEFINED VARIABLE - ann_tag_union_newline_comment.md:1:1:1:2
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),Newline(1:1-1:1),
OpenSquare(2:1-2:2),UpperIdent(2:2-2:3),Comma(2:3-2:4),Newline(1:1-1:1),
CloseSquare(3:1-3:2),LowerIdent(3:2-3:3),Newline(3:4-3:4),
UpperIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "k"))
~~~
# FORMATTED
~~~roc
k
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
