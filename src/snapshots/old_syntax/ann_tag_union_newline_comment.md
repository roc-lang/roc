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
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `k` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),Newline(1:1-1:1),
OpenSquare(2:1-2:2),UpperIdent(2:2-2:3),Comma(2:3-2:4),Newline(1:1-1:1),
CloseSquare(3:1-3:2),LowerIdent(3:2-3:3),Newline(3:4-3:4),
UpperIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "k"))
~~~
# FORMATTED
~~~roc
k
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
