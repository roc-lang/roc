# META
~~~ini
description=annotation_tuple_newline
type=expr
~~~
# SOURCE
~~~roc
d:(J,
)g
2
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `d` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),UpperIdent(1:4-1:5),Comma(1:5-1:6),Newline(1:1-1:1),
CloseRound(2:1-2:2),LowerIdent(2:2-2:3),Newline(1:1-1:1),
Int(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "d"))
~~~
# FORMATTED
~~~roc
d
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
