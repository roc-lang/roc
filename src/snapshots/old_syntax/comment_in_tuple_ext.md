# META
~~~ini
description=comment_in_tuple_ext
type=expr
~~~
# SOURCE
~~~roc
t:()(n#
#
)
p#
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `t` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),CloseRound(1:4-1:5),NoSpaceOpenRound(1:5-1:6),LowerIdent(1:6-1:7),Newline(1:8-1:8),
Newline(2:2-2:2),
CloseRound(3:1-3:2),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:3-4:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "t"))
~~~
# FORMATTED
~~~roc
t
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
