# META
~~~ini
description=when_in_binop_in_assign_with_sneaky_newline
type=expr
~~~
# SOURCE
~~~roc
j=m%when f
is e->(i
)
h
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `j` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:2-1:3),LowerIdent(1:3-1:4),OpPercent(1:4-1:5),LowerIdent(1:5-1:9),LowerIdent(1:10-1:11),Newline(1:1-1:1),
LowerIdent(2:1-2:3),LowerIdent(2:4-2:5),OpArrow(2:5-2:7),NoSpaceOpenRound(2:7-2:8),LowerIdent(2:8-2:9),Newline(1:1-1:1),
CloseRound(3:1-3:2),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "j"))
~~~
# FORMATTED
~~~roc
j
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
