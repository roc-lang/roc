# META
~~~ini
description=middle_when_branch_comment_after_parens
type=expr
~~~
# SOURCE
~~~roc
when n is
O->(s
)#
O->t
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `when` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),LowerIdent(1:6-1:7),LowerIdent(1:8-1:10),Newline(1:1-1:1),
UpperIdent(2:1-2:2),OpArrow(2:2-2:4),NoSpaceOpenRound(2:4-2:5),LowerIdent(2:5-2:6),Newline(1:1-1:1),
CloseRound(3:1-3:2),Newline(3:3-3:3),
UpperIdent(4:1-4:2),OpArrow(4:2-4:4),LowerIdent(4:4-4:5),EndOfFile(4:5-4:5),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (qaul "") (raw "when"))
~~~
# FORMATTED
~~~roc
when
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
