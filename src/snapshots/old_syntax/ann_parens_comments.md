# META
~~~ini
description=ann_parens_comments
type=expr
~~~
# SOURCE
~~~roc
r:(
r
#
#
)
h
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `r` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
Newline(3:2-3:2),
Newline(4:2-4:2),
CloseRound(5:1-5:2),Newline(1:1-1:1),
LowerIdent(6:1-6:2),Newline(1:1-1:1),
MalformedUnknownToken(7:1-7:2),MalformedUnknownToken(7:2-7:3),MalformedUnknownToken(7:3-7:4),EndOfFile(7:4-7:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "r"))
~~~
# FORMATTED
~~~roc
r
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
