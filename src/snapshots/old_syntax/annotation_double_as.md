# META
~~~ini
description=annotation_double_as
type=expr
~~~
# SOURCE
~~~roc
s:(e as A)as A
s
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `s` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpColon(1:2-1:3),NoSpaceOpenRound(1:3-1:4),LowerIdent(1:4-1:5),KwAs(1:6-1:8),UpperIdent(1:9-1:10),CloseRound(1:10-1:11),KwAs(1:11-1:13),UpperIdent(1:14-1:15),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "s"))
~~~
# FORMATTED
~~~roc
s
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
