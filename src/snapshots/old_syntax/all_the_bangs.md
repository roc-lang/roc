# META
~~~ini
description=all_the_bangs fail
type=expr
~~~
# SOURCE
~~~roc
p
!
.p!!
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `p` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),Newline(1:1-1:1),
OpBang(2:1-2:2),Newline(1:1-1:1),
DotLowerIdent(3:1-3:5),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "p"))
~~~
# FORMATTED
~~~roc
p
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
