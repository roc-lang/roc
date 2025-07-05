# META
~~~ini
description=backslash_closure_last_expr
type=expr
~~~
# SOURCE
~~~roc
b
\e->s
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `b` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),Newline(1:1-1:1),
OpBackslash(2:1-2:2),LowerIdent(2:2-2:3),OpArrow(2:3-2:5),LowerIdent(2:5-2:6),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "b"))
~~~
# FORMATTED
~~~roc
b
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
