# META
~~~ini
description=apply_three_args
type=expr
~~~
# SOURCE
~~~roc
a b c d
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `a` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),LowerIdent(1:3-1:4),LowerIdent(1:5-1:6),LowerIdent(1:7-1:8),EndOfFile(1:8-1:8),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "a"))
~~~
# FORMATTED
~~~roc
a
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
