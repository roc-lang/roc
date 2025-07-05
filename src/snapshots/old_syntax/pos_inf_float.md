# META
~~~ini
description=pos_inf_float
type=expr
~~~
# SOURCE
~~~roc
inf
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `inf` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.4 (raw "inf"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "Error"))
~~~
