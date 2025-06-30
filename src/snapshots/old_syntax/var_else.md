# META
~~~ini
description=var_else
type=expr
~~~
# SOURCE
~~~roc
elsewhere
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `elsewhere` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:10),EndOfFile(1:10-1:10),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.10 (qaul "") (raw "elsewhere"))
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
(expr @1.1-1.10 (type "Error"))
~~~
