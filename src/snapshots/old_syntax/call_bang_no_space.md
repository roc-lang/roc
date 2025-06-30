# META
~~~ini
description=call_bang_no_space
type=expr
~~~
# SOURCE
~~~roc
fxFn!arg
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `fxFn!arg` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:9),EndOfFile(1:9-1:9),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.9 (qaul "") (raw "fxFn!arg"))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
