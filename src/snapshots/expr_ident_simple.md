# META
~~~ini
description=Simple identifier lookup canonicalization
type=expr
~~~
# SOURCE
~~~roc
foo
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `foo` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:4),EndOfFile(1:4-1:4),
~~~
# PARSE
~~~clojure
(e-ident @1-1-1-4 (qaul "") (raw "foo"))
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
