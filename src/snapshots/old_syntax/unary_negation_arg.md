# META
~~~ini
description=unary_negation_arg
type=expr
~~~
# SOURCE
~~~roc
whee  12 -foo
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `whee` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),Int(1:7-1:9),OpUnaryMinus(1:10-1:11),LowerIdent(1:11-1:14),EndOfFile(1:14-1:14),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (qaul "") (raw "whee"))
~~~
# FORMATTED
~~~roc
whee
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
