# META
~~~ini
description=closure_with_binops_and_unary
type=expr
~~~
# SOURCE
~~~roc
m
 ^ -\w->m
 w
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `m` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),Newline(1:1-1:1),
OpCaret(2:2-2:3),OpUnaryMinus(2:4-2:5),OpBackslash(2:5-2:6),LowerIdent(2:6-2:7),OpArrow(2:7-2:9),LowerIdent(2:9-2:10),Newline(1:1-1:1),
LowerIdent(3:2-3:3),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "m"))
~~~
# FORMATTED
~~~roc
m
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
