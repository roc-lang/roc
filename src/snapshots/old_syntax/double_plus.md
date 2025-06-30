# META
~~~ini
description=double_plus fail
type=expr
~~~
# SOURCE
~~~roc
main =
    [] ++ []
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `main` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpAssign(1:6-1:7),Newline(1:1-1:1),
OpenSquare(2:5-2:6),CloseSquare(2:6-2:7),OpPlus(2:8-2:9),OpPlus(2:9-2:10),OpenSquare(2:11-2:12),CloseSquare(2:12-2:13),EndOfFile(2:13-2:13),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (qaul "") (raw "main"))
~~~
# FORMATTED
~~~roc
main
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
