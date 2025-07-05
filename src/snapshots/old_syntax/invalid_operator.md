# META
~~~ini
description=invalid_operator fail
type=expr
~~~
# SOURCE
~~~roc
main =
    5 ** 3
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `main` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:5),OpAssign(1:6-1:7),Newline(1:1-1:1),
Int(2:5-2:6),OpStar(2:7-2:8),OpStar(2:8-2:9),Int(2:10-2:11),EndOfFile(2:11-2:11),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.5 (raw "main"))
~~~
# FORMATTED
~~~roc
main
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.5 (type "Error"))
~~~
