# META
~~~ini
description=newline_after_equals
type=expr
~~~
# SOURCE
~~~roc
x =
    5

42
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),Newline(1:1-1:1),
Int(2:5-2:6),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(4:1-4:3),EndOfFile(4:3-4:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (qaul "") (raw "x"))
~~~
# FORMATTED
~~~roc
x
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope") (id 74))
~~~
# TYPES
~~~clojure
(expr (id 74) (type "Error"))
~~~
