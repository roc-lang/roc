# META
~~~ini
description=when_in_assignment
type=expr
~~~
# SOURCE
~~~roc
x = when n is
     0 -> 0
42
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),LowerIdent(1:5-1:9),LowerIdent(1:10-1:11),LowerIdent(1:12-1:14),Newline(1:1-1:1),
Int(2:6-2:7),OpArrow(2:8-2:10),Int(2:11-2:12),Newline(1:1-1:1),
Int(3:1-3:3),EndOfFile(3:3-3:3),
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
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
