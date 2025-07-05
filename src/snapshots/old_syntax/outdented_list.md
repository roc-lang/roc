# META
~~~ini
description=outdented_list
type=expr
~~~
# SOURCE
~~~roc
a = [
  1, 2, 3
]
a
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `a` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),OpenSquare(1:5-1:6),Newline(1:1-1:1),
Int(2:3-2:4),Comma(2:4-2:5),Int(2:6-2:7),Comma(2:7-2:8),Int(2:9-2:10),Newline(1:1-1:1),
CloseSquare(3:1-3:2),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "a"))
~~~
# FORMATTED
~~~roc
a
~~~
# CANONICALIZE
~~~clojure
(e-runtime-error (tag "ident_not_in_scope"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "Error"))
~~~
