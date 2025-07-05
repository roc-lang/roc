# META
~~~ini
description=expect_single_line
type=expr
~~~
# SOURCE
~~~roc
x = 5

expect x == y

expect y == z

42
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),Int(1:5-1:6),Newline(1:1-1:1),
Newline(1:1-1:1),
KwExpect(3:1-3:7),LowerIdent(3:8-3:9),OpEquals(3:10-3:12),LowerIdent(3:13-3:14),Newline(1:1-1:1),
Newline(1:1-1:1),
KwExpect(5:1-5:7),LowerIdent(5:8-5:9),OpEquals(5:10-5:12),LowerIdent(5:13-5:14),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(7:1-7:3),EndOfFile(7:3-7:3),
~~~
# PARSE
~~~clojure
(e-ident @1.1-1.2 (raw "x"))
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
