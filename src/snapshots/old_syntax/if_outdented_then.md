# META
~~~ini
description=if_outdented_then fail
type=expr
~~~
# SOURCE
~~~roc
x =
    if 5 == 5
then 2 else 3

x
~~~
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),Newline(1:1-1:1),
KwIf(2:5-2:7),Int(2:8-2:9),OpEquals(2:10-2:12),Int(2:13-2:14),Newline(1:1-1:1),
LowerIdent(3:1-3:5),Int(3:6-3:7),KwElse(3:8-3:12),Int(3:13-3:14),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:1-5:2),EndOfFile(5:2-5:2),
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
