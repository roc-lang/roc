# META
~~~ini
description=newline_and_spaces_before_less_than
type=expr
~~~
# SOURCE
~~~roc
x = 1
    < 2

42
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `x` in this scope.
Is there an `import` or `exposing` missing up-top?

# TOKENS
~~~zig
LowerIdent(1:1-1:2),OpAssign(1:3-1:4),Int(1:5-1:6),Newline(1:1-1:1),
OpLessThan(2:5-2:6),Int(2:7-2:8),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(4:1-4:3),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
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
