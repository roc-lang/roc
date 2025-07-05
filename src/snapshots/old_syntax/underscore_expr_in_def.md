# META
~~~ini
description=underscore_expr_in_def
type=expr
~~~
# SOURCE
~~~roc
J:R
n_p
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),UpperIdent(1:3-1:4),Newline(1:1-1:1),
LowerIdent(2:1-2:4),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "J"))
~~~
# FORMATTED
~~~roc
J
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "J"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[J]*"))
~~~
