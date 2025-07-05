# META
~~~ini
description=parens_in_type_def_apply malformed
type=expr
~~~
# SOURCE
~~~roc
U (b a):b
a
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpenRound(1:3-1:4),LowerIdent(1:4-1:5),LowerIdent(1:6-1:7),CloseRound(1:7-1:8),OpColon(1:8-1:9),LowerIdent(1:9-1:10),Newline(1:1-1:1),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "U"))
~~~
# FORMATTED
~~~roc
U
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "U"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[U]*"))
~~~
