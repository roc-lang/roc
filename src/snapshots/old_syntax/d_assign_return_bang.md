# META
~~~ini
description=d_assign_return_bang fail
type=expr
~~~
# SOURCE
~~~roc
D=return!-
 e
z#
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpAssign(1:2-1:3),LowerIdent(1:3-1:10),OpBinaryMinus(1:10-1:11),Newline(1:1-1:1),
LowerIdent(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:3-3:3),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "D"))
~~~
# FORMATTED
~~~roc
D
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "D"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[D]*"))
~~~
