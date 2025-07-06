# META
~~~ini
description=opaque_in_ann_apply_arg malformed
type=expr
~~~
# SOURCE
~~~roc
B@A:w
#
@A=e
i
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpaqueName(1:2-1:4),OpColon(1:4-1:5),LowerIdent(1:5-1:6),Newline(1:1-1:1),
Newline(2:2-2:2),
OpaqueName(3:1-3:3),OpAssign(3:3-3:4),LowerIdent(3:4-3:5),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "B"))
~~~
# FORMATTED
~~~roc
B
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "B"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[B]*"))
~~~
