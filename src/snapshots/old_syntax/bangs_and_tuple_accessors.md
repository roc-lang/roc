# META
~~~ini
description=bangs_and_tuple_accessors
type=expr
~~~
# SOURCE
~~~roc
J
!
.1!.0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),Newline(1:1-1:1),
OpBang(2:1-2:2),Newline(1:1-1:1),
DotInt(3:1-3:3),OpBang(3:3-3:4),NoSpaceDotInt(3:4-3:6),EndOfFile(3:6-3:6),
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
