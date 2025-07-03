# META
~~~ini
description=newline_before_and_after_implements_opaque
type=expr
~~~
# SOURCE
~~~roc
P:=W
  implements
   []
t
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColonEqual(1:2-1:4),UpperIdent(1:4-1:5),Newline(1:1-1:1),
KwImplements(2:3-2:13),Newline(1:1-1:1),
OpenSquare(3:4-3:5),CloseSquare(3:5-3:6),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "P"))
~~~
# FORMATTED
~~~roc
P
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "P") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[P]a"))
~~~
