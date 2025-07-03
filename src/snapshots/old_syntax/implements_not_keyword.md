# META
~~~ini
description=implements_not_keyword
type=expr
~~~
# SOURCE
~~~roc
A=B implements
 +s
1
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpAssign(1:2-1:3),UpperIdent(1:3-1:4),KwImplements(1:5-1:15),Newline(1:1-1:1),
OpPlus(2:2-2:3),LowerIdent(2:3-2:4),Newline(1:1-1:1),
Int(3:1-3:2),EndOfFile(3:2-3:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "A"))
~~~
# FORMATTED
~~~roc
A
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "A") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[A]a"))
~~~
