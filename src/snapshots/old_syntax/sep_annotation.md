# META
~~~ini
description=sep_annotation
type=expr
~~~
# SOURCE
~~~roc
E:i

E
=h
0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:2),OpColon(1:2-1:3),LowerIdent(1:3-1:4),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:2),Newline(1:1-1:1),
OpAssign(4:1-4:2),LowerIdent(4:2-4:3),Newline(1:1-1:1),
Int(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.2 (raw "E"))
~~~
# FORMATTED
~~~roc
E
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.2 (name "E") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.2 (type "[E]*"))
~~~
