# META
~~~ini
description=ability_first_demand_not_indented_enough fail
type=expr
~~~
# SOURCE
~~~roc
MEq implements
eq : a, a -> U64 where a implements MEq

1
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),KwImplements(1:5-1:15),Newline(1:1-1:1),
LowerIdent(2:1-2:3),OpColon(2:4-2:5),LowerIdent(2:6-2:7),Comma(2:7-2:8),LowerIdent(2:9-2:10),OpArrow(2:11-2:13),UpperIdent(2:14-2:17),KwWhere(2:18-2:23),LowerIdent(2:24-2:25),KwImplements(2:26-2:36),UpperIdent(2:37-2:40),Newline(1:1-1:1),
Newline(1:1-1:1),
Int(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.4 (raw "MEq"))
~~~
# FORMATTED
~~~roc
MEq
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.4 (name "MEq"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.4 (type "[MEq]*"))
~~~
