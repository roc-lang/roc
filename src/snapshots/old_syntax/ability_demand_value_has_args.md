# META
~~~ini
description=ability_demand_value_has_args fail
type=expr
~~~
# SOURCE
~~~roc
MEq implements
    eq b c : a, a -> U64 where a implements MEq

1
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:4),KwImplements(1:5-1:15),Newline(1:1-1:1),
LowerIdent(2:5-2:7),LowerIdent(2:8-2:9),LowerIdent(2:10-2:11),OpColon(2:12-2:13),LowerIdent(2:14-2:15),Comma(2:15-2:16),LowerIdent(2:17-2:18),OpArrow(2:19-2:21),UpperIdent(2:22-2:25),KwWhere(2:26-2:31),LowerIdent(2:32-2:33),KwImplements(2:34-2:44),UpperIdent(2:45-2:48),Newline(1:1-1:1),
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
