# META
~~~ini
description=annotated_tag_destructure
type=expr
~~~
# SOURCE
~~~roc
UserId x : [ UserId I64 ]
UserId x = UserId 42

x
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent(1:1-1:7),LowerIdent(1:8-1:9),OpColon(1:10-1:11),OpenSquare(1:12-1:13),UpperIdent(1:14-1:20),UpperIdent(1:21-1:24),CloseSquare(1:25-1:26),Newline(1:1-1:1),
UpperIdent(2:1-2:7),LowerIdent(2:8-2:9),OpAssign(2:10-2:11),UpperIdent(2:12-2:18),Int(2:19-2:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-tag @1.1-1.7 (raw "UserId"))
~~~
# FORMATTED
~~~roc
UserId
~~~
# CANONICALIZE
~~~clojure
(e-tag @1.1-1.7 (name "UserId") (args "TODO"))
~~~
# TYPES
~~~clojure
(expr @1.1-1.7 (type "[UserId]*"))
~~~
