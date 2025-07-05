# META
~~~ini
description=empty_record_assign_implements
type=expr
~~~
# SOURCE
~~~roc
{}=O{}implements
a
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),CloseCurly(1:2-1:3),OpAssign(1:3-1:4),UpperIdent(1:4-1:5),OpenCurly(1:5-1:6),CloseCurly(1:6-1:7),KwImplements(1:7-1:17),Newline(1:1-1:1),
LowerIdent(2:1-2:2),Newline(1:1-1:1),
MalformedUnknownToken(3:1-3:2),MalformedUnknownToken(3:2-3:3),MalformedUnknownToken(3:3-3:4),EndOfFile(3:4-3:4),
~~~
# PARSE
~~~clojure
(e-record @1.1-1.3)
~~~
# FORMATTED
~~~roc
{}
~~~
# CANONICALIZE
~~~clojure
(e-empty_record @1.1-1.3)
~~~
# TYPES
~~~clojure
(expr @1.1-1.3 (type "{}"))
~~~
