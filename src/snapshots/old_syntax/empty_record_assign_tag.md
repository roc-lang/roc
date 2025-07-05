# META
~~~ini
description=empty_record_assign_tag
type=expr
~~~
# SOURCE
~~~roc
{}=
 P
O
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),CloseCurly(1:2-1:3),OpAssign(1:3-1:4),Newline(1:1-1:1),
UpperIdent(2:2-2:3),Newline(1:1-1:1),
UpperIdent(3:1-3:2),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
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
