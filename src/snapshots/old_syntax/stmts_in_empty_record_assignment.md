# META
~~~ini
description=stmts_in_empty_record_assignment
type=expr
~~~
# SOURCE
~~~roc
{}=
 p
 t
J
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),CloseCurly(1:2-1:3),OpAssign(1:3-1:4),Newline(1:1-1:1),
LowerIdent(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:2-3:3),Newline(1:1-1:1),
UpperIdent(4:1-4:2),Newline(1:1-1:1),
MalformedUnknownToken(5:1-5:2),MalformedUnknownToken(5:2-5:3),MalformedUnknownToken(5:3-5:4),EndOfFile(5:4-5:4),
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
