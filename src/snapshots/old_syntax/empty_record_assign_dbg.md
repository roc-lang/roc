# META
~~~ini
description=empty_record_assign_dbg
type=expr
~~~
# SOURCE
~~~roc
{}=
 dbg c
 c
e
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),CloseCurly(1:2-1:3),OpAssign(1:3-1:4),Newline(1:1-1:1),
KwDbg(2:2-2:5),LowerIdent(2:6-2:7),Newline(1:1-1:1),
LowerIdent(3:2-3:3),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
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
