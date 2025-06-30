# META
~~~ini
description=empty_record_eq_dbg
type=expr
~~~
# SOURCE
~~~roc
{
}
=dbg n
d
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
CloseCurly(2:1-2:2),Newline(1:1-1:1),
OpAssign(3:1-3:2),KwDbg(3:2-3:5),LowerIdent(3:6-3:7),Newline(1:1-1:1),
LowerIdent(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-record @1.1-2.2)
~~~
# FORMATTED
~~~roc
{}
~~~
# CANONICALIZE
~~~clojure
(e-empty_record @1.1-2.2 (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "{}"))
~~~
