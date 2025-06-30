# META
~~~ini
description=comment_after_dbg_in_empty_record_assignment
type=expr
~~~
# SOURCE
~~~roc
{}=dbg l#
n
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),CloseCurly(1:2-1:3),OpAssign(1:3-1:4),KwDbg(1:4-1:7),LowerIdent(1:8-1:9),Newline(1:10-1:10),
LowerIdent(2:1-2:2),EndOfFile(2:2-2:2),
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
(e-empty_record @1.1-1.3 (id 73))
~~~
# TYPES
~~~clojure
(expr (id 73) (type "{}"))
~~~
