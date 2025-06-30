# META
~~~ini
description=empty_record_assign_return
type=expr
~~~
# SOURCE
~~~roc
{}=
return f
d
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),CloseCurly(1:2-1:3),OpAssign(1:3-1:4),Newline(1:1-1:1),
KwReturn(2:1-2:7),LowerIdent(2:8-2:9),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
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
