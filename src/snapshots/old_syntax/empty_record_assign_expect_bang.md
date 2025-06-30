# META
~~~ini
description=empty_record_assign_expect_bang fail
type=expr
~~~
# SOURCE
~~~roc
{}=expect!w
0
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),CloseCurly(1:2-1:3),OpAssign(1:3-1:4),LowerIdent(1:4-1:12),Newline(1:1-1:1),
Int(2:1-2:2),EndOfFile(2:2-2:2),
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
