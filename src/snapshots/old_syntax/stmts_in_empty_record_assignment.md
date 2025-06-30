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
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),CloseCurly(1:2-1:3),OpAssign(1:3-1:4),Newline(1:1-1:1),
LowerIdent(2:2-2:3),Newline(1:1-1:1),
LowerIdent(3:2-3:3),Newline(1:1-1:1),
UpperIdent(4:1-4:2),EndOfFile(4:2-4:2),
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
