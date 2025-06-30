# META
~~~ini
description=empty_record_assignment_d_when_bang fail
type=expr
~~~
# SOURCE
~~~roc
{
}=d when! 
s
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
CloseCurly(2:1-2:2),OpAssign(2:2-2:3),LowerIdent(2:3-2:4),LowerIdent(2:5-2:10),Newline(1:1-1:1),
LowerIdent(3:1-3:2),EndOfFile(3:2-3:2),
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
