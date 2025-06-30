# META
~~~ini
description=dbg_extra_parens
type=expr
~~~
# SOURCE
~~~roc
{
}=dbg d z
dd
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
CloseCurly(2:1-2:2),OpAssign(2:2-2:3),KwDbg(2:3-2:6),LowerIdent(2:7-2:8),LowerIdent(2:9-2:10),Newline(1:1-1:1),
LowerIdent(3:1-3:3),EndOfFile(3:3-3:3),
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
