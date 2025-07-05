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
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
CloseCurly(2:1-2:2),OpAssign(2:2-2:3),KwDbg(2:3-2:6),LowerIdent(2:7-2:8),LowerIdent(2:9-2:10),Newline(1:1-1:1),
LowerIdent(3:1-3:3),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
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
(e-empty_record @1.1-2.2)
~~~
# TYPES
~~~clojure
(expr @1.1-2.2 (type "{}"))
~~~
