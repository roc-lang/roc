# META
~~~ini
description=empty_record_eq_newlines_doubleeq
type=expr
~~~
# SOURCE
~~~roc
{
}
=d==
 g
d
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
CloseCurly(2:1-2:2),Newline(1:1-1:1),
OpAssign(3:1-3:2),LowerIdent(3:2-3:3),OpEquals(3:3-3:5),Newline(1:1-1:1),
LowerIdent(4:2-4:3),Newline(1:1-1:1),
LowerIdent(5:1-5:2),Newline(1:1-1:1),
MalformedUnknownToken(6:1-6:2),MalformedUnknownToken(6:2-6:3),MalformedUnknownToken(6:3-6:4),EndOfFile(6:4-6:4),
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
