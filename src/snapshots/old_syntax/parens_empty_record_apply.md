# META
~~~ini
description=parens_empty_record_apply
type=expr
~~~
# SOURCE
~~~roc
({
}){
}
~~~
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpenCurly(1:2-1:3),Newline(1:1-1:1),
CloseCurly(2:1-2:2),CloseRound(2:2-2:3),OpenCurly(2:3-2:4),Newline(1:1-1:1),
CloseCurly(3:1-3:2),Newline(1:1-1:1),
MalformedUnknownToken(4:1-4:2),MalformedUnknownToken(4:2-4:3),MalformedUnknownToken(4:3-4:4),EndOfFile(4:4-4:4),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-2.3
	(e-record @1.2-2.2))
~~~
# FORMATTED
~~~roc
(
	{},
)
~~~
# CANONICALIZE
~~~clojure
(e-empty_record @1.2-2.2)
~~~
# TYPES
~~~clojure
(expr @1.2-2.2 (type "{}"))
~~~
