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
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),OpenCurly(1:2-1:3),Newline(1:1-1:1),
CloseCurly(2:1-2:2),CloseRound(2:2-2:3),OpenCurly(2:3-2:4),Newline(1:1-1:1),
CloseCurly(3:1-3:2),EndOfFile(3:2-3:2),
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
