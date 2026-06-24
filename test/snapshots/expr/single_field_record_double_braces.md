# META
~~~ini
description=Single-field record written with double braces inside a block
type=expr
~~~
# SOURCE
~~~roc
{
	x = 5
	{ x }
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpAssign,Int,
OpenCurly,LowerIdent,CloseCurly,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-block
	(statements
		(s-decl
			(p-ident (raw "x"))
			(e-int (raw "5")))
		(e-record
			(field (field "x")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(e-record
		(fields
			(field (name "x")
				(e-lookup-local
					(p-assign (ident "x")))))))
~~~
# TYPES
~~~clojure
(expr (type "{ x: Dec }"))
~~~
