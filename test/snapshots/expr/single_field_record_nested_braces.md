# META
~~~ini
description=Extra braces around a single-field record are do-nothing blocks (issue #9723)
type=expr
~~~
# SOURCE
~~~roc
{
	x = 5
	{ { { x } } }
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
OpenCurly,OpenCurly,OpenCurly,LowerIdent,CloseCurly,CloseCurly,CloseCurly,
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
		(e-block
			(statements
				(e-block
					(statements
						(e-record
							(field (field "x")))))))))
~~~
# FORMATTED
~~~roc
{
	x = 5
	{
		{
			{ x }
		}
	}
}
~~~
# CANONICALIZE
~~~clojure
(e-block
	(s-let
		(p-assign (ident "x"))
		(e-num (value "5")))
	(e-block
		(e-block
			(e-record
				(fields
					(field (name "x")
						(e-lookup-local
							(p-assign (ident "x")))))))))
~~~
# TYPES
~~~clojure
(expr (type "{ x: Dec }"))
~~~
