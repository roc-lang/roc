# META
~~~ini
description=Block expression with two decls and final binop expr
type=expr
~~~
# SOURCE
~~~roc
{
    x = 42
    y = x + 1
    y * 2
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:6),OpAssign(2:7-2:8),Int(2:9-2:11),Newline(1:1-1:1),
LowerIdent(3:5-3:6),OpAssign(3:7-3:8),LowerIdent(3:9-3:10),OpPlus(3:11-3:12),Int(3:13-3:14),Newline(1:1-1:1),
LowerIdent(4:5-4:6),OpStar(4:7-4:8),Int(4:9-4:10),Newline(1:1-1:1),
CloseCurly(5:1-5:2),EndOfFile(5:2-5:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-5.2
	(statements
		(s-decl @2.5-2.11
			(p-ident @2.5-2.6 (raw "x"))
			(e-int @2.9-2.11 (raw "42")))
		(s-decl @3.5-4.6
			(p-ident @3.5-3.6 (raw "y"))
			(e-binop @3.9-4.6 (op "+")
				(e-ident @3.9-3.10 (qaul "") (raw "x"))
				(e-int @3.13-3.14 (raw "1"))))
		(e-binop @4.5-5.2 (op "*")
			(e-ident @4.5-4.6 (qaul "") (raw "y"))
			(e-int @4.9-4.10 (raw "2")))))
~~~
# FORMATTED
~~~roc
{
	x = 42
	y = x + 1
	y * 2
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-5.2
	(s-let @2.5-2.11
		(p-assign @2.5-2.6 (ident "x"))
		(e-int @2.9-2.11 (value "42")))
	(s-let @3.5-4.6
		(p-assign @3.5-3.6 (ident "y"))
		(e-binop @3.9-4.6 (op "add")
			(e-lookup-local @3.9-3.10
				(pattern @2.5-2.6))
			(e-int @3.13-3.14 (value "1"))))
	(e-binop @4.5-5.2 (op "mul")
		(e-lookup-local @4.5-4.6
			(pattern @3.5-3.6))
		(e-int @4.9-4.10 (value "2"))))
~~~
# TYPES
~~~clojure
(expr @1.1-5.2 (type "*"))
~~~
