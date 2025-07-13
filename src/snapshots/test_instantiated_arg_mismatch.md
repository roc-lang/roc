# META
~~~ini
description=Type mismatch with instantiated function arguments
type=expr
~~~
# SOURCE
~~~roc
{
    pair : a, a -> (a, a)
    pair = |x, y| (x, y)

    pair(42, "hello")
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),
LowerIdent(2:5-2:9),OpColon(2:10-2:11),LowerIdent(2:12-2:13),Comma(2:13-2:14),LowerIdent(2:15-2:16),OpArrow(2:17-2:19),OpenRound(2:20-2:21),LowerIdent(2:21-2:22),Comma(2:22-2:23),LowerIdent(2:24-2:25),CloseRound(2:25-2:26),
LowerIdent(3:5-3:9),OpAssign(3:10-3:11),OpBar(3:12-3:13),LowerIdent(3:13-3:14),Comma(3:14-3:15),LowerIdent(3:16-3:17),OpBar(3:17-3:18),OpenRound(3:19-3:20),LowerIdent(3:20-3:21),Comma(3:21-3:22),LowerIdent(3:23-3:24),CloseRound(3:24-3:25),
LowerIdent(5:5-5:9),NoSpaceOpenRound(5:9-5:10),Int(5:10-5:12),Comma(5:12-5:13),StringStart(5:14-5:15),StringPart(5:15-5:20),StringEnd(5:20-5:21),CloseRound(5:21-5:22),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-6.2
	(statements
		(s-type-anno @2.5-2.26 (name "pair")
			(ty-fn @2.12-2.26
				(ty-var @1.1-1.1 (raw "a"))
				(ty-var @1.1-1.1 (raw "a"))
				(ty-tuple @2.20-2.26
					(ty-var @2.21-2.21 (raw "a"))
					(ty-var @1.1-1.1 (raw "a")))))
		(s-decl @3.5-3.25
			(p-ident @3.5-3.9 (raw "pair"))
			(e-lambda @3.12-3.25
				(args
					(p-ident @3.13-3.14 (raw "x"))
					(p-ident @3.16-3.17 (raw "y")))
				(e-tuple @3.19-3.25
					(e-ident @3.20-3.21 (raw "x"))
					(e-ident @3.23-3.24 (raw "y")))))
		(e-apply @5.5-5.22
			(e-ident @5.5-5.9 (raw "pair"))
			(e-int @5.10-5.12 (raw "42"))
			(e-string @5.14-5.21
				(e-string-part @5.15-5.20 (raw "hello"))))))
~~~
# FORMATTED
~~~roc
{
	pair : a, a -> (a, a)
	pair = |x, y| (x, y)

	pair(42, "hello")
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-6.2
	(s-type-anno @2.5-2.26 (name "pair")
		(ty-fn @2.12-2.26 (effectful false)
			(ty-var @1.1-1.1 (name "a"))
			(ty-var @1.1-1.1 (name "a"))
			(ty-tuple @2.20-2.26
				(ty-var @2.21-2.21 (name "a"))
				(ty-var @1.1-1.1 (name "a")))))
	(s-let @3.5-3.25
		(p-assign @3.5-3.9 (ident "pair"))
		(e-lambda @3.12-3.25
			(args
				(p-assign @3.13-3.14 (ident "x"))
				(p-assign @3.16-3.17 (ident "y")))
			(e-tuple @3.19-3.25
				(elems
					(e-lookup-local @3.20-3.21
						(p-assign @3.13-3.14 (ident "x")))
					(e-lookup-local @3.23-3.24
						(p-assign @3.16-3.17 (ident "y")))))))
	(e-call @5.5-5.22
		(e-lookup-local @5.5-5.9
			(p-assign @3.5-3.9 (ident "pair")))
		(e-int @5.10-5.12 (value "42"))
		(e-string @5.14-5.21
			(e-literal @5.15-5.20 (string "hello")))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "(b, c)"))
~~~
