# META
~~~ini
description=Return type mismatch with instantiated function
type=expr
~~~
# SOURCE
~~~roc
{
    identity : a -> a
    identity = |x| x

    needs_string : ((Str -> Str) -> Str)
    needs_string = |f| f("hello")

    needs_string(identity)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:13),OpColon(2:14-2:15),LowerIdent(2:16-2:17),OpArrow(2:18-2:20),LowerIdent(2:21-2:22),Newline(1:1-1:1),
LowerIdent(3:5-3:13),OpAssign(3:14-3:15),OpBar(3:16-3:17),LowerIdent(3:17-3:18),OpBar(3:18-3:19),LowerIdent(3:20-3:21),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:5-5:17),OpColon(5:18-5:19),OpenRound(5:20-5:21),NoSpaceOpenRound(5:21-5:22),UpperIdent(5:22-5:25),OpArrow(5:26-5:28),UpperIdent(5:29-5:32),CloseRound(5:32-5:33),OpArrow(5:34-5:36),UpperIdent(5:37-5:40),CloseRound(5:40-5:41),Newline(1:1-1:1),
LowerIdent(6:5-6:17),OpAssign(6:18-6:19),OpBar(6:20-6:21),LowerIdent(6:21-6:22),OpBar(6:22-6:23),LowerIdent(6:24-6:25),NoSpaceOpenRound(6:25-6:26),StringStart(6:26-6:27),StringPart(6:27-6:32),StringEnd(6:32-6:33),CloseRound(6:33-6:34),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(8:5-8:17),NoSpaceOpenRound(8:17-8:18),LowerIdent(8:18-8:26),CloseRound(8:26-8:27),Newline(1:1-1:1),
CloseCurly(9:1-9:2),EndOfFile(9:2-9:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-9.2
	(statements
		(s-type-anno @2.5-3.13 (name "identity")
			(ty-fn @2.16-2.22
				(ty-var @2.16-2.17 (raw "a"))
				(ty-var @2.21-2.22 (raw "a"))))
		(s-decl @3.5-3.21
			(p-ident @3.5-3.13 (raw "identity"))
			(e-lambda @3.16-3.21
				(args
					(p-ident @3.17-3.18 (raw "x")))
				(e-ident @3.20-3.21 (raw "x"))))
		(s-type-anno @5.5-6.17 (name "needs_string")
			(ty-fn @5.21-5.40
				(ty-fn @5.22-5.32
					(ty @5.22-5.25 (name "Str"))
					(ty @5.29-5.32 (name "Str")))
				(ty @5.37-5.40 (name "Str"))))
		(s-decl @6.5-6.34
			(p-ident @6.5-6.17 (raw "needs_string"))
			(e-lambda @6.20-6.34
				(args
					(p-ident @6.21-6.22 (raw "f")))
				(e-apply @6.24-6.34
					(e-ident @6.24-6.25 (raw "f"))
					(e-string @6.26-6.33
						(e-string-part @6.27-6.32 (raw "hello"))))))
		(e-apply @8.5-8.27
			(e-ident @8.5-8.17 (raw "needs_string"))
			(e-ident @8.18-8.26 (raw "identity")))))
~~~
# FORMATTED
~~~roc
{
	identity : a -> a
	identity = |x| x

	needs_string : ((Str -> Str) -> Str)
	needs_string = |f| f("hello")

	needs_string(identity)
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-9.2
	(s-type-anno @2.5-3.13 (name "identity")
		(ty-fn @2.16-2.22 (effectful false)
			(ty-var @2.16-2.17 (name "a"))
			(ty-var @2.21-2.22 (name "a"))))
	(s-let @3.5-3.21
		(p-assign @3.5-3.13 (ident "identity"))
		(e-lambda @3.16-3.21
			(args
				(p-assign @3.17-3.18 (ident "x")))
			(e-lookup-local @3.20-3.21
				(pattern @3.17-3.18))))
	(s-type-anno @5.5-6.17 (name "needs_string")
		(ty-parens @5.20-5.41
			(ty-fn @5.21-5.40 (effectful false)
				(ty-parens @5.21-5.33
					(ty-fn @5.22-5.32 (effectful false)
						(ty @5.22-5.25 (name "Str"))
						(ty @5.29-5.32 (name "Str"))))
				(ty @5.37-5.40 (name "Str")))))
	(s-let @6.5-6.34
		(p-assign @6.5-6.17 (ident "needs_string"))
		(e-lambda @6.20-6.34
			(args
				(p-assign @6.21-6.22 (ident "f")))
			(e-call @6.24-6.34
				(e-lookup-local @6.24-6.25
					(pattern @6.21-6.22))
				(e-string @6.26-6.33
					(e-literal @6.27-6.32 (string "hello"))))))
	(e-call @8.5-8.27
		(e-lookup-local @8.5-8.17
			(pattern @6.5-6.17))
		(e-lookup-local @8.18-8.26
			(pattern @3.5-3.13))))
~~~
# TYPES
~~~clojure
(expr @1.1-9.2 (type "Str"))
~~~
