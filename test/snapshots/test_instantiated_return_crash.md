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
    needs_string = |f| f(["hello"])

    needs_string(identity)
}
~~~
# EXPECTED
TYPE MISMATCH - test_instantiated_return_crash.md:6:26:6:35
# PROBLEMS
**TYPE MISMATCH**
The first argument to this function is not what I expect:
**test_instantiated_return_crash.md:6:26:6:35:**
```roc
    needs_string = |f| f(["hello"])
```
                         ^^^^^^^^^

This argument is of type:
    _List(Str)_

But the function needs the first argumument to be:
    _Str_

# TOKENS
~~~zig
OpenCurly(1:1-1:2),
LowerIdent(2:5-2:13),OpColon(2:14-2:15),LowerIdent(2:16-2:17),OpArrow(2:18-2:20),LowerIdent(2:21-2:22),
LowerIdent(3:5-3:13),OpAssign(3:14-3:15),OpBar(3:16-3:17),LowerIdent(3:17-3:18),OpBar(3:18-3:19),LowerIdent(3:20-3:21),
LowerIdent(5:5-5:17),OpColon(5:18-5:19),OpenRound(5:20-5:21),NoSpaceOpenRound(5:21-5:22),UpperIdent(5:22-5:25),OpArrow(5:26-5:28),UpperIdent(5:29-5:32),CloseRound(5:32-5:33),OpArrow(5:34-5:36),UpperIdent(5:37-5:40),CloseRound(5:40-5:41),
LowerIdent(6:5-6:17),OpAssign(6:18-6:19),OpBar(6:20-6:21),LowerIdent(6:21-6:22),OpBar(6:22-6:23),LowerIdent(6:24-6:25),NoSpaceOpenRound(6:25-6:26),OpenSquare(6:26-6:27),StringStart(6:27-6:28),StringPart(6:28-6:33),StringEnd(6:33-6:34),CloseSquare(6:34-6:35),CloseRound(6:35-6:36),
LowerIdent(8:5-8:17),NoSpaceOpenRound(8:17-8:18),LowerIdent(8:18-8:26),CloseRound(8:26-8:27),
CloseCurly(9:1-9:2),EndOfFile(9:2-9:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-9.2
	(statements
		(s-type-anno @2.5-2.22 (name "identity")
			(ty-fn @2.16-2.22
				(ty-var @2.16-2.17 (raw "a"))
				(ty-var @2.21-2.22 (raw "a"))))
		(s-decl @3.5-3.21
			(p-ident @3.5-3.13 (raw "identity"))
			(e-lambda @3.16-3.21
				(args
					(p-ident @3.17-3.18 (raw "x")))
				(e-ident @3.20-3.21 (raw "x"))))
		(s-type-anno @5.5-5.41 (name "needs_string")
			(ty-fn @5.21-5.40
				(ty-fn @5.22-5.32
					(ty @5.22-5.25 (name "Str"))
					(ty @5.29-5.32 (name "Str")))
				(ty @5.37-5.40 (name "Str"))))
		(s-decl @6.5-6.36
			(p-ident @6.5-6.17 (raw "needs_string"))
			(e-lambda @6.20-6.36
				(args
					(p-ident @6.21-6.22 (raw "f")))
				(e-apply @6.24-6.36
					(e-ident @6.24-6.25 (raw "f"))
					(e-list @6.26-6.35
						(e-string @6.27-6.34
							(e-string-part @6.28-6.33 (raw "hello")))))))
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
	needs_string = |f| f(["hello"])

	needs_string(identity)
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-9.2
	(s-type-anno @2.5-2.22 (name "identity")
		(ty-fn @2.16-2.22 (effectful false)
			(ty-var @2.16-2.17 (name "a"))
			(ty-var @2.21-2.22 (name "a"))))
	(s-let @3.5-3.21
		(p-assign @3.5-3.13 (ident "identity"))
		(e-lambda @3.16-3.21
			(args
				(p-assign @3.17-3.18 (ident "x")))
			(e-lookup-local @3.20-3.21
				(p-assign @3.17-3.18 (ident "x")))))
	(s-type-anno @5.5-5.41 (name "needs_string")
		(ty-parens @5.20-5.41
			(ty-fn @5.21-5.40 (effectful false)
				(ty-parens @5.21-5.33
					(ty-fn @5.22-5.32 (effectful false)
						(ty @5.22-5.25 (name "Str"))
						(ty @5.29-5.32 (name "Str"))))
				(ty @5.37-5.40 (name "Str")))))
	(s-let @6.5-6.36
		(p-assign @6.5-6.17 (ident "needs_string"))
		(e-lambda @6.20-6.36
			(args
				(p-assign @6.21-6.22 (ident "f")))
			(e-call @6.24-6.36
				(e-lookup-local @6.24-6.25
					(p-assign @6.21-6.22 (ident "f")))
				(e-list @6.26-6.35
					(elems
						(e-string @6.27-6.34
							(e-literal @6.28-6.33 (string "hello"))))))))
	(e-call @8.5-8.27
		(e-lookup-local @8.5-8.17
			(p-assign @6.5-6.17 (ident "needs_string")))
		(e-lookup-local @8.18-8.26
			(p-assign @3.5-3.13 (ident "identity")))))
~~~
# TYPES
~~~clojure
(expr @1.1-9.2 (type "Error"))
~~~
