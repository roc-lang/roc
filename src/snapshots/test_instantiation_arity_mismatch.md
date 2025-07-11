# META
~~~ini
description=Polymorphic function instantiation with arity mismatch
type=expr
~~~
# SOURCE
~~~roc
{
    identity : (a, b) -> (a, b)
    identity = |pair| pair

    identity(1, 2)
}
~~~
# EXPECTED
TYPE MISMATCH - test_instantiation_arity_mismatch.md:5:5:5:13
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**test_instantiation_arity_mismatch.md:5:5:5:13:**
```roc
    identity(1, 2)
```
    ^^^^^^^^

It is of type:
    _* -> *_

But you are trying to use it as:
    _Num(*), Num(*) -> *_

# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:13),OpColon(2:14-2:15),OpenRound(2:16-2:17),LowerIdent(2:17-2:18),Comma(2:18-2:19),LowerIdent(2:20-2:21),CloseRound(2:21-2:22),OpArrow(2:23-2:25),OpenRound(2:26-2:27),LowerIdent(2:27-2:28),Comma(2:28-2:29),LowerIdent(2:30-2:31),CloseRound(2:31-2:32),Newline(1:1-1:1),
LowerIdent(3:5-3:13),OpAssign(3:14-3:15),OpBar(3:16-3:17),LowerIdent(3:17-3:21),OpBar(3:21-3:22),LowerIdent(3:23-3:27),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:5-5:13),NoSpaceOpenRound(5:13-5:14),Int(5:14-5:15),Comma(5:15-5:16),Int(5:17-5:18),CloseRound(5:18-5:19),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-6.2
	(statements
		(s-type-anno @2.5-3.13 (name "identity")
			(ty-fn @2.16-2.32
				(ty-tuple @2.16-2.22
					(ty-var @2.17-2.18 (raw "a"))
					(ty-var @2.20-2.21 (raw "b")))
				(ty-tuple @2.26-2.32
					(ty-var @2.27-2.28 (raw "a"))
					(ty-var @2.30-2.31 (raw "b")))))
		(s-decl @3.5-3.27
			(p-ident @3.5-3.13 (raw "identity"))
			(e-lambda @3.16-3.27
				(args
					(p-ident @3.17-3.21 (raw "pair")))
				(e-ident @3.23-3.27 (raw "pair"))))
		(e-apply @5.5-5.19
			(e-ident @5.5-5.13 (raw "identity"))
			(e-int @5.14-5.15 (raw "1"))
			(e-int @5.17-5.18 (raw "2")))))
~~~
# FORMATTED
~~~roc
{
	identity : (a, b) -> (a, b)
	identity = |pair| pair

	identity(1, 2)
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-6.2
	(s-type-anno @2.5-3.13 (name "identity")
		(ty-fn @2.16-2.32 (effectful false)
			(ty-tuple @2.16-2.22
				(ty-var @2.17-2.18 (name "a"))
				(ty-var @2.20-2.21 (name "b")))
			(ty-tuple @2.26-2.32
				(ty-var @2.27-2.28 (name "a"))
				(ty-var @2.30-2.31 (name "b")))))
	(s-let @3.5-3.27
		(p-assign @3.5-3.13 (ident "identity"))
		(e-lambda @3.16-3.27
			(args
				(p-assign @3.17-3.21 (ident "pair")))
			(e-lookup-local @3.23-3.27
				(p-assign @3.17-3.21 (ident "pair")))))
	(e-call @5.5-5.19
		(e-lookup-local @5.5-5.13
			(p-assign @3.5-3.13 (ident "identity")))
		(e-int @5.14-5.15 (value "1"))
		(e-int @5.17-5.18 (value "2"))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "*"))
~~~
