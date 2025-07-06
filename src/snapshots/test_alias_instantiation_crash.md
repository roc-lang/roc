# META
~~~ini
description=Type alias with polymorphic function causing instantiation crash
type=expr
~~~
# SOURCE
~~~roc
{
    make_identity : a -> a
    make_identity = |x| x

    make_identity(1, 2)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**test_alias_instantiation_crash.md:5:5:5:18:**
```roc
    make_identity(1, 2)
```
    ^^^^^^^^^^^^^

It is of type:
    _* -> *_

But you are trying to use it as:
    _Num(*), Num(*) -> *_

# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
LowerIdent(2:5-2:18),OpColon(2:19-2:20),LowerIdent(2:21-2:22),OpArrow(2:23-2:25),LowerIdent(2:26-2:27),Newline(1:1-1:1),
LowerIdent(3:5-3:18),OpAssign(3:19-3:20),OpBar(3:21-3:22),LowerIdent(3:22-3:23),OpBar(3:23-3:24),LowerIdent(3:25-3:26),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(5:5-5:18),NoSpaceOpenRound(5:18-5:19),Int(5:19-5:20),Comma(5:20-5:21),Int(5:22-5:23),CloseRound(5:23-5:24),Newline(1:1-1:1),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-6.2
	(statements
		(s-type-anno @2.5-3.18 (name "make_identity")
			(ty-fn @2.21-2.27
				(ty-var @2.21-2.22 (raw "a"))
				(ty-var @2.26-2.27 (raw "a"))))
		(s-decl @3.5-3.26
			(p-ident @3.5-3.18 (raw "make_identity"))
			(e-lambda @3.21-3.26
				(args
					(p-ident @3.22-3.23 (raw "x")))
				(e-ident @3.25-3.26 (raw "x"))))
		(e-apply @5.5-5.24
			(e-ident @5.5-5.18 (raw "make_identity"))
			(e-int @5.19-5.20 (raw "1"))
			(e-int @5.22-5.23 (raw "2")))))
~~~
# FORMATTED
~~~roc
{
	make_identity : a -> a
	make_identity = |x| x

	make_identity(1, 2)
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-6.2
	(s-type-anno @2.5-3.18 (name "make_identity")
		(ty-fn @2.21-2.27 (effectful false)
			(ty-var @2.21-2.22 (name "a"))
			(ty-var @2.26-2.27 (name "a"))))
	(s-let @3.5-3.26
		(p-assign @3.5-3.18 (ident "make_identity"))
		(e-lambda @3.21-3.26
			(args
				(p-assign @3.22-3.23 (ident "x")))
			(e-lookup-local @3.25-3.26
				(pattern @3.22-3.23))))
	(e-call @5.5-5.24
		(e-lookup-local @5.5-5.18
			(pattern @3.5-3.18))
		(e-int @5.19-5.20 (value "1"))
		(e-int @5.22-5.23 (value "2"))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "*"))
~~~
