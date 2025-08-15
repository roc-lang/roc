# META
~~~ini
description=tuple_type
type=expr
~~~
# SOURCE
~~~roc
{
    f : (Str, Str) -> (Str, Str)
    f = |x| x

    f((1, 2))
}
~~~
# EXPECTED
TYPE MISMATCH - tuple_type.md:5:7:5:13
# PROBLEMS
**TYPE MISMATCH**
The first argument to this function is not what I expect:
**tuple_type.md:5:7:5:13:**
```roc
    f((1, 2))
```
      ^^^^^^

This argument is of type:
    _(Num(_size), Num(_size2))_

But the function needs the first argumument to be:
    _(Str, Str)_

# TOKENS
~~~zig
OpenCurly(1:1-1:2),
LowerIdent(2:5-2:6),OpColon(2:7-2:8),OpenRound(2:9-2:10),UpperIdent(2:10-2:13),Comma(2:13-2:14),UpperIdent(2:15-2:18),CloseRound(2:18-2:19),OpArrow(2:20-2:22),OpenRound(2:23-2:24),UpperIdent(2:24-2:27),Comma(2:27-2:28),UpperIdent(2:29-2:32),CloseRound(2:32-2:33),
LowerIdent(3:5-3:6),OpAssign(3:7-3:8),OpBar(3:9-3:10),LowerIdent(3:10-3:11),OpBar(3:11-3:12),LowerIdent(3:13-3:14),
LowerIdent(5:5-5:6),NoSpaceOpenRound(5:6-5:7),NoSpaceOpenRound(5:7-5:8),Int(5:8-5:9),Comma(5:9-5:10),Int(5:11-5:12),CloseRound(5:12-5:13),CloseRound(5:13-5:14),
CloseCurly(6:1-6:2),EndOfFile(6:2-6:2),
~~~
# PARSE
~~~clojure
(e-block @1.1-6.2
	(statements
		(s-type-anno @2.5-2.33 (name "f")
			(ty-fn @2.9-2.33
				(ty-tuple @2.9-2.19
					(ty @2.10-2.13 (name "Str"))
					(ty @2.15-2.18 (name "Str")))
				(ty-tuple @2.23-2.33
					(ty @2.24-2.27 (name "Str"))
					(ty @2.29-2.32 (name "Str")))))
		(s-decl @3.5-3.14
			(p-ident @3.5-3.6 (raw "f"))
			(e-lambda @3.9-3.14
				(args
					(p-ident @3.10-3.11 (raw "x")))
				(e-ident @3.13-3.14 (raw "x"))))
		(e-apply @5.5-5.14
			(e-ident @5.5-5.6 (raw "f"))
			(e-tuple @5.7-5.13
				(e-int @5.8-5.9 (raw "1"))
				(e-int @5.11-5.12 (raw "2"))))))
~~~
# FORMATTED
~~~roc
{
	f : (Str, Str) -> (Str, Str)
	f = |x| x

	f((1, 2))
}
~~~
# CANONICALIZE
~~~clojure
(e-block @1.1-6.2
	(s-type-anno @2.5-2.33 (name "f")
		(ty-fn @2.9-2.33 (effectful false)
			(ty-tuple @2.9-2.19
				(ty @2.10-2.13 (name "Str"))
				(ty @2.15-2.18 (name "Str")))
			(ty-tuple @2.23-2.33
				(ty @2.24-2.27 (name "Str"))
				(ty @2.29-2.32 (name "Str")))))
	(s-let @3.5-3.14
		(p-assign @3.5-3.6 (ident "f"))
		(e-lambda @3.9-3.14
			(args
				(p-assign @3.10-3.11 (ident "x")))
			(e-lookup-local @3.13-3.14
				(p-assign @3.10-3.11 (ident "x")))))
	(e-call @5.5-5.14
		(e-lookup-local @5.5-5.6
			(p-assign @3.5-3.6 (ident "f")))
		(e-tuple @5.7-5.13
			(elems
				(e-int @5.8-5.9 (value "1"))
				(e-int @5.11-5.12 (value "2"))))))
~~~
# TYPES
~~~clojure
(expr @1.1-6.2 (type "Error"))
~~~
