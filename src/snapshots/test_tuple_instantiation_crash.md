# META
~~~ini
description=Polymorphic tuple function with instantiation crash
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# A polymorphic function that expects a tuple
swap : (a, b) -> (b, a)
swap = |(x, y)| (y, x)

# Call it with two separate arguments instead of a tuple
# This should trigger instantiation and then crash on error reporting
main = swap(1, 2)
~~~
# EXPECTED
TYPE MISMATCH - test_tuple_instantiation_crash.md:9:8:9:12
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**test_tuple_instantiation_crash.md:9:8:9:12:**
```roc
main = swap(1, 2)
```
       ^^^^

It is of type:
    _(a, b) -> (b, a)_

But you are trying to use it as:
    _Num(_size), Num(_size2) -> _ret_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),
LowerIdent(4:1-4:5),OpColon(4:6-4:7),OpenRound(4:8-4:9),LowerIdent(4:9-4:10),Comma(4:10-4:11),LowerIdent(4:12-4:13),CloseRound(4:13-4:14),OpArrow(4:15-4:17),OpenRound(4:18-4:19),LowerIdent(4:19-4:20),Comma(4:20-4:21),LowerIdent(4:22-4:23),CloseRound(4:23-4:24),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),OpBar(5:8-5:9),NoSpaceOpenRound(5:9-5:10),LowerIdent(5:10-5:11),Comma(5:11-5:12),LowerIdent(5:13-5:14),CloseRound(5:14-5:15),OpBar(5:15-5:16),OpenRound(5:17-5:18),LowerIdent(5:18-5:19),Comma(5:19-5:20),LowerIdent(5:21-5:22),CloseRound(5:22-5:23),
LowerIdent(9:1-9:5),OpAssign(9:6-9:7),LowerIdent(9:8-9:12),NoSpaceOpenRound(9:12-9:13),Int(9:13-9:14),Comma(9:14-9:15),Int(9:16-9:17),CloseRound(9:17-9:18),EndOfFile(9:18-9:18),
~~~
# PARSE
~~~clojure
(file @1.1-9.18
	(app @1.1-1.56
		(provides @1.5-1.11
			(exposed-lower-ident @1.6-1.10
				(text "main")))
		(record-field @1.14-1.54 (name "pf")
			(e-string @1.27-1.54
				(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))
		(packages @1.12-1.56
			(record-field @1.14-1.54 (name "pf")
				(e-string @1.27-1.54
					(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-anno @4.1-4.24 (name "swap")
			(ty-fn @4.8-4.24
				(ty-tuple @4.8-4.14
					(ty-var @4.9-4.10 (raw "a"))
					(ty-var @4.12-4.13 (raw "b")))
				(ty-tuple @4.18-4.24
					(ty-var @4.19-4.20 (raw "b"))
					(ty-var @4.22-4.23 (raw "a")))))
		(s-decl @5.1-5.23
			(p-ident @5.1-5.5 (raw "swap"))
			(e-lambda @5.8-5.23
				(args
					(p-tuple @5.9-5.15
						(p-ident @5.10-5.11 (raw "x"))
						(p-ident @5.13-5.14 (raw "y"))))
				(e-tuple @5.17-5.23
					(e-ident @5.18-5.19 (raw "y"))
					(e-ident @5.21-5.22 (raw "x")))))
		(s-decl @9.1-9.18
			(p-ident @9.1-9.5 (raw "main"))
			(e-apply @9.8-9.18
				(e-ident @9.8-9.12 (raw "swap"))
				(e-int @9.13-9.14 (raw "1"))
				(e-int @9.16-9.17 (raw "2"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.5 (ident "swap"))
		(e-lambda @5.8-5.23
			(args
				(p-tuple @5.9-5.15
					(patterns
						(p-assign @5.10-5.11 (ident "x"))
						(p-assign @5.13-5.14 (ident "y")))))
			(e-tuple @5.17-5.23
				(elems
					(e-lookup-local @5.18-5.19
						(p-assign @5.13-5.14 (ident "y")))
					(e-lookup-local @5.21-5.22
						(p-assign @5.10-5.11 (ident "x"))))))
		(annotation @5.1-5.5
			(declared-type
				(ty-fn @4.8-4.24 (effectful false)
					(ty-tuple @4.8-4.14
						(ty-var @4.9-4.10 (name "a"))
						(ty-var @4.12-4.13 (name "b")))
					(ty-tuple @4.18-4.24
						(ty-var @4.19-4.20 (name "b"))
						(ty-var @4.22-4.23 (name "a")))))))
	(d-let
		(p-assign @9.1-9.5 (ident "main"))
		(e-call @9.8-9.18
			(e-lookup-local @9.8-9.12
				(p-assign @5.1-5.5 (ident "swap")))
			(e-int @9.13-9.14 (value "1"))
			(e-int @9.16-9.17 (value "2")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.5 (type "Error"))
		(patt @9.1-9.5 (type "_c")))
	(expressions
		(expr @5.8-5.23 (type "Error"))
		(expr @9.8-9.18 (type "_c"))))
~~~
