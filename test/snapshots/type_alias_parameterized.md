# META
~~~ini
description=Parameterized type alias with type variables
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

Pair(a, b) : (a, b)

swapPair : Pair(a, b) -> Pair(b, a)
swapPair = |(x, y)| (y, x)

main! = |_| swapPair(1, 2)
~~~
# EXPECTED
TYPE MISMATCH - type_alias_parameterized.md:8:13:8:21
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_alias_parameterized.md:8:13:8:21:**
```roc
main! = |_| swapPair(1, 2)
```
            ^^^^^^^^

It is of type:
    _Num(_size), Num(_size2) -> _ret_

But you are trying to use it as:
    _Pair(a, a) -> Pair(a, a)_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
UpperIdent(3:1-3:5),NoSpaceOpenRound(3:5-3:6),LowerIdent(3:6-3:7),Comma(3:7-3:8),LowerIdent(3:9-3:10),CloseRound(3:10-3:11),OpColon(3:12-3:13),OpenRound(3:14-3:15),LowerIdent(3:15-3:16),Comma(3:16-3:17),LowerIdent(3:18-3:19),CloseRound(3:19-3:20),
LowerIdent(5:1-5:9),OpColon(5:10-5:11),UpperIdent(5:12-5:16),NoSpaceOpenRound(5:16-5:17),LowerIdent(5:17-5:18),Comma(5:18-5:19),LowerIdent(5:20-5:21),CloseRound(5:21-5:22),OpArrow(5:23-5:25),UpperIdent(5:26-5:30),NoSpaceOpenRound(5:30-5:31),LowerIdent(5:31-5:32),Comma(5:32-5:33),LowerIdent(5:34-5:35),CloseRound(5:35-5:36),
LowerIdent(6:1-6:9),OpAssign(6:10-6:11),OpBar(6:12-6:13),NoSpaceOpenRound(6:13-6:14),LowerIdent(6:14-6:15),Comma(6:15-6:16),LowerIdent(6:17-6:18),CloseRound(6:18-6:19),OpBar(6:19-6:20),OpenRound(6:21-6:22),LowerIdent(6:22-6:23),Comma(6:23-6:24),LowerIdent(6:25-6:26),CloseRound(6:26-6:27),
LowerIdent(8:1-8:6),OpAssign(8:7-8:8),OpBar(8:9-8:10),Underscore(8:10-8:11),OpBar(8:11-8:12),LowerIdent(8:13-8:21),NoSpaceOpenRound(8:21-8:22),Int(8:22-8:23),Comma(8:23-8:24),Int(8:25-8:26),CloseRound(8:26-8:27),EndOfFile(8:27-8:27),
~~~
# PARSE
~~~clojure
(file @1.1-8.27
	(app @1.1-1.53
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.51 (name "pf")
			(e-string @1.28-1.51
				(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))
		(packages @1.13-1.53
			(record-field @1.15-1.51 (name "pf")
				(e-string @1.28-1.51
					(e-string-part @1.29-1.50 (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-decl @3.1-3.20
			(header @3.1-3.11 (name "Pair")
				(args
					(ty-var @3.6-3.7 (raw "a"))
					(ty-var @3.9-3.10 (raw "b"))))
			(ty-tuple @3.14-3.20
				(ty-var @3.15-3.16 (raw "a"))
				(ty-var @3.18-3.19 (raw "b"))))
		(s-type-anno @5.1-5.36 (name "swapPair")
			(ty-fn @5.12-5.36
				(ty-apply @5.12-5.22
					(ty @5.12-5.16 (name "Pair"))
					(ty-var @5.17-5.18 (raw "a"))
					(ty-var @5.20-5.21 (raw "b")))
				(ty-apply @5.26-5.36
					(ty @5.26-5.30 (name "Pair"))
					(ty-var @5.31-5.32 (raw "b"))
					(ty-var @5.34-5.35 (raw "a")))))
		(s-decl @6.1-6.27
			(p-ident @6.1-6.9 (raw "swapPair"))
			(e-lambda @6.12-6.27
				(args
					(p-tuple @6.13-6.19
						(p-ident @6.14-6.15 (raw "x"))
						(p-ident @6.17-6.18 (raw "y"))))
				(e-tuple @6.21-6.27
					(e-ident @6.22-6.23 (raw "y"))
					(e-ident @6.25-6.26 (raw "x")))))
		(s-decl @8.1-8.27
			(p-ident @8.1-8.6 (raw "main!"))
			(e-lambda @8.9-8.27
				(args
					(p-underscore))
				(e-apply @8.13-8.27
					(e-ident @8.13-8.21 (raw "swapPair"))
					(e-int @8.22-8.23 (raw "1"))
					(e-int @8.25-8.26 (raw "2")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.9 (ident "swapPair"))
		(e-lambda @6.12-6.27
			(args
				(p-tuple @6.13-6.19
					(patterns
						(p-assign @6.14-6.15 (ident "x"))
						(p-assign @6.17-6.18 (ident "y")))))
			(e-tuple @6.21-6.27
				(elems
					(e-lookup-local @6.22-6.23
						(p-assign @6.17-6.18 (ident "y")))
					(e-lookup-local @6.25-6.26
						(p-assign @6.14-6.15 (ident "x"))))))
		(annotation @6.1-6.9
			(declared-type
				(ty-fn @5.12-5.36 (effectful false)
					(ty-apply @5.12-5.22 (symbol "Pair")
						(ty-var @5.17-5.18 (name "a"))
						(ty-var @5.20-5.21 (name "b")))
					(ty-apply @5.26-5.36 (symbol "Pair")
						(ty-var @5.31-5.32 (name "b"))
						(ty-var @5.34-5.35 (name "a")))))))
	(d-let
		(p-assign @8.1-8.6 (ident "main!"))
		(e-closure @8.9-8.27
			(captures
				(capture @6.1-6.9 (ident "swapPair")))
			(e-lambda @8.9-8.27
				(args
					(p-underscore @8.10-8.11))
				(e-call @8.13-8.27
					(e-lookup-local @8.13-8.21
						(p-assign @6.1-6.9 (ident "swapPair")))
					(e-int @8.22-8.23 (value "1"))
					(e-int @8.25-8.26 (value "2"))))))
	(s-alias-decl @3.1-3.20
		(ty-header @3.1-3.11 (name "Pair")
			(ty-args
				(ty-var @3.6-3.7 (name "a"))
				(ty-var @3.9-3.10 (name "b"))))
		(ty-tuple @3.14-3.20
			(ty-var @3.15-3.16 (name "a"))
			(ty-var @3.18-3.19 (name "b")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.9 (type "Pair(a, a) -> Pair(a, a)"))
		(patt @8.1-8.6 (type "_arg -> _ret")))
	(type_decls
		(alias @3.1-3.20 (type "Pair(a, b)")
			(ty-header @3.1-3.11 (name "Pair")
				(ty-args
					(ty-var @3.6-3.7 (name "a"))
					(ty-var @3.9-3.10 (name "b"))))))
	(expressions
		(expr @6.12-6.27 (type "Pair(a, a) -> Pair(a, a)"))
		(expr @8.9-8.27 (type "_arg -> _ret"))))
~~~
