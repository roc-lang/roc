# META
~~~ini
description=Example of a more complex nominal tag union with two payload types
type=file
~~~
# SOURCE
~~~roc
module [MyResult, ok, is_ok]

MyResult(ok, err) := [Ok(ok), Err(err)]

ok : ok -> MyResult(ok, _)
ok = |a| MyResult.Ok(a)

is_ok : MyResult(_ok, _err) -> Bool
is_ok = |result| match result {
    MyResult.Ok(_) => Bool.True
    MyResult.Err(_) => Bool.False
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwModule(1:1-1:7),OpenSquare(1:8-1:9),UpperIdent(1:9-1:17),Comma(1:17-1:18),LowerIdent(1:19-1:21),Comma(1:21-1:22),LowerIdent(1:23-1:28),CloseSquare(1:28-1:29),
UpperIdent(3:1-3:9),NoSpaceOpenRound(3:9-3:10),LowerIdent(3:10-3:12),Comma(3:12-3:13),LowerIdent(3:14-3:17),CloseRound(3:17-3:18),OpColonEqual(3:19-3:21),OpenSquare(3:22-3:23),UpperIdent(3:23-3:25),NoSpaceOpenRound(3:25-3:26),LowerIdent(3:26-3:28),CloseRound(3:28-3:29),Comma(3:29-3:30),UpperIdent(3:31-3:34),NoSpaceOpenRound(3:34-3:35),LowerIdent(3:35-3:38),CloseRound(3:38-3:39),CloseSquare(3:39-3:40),
LowerIdent(5:1-5:3),OpColon(5:4-5:5),LowerIdent(5:6-5:8),OpArrow(5:9-5:11),UpperIdent(5:12-5:20),NoSpaceOpenRound(5:20-5:21),LowerIdent(5:21-5:23),Comma(5:23-5:24),Underscore(5:25-5:26),CloseRound(5:26-5:27),
LowerIdent(6:1-6:3),OpAssign(6:4-6:5),OpBar(6:6-6:7),LowerIdent(6:7-6:8),OpBar(6:8-6:9),UpperIdent(6:10-6:18),NoSpaceDotUpperIdent(6:18-6:21),NoSpaceOpenRound(6:21-6:22),LowerIdent(6:22-6:23),CloseRound(6:23-6:24),
LowerIdent(8:1-8:6),OpColon(8:7-8:8),UpperIdent(8:9-8:17),NoSpaceOpenRound(8:17-8:18),NamedUnderscore(8:18-8:21),Comma(8:21-8:22),NamedUnderscore(8:23-8:27),CloseRound(8:27-8:28),OpArrow(8:29-8:31),UpperIdent(8:32-8:36),
LowerIdent(9:1-9:6),OpAssign(9:7-9:8),OpBar(9:9-9:10),LowerIdent(9:10-9:16),OpBar(9:16-9:17),KwMatch(9:18-9:23),LowerIdent(9:24-9:30),OpenCurly(9:31-9:32),
UpperIdent(10:5-10:13),NoSpaceDotUpperIdent(10:13-10:16),NoSpaceOpenRound(10:16-10:17),Underscore(10:17-10:18),CloseRound(10:18-10:19),OpFatArrow(10:20-10:22),UpperIdent(10:23-10:27),NoSpaceDotUpperIdent(10:27-10:32),
UpperIdent(11:5-11:13),NoSpaceDotUpperIdent(11:13-11:17),NoSpaceOpenRound(11:17-11:18),Underscore(11:18-11:19),CloseRound(11:19-11:20),OpFatArrow(11:21-11:23),UpperIdent(11:24-11:28),NoSpaceDotUpperIdent(11:28-11:34),
CloseCurly(12:1-12:2),EndOfFile(12:2-12:2),
~~~
# PARSE
~~~clojure
(file @1.1-12.2
	(module @1.1-1.29
		(exposes @1.8-1.29
			(exposed-upper-ident @1.9-1.17 (text "MyResult"))
			(exposed-lower-ident @1.19-1.21
				(text "ok"))
			(exposed-lower-ident @1.23-1.28
				(text "is_ok"))))
	(statements
		(s-type-decl @3.1-3.40
			(header @3.1-3.18 (name "MyResult")
				(args
					(ty-var @3.10-3.12 (raw "ok"))
					(ty-var @3.14-3.17 (raw "err"))))
			(ty-tag-union @3.22-3.40
				(tags
					(ty-apply @3.23-3.29
						(ty @3.23-3.25 (name "Ok"))
						(ty-var @3.26-3.28 (raw "ok")))
					(ty-apply @3.31-3.39
						(ty @3.31-3.34 (name "Err"))
						(ty-var @3.35-3.38 (raw "err"))))))
		(s-type-anno @5.1-5.27 (name "ok")
			(ty-fn @5.6-5.27
				(ty-var @5.6-5.8 (raw "ok"))
				(ty-apply @5.12-5.27
					(ty @5.12-5.20 (name "MyResult"))
					(ty-var @5.21-5.23 (raw "ok"))
					(_))))
		(s-decl @6.1-6.24
			(p-ident @6.1-6.3 (raw "ok"))
			(e-lambda @6.6-6.24
				(args
					(p-ident @6.7-6.8 (raw "a")))
				(e-apply @6.10-6.24
					(e-tag @6.10-6.21 (raw "MyResult.Ok"))
					(e-ident @6.22-6.23 (raw "a")))))
		(s-type-anno @8.1-8.36 (name "is_ok")
			(ty-fn @8.9-8.36
				(ty-apply @8.9-8.28
					(ty @8.9-8.17 (name "MyResult"))
					(underscore-ty-var @8.18-8.21 (raw "_ok"))
					(underscore-ty-var @8.23-8.27 (raw "_err")))
				(ty @8.32-8.36 (name "Bool"))))
		(s-decl @9.1-12.2
			(p-ident @9.1-9.6 (raw "is_ok"))
			(e-lambda @9.9-12.2
				(args
					(p-ident @9.10-9.16 (raw "result")))
				(e-match
					(e-ident @9.24-9.30 (raw "result"))
					(branches
						(branch @10.5-10.32
							(p-tag @10.5-10.19 (raw ".Ok")
								(p-underscore))
							(e-tag @10.23-10.32 (raw "Bool.True")))
						(branch @11.5-11.34
							(p-tag @11.5-11.20 (raw ".Err")
								(p-underscore))
							(e-tag @11.24-11.34 (raw "Bool.False")))))))))
~~~
# FORMATTED
~~~roc
module [MyResult, ok, is_ok]

MyResult(ok, err) := [Ok(ok), Err(err)]

ok : ok -> MyResult(ok, _)
ok = |a| MyResult.Ok(a)

is_ok : MyResult(_ok, _err) -> Bool
is_ok = |result| match result {
	MyResult.Ok(_) => Bool.True
	MyResult.Err(_) => Bool.False
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @6.1-6.3 (ident "ok"))
		(e-lambda @6.6-6.24
			(args
				(p-assign @6.7-6.8 (ident "a")))
			(e-nominal @6.10-6.21 (nominal "MyResult")
				(e-tag @6.10-6.21 (name "Ok")
					(args
						(e-lookup-local @6.22-6.23
							(p-assign @6.7-6.8 (ident "a")))))))
		(annotation @6.1-6.3
			(declared-type
				(ty-fn @5.6-5.27 (effectful false)
					(ty-var @5.6-5.8 (name "ok"))
					(ty-apply @5.12-5.27 (symbol "MyResult")
						(ty-var @5.21-5.23 (name "ok"))
						(ty-underscore @1.1-1.1))))))
	(d-let
		(p-assign @9.1-9.6 (ident "is_ok"))
		(e-lambda @9.9-12.2
			(args
				(p-assign @9.10-9.16 (ident "result")))
			(e-match @9.18-12.2
				(match @9.18-12.2
					(cond
						(e-lookup-local @9.24-9.30
							(p-assign @9.10-9.16 (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal @10.5-10.19
										(p-applied-tag @10.5-10.19))))
							(value
								(e-nominal @10.23-10.32 (nominal "Bool")
									(e-tag @10.23-10.32 (name "True")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal @11.5-11.20
										(p-applied-tag @11.5-11.20))))
							(value
								(e-nominal @11.24-11.34 (nominal "Bool")
									(e-tag @11.24-11.34 (name "False")))))))))
		(annotation @9.1-9.6
			(declared-type
				(ty-fn @8.9-8.36 (effectful false)
					(ty-apply @8.9-8.28 (symbol "MyResult")
						(ty-var @8.18-8.21 (name "_ok"))
						(ty-var @8.23-8.27 (name "_err")))
					(ty @8.32-8.36 (name "Bool"))))))
	(s-nominal-decl @3.1-3.40
		(ty-header @3.1-3.18 (name "MyResult")
			(ty-args
				(ty-var @3.10-3.12 (name "ok"))
				(ty-var @3.14-3.17 (name "err"))))
		(ty-tag-union @3.22-3.40
			(ty-apply @3.23-3.29 (symbol "Ok")
				(ty-var @3.26-3.28 (name "ok")))
			(ty-apply @3.31-3.39 (symbol "Err")
				(ty-var @3.35-3.38 (name "err"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @6.1-6.3 (type "ok -> MyResult(ok, err)"))
		(patt @9.1-9.6 (type "MyResult(ok, err) -> Bool")))
	(type_decls
		(nominal @3.1-3.40 (type "MyResult(ok, err)")
			(ty-header @3.1-3.18 (name "MyResult")
				(ty-args
					(ty-var @3.10-3.12 (name "ok"))
					(ty-var @3.14-3.17 (name "err"))))))
	(expressions
		(expr @6.6-6.24 (type "ok -> MyResult(ok, err)"))
		(expr @9.9-12.2 (type "MyResult(ok, err) -> Bool"))))
~~~
