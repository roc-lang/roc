# META
~~~ini
description=Example of a more complex nominal tag union with two payload types
type=snippet
~~~
# SOURCE
~~~roc
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
**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**nominal_tag_payload_two.md:6:32:6:36:**
```roc
is_ok : MyResult(_ok, _err) -> Bool
```
                               ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**nominal_tag_payload_two.md:8:23:8:27:**
```roc
    MyResult.Ok(_) => Bool.True
```
                      ^^^^


**UNDECLARED TYPE**
The type _Bool_ is not declared in this scope.

This type is referenced here:
**nominal_tag_payload_two.md:9:24:9:28:**
```roc
    MyResult.Err(_) => Bool.False
```
                       ^^^^


# TOKENS
~~~zig
UpperIdent(1:1-1:9),NoSpaceOpenRound(1:9-1:10),LowerIdent(1:10-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:17),CloseRound(1:17-1:18),OpColonEqual(1:19-1:21),OpenSquare(1:22-1:23),UpperIdent(1:23-1:25),NoSpaceOpenRound(1:25-1:26),LowerIdent(1:26-1:28),CloseRound(1:28-1:29),Comma(1:29-1:30),UpperIdent(1:31-1:34),NoSpaceOpenRound(1:34-1:35),LowerIdent(1:35-1:38),CloseRound(1:38-1:39),CloseSquare(1:39-1:40),
LowerIdent(3:1-3:3),OpColon(3:4-3:5),LowerIdent(3:6-3:8),OpArrow(3:9-3:11),UpperIdent(3:12-3:20),NoSpaceOpenRound(3:20-3:21),LowerIdent(3:21-3:23),Comma(3:23-3:24),Underscore(3:25-3:26),CloseRound(3:26-3:27),
LowerIdent(4:1-4:3),OpAssign(4:4-4:5),OpBar(4:6-4:7),LowerIdent(4:7-4:8),OpBar(4:8-4:9),UpperIdent(4:10-4:18),NoSpaceDotUpperIdent(4:18-4:21),NoSpaceOpenRound(4:21-4:22),LowerIdent(4:22-4:23),CloseRound(4:23-4:24),
LowerIdent(6:1-6:6),OpColon(6:7-6:8),UpperIdent(6:9-6:17),NoSpaceOpenRound(6:17-6:18),NamedUnderscore(6:18-6:21),Comma(6:21-6:22),NamedUnderscore(6:23-6:27),CloseRound(6:27-6:28),OpArrow(6:29-6:31),UpperIdent(6:32-6:36),
LowerIdent(7:1-7:6),OpAssign(7:7-7:8),OpBar(7:9-7:10),LowerIdent(7:10-7:16),OpBar(7:16-7:17),KwMatch(7:18-7:23),LowerIdent(7:24-7:30),OpenCurly(7:31-7:32),
UpperIdent(8:5-8:13),NoSpaceDotUpperIdent(8:13-8:16),NoSpaceOpenRound(8:16-8:17),Underscore(8:17-8:18),CloseRound(8:18-8:19),OpFatArrow(8:20-8:22),UpperIdent(8:23-8:27),NoSpaceDotUpperIdent(8:27-8:32),
UpperIdent(9:5-9:13),NoSpaceDotUpperIdent(9:13-9:17),NoSpaceOpenRound(9:17-9:18),Underscore(9:18-9:19),CloseRound(9:19-9:20),OpFatArrow(9:21-9:23),UpperIdent(9:24-9:28),NoSpaceDotUpperIdent(9:28-9:34),
CloseCurly(10:1-10:2),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(file @1.1-10.2
	(type-module @1.1-1.9)
	(statements
		(s-type-decl @1.1-1.40
			(header @1.1-1.18 (name "MyResult")
				(args
					(ty-var @1.10-1.12 (raw "ok"))
					(ty-var @1.14-1.17 (raw "err"))))
			(ty-tag-union @1.22-1.40
				(tags
					(ty-apply @1.23-1.29
						(ty @1.23-1.25 (name "Ok"))
						(ty-var @1.26-1.28 (raw "ok")))
					(ty-apply @1.31-1.39
						(ty @1.31-1.34 (name "Err"))
						(ty-var @1.35-1.38 (raw "err"))))))
		(s-type-anno @3.1-3.27 (name "ok")
			(ty-fn @3.6-3.27
				(ty-var @3.6-3.8 (raw "ok"))
				(ty-apply @3.12-3.27
					(ty @3.12-3.20 (name "MyResult"))
					(ty-var @3.21-3.23 (raw "ok"))
					(_))))
		(s-decl @4.1-4.24
			(p-ident @4.1-4.3 (raw "ok"))
			(e-lambda @4.6-4.24
				(args
					(p-ident @4.7-4.8 (raw "a")))
				(e-apply @4.10-4.24
					(e-tag @4.10-4.21 (raw "MyResult.Ok"))
					(e-ident @4.22-4.23 (raw "a")))))
		(s-type-anno @6.1-6.36 (name "is_ok")
			(ty-fn @6.9-6.36
				(ty-apply @6.9-6.28
					(ty @6.9-6.17 (name "MyResult"))
					(underscore-ty-var @6.18-6.21 (raw "_ok"))
					(underscore-ty-var @6.23-6.27 (raw "_err")))
				(ty @6.32-6.36 (name "Bool"))))
		(s-decl @7.1-10.2
			(p-ident @7.1-7.6 (raw "is_ok"))
			(e-lambda @7.9-10.2
				(args
					(p-ident @7.10-7.16 (raw "result")))
				(e-match
					(e-ident @7.24-7.30 (raw "result"))
					(branches
						(branch @8.5-8.32
							(p-tag @8.5-8.19 (raw ".Ok")
								(p-underscore))
							(e-tag @8.23-8.32 (raw "Bool.True")))
						(branch @9.5-9.34
							(p-tag @9.5-9.20 (raw ".Err")
								(p-underscore))
							(e-tag @9.24-9.34 (raw "Bool.False")))))))))
~~~
# FORMATTED
~~~roc
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
		(p-assign @4.1-4.3 (ident "ok"))
		(e-lambda @4.6-4.24
			(args
				(p-assign @4.7-4.8 (ident "a")))
			(e-nominal @4.10-4.24 (nominal "MyResult")
				(e-tag @4.10-4.24 (name "Ok")
					(args
						(e-lookup-local @4.22-4.23
							(p-assign @4.7-4.8 (ident "a")))))))
		(annotation @4.1-4.3
			(declared-type
				(ty-fn @3.6-3.27 (effectful false)
					(ty-rigid-var @3.6-3.8 (name "ok"))
					(ty-apply @3.12-3.27 (name "MyResult") (local)
						(ty-rigid-var-lookup (ty-rigid-var @3.6-3.8 (name "ok")))
						(ty-underscore @3.12-3.27))))))
	(d-let
		(p-assign @7.1-7.6 (ident "is_ok"))
		(e-lambda @7.9-10.2
			(args
				(p-assign @7.10-7.16 (ident "result")))
			(e-match @7.18-10.2
				(match @7.18-10.2
					(cond
						(e-lookup-local @7.24-7.30
							(p-assign @7.10-7.16 (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal @8.5-8.19
										(p-applied-tag @8.5-8.19))))
							(value
								(e-runtime-error (tag "undeclared_type"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal @9.5-9.20
										(p-applied-tag @9.5-9.20))))
							(value
								(e-runtime-error (tag "undeclared_type"))))))))
		(annotation @7.1-7.6
			(declared-type
				(ty-fn @6.9-6.36 (effectful false)
					(ty-apply @6.9-6.28 (name "MyResult") (local)
						(ty-rigid-var @6.9-6.28 (name "_ok"))
						(ty-rigid-var @6.9-6.28 (name "_err")))
					(ty-malformed @6.32-6.36)))))
	(s-nominal-decl @1.1-1.40
		(ty-header @1.1-1.18 (name "MyResult")
			(ty-args
				(ty-rigid-var @1.10-1.12 (name "ok"))
				(ty-rigid-var @1.14-1.17 (name "err"))))
		(ty-tag-union @1.22-1.40
			(ty-tag-name @1.23-1.29 (name "Ok")
				(ty-rigid-var-lookup (ty-rigid-var @1.10-1.12 (name "ok"))))
			(ty-tag-name @1.31-1.39 (name "Err")
				(ty-rigid-var-lookup (ty-rigid-var @1.14-1.17 (name "err")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.3 (type "ok -> MyResult(ok, err)"))
		(patt @7.1-7.6 (type "MyResult(_ok, _err) -> Error")))
	(type_decls
		(nominal @1.1-1.40 (type "MyResult(ok, err)")
			(ty-header @1.1-1.18 (name "MyResult")
				(ty-args
					(ty-rigid-var @1.10-1.12 (name "ok"))
					(ty-rigid-var @1.14-1.17 (name "err"))))))
	(expressions
		(expr @4.6-4.24 (type "ok -> MyResult(ok, err)"))
		(expr @7.9-10.2 (type "MyResult(_ok, _err) -> Error"))))
~~~
