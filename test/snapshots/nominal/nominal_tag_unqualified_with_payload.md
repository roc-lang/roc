# META
~~~ini
description=Unqualified tags with payloads on polymorphic nominal types
type=snippet
~~~
# SOURCE
~~~roc
MyResult(ok, err) := [Ok(ok), Err(err)]

myResult : MyResult(Str, I32)
myResult = Ok("success")

isOk : MyResult(ok, err) -> Bool
isOk = |result| match result {
    Ok(_) => Bool.True
    Err(_) => Bool.False
}
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_tag_unqualified_with_payload.md:4:12:4:25:**
```roc
myResult = Ok("success")
```
           ^^^^^^^^^^^^^

It has the type:
    _Result(Str, err)_

But the type annotation says it should have the type:
    _MyResult(Str, Num(Int(Signed32)))_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**nominal_tag_unqualified_with_payload.md:8:5:8:10:**
```roc
    Ok(_) => Bool.True
```
    ^^^^^

It has the type:
    _Result(ok, err)_

But I expected it to be:
    _MyResult(ok, err)_

# TOKENS
~~~zig
UpperIdent(1:1-1:9),NoSpaceOpenRound(1:9-1:10),LowerIdent(1:10-1:12),Comma(1:12-1:13),LowerIdent(1:14-1:17),CloseRound(1:17-1:18),OpColonEqual(1:19-1:21),OpenSquare(1:22-1:23),UpperIdent(1:23-1:25),NoSpaceOpenRound(1:25-1:26),LowerIdent(1:26-1:28),CloseRound(1:28-1:29),Comma(1:29-1:30),UpperIdent(1:31-1:34),NoSpaceOpenRound(1:34-1:35),LowerIdent(1:35-1:38),CloseRound(1:38-1:39),CloseSquare(1:39-1:40),
LowerIdent(3:1-3:9),OpColon(3:10-3:11),UpperIdent(3:12-3:20),NoSpaceOpenRound(3:20-3:21),UpperIdent(3:21-3:24),Comma(3:24-3:25),UpperIdent(3:26-3:29),CloseRound(3:29-3:30),
LowerIdent(4:1-4:9),OpAssign(4:10-4:11),UpperIdent(4:12-4:14),NoSpaceOpenRound(4:14-4:15),StringStart(4:15-4:16),StringPart(4:16-4:23),StringEnd(4:23-4:24),CloseRound(4:24-4:25),
LowerIdent(6:1-6:5),OpColon(6:6-6:7),UpperIdent(6:8-6:16),NoSpaceOpenRound(6:16-6:17),LowerIdent(6:17-6:19),Comma(6:19-6:20),LowerIdent(6:21-6:24),CloseRound(6:24-6:25),OpArrow(6:26-6:28),UpperIdent(6:29-6:33),
LowerIdent(7:1-7:5),OpAssign(7:6-7:7),OpBar(7:8-7:9),LowerIdent(7:9-7:15),OpBar(7:15-7:16),KwMatch(7:17-7:22),LowerIdent(7:23-7:29),OpenCurly(7:30-7:31),
UpperIdent(8:5-8:7),NoSpaceOpenRound(8:7-8:8),Underscore(8:8-8:9),CloseRound(8:9-8:10),OpFatArrow(8:11-8:13),UpperIdent(8:14-8:18),NoSpaceDotUpperIdent(8:18-8:23),
UpperIdent(9:5-9:8),NoSpaceOpenRound(9:8-9:9),Underscore(9:9-9:10),CloseRound(9:10-9:11),OpFatArrow(9:12-9:14),UpperIdent(9:15-9:19),NoSpaceDotUpperIdent(9:19-9:25),
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
		(s-type-anno @3.1-3.30 (name "myResult")
			(ty-apply @3.12-3.30
				(ty @3.12-3.20 (name "MyResult"))
				(ty @3.21-3.24 (name "Str"))
				(ty @3.26-3.29 (name "I32"))))
		(s-decl @4.1-4.25
			(p-ident @4.1-4.9 (raw "myResult"))
			(e-apply @4.12-4.25
				(e-tag @4.12-4.14 (raw "Ok"))
				(e-string @4.15-4.24
					(e-string-part @4.16-4.23 (raw "success")))))
		(s-type-anno @6.1-6.33 (name "isOk")
			(ty-fn @6.8-6.33
				(ty-apply @6.8-6.25
					(ty @6.8-6.16 (name "MyResult"))
					(ty-var @6.17-6.19 (raw "ok"))
					(ty-var @6.21-6.24 (raw "err")))
				(ty @6.29-6.33 (name "Bool"))))
		(s-decl @7.1-10.2
			(p-ident @7.1-7.5 (raw "isOk"))
			(e-lambda @7.8-10.2
				(args
					(p-ident @7.9-7.15 (raw "result")))
				(e-match
					(e-ident @7.23-7.29 (raw "result"))
					(branches
						(branch @8.5-8.23
							(p-tag @8.5-8.10 (raw "Ok")
								(p-underscore))
							(e-tag @8.14-8.23 (raw "Bool.True")))
						(branch @9.5-9.25
							(p-tag @9.5-9.11 (raw "Err")
								(p-underscore))
							(e-tag @9.15-9.25 (raw "Bool.False")))))))))
~~~
# FORMATTED
~~~roc
MyResult(ok, err) := [Ok(ok), Err(err)]

myResult : MyResult(Str, I32)
myResult = Ok("success")

isOk : MyResult(ok, err) -> Bool
isOk = |result| match result {
	Ok(_) => Bool.True
	Err(_) => Bool.False
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.9 (ident "myResult"))
		(e-nominal @4.12-4.25 (nominal "Result")
			(e-tag @4.12-4.25 (name "Ok")
				(args
					(e-string @4.15-4.24
						(e-literal @4.16-4.23 (string "success"))))))
		(annotation @4.1-4.9
			(declared-type
				(ty-apply @3.12-3.30 (name "MyResult") (local)
					(ty-lookup @3.12-3.30 (name "Str") (builtin))
					(ty-lookup @3.12-3.30 (name "I32") (builtin))))))
	(d-let
		(p-assign @7.1-7.5 (ident "isOk"))
		(e-lambda @7.8-10.2
			(args
				(p-assign @7.9-7.15 (ident "result")))
			(e-match @7.17-10.2
				(match @7.17-10.2
					(cond
						(e-lookup-local @7.23-7.29
							(p-assign @7.9-7.15 (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal @8.5-8.10
										(p-applied-tag @8.5-8.10))))
							(value
								(e-nominal @8.14-8.23 (nominal "Bool")
									(e-tag @8.14-8.23 (name "True")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal @9.5-9.11
										(p-applied-tag @9.5-9.11))))
							(value
								(e-nominal @9.15-9.25 (nominal "Bool")
									(e-tag @9.15-9.25 (name "False")))))))))
		(annotation @7.1-7.5
			(declared-type
				(ty-fn @6.8-6.33 (effectful false)
					(ty-apply @6.8-6.25 (name "MyResult") (local)
						(ty-rigid-var @6.8-6.25 (name "ok"))
						(ty-rigid-var @6.8-6.25 (name "err")))
					(ty-lookup @6.29-6.33 (name "Bool") (local))))))
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
		(patt @4.1-4.9 (type "Error"))
		(patt @7.1-7.5 (type "MyResult(ok, err) -> Bool")))
	(type_decls
		(nominal @1.1-1.40 (type "MyResult(ok, err)")
			(ty-header @1.1-1.18 (name "MyResult")
				(ty-args
					(ty-rigid-var @1.10-1.12 (name "ok"))
					(ty-rigid-var @1.14-1.17 (name "err"))))))
	(expressions
		(expr @4.12-4.25 (type "Error"))
		(expr @7.8-10.2 (type "MyResult(ok, err) -> Bool"))))
~~~
