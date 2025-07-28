# META
~~~ini
description=Basic tag union type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

process : [Some(Str), None] -> Str
process = |maybe| "result"

is_ok_ret_unqualified_bool : [Ok(_ok), Err(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result {
    Ok(_) => True
    Err(_) => False
}

is_ok_ret_bool : [Ok(_ok2), Err(_err2)] -> Bool
is_ok_ret_bool = |result| match result {
    Ok(_) => Bool.True
    Err(_) => Bool.False
}

main! = |_| {}
~~~
# EXPECTED
UNUSED VARIABLE - type_tag_union_basic.md:4:12:4:17
# PROBLEMS
**UNUSED VARIABLE**
Variable `maybe` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_maybe` to suppress this warning.
The unused variable is declared here:
**type_tag_union_basic.md:4:12:4:17:**
```roc
process = |maybe| "result"
```
           ^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),OpenSquare(3:11-3:12),UpperIdent(3:12-3:16),NoSpaceOpenRound(3:16-3:17),UpperIdent(3:17-3:20),CloseRound(3:20-3:21),Comma(3:21-3:22),UpperIdent(3:23-3:27),CloseSquare(3:27-3:28),OpArrow(3:29-3:31),UpperIdent(3:32-3:35),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),LowerIdent(4:12-4:17),OpBar(4:17-4:18),StringStart(4:19-4:20),StringPart(4:20-4:26),StringEnd(4:26-4:27),
LowerIdent(6:1-6:27),OpColon(6:28-6:29),OpenSquare(6:30-6:31),UpperIdent(6:31-6:33),NoSpaceOpenRound(6:33-6:34),NamedUnderscore(6:34-6:37),CloseRound(6:37-6:38),Comma(6:38-6:39),UpperIdent(6:40-6:43),NoSpaceOpenRound(6:43-6:44),NamedUnderscore(6:44-6:48),CloseRound(6:48-6:49),CloseSquare(6:49-6:50),OpArrow(6:51-6:53),UpperIdent(6:54-6:58),
LowerIdent(7:1-7:27),OpAssign(7:28-7:29),OpBar(7:30-7:31),LowerIdent(7:31-7:37),OpBar(7:37-7:38),KwMatch(7:39-7:44),LowerIdent(7:45-7:51),OpenCurly(7:52-7:53),
UpperIdent(8:5-8:7),NoSpaceOpenRound(8:7-8:8),Underscore(8:8-8:9),CloseRound(8:9-8:10),OpFatArrow(8:11-8:13),UpperIdent(8:14-8:18),
UpperIdent(9:5-9:8),NoSpaceOpenRound(9:8-9:9),Underscore(9:9-9:10),CloseRound(9:10-9:11),OpFatArrow(9:12-9:14),UpperIdent(9:15-9:20),
CloseCurly(10:1-10:2),
LowerIdent(12:1-12:15),OpColon(12:16-12:17),OpenSquare(12:18-12:19),UpperIdent(12:19-12:21),NoSpaceOpenRound(12:21-12:22),NamedUnderscore(12:22-12:26),CloseRound(12:26-12:27),Comma(12:27-12:28),UpperIdent(12:29-12:32),NoSpaceOpenRound(12:32-12:33),NamedUnderscore(12:33-12:38),CloseRound(12:38-12:39),CloseSquare(12:39-12:40),OpArrow(12:41-12:43),UpperIdent(12:44-12:48),
LowerIdent(13:1-13:15),OpAssign(13:16-13:17),OpBar(13:18-13:19),LowerIdent(13:19-13:25),OpBar(13:25-13:26),KwMatch(13:27-13:32),LowerIdent(13:33-13:39),OpenCurly(13:40-13:41),
UpperIdent(14:5-14:7),NoSpaceOpenRound(14:7-14:8),Underscore(14:8-14:9),CloseRound(14:9-14:10),OpFatArrow(14:11-14:13),UpperIdent(14:14-14:18),NoSpaceDotUpperIdent(14:18-14:23),
UpperIdent(15:5-15:8),NoSpaceOpenRound(15:8-15:9),Underscore(15:9-15:10),CloseRound(15:10-15:11),OpFatArrow(15:12-15:14),UpperIdent(15:15-15:19),NoSpaceDotUpperIdent(15:19-15:25),
CloseCurly(16:1-16:2),
LowerIdent(18:1-18:6),OpAssign(18:7-18:8),OpBar(18:9-18:10),Underscore(18:10-18:11),OpBar(18:11-18:12),OpenCurly(18:13-18:14),CloseCurly(18:14-18:15),EndOfFile(18:15-18:15),
~~~
# PARSE
~~~clojure
(file @1.1-18.15
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
		(s-type-anno @3.1-3.35 (name "process")
			(ty-fn @3.11-3.35
				(ty-tag-union @3.11-3.28
					(tags
						(ty-apply @3.12-3.21
							(ty @3.12-3.16 (name "Some"))
							(ty @3.17-3.20 (name "Str")))
						(ty @3.23-3.27 (name "None"))))
				(ty @3.32-3.35 (name "Str"))))
		(s-decl @4.1-4.27
			(p-ident @4.1-4.8 (raw "process"))
			(e-lambda @4.11-4.27
				(args
					(p-ident @4.12-4.17 (raw "maybe")))
				(e-string @4.19-4.27
					(e-string-part @4.20-4.26 (raw "result")))))
		(s-type-anno @6.1-6.58 (name "is_ok_ret_unqualified_bool")
			(ty-fn @6.30-6.58
				(ty-tag-union @6.30-6.50
					(tags
						(ty-apply @6.31-6.38
							(ty @6.31-6.33 (name "Ok"))
							(underscore-ty-var @6.34-6.37 (raw "_ok")))
						(ty-apply @6.40-6.49
							(ty @6.40-6.43 (name "Err"))
							(underscore-ty-var @6.44-6.48 (raw "_err")))))
				(ty @6.54-6.58 (name "Bool"))))
		(s-decl @7.1-10.2
			(p-ident @7.1-7.27 (raw "is_ok_ret_unqualified_bool"))
			(e-lambda @7.30-10.2
				(args
					(p-ident @7.31-7.37 (raw "result")))
				(e-match
					(e-ident @7.45-7.51 (raw "result"))
					(branches
						(branch @8.5-8.18
							(p-tag @8.5-8.10 (raw "Ok")
								(p-underscore))
							(e-tag @8.14-8.18 (raw "True")))
						(branch @9.5-9.20
							(p-tag @9.5-9.11 (raw "Err")
								(p-underscore))
							(e-tag @9.15-9.20 (raw "False")))))))
		(s-type-anno @12.1-12.48 (name "is_ok_ret_bool")
			(ty-fn @12.18-12.48
				(ty-tag-union @12.18-12.40
					(tags
						(ty-apply @12.19-12.27
							(ty @12.19-12.21 (name "Ok"))
							(underscore-ty-var @12.22-12.26 (raw "_ok2")))
						(ty-apply @12.29-12.39
							(ty @12.29-12.32 (name "Err"))
							(underscore-ty-var @12.33-12.38 (raw "_err2")))))
				(ty @12.44-12.48 (name "Bool"))))
		(s-decl @13.1-16.2
			(p-ident @13.1-13.15 (raw "is_ok_ret_bool"))
			(e-lambda @13.18-16.2
				(args
					(p-ident @13.19-13.25 (raw "result")))
				(e-match
					(e-ident @13.33-13.39 (raw "result"))
					(branches
						(branch @14.5-14.23
							(p-tag @14.5-14.10 (raw "Ok")
								(p-underscore))
							(e-tag @14.14-14.23 (raw "Bool.True")))
						(branch @15.5-15.25
							(p-tag @15.5-15.11 (raw "Err")
								(p-underscore))
							(e-tag @15.15-15.25 (raw "Bool.False")))))))
		(s-decl @18.1-18.15
			(p-ident @18.1-18.6 (raw "main!"))
			(e-lambda @18.9-18.15
				(args
					(p-underscore))
				(e-record @18.13-18.15)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

process : [Some(Str), None] -> Str
process = |maybe| "result"

is_ok_ret_unqualified_bool : [Ok(_ok), Err(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result {
	Ok(_) => True
	Err(_) => False
}

is_ok_ret_bool : [Ok(_ok2), Err(_err2)] -> Bool
is_ok_ret_bool = |result| match result {
	Ok(_) => Bool.True
	Err(_) => Bool.False
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "process"))
		(e-lambda @4.11-4.27
			(args
				(p-assign @4.12-4.17 (ident "maybe")))
			(e-string @4.19-4.27
				(e-literal @4.20-4.26 (string "result"))))
		(annotation @4.1-4.8
			(declared-type
				(ty-fn @3.11-3.35 (effectful false)
					(ty-tag-union @3.11-3.28
						(ty-apply @3.12-3.21 (symbol "some")
							(ty @3.17-3.20 (name "str")))
						(ty @3.23-3.27 (name "none")))
					(ty @3.32-3.35 (name "str"))))))
	(d-let
		(p-assign @7.1-7.27 (ident "is_ok_ret_unqualified_bool"))
		(e-lambda @7.30-10.2
			(args
				(p-assign @7.31-7.37 (ident "result")))
			(e-match @7.39-10.2
				(match @7.39-10.2
					(cond
						(e-lookup-local @7.45-7.51
							(p-assign @7.31-7.37 (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @8.5-8.10)))
							(value
								(e-nominal @8.14-8.18 (nominal "Bool")
									(e-tag @8.14-8.18 (name "True")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @9.5-9.11)))
							(value
								(e-nominal @9.15-9.20 (nominal "Bool")
									(e-tag @9.15-9.20 (name "False")))))))))
		(annotation @7.1-7.27
			(declared-type
				(ty-fn @6.30-6.58 (effectful false)
					(ty-tag-union @6.30-6.50
						(ty-apply @6.31-6.38 (symbol "ok")
							(ty-var @6.34-6.37 (name "_ok")))
						(ty-apply @6.40-6.49 (symbol "err")
							(ty-var @6.44-6.48 (name "_err"))))
					(ty @6.54-6.58 (name "bool"))))))
	(d-let
		(p-assign @13.1-13.15 (ident "is_ok_ret_bool"))
		(e-lambda @13.18-16.2
			(args
				(p-assign @13.19-13.25 (ident "result")))
			(e-match @13.27-16.2
				(match @13.27-16.2
					(cond
						(e-lookup-local @13.33-13.39
							(p-assign @13.19-13.25 (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @14.5-14.10)))
							(value
								(e-nominal @14.14-14.18 (nominal "Bool")
									(e-tag @14.14-14.23 (name "True")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @15.5-15.11)))
							(value
								(e-nominal @15.15-15.19 (nominal "Bool")
									(e-tag @15.15-15.25 (name "False")))))))))
		(annotation @13.1-13.15
			(declared-type
				(ty-fn @12.18-12.48 (effectful false)
					(ty-tag-union @12.18-12.40
						(ty-apply @12.19-12.27 (symbol "ok")
							(ty-var @12.22-12.26 (name "_ok2")))
						(ty-apply @12.29-12.39 (symbol "err")
							(ty-var @12.33-12.38 (name "_err2"))))
					(ty @12.44-12.48 (name "bool"))))))
	(d-let
		(p-assign @18.1-18.6 (ident "main!"))
		(e-lambda @18.9-18.15
			(args
				(p-underscore @18.10-18.11))
			(e-empty_record @18.13-18.15))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.8 (type "[None, Some(Str)] -> Str"))
		(patt @7.1-7.27 (type "[Err(_err), Ok(_ok)] -> Bool"))
		(patt @13.1-13.15 (type "[Err(_err2), Ok(_ok2)] -> Bool"))
		(patt @18.1-18.6 (type "_arg -> {}")))
	(expressions
		(expr @4.11-4.27 (type "[None, Some(Str)] -> Str"))
		(expr @7.30-10.2 (type "[Err(_err), Ok(_ok)] -> Bool"))
		(expr @13.18-16.2 (type "[Err(_err2), Ok(_ok2)] -> Bool"))
		(expr @18.9-18.15 (type "_arg -> {}"))))
~~~
