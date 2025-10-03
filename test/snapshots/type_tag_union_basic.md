# META
~~~ini
description=Basic tag union type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

process : [Some(Str), None] -> Str
process = |_maybe| "result"

is_ok_ret_unqualified_bool : [Ok2(_ok), Err2(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result {
    Ok2(_) => True
    Err2(_) => False
}

is_ok_ret_bool : [Ok2(_ok2), Err2(_err2)] -> Bool
is_ok_ret_bool = |result| match result {
    Ok2(_) => Bool.True
    Err2(_) => Bool.False
}

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
LowerIdent(3:1-3:8),OpColon(3:9-3:10),OpenSquare(3:11-3:12),UpperIdent(3:12-3:16),NoSpaceOpenRound(3:16-3:17),UpperIdent(3:17-3:20),CloseRound(3:20-3:21),Comma(3:21-3:22),UpperIdent(3:23-3:27),CloseSquare(3:27-3:28),OpArrow(3:29-3:31),UpperIdent(3:32-3:35),
LowerIdent(4:1-4:8),OpAssign(4:9-4:10),OpBar(4:11-4:12),NamedUnderscore(4:12-4:18),OpBar(4:18-4:19),StringStart(4:20-4:21),StringPart(4:21-4:27),StringEnd(4:27-4:28),
LowerIdent(6:1-6:27),OpColon(6:28-6:29),OpenSquare(6:30-6:31),UpperIdent(6:31-6:34),NoSpaceOpenRound(6:34-6:35),NamedUnderscore(6:35-6:38),CloseRound(6:38-6:39),Comma(6:39-6:40),UpperIdent(6:41-6:45),NoSpaceOpenRound(6:45-6:46),NamedUnderscore(6:46-6:50),CloseRound(6:50-6:51),CloseSquare(6:51-6:52),OpArrow(6:53-6:55),UpperIdent(6:56-6:60),
LowerIdent(7:1-7:27),OpAssign(7:28-7:29),OpBar(7:30-7:31),LowerIdent(7:31-7:37),OpBar(7:37-7:38),KwMatch(7:39-7:44),LowerIdent(7:45-7:51),OpenCurly(7:52-7:53),
UpperIdent(8:5-8:8),NoSpaceOpenRound(8:8-8:9),Underscore(8:9-8:10),CloseRound(8:10-8:11),OpFatArrow(8:12-8:14),UpperIdent(8:15-8:19),
UpperIdent(9:5-9:9),NoSpaceOpenRound(9:9-9:10),Underscore(9:10-9:11),CloseRound(9:11-9:12),OpFatArrow(9:13-9:15),UpperIdent(9:16-9:21),
CloseCurly(10:1-10:2),
LowerIdent(12:1-12:15),OpColon(12:16-12:17),OpenSquare(12:18-12:19),UpperIdent(12:19-12:22),NoSpaceOpenRound(12:22-12:23),NamedUnderscore(12:23-12:27),CloseRound(12:27-12:28),Comma(12:28-12:29),UpperIdent(12:30-12:34),NoSpaceOpenRound(12:34-12:35),NamedUnderscore(12:35-12:40),CloseRound(12:40-12:41),CloseSquare(12:41-12:42),OpArrow(12:43-12:45),UpperIdent(12:46-12:50),
LowerIdent(13:1-13:15),OpAssign(13:16-13:17),OpBar(13:18-13:19),LowerIdent(13:19-13:25),OpBar(13:25-13:26),KwMatch(13:27-13:32),LowerIdent(13:33-13:39),OpenCurly(13:40-13:41),
UpperIdent(14:5-14:8),NoSpaceOpenRound(14:8-14:9),Underscore(14:9-14:10),CloseRound(14:10-14:11),OpFatArrow(14:12-14:14),UpperIdent(14:15-14:19),NoSpaceDotUpperIdent(14:19-14:24),
UpperIdent(15:5-15:9),NoSpaceOpenRound(15:9-15:10),Underscore(15:10-15:11),CloseRound(15:11-15:12),OpFatArrow(15:13-15:15),UpperIdent(15:16-15:20),NoSpaceDotUpperIdent(15:20-15:26),
CloseCurly(16:1-16:2),
LowerIdent(18:1-18:6),OpAssign(18:7-18:8),OpBar(18:9-18:10),Underscore(18:10-18:11),OpBar(18:11-18:12),OpenCurly(18:13-18:14),CloseCurly(18:14-18:15),
EndOfFile(19:1-19:1),
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
		(s-decl @4.1-4.28
			(p-ident @4.1-4.8 (raw "process"))
			(e-lambda @4.11-4.28
				(args
					(p-ident @4.12-4.18 (raw "_maybe")))
				(e-string @4.20-4.28
					(e-string-part @4.21-4.27 (raw "result")))))
		(s-type-anno @6.1-6.60 (name "is_ok_ret_unqualified_bool")
			(ty-fn @6.30-6.60
				(ty-tag-union @6.30-6.52
					(tags
						(ty-apply @6.31-6.39
							(ty @6.31-6.34 (name "Ok2"))
							(underscore-ty-var @6.35-6.38 (raw "_ok")))
						(ty-apply @6.41-6.51
							(ty @6.41-6.45 (name "Err2"))
							(underscore-ty-var @6.46-6.50 (raw "_err")))))
				(ty @6.56-6.60 (name "Bool"))))
		(s-decl @7.1-10.2
			(p-ident @7.1-7.27 (raw "is_ok_ret_unqualified_bool"))
			(e-lambda @7.30-10.2
				(args
					(p-ident @7.31-7.37 (raw "result")))
				(e-match
					(e-ident @7.45-7.51 (raw "result"))
					(branches
						(branch @8.5-8.19
							(p-tag @8.5-8.11 (raw "Ok2")
								(p-underscore))
							(e-tag @8.15-8.19 (raw "True")))
						(branch @9.5-9.21
							(p-tag @9.5-9.12 (raw "Err2")
								(p-underscore))
							(e-tag @9.16-9.21 (raw "False")))))))
		(s-type-anno @12.1-12.50 (name "is_ok_ret_bool")
			(ty-fn @12.18-12.50
				(ty-tag-union @12.18-12.42
					(tags
						(ty-apply @12.19-12.28
							(ty @12.19-12.22 (name "Ok2"))
							(underscore-ty-var @12.23-12.27 (raw "_ok2")))
						(ty-apply @12.30-12.41
							(ty @12.30-12.34 (name "Err2"))
							(underscore-ty-var @12.35-12.40 (raw "_err2")))))
				(ty @12.46-12.50 (name "Bool"))))
		(s-decl @13.1-16.2
			(p-ident @13.1-13.15 (raw "is_ok_ret_bool"))
			(e-lambda @13.18-16.2
				(args
					(p-ident @13.19-13.25 (raw "result")))
				(e-match
					(e-ident @13.33-13.39 (raw "result"))
					(branches
						(branch @14.5-14.24
							(p-tag @14.5-14.11 (raw "Ok2")
								(p-underscore))
							(e-tag @14.15-14.24 (raw "Bool.True")))
						(branch @15.5-15.26
							(p-tag @15.5-15.12 (raw "Err2")
								(p-underscore))
							(e-tag @15.16-15.26 (raw "Bool.False")))))))
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
process = |_maybe| "result"

is_ok_ret_unqualified_bool : [Ok2(_ok), Err2(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result {
	Ok2(_) => True
	Err2(_) => False
}

is_ok_ret_bool : [Ok2(_ok2), Err2(_err2)] -> Bool
is_ok_ret_bool = |result| match result {
	Ok2(_) => Bool.True
	Err2(_) => Bool.False
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.8 (ident "process"))
		(e-lambda @4.11-4.28
			(args
				(p-assign @4.12-4.18 (ident "_maybe")))
			(e-string @4.20-4.28
				(e-literal @4.21-4.27 (string "result"))))
		(annotation @4.1-4.8
			(declared-type
				(ty-fn @3.11-3.35 (effectful false)
					(ty-tag-union @3.11-3.28
						(ty-tag-name @3.12-3.21 (name "Some")
							(ty-lookup @3.17-3.20 (name "Str") (builtin)))
						(ty-tag-name @3.23-3.27 (name "None")))
					(ty-lookup @3.32-3.35 (name "Str") (builtin))))))
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
									(p-applied-tag @8.5-8.11)))
							(value
								(e-nominal @8.15-8.19 (nominal "Bool")
									(e-tag @8.15-8.19 (name "True")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @9.5-9.12)))
							(value
								(e-nominal @9.16-9.21 (nominal "Bool")
									(e-tag @9.16-9.21 (name "False")))))))))
		(annotation @7.1-7.27
			(declared-type
				(ty-fn @6.30-6.60 (effectful false)
					(ty-tag-union @6.30-6.52
						(ty-tag-name @6.31-6.39 (name "Ok2")
							(ty-rigid-var @6.35-6.38 (name "_ok")))
						(ty-tag-name @6.41-6.51 (name "Err2")
							(ty-rigid-var @6.46-6.50 (name "_err"))))
					(ty-lookup @6.56-6.60 (name "Bool") (local))))))
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
									(p-applied-tag @14.5-14.11)))
							(value
								(e-nominal @14.15-14.24 (nominal "Bool")
									(e-tag @14.15-14.24 (name "True")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag @15.5-15.12)))
							(value
								(e-nominal @15.16-15.26 (nominal "Bool")
									(e-tag @15.16-15.26 (name "False")))))))))
		(annotation @13.1-13.15
			(declared-type
				(ty-fn @12.18-12.50 (effectful false)
					(ty-tag-union @12.18-12.42
						(ty-tag-name @12.19-12.28 (name "Ok2")
							(ty-rigid-var @12.23-12.27 (name "_ok2")))
						(ty-tag-name @12.30-12.41 (name "Err2")
							(ty-rigid-var @12.35-12.40 (name "_err2"))))
					(ty-lookup @12.46-12.50 (name "Bool") (local))))))
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
		(patt @7.1-7.27 (type "[Err2(_err), Ok2(_ok)] -> Bool"))
		(patt @13.1-13.15 (type "[Err2(_err2), Ok2(_ok2)] -> Bool"))
		(patt @18.1-18.6 (type "_arg -> {}")))
	(expressions
		(expr @4.11-4.28 (type "[None, Some(Str)] -> Str"))
		(expr @7.30-10.2 (type "[Err2(_err), Ok2(_ok)] -> Bool"))
		(expr @13.18-16.2 (type "[Err2(_err2), Ok2(_ok2)] -> Bool"))
		(expr @18.9-18.15 (type "_arg -> {}"))))
~~~
