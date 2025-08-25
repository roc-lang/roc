# META
~~~ini
description=Type variables nested within complex type constructors
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "platform.roc" }

# Map over Result type
map_result : Result(a, e), (a -> b) -> Result(b, e)
map_result = |result, transform| {
    match result {
        Ok(value) => Ok(transform(value))
        Err(error) => Err(error)
    }
}

# Simple identity function with type variable
identity : a -> a
identity = |x| x

# Nested type variables in records
make_pair : a, b -> { first: a, second: b }
make_pair = |x, y| { first: x, second: y }

# Function that works with lists of any type
list_length : List(_a) -> U64
list_length = |_lst| 42

# Nested Result types
wrap_in_result : a -> Result(Result(a, Str), Str)
wrap_in_result = |value| Ok(Ok(value))

main = |_| "done"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:40),StringEnd(1:40-1:41),CloseCurly(1:42-1:43),
LowerIdent(4:1-4:11),OpColon(4:12-4:13),UpperIdent(4:14-4:20),NoSpaceOpenRound(4:20-4:21),LowerIdent(4:21-4:22),Comma(4:22-4:23),LowerIdent(4:24-4:25),CloseRound(4:25-4:26),Comma(4:26-4:27),OpenRound(4:28-4:29),LowerIdent(4:29-4:30),OpArrow(4:31-4:33),LowerIdent(4:34-4:35),CloseRound(4:35-4:36),OpArrow(4:37-4:39),UpperIdent(4:40-4:46),NoSpaceOpenRound(4:46-4:47),LowerIdent(4:47-4:48),Comma(4:48-4:49),LowerIdent(4:50-4:51),CloseRound(4:51-4:52),
LowerIdent(5:1-5:11),OpAssign(5:12-5:13),OpBar(5:14-5:15),LowerIdent(5:15-5:21),Comma(5:21-5:22),LowerIdent(5:23-5:32),OpBar(5:32-5:33),OpenCurly(5:34-5:35),
KwMatch(6:5-6:10),LowerIdent(6:11-6:17),OpenCurly(6:18-6:19),
UpperIdent(7:9-7:11),NoSpaceOpenRound(7:11-7:12),LowerIdent(7:12-7:17),CloseRound(7:17-7:18),OpFatArrow(7:19-7:21),UpperIdent(7:22-7:24),NoSpaceOpenRound(7:24-7:25),LowerIdent(7:25-7:34),NoSpaceOpenRound(7:34-7:35),LowerIdent(7:35-7:40),CloseRound(7:40-7:41),CloseRound(7:41-7:42),
UpperIdent(8:9-8:12),NoSpaceOpenRound(8:12-8:13),LowerIdent(8:13-8:18),CloseRound(8:18-8:19),OpFatArrow(8:20-8:22),UpperIdent(8:23-8:26),NoSpaceOpenRound(8:26-8:27),LowerIdent(8:27-8:32),CloseRound(8:32-8:33),
CloseCurly(9:5-9:6),
CloseCurly(10:1-10:2),
LowerIdent(13:1-13:9),OpColon(13:10-13:11),LowerIdent(13:12-13:13),OpArrow(13:14-13:16),LowerIdent(13:17-13:18),
LowerIdent(14:1-14:9),OpAssign(14:10-14:11),OpBar(14:12-14:13),LowerIdent(14:13-14:14),OpBar(14:14-14:15),LowerIdent(14:16-14:17),
LowerIdent(17:1-17:10),OpColon(17:11-17:12),LowerIdent(17:13-17:14),Comma(17:14-17:15),LowerIdent(17:16-17:17),OpArrow(17:18-17:20),OpenCurly(17:21-17:22),LowerIdent(17:23-17:28),OpColon(17:28-17:29),LowerIdent(17:30-17:31),Comma(17:31-17:32),LowerIdent(17:33-17:39),OpColon(17:39-17:40),LowerIdent(17:41-17:42),CloseCurly(17:43-17:44),
LowerIdent(18:1-18:10),OpAssign(18:11-18:12),OpBar(18:13-18:14),LowerIdent(18:14-18:15),Comma(18:15-18:16),LowerIdent(18:17-18:18),OpBar(18:18-18:19),OpenCurly(18:20-18:21),LowerIdent(18:22-18:27),OpColon(18:27-18:28),LowerIdent(18:29-18:30),Comma(18:30-18:31),LowerIdent(18:32-18:38),OpColon(18:38-18:39),LowerIdent(18:40-18:41),CloseCurly(18:42-18:43),
LowerIdent(21:1-21:12),OpColon(21:13-21:14),UpperIdent(21:15-21:19),NoSpaceOpenRound(21:19-21:20),NamedUnderscore(21:20-21:22),CloseRound(21:22-21:23),OpArrow(21:24-21:26),UpperIdent(21:27-21:30),
LowerIdent(22:1-22:12),OpAssign(22:13-22:14),OpBar(22:15-22:16),NamedUnderscore(22:16-22:20),OpBar(22:20-22:21),Int(22:22-22:24),
LowerIdent(25:1-25:15),OpColon(25:16-25:17),LowerIdent(25:18-25:19),OpArrow(25:20-25:22),UpperIdent(25:23-25:29),NoSpaceOpenRound(25:29-25:30),UpperIdent(25:30-25:36),NoSpaceOpenRound(25:36-25:37),LowerIdent(25:37-25:38),Comma(25:38-25:39),UpperIdent(25:40-25:43),CloseRound(25:43-25:44),Comma(25:44-25:45),UpperIdent(25:46-25:49),CloseRound(25:49-25:50),
LowerIdent(26:1-26:15),OpAssign(26:16-26:17),OpBar(26:18-26:19),LowerIdent(26:19-26:24),OpBar(26:24-26:25),UpperIdent(26:26-26:28),NoSpaceOpenRound(26:28-26:29),UpperIdent(26:29-26:31),NoSpaceOpenRound(26:31-26:32),LowerIdent(26:32-26:37),CloseRound(26:37-26:38),CloseRound(26:38-26:39),
LowerIdent(28:1-28:5),OpAssign(28:6-28:7),OpBar(28:8-28:9),Underscore(28:9-28:10),OpBar(28:10-28:11),StringStart(28:12-28:13),StringPart(28:13-28:17),StringEnd(28:17-28:18),EndOfFile(28:18-28:18),
~~~
# PARSE
~~~clojure
(file @1.1-28.18
	(app @1.1-1.43
		(provides @1.5-1.11
			(exposed-lower-ident @1.6-1.10
				(text "main")))
		(record-field @1.14-1.41 (name "pf")
			(e-string @1.27-1.41
				(e-string-part @1.28-1.40 (raw "platform.roc"))))
		(packages @1.12-1.43
			(record-field @1.14-1.41 (name "pf")
				(e-string @1.27-1.41
					(e-string-part @1.28-1.40 (raw "platform.roc"))))))
	(statements
		(s-type-anno @4.1-4.52 (name "map_result")
			(ty-fn @4.14-4.52
				(ty-apply @4.14-4.26
					(ty @4.14-4.20 (name "Result"))
					(ty-var @4.21-4.22 (raw "a"))
					(ty-var @4.24-4.25 (raw "e")))
				(ty-fn @4.29-4.35
					(ty-var @4.29-4.30 (raw "a"))
					(ty-var @4.34-4.35 (raw "b")))
				(ty-apply @4.40-4.52
					(ty @4.40-4.46 (name "Result"))
					(ty-var @4.47-4.48 (raw "b"))
					(ty-var @4.50-4.51 (raw "e")))))
		(s-decl @5.1-10.2
			(p-ident @5.1-5.11 (raw "map_result"))
			(e-lambda @5.14-10.2
				(args
					(p-ident @5.15-5.21 (raw "result"))
					(p-ident @5.23-5.32 (raw "transform")))
				(e-block @5.34-10.2
					(statements
						(e-match
							(e-ident @6.11-6.17 (raw "result"))
							(branches
								(branch @7.9-7.42
									(p-tag @7.9-7.18 (raw "Ok")
										(p-ident @7.12-7.17 (raw "value")))
									(e-apply @7.22-7.42
										(e-tag @7.22-7.24 (raw "Ok"))
										(e-apply @7.25-7.41
											(e-ident @7.25-7.34 (raw "transform"))
											(e-ident @7.35-7.40 (raw "value")))))
								(branch @8.9-8.33
									(p-tag @8.9-8.19 (raw "Err")
										(p-ident @8.13-8.18 (raw "error")))
									(e-apply @8.23-8.33
										(e-tag @8.23-8.26 (raw "Err"))
										(e-ident @8.27-8.32 (raw "error"))))))))))
		(s-type-anno @13.1-13.18 (name "identity")
			(ty-fn @13.12-13.18
				(ty-var @13.12-13.13 (raw "a"))
				(ty-var @13.17-13.18 (raw "a"))))
		(s-decl @14.1-14.17
			(p-ident @14.1-14.9 (raw "identity"))
			(e-lambda @14.12-14.17
				(args
					(p-ident @14.13-14.14 (raw "x")))
				(e-ident @14.16-14.17 (raw "x"))))
		(s-type-anno @17.1-17.44 (name "make_pair")
			(ty-fn @17.13-17.44
				(ty-var @17.13-17.14 (raw "a"))
				(ty-var @17.16-17.17 (raw "b"))
				(ty-record @17.21-17.44
					(anno-record-field @17.23-17.31 (name "first")
						(ty-var @17.30-17.31 (raw "a")))
					(anno-record-field @17.33-17.42 (name "second")
						(ty-var @17.41-17.42 (raw "b"))))))
		(s-decl @18.1-18.43
			(p-ident @18.1-18.10 (raw "make_pair"))
			(e-lambda @18.13-18.43
				(args
					(p-ident @18.14-18.15 (raw "x"))
					(p-ident @18.17-18.18 (raw "y")))
				(e-record @18.20-18.43
					(field (field "first")
						(e-ident @18.29-18.30 (raw "x")))
					(field (field "second")
						(e-ident @18.40-18.41 (raw "y"))))))
		(s-type-anno @21.1-21.30 (name "list_length")
			(ty-fn @21.15-21.30
				(ty-apply @21.15-21.23
					(ty @21.15-21.19 (name "List"))
					(underscore-ty-var @21.20-21.22 (raw "_a")))
				(ty @21.27-21.30 (name "U64"))))
		(s-decl @22.1-22.24
			(p-ident @22.1-22.12 (raw "list_length"))
			(e-lambda @22.15-22.24
				(args
					(p-ident @22.16-22.20 (raw "_lst")))
				(e-int @22.22-22.24 (raw "42"))))
		(s-type-anno @25.1-25.50 (name "wrap_in_result")
			(ty-fn @25.18-25.50
				(ty-var @25.18-25.19 (raw "a"))
				(ty-apply @25.23-25.50
					(ty @25.23-25.29 (name "Result"))
					(ty-apply @25.30-25.44
						(ty @25.30-25.36 (name "Result"))
						(ty-var @25.37-25.38 (raw "a"))
						(ty @25.40-25.43 (name "Str")))
					(ty @25.46-25.49 (name "Str")))))
		(s-decl @26.1-26.39
			(p-ident @26.1-26.15 (raw "wrap_in_result"))
			(e-lambda @26.18-26.39
				(args
					(p-ident @26.19-26.24 (raw "value")))
				(e-apply @26.26-26.39
					(e-tag @26.26-26.28 (raw "Ok"))
					(e-apply @26.29-26.38
						(e-tag @26.29-26.31 (raw "Ok"))
						(e-ident @26.32-26.37 (raw "value"))))))
		(s-decl @28.1-28.18
			(p-ident @28.1-28.5 (raw "main"))
			(e-lambda @28.8-28.18
				(args
					(p-underscore))
				(e-string @28.12-28.18
					(e-string-part @28.13-28.17 (raw "done")))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "platform.roc" }

# Map over Result type
map_result : Result(a, e), (a -> b) -> Result(b, e)
map_result = |result, transform| {
	match result {
		Ok(value) => Ok(transform(value))
		Err(error) => Err(error)
	}
}

# Simple identity function with type variable
identity : a -> a
identity = |x| x

# Nested type variables in records
make_pair : a, b -> { first : a, second : b }
make_pair = |x, y| { first: x, second: y }

# Function that works with lists of any type
list_length : List(_a) -> U64
list_length = |_lst| 42

# Nested Result types
wrap_in_result : a -> Result(Result(a, Str), Str)
wrap_in_result = |value| Ok(Ok(value))

main = |_| "done"
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @5.1-5.11 (ident "map_result"))
		(e-closure @5.14-10.2
			(captures
				(capture @8.13-8.18 (ident "error"))
				(capture @7.12-7.17 (ident "value")))
			(e-lambda @5.14-10.2
				(args
					(p-assign @5.15-5.21 (ident "result"))
					(p-assign @5.23-5.32 (ident "transform")))
				(e-block @5.34-10.2
					(e-match @6.5-9.6
						(match @6.5-9.6
							(cond
								(e-lookup-local @6.11-6.17
									(p-assign @5.15-5.21 (ident "result"))))
							(branches
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal @7.9-7.18
												(p-applied-tag @7.9-7.18))))
									(value
										(e-nominal @7.22-7.42 (nominal "Result")
											(e-tag @7.22-7.42 (name "Ok")
												(args
													(e-call @7.25-7.41
														(e-lookup-local @7.25-7.34
															(p-assign @5.23-5.32 (ident "transform")))
														(e-lookup-local @7.35-7.40
															(p-assign @7.12-7.17 (ident "value")))))))))
								(branch
									(patterns
										(pattern (degenerate false)
											(p-nominal @8.9-8.19
												(p-applied-tag @8.9-8.19))))
									(value
										(e-nominal @8.23-8.33 (nominal "Result")
											(e-tag @8.23-8.33 (name "Err")
												(args
													(e-lookup-local @8.27-8.32
														(p-assign @8.13-8.18 (ident "error"))))))))))))))
		(annotation @5.1-5.11
			(declared-type
				(ty-fn @4.14-4.52 (effectful false)
					(ty-apply @4.14-4.26 (symbol "Result")
						(ty-var @4.21-4.22 (name "a"))
						(ty-var @4.24-4.25 (name "e")))
					(ty-parens @4.28-4.36
						(ty-fn @4.29-4.35 (effectful false)
							(ty-var @4.29-4.30 (name "a"))
							(ty-var @4.34-4.35 (name "b"))))
					(ty-apply @4.40-4.52 (symbol "Result")
						(ty-var @4.47-4.48 (name "b"))
						(ty-var @4.50-4.51 (name "e")))))))
	(d-let
		(p-assign @14.1-14.9 (ident "identity"))
		(e-lambda @14.12-14.17
			(args
				(p-assign @14.13-14.14 (ident "x")))
			(e-lookup-local @14.16-14.17
				(p-assign @14.13-14.14 (ident "x"))))
		(annotation @14.1-14.9
			(declared-type
				(ty-fn @13.12-13.18 (effectful false)
					(ty-var @13.12-13.13 (name "a"))
					(ty-var @13.17-13.18 (name "a"))))))
	(d-let
		(p-assign @18.1-18.10 (ident "make_pair"))
		(e-lambda @18.13-18.43
			(args
				(p-assign @18.14-18.15 (ident "x"))
				(p-assign @18.17-18.18 (ident "y")))
			(e-record @18.20-18.43
				(fields
					(field (name "first")
						(e-lookup-local @18.29-18.30
							(p-assign @18.14-18.15 (ident "x"))))
					(field (name "second")
						(e-lookup-local @18.40-18.41
							(p-assign @18.17-18.18 (ident "y")))))))
		(annotation @18.1-18.10
			(declared-type
				(ty-fn @17.13-17.44 (effectful false)
					(ty-var @17.13-17.14 (name "a"))
					(ty-var @17.16-17.17 (name "b"))
					(ty-record @17.21-17.44
						(field (field "first")
							(ty-var @17.30-17.31 (name "a")))
						(field (field "second")
							(ty-var @17.41-17.42 (name "b"))))))))
	(d-let
		(p-assign @22.1-22.12 (ident "list_length"))
		(e-lambda @22.15-22.24
			(args
				(p-assign @22.16-22.20 (ident "_lst")))
			(e-int @22.22-22.24 (value "42")))
		(annotation @22.1-22.12
			(declared-type
				(ty-fn @21.15-21.30 (effectful false)
					(ty-apply @21.15-21.23 (symbol "List")
						(ty-var @21.20-21.22 (name "_a")))
					(ty @21.27-21.30 (name "U64"))))))
	(d-let
		(p-assign @26.1-26.15 (ident "wrap_in_result"))
		(e-lambda @26.18-26.39
			(args
				(p-assign @26.19-26.24 (ident "value")))
			(e-nominal @26.26-26.39 (nominal "Result")
				(e-tag @26.26-26.39 (name "Ok")
					(args
						(e-nominal @26.29-26.38 (nominal "Result")
							(e-tag @26.29-26.38 (name "Ok")
								(args
									(e-lookup-local @26.32-26.37
										(p-assign @26.19-26.24 (ident "value"))))))))))
		(annotation @26.1-26.15
			(declared-type
				(ty-fn @25.18-25.50 (effectful false)
					(ty-var @25.18-25.19 (name "a"))
					(ty-apply @25.23-25.50 (symbol "Result")
						(ty-apply @25.30-25.44 (symbol "Result")
							(ty-var @25.37-25.38 (name "a"))
							(ty @25.40-25.43 (name "Str")))
						(ty @25.46-25.49 (name "Str")))))))
	(d-let
		(p-assign @28.1-28.5 (ident "main"))
		(e-lambda @28.8-28.18
			(args
				(p-underscore @28.9-28.10))
			(e-string @28.12-28.18
				(e-literal @28.13-28.17 (string "done"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @5.1-5.11 (type "Result(a, err), a -> b -> Result(ok, err)"))
		(patt @14.1-14.9 (type "a -> a"))
		(patt @18.1-18.10 (type "a, b -> { first: a, second: b }"))
		(patt @22.1-22.12 (type "List(_a) -> U64"))
		(patt @26.1-26.15 (type "a -> Result(Error, err)"))
		(patt @28.1-28.5 (type "_arg -> Str")))
	(expressions
		(expr @5.14-10.2 (type "Result(a, err), a -> b -> Result(ok, err)"))
		(expr @14.12-14.17 (type "a -> a"))
		(expr @18.13-18.43 (type "a, b -> { first: a, second: b }"))
		(expr @22.15-22.24 (type "List(_a) -> U64"))
		(expr @26.18-26.39 (type "a -> Result(Error, err)"))
		(expr @28.8-28.18 (type "_arg -> Str"))))
~~~
