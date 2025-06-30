# META
~~~ini
description=Multiple type aliases with cross-references
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

UserId : U64
UserName : Str
UserAge : U8
User : { id: UserId, name: UserName, age: UserAge }

createUser : UserId, UserName, UserAge -> User
createUser = |id, name, age| { id, name, age }

getUserName : User -> UserName
getUserName = |user| user.name

main! = |_| {
    user = createUser(123, "Alice", 25)
    getUserName(user)
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),Newline(1:1-1:1),
Newline(1:1-1:1),
UpperIdent(3:1-3:7),OpColon(3:8-3:9),UpperIdent(3:10-3:13),Newline(1:1-1:1),
UpperIdent(4:1-4:9),OpColon(4:10-4:11),UpperIdent(4:12-4:15),Newline(1:1-1:1),
UpperIdent(5:1-5:8),OpColon(5:9-5:10),UpperIdent(5:11-5:13),Newline(1:1-1:1),
UpperIdent(6:1-6:5),OpColon(6:6-6:7),OpenCurly(6:8-6:9),LowerIdent(6:10-6:12),OpColon(6:12-6:13),UpperIdent(6:14-6:20),Comma(6:20-6:21),LowerIdent(6:22-6:26),OpColon(6:26-6:27),UpperIdent(6:28-6:36),Comma(6:36-6:37),LowerIdent(6:38-6:41),OpColon(6:41-6:42),UpperIdent(6:43-6:50),CloseCurly(6:51-6:52),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(8:1-8:11),OpColon(8:12-8:13),UpperIdent(8:14-8:20),Comma(8:20-8:21),UpperIdent(8:22-8:30),Comma(8:30-8:31),UpperIdent(8:32-8:39),OpArrow(8:40-8:42),UpperIdent(8:43-8:47),Newline(1:1-1:1),
LowerIdent(9:1-9:11),OpAssign(9:12-9:13),OpBar(9:14-9:15),LowerIdent(9:15-9:17),Comma(9:17-9:18),LowerIdent(9:19-9:23),Comma(9:23-9:24),LowerIdent(9:25-9:28),OpBar(9:28-9:29),OpenCurly(9:30-9:31),LowerIdent(9:32-9:34),Comma(9:34-9:35),LowerIdent(9:36-9:40),Comma(9:40-9:41),LowerIdent(9:42-9:45),CloseCurly(9:46-9:47),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(11:1-11:12),OpColon(11:13-11:14),UpperIdent(11:15-11:19),OpArrow(11:20-11:22),UpperIdent(11:23-11:31),Newline(1:1-1:1),
LowerIdent(12:1-12:12),OpAssign(12:13-12:14),OpBar(12:15-12:16),LowerIdent(12:16-12:20),OpBar(12:20-12:21),LowerIdent(12:22-12:26),NoSpaceDotLowerIdent(12:26-12:31),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(14:1-14:6),OpAssign(14:7-14:8),OpBar(14:9-14:10),Underscore(14:10-14:11),OpBar(14:11-14:12),OpenCurly(14:13-14:14),Newline(1:1-1:1),
LowerIdent(15:5-15:9),OpAssign(15:10-15:11),LowerIdent(15:12-15:22),NoSpaceOpenRound(15:22-15:23),Int(15:23-15:26),Comma(15:26-15:27),StringStart(15:28-15:29),StringPart(15:29-15:34),StringEnd(15:34-15:35),Comma(15:35-15:36),Int(15:37-15:39),CloseRound(15:39-15:40),Newline(1:1-1:1),
LowerIdent(16:5-16:16),NoSpaceOpenRound(16:16-16:17),LowerIdent(16:17-16:21),CloseRound(16:21-16:22),Newline(1:1-1:1),
CloseCurly(17:1-17:2),EndOfFile(17:2-17:2),
~~~
# PARSE
~~~clojure
(file @1.1-17.2
	(app @1.1-1.57
		(provides @1.6-1.12
			(exposed-lower-ident (text "main!")))
		(record-field @1.15-1.57 (name "pf")
			(e-string @1.28-1.55
				(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))
		(packages @1.13-1.57
			(record-field @1.15-1.57 (name "pf")
				(e-string @1.28-1.55
					(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-decl @3.1-4.9
			(header @3.1-3.7 (name "UserId")
				(args))
			(ty (name "U64")))
		(s-type-decl @4.1-5.8
			(header @4.1-4.9 (name "UserName")
				(args))
			(ty (name "Str")))
		(s-type-decl @5.1-6.5
			(header @5.1-5.8 (name "UserAge")
				(args))
			(ty (name "U8")))
		(s-type-decl @6.1-8.11
			(header @6.1-6.5 (name "User")
				(args))
			(ty-record @6.8-6.52
				(anno-record-field @6.10-6.21 (name "id")
					(ty (name "UserId")))
				(anno-record-field @6.22-6.37 (name "name")
					(ty (name "UserName")))
				(anno-record-field @6.38-6.52 (name "age")
					(ty (name "UserAge")))))
		(s-type-anno @8.1-9.11 (name "createUser")
			(ty-fn @8.14-8.47
				(ty (name "UserId"))
				(ty (name "UserName"))
				(ty (name "UserAge"))
				(ty (name "User"))))
		(s-decl @9.1-9.47
			(p-ident @9.1-9.11 (raw "createUser"))
			(e-lambda @9.14-9.47
				(args
					(p-ident @9.15-9.17 (raw "id"))
					(p-ident @9.19-9.23 (raw "name"))
					(p-ident @9.25-9.28 (raw "age")))
				(e-record @9.30-9.47
					(field (field "id") (optional false))
					(field (field "name") (optional false))
					(field (field "age") (optional false)))))
		(s-type-anno @11.1-12.12 (name "getUserName")
			(ty-fn @11.15-11.31
				(ty (name "User"))
				(ty (name "UserName"))))
		(s-decl @12.1-14.6
			(p-ident @12.1-12.12 (raw "getUserName"))
			(e-lambda @12.15-14.6
				(args
					(p-ident @12.16-12.20 (raw "user")))
				(e-field-access @12.22-14.6
					(e-ident @12.22-12.26 (qaul "") (raw "user"))
					(e-ident @12.26-12.31 (qaul "") (raw ".name")))))
		(s-decl @14.1-17.2
			(p-ident @14.1-14.6 (raw "main!"))
			(e-lambda @14.9-17.2
				(args
					(p-underscore))
				(e-block @14.13-17.2
					(statements
						(s-decl @15.5-15.40
							(p-ident @15.5-15.9 (raw "user"))
							(e-apply @15.12-15.40
								(e-ident @15.12-15.22 (qaul "") (raw "createUser"))
								(e-int @15.23-15.26 (raw "123"))
								(e-string @15.28-15.35
									(e-string-part @15.29-15.34 (raw "Alice")))
								(e-int @15.37-15.39 (raw "25"))))
						(e-apply @16.5-16.22
							(e-ident @16.5-16.16 (qaul "") (raw "getUserName"))
							(e-ident @16.17-16.21 (qaul "") (raw "user")))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/platform.roc" }

UserId : U64
UserName : Str
UserAge : U8
User : { id : UserId, name : UserName, age : UserAge }

createUser : UserId, UserName, UserAge -> User
createUser = |id, name, age| { id, name, age }

getUserName : User -> UserName
getUserName = |user| user.name

main! = |_| {
	user = createUser(123, "Alice", 25)
	getUserName(user)
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @9.1-9.11 (ident "createUser"))
		(e-lambda @9.14-9.47
			(args
				(p-assign @9.15-9.17 (ident "id"))
				(p-assign @9.19-9.23 (ident "name"))
				(p-assign @9.25-9.28 (ident "age")))
			(e-record @9.30-9.47
				(fields
					(field (name "id")
						(e-lookup-local @9.32-9.35
							(pattern @9.15-9.17)))
					(field (name "name")
						(e-lookup-local @9.36-9.41
							(pattern @9.19-9.23)))
					(field (name "age")
						(e-lookup-local @9.42-9.47
							(pattern @9.25-9.28))))))
		(annotation @9.1-9.11
			(declared-type
				(ty-fn @8.14-8.47 (effectful false)
					(ty @8.14-8.20 (name "UserId"))
					(ty @8.22-8.30 (name "UserName"))
					(ty @8.32-8.39 (name "UserAge"))
					(ty @8.43-8.47 (name "User"))))))
	(d-let
		(p-assign @12.1-12.12 (ident "getUserName"))
		(e-lambda @12.15-14.6
			(args
				(p-assign @12.16-12.20 (ident "user")))
			(e-dot-access @12.22-14.6 (field "name")
				(receiver
					(e-lookup-local @12.22-12.26
						(pattern @12.16-12.20)))))
		(annotation @12.1-12.12
			(declared-type
				(ty-fn @11.15-11.31 (effectful false)
					(ty @11.15-11.19 (name "User"))
					(ty @11.23-11.31 (name "UserName"))))))
	(d-let
		(p-assign @14.1-14.6 (ident "main!"))
		(e-lambda @14.9-17.2
			(args
				(p-underscore @14.10-14.11))
			(e-block @14.13-17.2
				(s-let @15.5-15.40
					(p-assign @15.5-15.9 (ident "user"))
					(e-call @15.12-15.40
						(e-lookup-local @15.12-15.22
							(pattern @9.1-9.11))
						(e-int @15.23-15.26 (value "123"))
						(e-string @15.28-15.35
							(e-literal @15.29-15.34 (string "Alice")))
						(e-int @15.37-15.39 (value "25"))))
				(e-call @16.5-16.22
					(e-lookup-local @16.5-16.16
						(pattern @12.1-12.12))
					(e-lookup-local @16.17-16.21
						(pattern @15.5-15.9))))))
	(s-type-decl @3.1-4.9
		(ty-header @3.1-3.7 (name "UserId"))
		(ty @3.10-3.13 (name "U64")))
	(s-type-decl @4.1-5.8
		(ty-header @4.1-4.9 (name "UserName"))
		(ty @4.12-4.15 (name "Str")))
	(s-type-decl @5.1-6.5
		(ty-header @5.1-5.8 (name "UserAge"))
		(ty @5.11-5.13 (name "U8")))
	(s-type-decl @6.1-8.11
		(ty-header @6.1-6.5 (name "User"))
		(ty-record @6.8-6.52
			(field (field "id")
				(ty @6.14-6.20 (name "UserId")))
			(field (field "name")
				(ty @6.28-6.36 (name "UserName")))
			(field (field "age")
				(ty @6.43-6.50 (name "UserAge"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(d-assign @9.1-9.11 (type "UserId, UserName, UserAge -> { id: *, name: *, age: * }"))
		(d-assign @12.1-12.12 (type "User -> UserName"))
		(d-assign @14.1-14.6 (type "* ? *")))
	(expressions
		(expr @9.14-9.47 (type "UserId, UserName, UserAge -> { id: *, name: *, age: * }"))
		(expr @12.15-14.6 (type "User -> UserName"))
		(expr @14.9-17.2 (type "* ? *"))))
~~~
