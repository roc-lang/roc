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
User : { id : UserId, name : UserName, age : UserAge }

create_user : UserId, UserName, UserAge -> User
create_user = |id, name, age| { id, name, age }

get_user_name : User -> UserName
get_user_name = |user| user.name

main! = |_| {
	user = create_user(123, "Alice", 25)
	get_user_name(user)
}
~~~
# EXPECTED
NIL
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_multiple_aliases.md:15:21:15:24:**
```roc
	user = create_user(123, "Alice", 25)
```
                    ^^^

It is of type:
    _Num(_size)_

But you are trying to use it as:
    _UserId_

**TYPE MISMATCH**
This expression is used in an unexpected way:
**type_multiple_aliases.md:15:35:15:37:**
```roc
	user = create_user(123, "Alice", 25)
```
                                  ^^

It is of type:
    _Num(_size)_

But you are trying to use it as:
    _UserAge_

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:54),StringEnd(1:54-1:55),CloseCurly(1:56-1:57),
UpperIdent(3:1-3:7),OpColon(3:8-3:9),UpperIdent(3:10-3:13),
UpperIdent(4:1-4:9),OpColon(4:10-4:11),UpperIdent(4:12-4:15),
UpperIdent(5:1-5:8),OpColon(5:9-5:10),UpperIdent(5:11-5:13),
UpperIdent(6:1-6:5),OpColon(6:6-6:7),OpenCurly(6:8-6:9),LowerIdent(6:10-6:12),OpColon(6:13-6:14),UpperIdent(6:15-6:21),Comma(6:21-6:22),LowerIdent(6:23-6:27),OpColon(6:28-6:29),UpperIdent(6:30-6:38),Comma(6:38-6:39),LowerIdent(6:40-6:43),OpColon(6:44-6:45),UpperIdent(6:46-6:53),CloseCurly(6:54-6:55),
LowerIdent(8:1-8:12),OpColon(8:13-8:14),UpperIdent(8:15-8:21),Comma(8:21-8:22),UpperIdent(8:23-8:31),Comma(8:31-8:32),UpperIdent(8:33-8:40),OpArrow(8:41-8:43),UpperIdent(8:44-8:48),
LowerIdent(9:1-9:12),OpAssign(9:13-9:14),OpBar(9:15-9:16),LowerIdent(9:16-9:18),Comma(9:18-9:19),LowerIdent(9:20-9:24),Comma(9:24-9:25),LowerIdent(9:26-9:29),OpBar(9:29-9:30),OpenCurly(9:31-9:32),LowerIdent(9:33-9:35),Comma(9:35-9:36),LowerIdent(9:37-9:41),Comma(9:41-9:42),LowerIdent(9:43-9:46),CloseCurly(9:47-9:48),
LowerIdent(11:1-11:14),OpColon(11:15-11:16),UpperIdent(11:17-11:21),OpArrow(11:22-11:24),UpperIdent(11:25-11:33),
LowerIdent(12:1-12:14),OpAssign(12:15-12:16),OpBar(12:17-12:18),LowerIdent(12:18-12:22),OpBar(12:22-12:23),LowerIdent(12:24-12:28),NoSpaceDotLowerIdent(12:28-12:33),
LowerIdent(14:1-14:6),OpAssign(14:7-14:8),OpBar(14:9-14:10),Underscore(14:10-14:11),OpBar(14:11-14:12),OpenCurly(14:13-14:14),
LowerIdent(15:2-15:6),OpAssign(15:7-15:8),LowerIdent(15:9-15:20),NoSpaceOpenRound(15:20-15:21),Int(15:21-15:24),Comma(15:24-15:25),StringStart(15:26-15:27),StringPart(15:27-15:32),StringEnd(15:32-15:33),Comma(15:33-15:34),Int(15:35-15:37),CloseRound(15:37-15:38),
LowerIdent(16:2-16:15),NoSpaceOpenRound(16:15-16:16),LowerIdent(16:16-16:20),CloseRound(16:20-16:21),
CloseCurly(17:1-17:2),EndOfFile(17:2-17:2),
~~~
# PARSE
~~~clojure
(file @1.1-17.2
	(app @1.1-1.57
		(provides @1.5-1.12
			(exposed-lower-ident @1.6-1.11
				(text "main!")))
		(record-field @1.15-1.55 (name "pf")
			(e-string @1.28-1.55
				(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))
		(packages @1.13-1.57
			(record-field @1.15-1.55 (name "pf")
				(e-string @1.28-1.55
					(e-string-part @1.29-1.54 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-decl @3.1-3.13
			(header @3.1-3.7 (name "UserId")
				(args))
			(ty @3.10-3.13 (name "U64")))
		(s-type-decl @4.1-4.15
			(header @4.1-4.9 (name "UserName")
				(args))
			(ty @4.12-4.15 (name "Str")))
		(s-type-decl @5.1-5.13
			(header @5.1-5.8 (name "UserAge")
				(args))
			(ty @5.11-5.13 (name "U8")))
		(s-type-decl @6.1-6.55
			(header @6.1-6.5 (name "User")
				(args))
			(ty-record @6.8-6.55
				(anno-record-field @6.10-6.21 (name "id")
					(ty @6.15-6.21 (name "UserId")))
				(anno-record-field @6.23-6.38 (name "name")
					(ty @6.30-6.38 (name "UserName")))
				(anno-record-field @6.40-6.53 (name "age")
					(ty @6.46-6.53 (name "UserAge")))))
		(s-type-anno @8.1-8.48 (name "create_user")
			(ty-fn @8.15-8.48
				(ty @8.15-8.21 (name "UserId"))
				(ty @8.23-8.31 (name "UserName"))
				(ty @8.33-8.40 (name "UserAge"))
				(ty @8.44-8.48 (name "User"))))
		(s-decl @9.1-9.48
			(p-ident @9.1-9.12 (raw "create_user"))
			(e-lambda @9.15-9.48
				(args
					(p-ident @9.16-9.18 (raw "id"))
					(p-ident @9.20-9.24 (raw "name"))
					(p-ident @9.26-9.29 (raw "age")))
				(e-record @9.31-9.48
					(field (field "id"))
					(field (field "name"))
					(field (field "age")))))
		(s-type-anno @11.1-11.33 (name "get_user_name")
			(ty-fn @11.17-11.33
				(ty @11.17-11.21 (name "User"))
				(ty @11.25-11.33 (name "UserName"))))
		(s-decl @12.1-12.33
			(p-ident @12.1-12.14 (raw "get_user_name"))
			(e-lambda @12.17-12.33
				(args
					(p-ident @12.18-12.22 (raw "user")))
				(e-field-access @12.24-12.33
					(e-ident @12.24-12.28 (raw "user"))
					(e-ident @12.28-12.33 (raw "name")))))
		(s-decl @14.1-17.2
			(p-ident @14.1-14.6 (raw "main!"))
			(e-lambda @14.9-17.2
				(args
					(p-underscore))
				(e-block @14.13-17.2
					(statements
						(s-decl @15.2-15.38
							(p-ident @15.2-15.6 (raw "user"))
							(e-apply @15.9-15.38
								(e-ident @15.9-15.20 (raw "create_user"))
								(e-int @15.21-15.24 (raw "123"))
								(e-string @15.26-15.33
									(e-string-part @15.27-15.32 (raw "Alice")))
								(e-int @15.35-15.37 (raw "25"))))
						(e-apply @16.2-16.21
							(e-ident @16.2-16.15 (raw "get_user_name"))
							(e-ident @16.16-16.20 (raw "user")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @9.1-9.12 (ident "create_user"))
		(e-lambda @9.15-9.48
			(args
				(p-assign @9.16-9.18 (ident "id"))
				(p-assign @9.20-9.24 (ident "name"))
				(p-assign @9.26-9.29 (ident "age")))
			(e-record @9.31-9.48
				(fields
					(field (name "id")
						(e-lookup-local @9.33-9.35
							(p-assign @9.16-9.18 (ident "id"))))
					(field (name "name")
						(e-lookup-local @9.37-9.41
							(p-assign @9.20-9.24 (ident "name"))))
					(field (name "age")
						(e-lookup-local @9.43-9.46
							(p-assign @9.26-9.29 (ident "age")))))))
		(annotation @9.1-9.12
			(declared-type
				(ty-fn @8.15-8.48 (effectful false)
					(ty @8.15-8.21 (name "UserId"))
					(ty @8.23-8.31 (name "UserName"))
					(ty @8.33-8.40 (name "UserAge"))
					(ty @8.44-8.48 (name "User"))))))
	(d-let
		(p-assign @12.1-12.14 (ident "get_user_name"))
		(e-lambda @12.17-12.33
			(args
				(p-assign @12.18-12.22 (ident "user")))
			(e-dot-access @12.24-12.33 (field "name")
				(receiver
					(e-lookup-local @12.24-12.28
						(p-assign @12.18-12.22 (ident "user"))))))
		(annotation @12.1-12.14
			(declared-type
				(ty-fn @11.17-11.33 (effectful false)
					(ty @11.17-11.21 (name "User"))
					(ty @11.25-11.33 (name "UserName"))))))
	(d-let
		(p-assign @14.1-14.6 (ident "main!"))
		(e-closure @14.9-17.2
			(captures
				(capture @9.1-9.12 (ident "create_user"))
				(capture @12.1-12.14 (ident "get_user_name")))
			(e-lambda @14.9-17.2
				(args
					(p-underscore @14.10-14.11))
				(e-block @14.13-17.2
					(s-let @15.2-15.38
						(p-assign @15.2-15.6 (ident "user"))
						(e-call @15.9-15.38
							(e-lookup-local @15.9-15.20
								(p-assign @9.1-9.12 (ident "create_user")))
							(e-int @15.21-15.24 (value "123"))
							(e-string @15.26-15.33
								(e-literal @15.27-15.32 (string "Alice")))
							(e-int @15.35-15.37 (value "25"))))
					(e-call @16.2-16.21
						(e-lookup-local @16.2-16.15
							(p-assign @12.1-12.14 (ident "get_user_name")))
						(e-lookup-local @16.16-16.20
							(p-assign @15.2-15.6 (ident "user"))))))))
	(s-alias-decl @3.1-3.13
		(ty-header @3.1-3.7 (name "UserId"))
		(ty @3.10-3.13 (name "U64")))
	(s-alias-decl @4.1-4.15
		(ty-header @4.1-4.9 (name "UserName"))
		(ty @4.12-4.15 (name "Str")))
	(s-alias-decl @5.1-5.13
		(ty-header @5.1-5.8 (name "UserAge"))
		(ty @5.11-5.13 (name "U8")))
	(s-alias-decl @6.1-6.55
		(ty-header @6.1-6.5 (name "User"))
		(ty-record @6.8-6.55
			(field (field "id")
				(ty @6.15-6.21 (name "UserId")))
			(field (field "name")
				(ty @6.30-6.38 (name "UserName")))
			(field (field "age")
				(ty @6.46-6.53 (name "UserAge"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @9.1-9.12 (type "UserId, UserName, UserAge -> { id: UserId, name: UserName, age: UserAge }"))
		(patt @12.1-12.14 (type "{ id: UserId, name: UserName, age: UserAge } -> UserName"))
		(patt @14.1-14.6 (type "_arg -> UserName")))
	(type_decls
		(alias @3.1-3.13 (type "UserId")
			(ty-header @3.1-3.7 (name "UserId")))
		(alias @4.1-4.15 (type "UserName")
			(ty-header @4.1-4.9 (name "UserName")))
		(alias @5.1-5.13 (type "UserAge")
			(ty-header @5.1-5.8 (name "UserAge")))
		(alias @6.1-6.55 (type "User")
			(ty-header @6.1-6.5 (name "User"))))
	(expressions
		(expr @9.15-9.48 (type "UserId, UserName, UserAge -> { id: UserId, name: UserName, age: UserAge }"))
		(expr @12.17-12.33 (type "{ id: UserId, name: UserName, age: UserAge } -> UserName"))
		(expr @14.9-17.2 (type "_arg -> UserName"))))
~~~
