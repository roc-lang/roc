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
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

**UNUSED VARIABLE**
Variable ``name`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_name` to suppress this warning.
The unused variable is declared here:
**type_multiple_aliases.md:9:19:9:23:**
```roc
createUser = |id, name, age| { id, name, age }
```


**UNUSED VARIABLE**
Variable ``age`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_age` to suppress this warning.
The unused variable is declared here:
**type_multiple_aliases.md:9:25:9:28:**
```roc
createUser = |id, name, age| { id, name, age }
```


**UNUSED VARIABLE**
Variable ``id`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_id` to suppress this warning.
The unused variable is declared here:
**type_multiple_aliases.md:9:15:9:17:**
```roc
createUser = |id, name, age| { id, name, age }
```


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
(file (1:1-17:2)
	(app (1:1-1:57)
		(provides (1:6-1:12) (exposed_item (lower_ident "main!")))
		(record_field (1:15-1:57)
			"pf"
			(string (1:28-1:55) (string_part (1:29-1:54) "../basic-cli/platform.roc")))
		(packages (1:13-1:57)
			(record_field (1:15-1:57)
				"pf"
				(string (1:28-1:55) (string_part (1:29-1:54) "../basic-cli/platform.roc")))))
	(statements
		(type_decl (3:1-4:9)
			(header (3:1-3:7) "UserId" (args))
			(ty "U64"))
		(type_decl (4:1-5:8)
			(header (4:1-4:9) "UserName" (args))
			(ty "Str"))
		(type_decl (5:1-6:5)
			(header (5:1-5:8) "UserAge" (args))
			(ty "U8"))
		(type_decl (6:1-8:11)
			(header (6:1-6:5) "User" (args))
			(record (6:8-6:52)
				(anno_record_field (6:10-6:21) "id" (ty "UserId"))
				(anno_record_field (6:22-6:37) "name" (ty "UserName"))
				(anno_record_field (6:38-6:52) "age" (ty "UserAge"))))
		(type_anno (8:1-9:11)
			"createUser"
			(fn (8:14-8:47)
				(ty "UserId")
				(ty "UserName")
				(ty "UserAge")
				(ty "User")))
		(decl (9:1-9:47)
			(ident (9:1-9:11) "createUser")
			(lambda (9:14-9:47)
				(args
					(ident (9:15-9:17) "id")
					(ident (9:19-9:23) "name")
					(ident (9:25-9:28) "age"))
				(record (9:30-9:47)
					(field "id")
					(field "name")
					(field "age"))))
		(type_anno (11:1-12:12)
			"getUserName"
			(fn (11:15-11:31)
				(ty "User")
				(ty "UserName")))
		(decl (12:1-14:6)
			(ident (12:1-12:12) "getUserName")
			(lambda (12:15-14:6)
				(args (ident (12:16-12:20) "user"))
				(field_access (12:22-14:6)
					(binop (12:22-14:6)
						"app"
						(ident (12:22-12:26) "" "user")
						(ident (12:26-12:31) "" ".name")))))
		(decl (14:1-17:2)
			(ident (14:1-14:6) "main!")
			(lambda (14:9-17:2)
				(args (underscore))
				(block (14:13-17:2)
					(statements
						(decl (15:5-15:40)
							(ident (15:5-15:9) "user")
							(apply (15:12-15:40)
								(ident (15:12-15:22) "" "createUser")
								(int (15:23-15:26) "123")
								(string (15:28-15:35) (string_part (15:29-15:34) "Alice"))
								(int (15:37-15:39) "25")))
						(apply (16:5-16:22)
							(ident (16:5-16:16) "" "getUserName")
							(ident (16:17-16:21) "" "user"))))))))
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
(can_ir
	(d_let
		(def_pattern
			(p_assign (9:1-9:11)
				(pid 95)
				(ident "createUser")))
		(def_expr
			(e_lambda (9:14-9:47)
				(args
					(p_assign (9:15-9:17)
						(pid 96)
						(ident "id"))
					(p_assign (9:19-9:23)
						(pid 97)
						(ident "name"))
					(p_assign (9:25-9:28)
						(pid 98)
						(ident "age")))
				(e_runtime_error (1:1-1:1) "not_implemented")))
		(annotation (9:1-9:11)
			(signature 110)
			(declared_type
				(fn (8:14-8:47)
					(ty (8:14-8:20) "UserId")
					(ty (8:22-8:30) "UserName")
					(ty (8:32-8:39) "UserAge")
					(ty (8:43-8:47) "User")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (12:1-12:12)
				(pid 116)
				(ident "getUserName")))
		(def_expr
			(e_lambda (12:15-14:6)
				(args
					(p_assign (12:16-12:20)
						(pid 117)
						(ident "user")))
				(e_dot_access (12:22-14:6)
					(e_lookup (12:22-12:26) (pid 117))
					"name")))
		(annotation (12:1-12:12)
			(signature 124)
			(declared_type
				(fn (11:15-11:31)
					(ty (11:15-11:19) "User")
					(ty (11:23-11:31) "UserName")
					"false"))))
	(d_let
		(def_pattern
			(p_assign (14:1-14:6)
				(pid 127)
				(ident "main!")))
		(def_expr
			(e_lambda (14:9-17:2)
				(args (p_underscore (14:10-14:11) (pid 128)))
				(e_block (14:13-17:2)
					(s_let (15:5-15:40)
						(p_assign (15:5-15:9)
							(pid 129)
							(ident "user"))
						(e_call (15:12-15:40)
							(e_lookup (15:12-15:22) (pid 95))
							(e_int (15:23-15:26)
								(int_var 132)
								(precision_var 131)
								(literal "123")
								(value "TODO")
								(bound "u8"))
							(e_string (15:28-15:35) (e_literal (15:29-15:34) "Alice"))
							(e_int (15:37-15:39)
								(int_var 137)
								(precision_var 136)
								(literal "25")
								(value "TODO")
								(bound "u8"))))
					(e_call (16:5-16:22)
						(e_lookup (16:5-16:16) (pid 116))
						(e_lookup (16:17-16:21) (pid 129)))))))
	(s_type_decl (3:1-4:9)
		(type_header (3:1-3:7) "UserId")
		(ty (3:10-3:13) "U64"))
	(s_type_decl (4:1-5:8)
		(type_header (4:1-4:9) "UserName")
		(ty (4:12-4:15) "Str"))
	(s_type_decl (5:1-6:5)
		(type_header (5:1-5:8) "UserAge")
		(ty (5:11-5:13) "U8"))
	(s_type_decl (6:1-8:11)
		(type_header (6:1-6:5) "User")
		(record (6:8-6:52)
			(record_field "id" (ty (6:14-6:20) "UserId"))
			(record_field "name" (ty (6:28-6:36) "UserName"))
			(record_field "age" (ty (6:43-6:50) "UserAge")))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "createUser" 112 (type "*"))
		(def "getUserName" 126 (type "*"))
		(def "main!" 146 (type "*")))
	(expressions
		(expr (9:14-9:47) 101 (type "*"))
		(expr (12:15-14:6) 120 (type "*"))
		(expr (14:9-17:2) 145 (type "*"))))
~~~