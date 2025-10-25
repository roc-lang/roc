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
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,Comma,StringStart,StringPart,StringEnd,Comma,Int,CloseRound,
LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/platform.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-type-decl
			(header (name "UserId")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "UserName")
				(args))
			(ty (name "Str")))
		(s-type-decl
			(header (name "UserAge")
				(args))
			(ty (name "U8")))
		(s-type-decl
			(header (name "User")
				(args))
			(ty-record
				(anno-record-field (name "id")
					(ty (name "UserId")))
				(anno-record-field (name "name")
					(ty (name "UserName")))
				(anno-record-field (name "age")
					(ty (name "UserAge")))))
		(s-type-anno (name "create_user")
			(ty-fn
				(ty (name "UserId"))
				(ty (name "UserName"))
				(ty (name "UserAge"))
				(ty (name "User"))))
		(s-decl
			(p-ident (raw "create_user"))
			(e-lambda
				(args
					(p-ident (raw "id"))
					(p-ident (raw "name"))
					(p-ident (raw "age")))
				(e-record
					(field (field "id"))
					(field (field "name"))
					(field (field "age")))))
		(s-type-anno (name "get_user_name")
			(ty-fn
				(ty (name "User"))
				(ty (name "UserName"))))
		(s-decl
			(p-ident (raw "get_user_name"))
			(e-lambda
				(args
					(p-ident (raw "user")))
				(e-field-access
					(e-ident (raw "user"))
					(e-ident (raw "name")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "user"))
							(e-apply
								(e-ident (raw "create_user"))
								(e-int (raw "123"))
								(e-string
									(e-string-part (raw "Alice")))
								(e-int (raw "25"))))
						(e-apply
							(e-ident (raw "get_user_name"))
							(e-ident (raw "user")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "create_user"))
		(e-lambda
			(args
				(p-assign (ident "id"))
				(p-assign (ident "name"))
				(p-assign (ident "age")))
			(e-record
				(fields
					(field (name "id")
						(e-lookup-local
							(p-assign (ident "id"))))
					(field (name "name")
						(e-lookup-local
							(p-assign (ident "name"))))
					(field (name "age")
						(e-lookup-local
							(p-assign (ident "age")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "UserId") (local))
				(ty-lookup (name "UserName") (local))
				(ty-lookup (name "UserAge") (local))
				(ty-lookup (name "User") (local)))))
	(d-let
		(p-assign (ident "get_user_name"))
		(e-lambda
			(args
				(p-assign (ident "user")))
			(e-dot-access (field "name")
				(receiver
					(e-lookup-local
						(p-assign (ident "user"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "User") (local))
				(ty-lookup (name "UserName") (local)))))
	(d-let
		(p-assign (ident "main!"))
		(e-closure
			(captures
				(capture (ident "create_user"))
				(capture (ident "get_user_name")))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(s-let
						(p-assign (ident "user"))
						(e-call
							(e-lookup-local
								(p-assign (ident "create_user")))
							(e-num (value "123"))
							(e-string
								(e-literal (string "Alice")))
							(e-num (value "25"))))
					(e-call
						(e-lookup-local
							(p-assign (ident "get_user_name")))
						(e-lookup-local
							(p-assign (ident "user"))))))))
	(s-alias-decl
		(ty-header (name "UserId"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "UserName"))
		(ty-lookup (name "Str") (builtin)))
	(s-alias-decl
		(ty-header (name "UserAge"))
		(ty-lookup (name "U8") (builtin)))
	(s-alias-decl
		(ty-header (name "User"))
		(ty-record
			(field (field "id")
				(ty-lookup (name "UserId") (local)))
			(field (field "name")
				(ty-lookup (name "UserName") (local)))
			(field (field "age")
				(ty-lookup (name "UserAge") (local))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "UserId, UserName, UserAge -> User"))
		(patt (type "User -> UserName"))
		(patt (type "_arg -> UserName")))
	(type_decls
		(alias (type "UserId")
			(ty-header (name "UserId")))
		(alias (type "UserName")
			(ty-header (name "UserName")))
		(alias (type "UserAge")
			(ty-header (name "UserAge")))
		(alias (type "User")
			(ty-header (name "User"))))
	(expressions
		(expr (type "UserId, UserName, UserAge -> User"))
		(expr (type "User -> UserName"))
		(expr (type "_arg -> UserName"))))
~~~
