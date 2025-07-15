# META
~~~ini
description=Complex tag union types with multiple variants and nesting
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Simple tag union with no-argument tags
Status : [Loading, Complete, Failed]

# Tag union with mixed argument types
Result : [Success(Str), Error(Str), Warning(Str, I32)]

# Nested tag unions
Response : [Ok(Result), NetworkError, ParseError]

# Multiple tag unions using similar tag names
UserState : [Active(Str), Inactive, Suspended(Str)]
ConnectionState : [Active, Disconnected, Connecting(Str)]

# Function using tag unions
processResult : Result -> Str
processResult = |_result| "processed"

# Function with nested tag union
handleResponse : Response -> Str
handleResponse = |_response| "handled"

main! = |_| {}
~~~
# EXPECTED
TYPE REDECLARED - type_tag_union_complex.md:7:1:7:55
# PROBLEMS
**TYPE REDECLARED**
The type `Result` is being redeclared.

The redeclaration is here:
**type_tag_union_complex.md:7:1:7:55:**
```roc
Result : [Success(Str), Error(Str), Warning(Str, I32)]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

But `Result` was already declared here:
**type_tag_union_complex.md:1:1:1:1:**
```roc
app [main!] { pf: platform "../basic-cli/main.roc" }
```



# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
UpperIdent(4:1-4:7),OpColon(4:8-4:9),OpenSquare(4:10-4:11),UpperIdent(4:11-4:18),Comma(4:18-4:19),UpperIdent(4:20-4:28),Comma(4:28-4:29),UpperIdent(4:30-4:36),CloseSquare(4:36-4:37),
UpperIdent(7:1-7:7),OpColon(7:8-7:9),OpenSquare(7:10-7:11),UpperIdent(7:11-7:18),NoSpaceOpenRound(7:18-7:19),UpperIdent(7:19-7:22),CloseRound(7:22-7:23),Comma(7:23-7:24),UpperIdent(7:25-7:30),NoSpaceOpenRound(7:30-7:31),UpperIdent(7:31-7:34),CloseRound(7:34-7:35),Comma(7:35-7:36),UpperIdent(7:37-7:44),NoSpaceOpenRound(7:44-7:45),UpperIdent(7:45-7:48),Comma(7:48-7:49),UpperIdent(7:50-7:53),CloseRound(7:53-7:54),CloseSquare(7:54-7:55),
UpperIdent(10:1-10:9),OpColon(10:10-10:11),OpenSquare(10:12-10:13),UpperIdent(10:13-10:15),NoSpaceOpenRound(10:15-10:16),UpperIdent(10:16-10:22),CloseRound(10:22-10:23),Comma(10:23-10:24),UpperIdent(10:25-10:37),Comma(10:37-10:38),UpperIdent(10:39-10:49),CloseSquare(10:49-10:50),
UpperIdent(13:1-13:10),OpColon(13:11-13:12),OpenSquare(13:13-13:14),UpperIdent(13:14-13:20),NoSpaceOpenRound(13:20-13:21),UpperIdent(13:21-13:24),CloseRound(13:24-13:25),Comma(13:25-13:26),UpperIdent(13:27-13:35),Comma(13:35-13:36),UpperIdent(13:37-13:46),NoSpaceOpenRound(13:46-13:47),UpperIdent(13:47-13:50),CloseRound(13:50-13:51),CloseSquare(13:51-13:52),
UpperIdent(14:1-14:16),OpColon(14:17-14:18),OpenSquare(14:19-14:20),UpperIdent(14:20-14:26),Comma(14:26-14:27),UpperIdent(14:28-14:40),Comma(14:40-14:41),UpperIdent(14:42-14:52),NoSpaceOpenRound(14:52-14:53),UpperIdent(14:53-14:56),CloseRound(14:56-14:57),CloseSquare(14:57-14:58),
LowerIdent(17:1-17:14),OpColon(17:15-17:16),UpperIdent(17:17-17:23),OpArrow(17:24-17:26),UpperIdent(17:27-17:30),
LowerIdent(18:1-18:14),OpAssign(18:15-18:16),OpBar(18:17-18:18),NamedUnderscore(18:18-18:25),OpBar(18:25-18:26),StringStart(18:27-18:28),StringPart(18:28-18:37),StringEnd(18:37-18:38),
LowerIdent(21:1-21:15),OpColon(21:16-21:17),UpperIdent(21:18-21:26),OpArrow(21:27-21:29),UpperIdent(21:30-21:33),
LowerIdent(22:1-22:15),OpAssign(22:16-22:17),OpBar(22:18-22:19),NamedUnderscore(22:19-22:28),OpBar(22:28-22:29),StringStart(22:30-22:31),StringPart(22:31-22:38),StringEnd(22:38-22:39),
LowerIdent(24:1-24:6),OpAssign(24:7-24:8),OpBar(24:9-24:10),Underscore(24:10-24:11),OpBar(24:11-24:12),OpenCurly(24:13-24:14),CloseCurly(24:14-24:15),EndOfFile(24:15-24:15),
~~~
# PARSE
~~~clojure
(file @1.1-24.15
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
		(s-type-decl @4.1-4.37
			(header @4.1-4.7 (name "Status")
				(args))
			(ty-tag-union @4.10-4.37
				(tags
					(ty @4.11-4.18 (name "Loading"))
					(ty @4.20-4.28 (name "Complete"))
					(ty @4.30-4.36 (name "Failed")))))
		(s-type-decl @7.1-7.55
			(header @7.1-7.7 (name "Result")
				(args))
			(ty-tag-union @7.10-7.55
				(tags
					(ty-apply @7.11-7.23
						(ty @7.11-7.18 (name "Success"))
						(ty @7.19-7.22 (name "Str")))
					(ty-apply @7.25-7.35
						(ty @7.25-7.30 (name "Error"))
						(ty @7.31-7.34 (name "Str")))
					(ty-apply @7.37-7.54
						(ty @7.37-7.44 (name "Warning"))
						(ty @7.45-7.48 (name "Str"))
						(ty @7.50-7.53 (name "I32"))))))
		(s-type-decl @10.1-10.50
			(header @10.1-10.9 (name "Response")
				(args))
			(ty-tag-union @10.12-10.50
				(tags
					(ty-apply @10.13-10.23
						(ty @10.13-10.15 (name "Ok"))
						(ty @10.16-10.22 (name "Result")))
					(ty @10.25-10.37 (name "NetworkError"))
					(ty @10.39-10.49 (name "ParseError")))))
		(s-type-decl @13.1-13.52
			(header @13.1-13.10 (name "UserState")
				(args))
			(ty-tag-union @13.13-13.52
				(tags
					(ty-apply @13.14-13.25
						(ty @13.14-13.20 (name "Active"))
						(ty @13.21-13.24 (name "Str")))
					(ty @13.27-13.35 (name "Inactive"))
					(ty-apply @13.37-13.51
						(ty @13.37-13.46 (name "Suspended"))
						(ty @13.47-13.50 (name "Str"))))))
		(s-type-decl @14.1-14.58
			(header @14.1-14.16 (name "ConnectionState")
				(args))
			(ty-tag-union @14.19-14.58
				(tags
					(ty @14.20-14.26 (name "Active"))
					(ty @14.28-14.40 (name "Disconnected"))
					(ty-apply @14.42-14.57
						(ty @14.42-14.52 (name "Connecting"))
						(ty @14.53-14.56 (name "Str"))))))
		(s-type-anno @17.1-17.30 (name "processResult")
			(ty-fn @17.17-17.30
				(ty @17.17-17.23 (name "Result"))
				(ty @17.27-17.30 (name "Str"))))
		(s-decl @18.1-18.38
			(p-ident @18.1-18.14 (raw "processResult"))
			(e-lambda @18.17-18.38
				(args
					(p-ident @18.18-18.25 (raw "_result")))
				(e-string @18.27-18.38
					(e-string-part @18.28-18.37 (raw "processed")))))
		(s-type-anno @21.1-21.33 (name "handleResponse")
			(ty-fn @21.18-21.33
				(ty @21.18-21.26 (name "Response"))
				(ty @21.30-21.33 (name "Str"))))
		(s-decl @22.1-22.39
			(p-ident @22.1-22.15 (raw "handleResponse"))
			(e-lambda @22.18-22.39
				(args
					(p-ident @22.19-22.28 (raw "_response")))
				(e-string @22.30-22.39
					(e-string-part @22.31-22.38 (raw "handled")))))
		(s-decl @24.1-24.15
			(p-ident @24.1-24.6 (raw "main!"))
			(e-lambda @24.9-24.15
				(args
					(p-underscore))
				(e-record @24.13-24.15)))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @18.1-18.14 (ident "processResult"))
		(e-lambda @18.17-18.38
			(args
				(p-assign @18.18-18.25 (ident "_result")))
			(e-string @18.27-18.38
				(e-literal @18.28-18.37 (string "processed"))))
		(annotation @18.1-18.14
			(declared-type
				(ty-fn @17.17-17.30 (effectful false)
					(ty @17.17-17.23 (name "Result"))
					(ty @17.27-17.30 (name "Str"))))))
	(d-let
		(p-assign @22.1-22.15 (ident "handleResponse"))
		(e-lambda @22.18-22.39
			(args
				(p-assign @22.19-22.28 (ident "_response")))
			(e-string @22.30-22.39
				(e-literal @22.31-22.38 (string "handled"))))
		(annotation @22.1-22.15
			(declared-type
				(ty-fn @21.18-21.33 (effectful false)
					(ty @21.18-21.26 (name "Response"))
					(ty @21.30-21.33 (name "Str"))))))
	(d-let
		(p-assign @24.1-24.6 (ident "main!"))
		(e-lambda @24.9-24.15
			(args
				(p-underscore @24.10-24.11))
			(e-empty_record @24.13-24.15)))
	(s-alias-decl @4.1-4.37
		(ty-header @4.1-4.7 (name "Status"))
		(ty-tag-union @4.10-4.37
			(ty @4.11-4.18 (name "Loading"))
			(ty @4.20-4.28 (name "Complete"))
			(ty @4.30-4.36 (name "Failed"))))
	(s-alias-decl @7.1-7.55
		(ty-header @7.1-7.7 (name "Result"))
		(ty-tag-union @7.10-7.55
			(ty-apply @7.11-7.23 (symbol "Success")
				(ty @7.19-7.22 (name "Str")))
			(ty-apply @7.25-7.35 (symbol "Error")
				(ty @7.31-7.34 (name "Str")))
			(ty-apply @7.37-7.54 (symbol "Warning")
				(ty @7.45-7.48 (name "Str"))
				(ty @7.50-7.53 (name "I32")))))
	(s-alias-decl @10.1-10.50
		(ty-header @10.1-10.9 (name "Response"))
		(ty-tag-union @10.12-10.50
			(ty-apply @10.13-10.23 (symbol "Ok")
				(ty @10.16-10.22 (name "Result")))
			(ty @10.25-10.37 (name "NetworkError"))
			(ty @10.39-10.49 (name "ParseError"))))
	(s-alias-decl @13.1-13.52
		(ty-header @13.1-13.10 (name "UserState"))
		(ty-tag-union @13.13-13.52
			(ty-apply @13.14-13.25 (symbol "Active")
				(ty @13.21-13.24 (name "Str")))
			(ty @13.27-13.35 (name "Inactive"))
			(ty-apply @13.37-13.51 (symbol "Suspended")
				(ty @13.47-13.50 (name "Str")))))
	(s-alias-decl @14.1-14.58
		(ty-header @14.1-14.16 (name "ConnectionState"))
		(ty-tag-union @14.19-14.58
			(ty @14.20-14.26 (name "Active"))
			(ty @14.28-14.40 (name "Disconnected"))
			(ty-apply @14.42-14.57 (symbol "Connecting")
				(ty @14.53-14.56 (name "Str"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @18.1-18.14 (type "Result -> Str"))
		(patt @22.1-22.15 (type "Response -> Str"))
		(patt @24.1-24.6 (type "_arg -> {}")))
	(type_decls
		(alias @4.1-4.37 (type "Status")
			(ty-header @4.1-4.7 (name "Status")))
		(alias @7.1-7.55 (type "Result")
			(ty-header @7.1-7.7 (name "Result")))
		(alias @10.1-10.50 (type "Response")
			(ty-header @10.1-10.9 (name "Response")))
		(alias @13.1-13.52 (type "UserState")
			(ty-header @13.1-13.10 (name "UserState")))
		(alias @14.1-14.58 (type "ConnectionState")
			(ty-header @14.1-14.16 (name "ConnectionState"))))
	(expressions
		(expr @18.17-18.38 (type "Result -> Str"))
		(expr @22.18-22.39 (type "Response -> Str"))
		(expr @24.9-24.15 (type "_arg -> {}"))))
~~~
