# META
~~~ini
description=Type aliases and declarations with various forms
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Simple type alias
UserId : U64

# Generic type alias
Result(ok, err) : [Ok(ok), Err(err)]

# Record type alias
Person : { name : Str, age : U64 }

# Function type alias
MapFn(a, b) : a -> b

# Complex nested type alias
ApiResponse(data) : Result(data, Str)

# Type declaration with tag union
Color : [Red, Green, Blue, Custom(U8, U8, U8)]

# Type declaration with records and generics
Container(item) : {
    contents : List(item),
    metadata : { size : U64, created : Str }
}

main! = |_| {
    # Use the types to validate they work
    userId : UserId
    userId = 123

    person : Person
    person = { name: "Alice", age: 30 }

    color : Color
    color = Red

    userId
}
~~~
# EXPECTED
COMPILER DIAGNOSTIC - type_alias_decl.md:0:0:0:0
UNUSED VARIABLE - type_alias_decl.md:36:5:36:10
UNUSED VARIABLE - type_alias_decl.md:33:5:33:11
# PROBLEMS
**COMPILER DIAGNOSTIC**

**Compiler Diagnostic**
Diagnostic type 'type_redeclared' is not yet handled in report generation.
**type_alias_decl.md:0:0:0:0**

**UNUSED VARIABLE**
Variable `color` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_color` to suppress this warning.
The unused variable is declared here:
**type_alias_decl.md:36:5:36:10:**
```roc
    color = Red
```
    ^^^^^


**UNUSED VARIABLE**
Variable `person` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_person` to suppress this warning.
The unused variable is declared here:
**type_alias_decl.md:33:5:33:11:**
```roc
    person = { name: "Alice", age: 30 }
```
    ^^^^^^


# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),
UpperIdent(4:1-4:7),OpColon(4:8-4:9),UpperIdent(4:10-4:13),
UpperIdent(7:1-7:7),NoSpaceOpenRound(7:7-7:8),LowerIdent(7:8-7:10),Comma(7:10-7:11),LowerIdent(7:12-7:15),CloseRound(7:15-7:16),OpColon(7:17-7:18),OpenSquare(7:19-7:20),UpperIdent(7:20-7:22),NoSpaceOpenRound(7:22-7:23),LowerIdent(7:23-7:25),CloseRound(7:25-7:26),Comma(7:26-7:27),UpperIdent(7:28-7:31),NoSpaceOpenRound(7:31-7:32),LowerIdent(7:32-7:35),CloseRound(7:35-7:36),CloseSquare(7:36-7:37),
UpperIdent(10:1-10:7),OpColon(10:8-10:9),OpenCurly(10:10-10:11),LowerIdent(10:12-10:16),OpColon(10:17-10:18),UpperIdent(10:19-10:22),Comma(10:22-10:23),LowerIdent(10:24-10:27),OpColon(10:28-10:29),UpperIdent(10:30-10:33),CloseCurly(10:34-10:35),
UpperIdent(13:1-13:6),NoSpaceOpenRound(13:6-13:7),LowerIdent(13:7-13:8),Comma(13:8-13:9),LowerIdent(13:10-13:11),CloseRound(13:11-13:12),OpColon(13:13-13:14),LowerIdent(13:15-13:16),OpArrow(13:17-13:19),LowerIdent(13:20-13:21),
UpperIdent(16:1-16:12),NoSpaceOpenRound(16:12-16:13),LowerIdent(16:13-16:17),CloseRound(16:17-16:18),OpColon(16:19-16:20),UpperIdent(16:21-16:27),NoSpaceOpenRound(16:27-16:28),LowerIdent(16:28-16:32),Comma(16:32-16:33),UpperIdent(16:34-16:37),CloseRound(16:37-16:38),
UpperIdent(19:1-19:6),OpColon(19:7-19:8),OpenSquare(19:9-19:10),UpperIdent(19:10-19:13),Comma(19:13-19:14),UpperIdent(19:15-19:20),Comma(19:20-19:21),UpperIdent(19:22-19:26),Comma(19:26-19:27),UpperIdent(19:28-19:34),NoSpaceOpenRound(19:34-19:35),UpperIdent(19:35-19:37),Comma(19:37-19:38),UpperIdent(19:39-19:41),Comma(19:41-19:42),UpperIdent(19:43-19:45),CloseRound(19:45-19:46),CloseSquare(19:46-19:47),
UpperIdent(22:1-22:10),NoSpaceOpenRound(22:10-22:11),LowerIdent(22:11-22:15),CloseRound(22:15-22:16),OpColon(22:17-22:18),OpenCurly(22:19-22:20),
LowerIdent(23:5-23:13),OpColon(23:14-23:15),UpperIdent(23:16-23:20),NoSpaceOpenRound(23:20-23:21),LowerIdent(23:21-23:25),CloseRound(23:25-23:26),Comma(23:26-23:27),
LowerIdent(24:5-24:13),OpColon(24:14-24:15),OpenCurly(24:16-24:17),LowerIdent(24:18-24:22),OpColon(24:23-24:24),UpperIdent(24:25-24:28),Comma(24:28-24:29),LowerIdent(24:30-24:37),OpColon(24:38-24:39),UpperIdent(24:40-24:43),CloseCurly(24:44-24:45),
CloseCurly(25:1-25:2),
LowerIdent(27:1-27:6),OpAssign(27:7-27:8),OpBar(27:9-27:10),Underscore(27:10-27:11),OpBar(27:11-27:12),OpenCurly(27:13-27:14),
LowerIdent(29:5-29:11),OpColon(29:12-29:13),UpperIdent(29:14-29:20),
LowerIdent(30:5-30:11),OpAssign(30:12-30:13),Int(30:14-30:17),
LowerIdent(32:5-32:11),OpColon(32:12-32:13),UpperIdent(32:14-32:20),
LowerIdent(33:5-33:11),OpAssign(33:12-33:13),OpenCurly(33:14-33:15),LowerIdent(33:16-33:20),OpColon(33:20-33:21),StringStart(33:22-33:23),StringPart(33:23-33:28),StringEnd(33:28-33:29),Comma(33:29-33:30),LowerIdent(33:31-33:34),OpColon(33:34-33:35),Int(33:36-33:38),CloseCurly(33:39-33:40),
LowerIdent(35:5-35:10),OpColon(35:11-35:12),UpperIdent(35:13-35:18),
LowerIdent(36:5-36:10),OpAssign(36:11-36:12),UpperIdent(36:13-36:16),
LowerIdent(38:5-38:11),
CloseCurly(39:1-39:2),EndOfFile(39:2-39:2),
~~~
# PARSE
~~~clojure
(file @1.1-39.2
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
		(s-type-decl @4.1-4.13
			(header @4.1-4.7 (name "UserId")
				(args))
			(ty @4.10-4.13 (name "U64")))
		(s-type-decl @7.1-7.37
			(header @7.1-7.16 (name "Result")
				(args
					(ty-var @7.8-7.10 (raw "ok"))
					(ty-var @7.12-7.15 (raw "err"))))
			(ty-tag-union @7.19-7.37
				(tags
					(ty-apply @7.20-7.26
						(ty @7.20-7.22 (name "Ok"))
						(ty-var @7.23-7.25 (raw "ok")))
					(ty-apply @7.28-7.36
						(ty @7.28-7.31 (name "Err"))
						(ty-var @7.32-7.35 (raw "err"))))))
		(s-type-decl @10.1-10.35
			(header @10.1-10.7 (name "Person")
				(args))
			(ty-record @10.10-10.35
				(anno-record-field @10.12-10.22 (name "name")
					(ty @10.19-10.22 (name "Str")))
				(anno-record-field @10.24-10.33 (name "age")
					(ty @10.30-10.33 (name "U64")))))
		(s-type-decl @13.1-13.21
			(header @13.1-13.12 (name "MapFn")
				(args
					(ty-var @13.7-13.8 (raw "a"))
					(ty-var @13.10-13.11 (raw "b"))))
			(ty-fn @13.15-13.21
				(ty-var @13.15-13.16 (raw "a"))
				(ty-var @13.20-13.21 (raw "b"))))
		(s-type-decl @16.1-16.38
			(header @16.1-16.18 (name "ApiResponse")
				(args
					(ty-var @16.13-16.17 (raw "data"))))
			(ty-apply @16.21-16.38
				(ty @16.21-16.27 (name "Result"))
				(ty-var @16.28-16.32 (raw "data"))
				(ty @16.34-16.37 (name "Str"))))
		(s-type-decl @19.1-19.47
			(header @19.1-19.6 (name "Color")
				(args))
			(ty-tag-union @19.9-19.47
				(tags
					(ty @19.10-19.13 (name "Red"))
					(ty @19.15-19.20 (name "Green"))
					(ty @19.22-19.26 (name "Blue"))
					(ty-apply @19.28-19.46
						(ty @19.28-19.34 (name "Custom"))
						(ty @19.35-19.37 (name "U8"))
						(ty @19.39-19.41 (name "U8"))
						(ty @19.43-19.45 (name "U8"))))))
		(s-type-decl @22.1-25.2
			(header @22.1-22.16 (name "Container")
				(args
					(ty-var @22.11-22.15 (raw "item"))))
			(ty-record @22.19-25.2
				(anno-record-field @23.5-23.26 (name "contents")
					(ty-apply @23.16-23.26
						(ty @23.16-23.20 (name "List"))
						(ty-var @23.21-23.25 (raw "item"))))
				(anno-record-field @24.5-24.45 (name "metadata")
					(ty-record @24.16-24.45
						(anno-record-field @24.18-24.28 (name "size")
							(ty @24.25-24.28 (name "U64")))
						(anno-record-field @24.30-24.43 (name "created")
							(ty @24.40-24.43 (name "Str")))))))
		(s-decl @27.1-39.2
			(p-ident @27.1-27.6 (raw "main!"))
			(e-lambda @27.9-39.2
				(args
					(p-underscore))
				(e-block @27.13-39.2
					(statements
						(s-type-anno @29.5-29.20 (name "userId")
							(ty @29.14-29.20 (name "UserId")))
						(s-decl @30.5-30.17
							(p-ident @30.5-30.11 (raw "userId"))
							(e-int @30.14-30.17 (raw "123")))
						(s-type-anno @32.5-32.20 (name "person")
							(ty @32.14-32.20 (name "Person")))
						(s-decl @33.5-33.40
							(p-ident @33.5-33.11 (raw "person"))
							(e-record @33.14-33.40
								(field (field "name")
									(e-string @33.22-33.29
										(e-string-part @33.23-33.28 (raw "Alice"))))
								(field (field "age")
									(e-int @33.36-33.38 (raw "30")))))
						(s-type-anno @35.5-35.18 (name "color")
							(ty @35.13-35.18 (name "Color")))
						(s-decl @36.5-36.16
							(p-ident @36.5-36.10 (raw "color"))
							(e-tag @36.13-36.16 (raw "Red")))
						(e-ident @38.5-38.11 (raw "userId"))))))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

# Simple type alias
UserId : U64

# Generic type alias
Result(ok, err) : [Ok(ok), Err(err)]

# Record type alias
Person : { name : Str, age : U64 }

# Function type alias
MapFn(a, b) : a -> b

# Complex nested type alias
ApiResponse(data) : Result(data, Str)

# Type declaration with tag union
Color : [Red, Green, Blue, Custom(U8, U8, U8)]

# Type declaration with records and generics
Container(item) : {
	contents : List(item),
	metadata : { size : U64, created : Str },
}

main! = |_| {
	# Use the types to validate they work
	userId : UserId
	userId = 123

	person : Person
	person = { name: "Alice", age: 30 }

	color : Color
	color = Red

	userId
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(def
		(pattern
			(p-assign @27.1-27.6 (ident "main!")))
		(expr
			(e-lambda @27.9-39.2
				(args
					(p-underscore @27.10-27.11))
				(e-block @27.13-39.2
					(s-type-anno @29.5-29.20 (name "userId")
						(ty @29.14-29.20 (name "UserId")))
					(s-let @30.5-30.17
						(p-assign @30.5-30.11 (ident "userId"))
						(e-int @30.14-30.17 (value "123")))
					(s-type-anno @32.5-32.20 (name "person")
						(ty @32.14-32.20 (name "Person")))
					(s-let @33.5-33.40
						(p-assign @33.5-33.11 (ident "person"))
						(e-record @33.14-33.40
							(fields
								(record-field (label "name")
									(value
										(e-string @33.22-33.29
											(e-literal @33.23-33.28 (string "Alice")))))
								(record-field (label "age")
									(value
										(e-int @33.36-33.38 (value "30")))))))
					(s-type-anno @35.5-35.18 (name "color")
						(ty @35.13-35.18 (name "Color")))
					(s-let @36.5-36.16
						(p-assign @36.5-36.10 (ident "color"))
						(e-tag @36.13-36.16 (name "Red")))
					(e-lookup-local @38.5-38.11
						(p-assign @30.5-30.11 (ident "userId")))))))
	(s-alias-decl @4.1-4.13
		(type-header (name "UserId"))
		(ty @4.10-4.13 (name "U64")))
	(s-alias-decl @7.1-7.37
		(type-header (name "Result")
			(args
				(ty-var @7.8-7.10 (name "ok"))
				(ty-var @7.12-7.15 (name "err"))))
		(ty-tag-union @7.19-7.37
			(ty-apply @7.20-7.26 (symbol "Ok")
				(ty-var @7.23-7.25 (name "ok")))
			(ty-apply @7.28-7.36 (symbol "Err")
				(ty-var @7.32-7.35 (name "err")))))
	(s-alias-decl @10.1-10.35
		(type-header (name "Person"))
		(ty-record @10.10-10.35
			(field (field "name")
				(ty @10.19-10.22 (name "Str")))
			(field (field "age")
				(ty @10.30-10.33 (name "U64")))))
	(s-alias-decl @13.1-13.21
		(type-header (name "MapFn")
			(args
				(ty-var @13.7-13.8 (name "a"))
				(ty-var @13.10-13.11 (name "b"))))
		(ty-fn @13.15-13.21 (effectful false)
			(ty-var @13.15-13.16 (name "a"))
			(ty-var @13.20-13.21 (name "b"))))
	(s-alias-decl @16.1-16.38
		(type-header (name "ApiResponse")
			(args
				(ty-var @16.13-16.17 (name "data"))))
		(ty-apply @16.21-16.38 (symbol "Result")
			(ty-var @16.28-16.32 (name "data"))
			(ty @16.34-16.37 (name "Str"))))
	(s-alias-decl @19.1-19.47
		(type-header (name "Color"))
		(ty-tag-union @19.9-19.47
			(ty @19.10-19.13 (name "Red"))
			(ty @19.15-19.20 (name "Green"))
			(ty @19.22-19.26 (name "Blue"))
			(ty-apply @19.28-19.46 (symbol "Custom")
				(ty @19.35-19.37 (name "U8"))
				(ty @19.39-19.41 (name "U8"))
				(ty @19.43-19.45 (name "U8")))))
	(s-alias-decl @22.1-25.2
		(type-header (name "Container")
			(args
				(ty-var @22.11-22.15 (name "item"))))
		(ty-record @22.19-25.2
			(field (field "contents")
				(ty-apply @23.16-23.26 (symbol "List")
					(ty-var @23.21-23.25 (name "item"))))
			(field (field "metadata")
				(ty-record @24.16-24.45
					(field (field "size")
						(ty @24.25-24.28 (name "U64")))
					(field (field "created")
						(ty @24.40-24.43 (name "Str"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @27.1-27.6 (type "_arg -> Num(_size2)")))
	(type_decls
		(alias @4.1-4.13 (type "UserId")
			(type-header (name "UserId")))
		(alias @7.1-7.37 (type "Result(ok, err)")
			(type-header (name "Result")
				(args
					(ty-var @7.8-7.10 (name "ok"))
					(ty-var @7.12-7.15 (name "err")))))
		(alias @10.1-10.35 (type "Person")
			(type-header (name "Person")))
		(alias @13.1-13.21 (type "MapFn(a, b)")
			(type-header (name "MapFn")
				(args
					(ty-var @13.7-13.8 (name "a"))
					(ty-var @13.10-13.11 (name "b")))))
		(alias @16.1-16.38 (type "ApiResponse(data)")
			(type-header (name "ApiResponse")
				(args
					(ty-var @16.13-16.17 (name "data")))))
		(alias @19.1-19.47 (type "Color")
			(type-header (name "Color")))
		(alias @22.1-25.2 (type "Container(item)")
			(type-header (name "Container")
				(args
					(ty-var @22.11-22.15 (name "item"))))))
	(expressions
		(expr @27.9-39.2 (type "_arg -> Num(_size2)"))))
~~~
