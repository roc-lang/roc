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
# PROBLEMS
**PARSE ERROR**
A parsing error occurred: `expected_expr_close_curly_or_comma`
This is an unexpected parsing error. Please check your syntax.

Here is the problematic code:
**type_alias_decl.md:30:5:30:13:**
```roc
    userId = 123
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **= 123** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_alias_decl.md:30:12:30:17:**
```roc
    userId = 123
```


**UNEXPECTED TOKEN IN EXPRESSION**
The token **}** is not expected in an expression.
Expressions can be identifiers, literals, function calls, or operators.

Here is the problematic code:
**type_alias_decl.md:39:1:39:2:**
```roc
}
```


**TYPE REDECLARED**
The type ``Result`` is being redeclared.

The redeclaration is here:
**type_alias_decl.md:8:2:11:8:**
```roc

# Record type alias
Person : { name : Str, age : U64 }

```

But ``Result`` was already declared here:
**type_alias_decl.md:2:2:2:2:**
```roc

```


**UNDECLARED TYPE**
The type ``Red`` is not declared in this scope.

This type is referenced here:
**type_alias_decl.md:20:11:20:14:**
```roc

```


**UNDECLARED TYPE**
The type ``Green`` is not declared in this scope.

This type is referenced here:
**type_alias_decl.md:20:16:20:21:**
```roc

```


**UNDECLARED TYPE**
The type ``Blue`` is not declared in this scope.

This type is referenced here:
**type_alias_decl.md:20:23:20:27:**
```roc

```


**INVALID LAMBDA**
The body of this lambda expression is not valid.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

**INVALID STATEMENT**
The statement **expr** is not allowed at the top level.
Only definitions, type annotations, and imports are allowed at the top level.

# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:11),CloseSquare(1:11-1:12),OpenCurly(1:13-1:14),LowerIdent(1:15-1:17),OpColon(1:17-1:18),KwPlatform(1:19-1:27),StringStart(1:28-1:29),StringPart(1:29-1:50),StringEnd(1:50-1:51),CloseCurly(1:52-1:53),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:20),
UpperIdent(4:1-4:7),OpColon(4:8-4:9),UpperIdent(4:10-4:13),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:21),
UpperIdent(7:1-7:7),NoSpaceOpenRound(7:7-7:8),LowerIdent(7:8-7:10),Comma(7:10-7:11),LowerIdent(7:12-7:15),CloseRound(7:15-7:16),OpColon(7:17-7:18),OpenSquare(7:19-7:20),UpperIdent(7:20-7:22),NoSpaceOpenRound(7:22-7:23),LowerIdent(7:23-7:25),CloseRound(7:25-7:26),Comma(7:26-7:27),UpperIdent(7:28-7:31),NoSpaceOpenRound(7:31-7:32),LowerIdent(7:32-7:35),CloseRound(7:35-7:36),CloseSquare(7:36-7:37),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(9:2-9:20),
UpperIdent(10:1-10:7),OpColon(10:8-10:9),OpenCurly(10:10-10:11),LowerIdent(10:12-10:16),OpColon(10:17-10:18),UpperIdent(10:19-10:22),Comma(10:22-10:23),LowerIdent(10:24-10:27),OpColon(10:28-10:29),UpperIdent(10:30-10:33),CloseCurly(10:34-10:35),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(12:2-12:22),
UpperIdent(13:1-13:6),NoSpaceOpenRound(13:6-13:7),LowerIdent(13:7-13:8),Comma(13:8-13:9),LowerIdent(13:10-13:11),CloseRound(13:11-13:12),OpColon(13:13-13:14),LowerIdent(13:15-13:16),OpArrow(13:17-13:19),LowerIdent(13:20-13:21),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(15:2-15:28),
UpperIdent(16:1-16:12),NoSpaceOpenRound(16:12-16:13),LowerIdent(16:13-16:17),CloseRound(16:17-16:18),OpColon(16:19-16:20),UpperIdent(16:21-16:27),NoSpaceOpenRound(16:27-16:28),LowerIdent(16:28-16:32),Comma(16:32-16:33),UpperIdent(16:34-16:37),CloseRound(16:37-16:38),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(18:2-18:34),
UpperIdent(19:1-19:6),OpColon(19:7-19:8),OpenSquare(19:9-19:10),UpperIdent(19:10-19:13),Comma(19:13-19:14),UpperIdent(19:15-19:20),Comma(19:20-19:21),UpperIdent(19:22-19:26),Comma(19:26-19:27),UpperIdent(19:28-19:34),NoSpaceOpenRound(19:34-19:35),UpperIdent(19:35-19:37),Comma(19:37-19:38),UpperIdent(19:39-19:41),Comma(19:41-19:42),UpperIdent(19:43-19:45),CloseRound(19:45-19:46),CloseSquare(19:46-19:47),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(21:2-21:45),
UpperIdent(22:1-22:10),NoSpaceOpenRound(22:10-22:11),LowerIdent(22:11-22:15),CloseRound(22:15-22:16),OpColon(22:17-22:18),OpenCurly(22:19-22:20),Newline(1:1-1:1),
LowerIdent(23:5-23:13),OpColon(23:14-23:15),UpperIdent(23:16-23:20),NoSpaceOpenRound(23:20-23:21),LowerIdent(23:21-23:25),CloseRound(23:25-23:26),Comma(23:26-23:27),Newline(1:1-1:1),
LowerIdent(24:5-24:13),OpColon(24:14-24:15),OpenCurly(24:16-24:17),LowerIdent(24:18-24:22),OpColon(24:23-24:24),UpperIdent(24:25-24:28),Comma(24:28-24:29),LowerIdent(24:30-24:37),OpColon(24:38-24:39),UpperIdent(24:40-24:43),CloseCurly(24:44-24:45),Newline(1:1-1:1),
CloseCurly(25:1-25:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(27:1-27:6),OpAssign(27:7-27:8),OpBar(27:9-27:10),Underscore(27:10-27:11),OpBar(27:11-27:12),OpenCurly(27:13-27:14),Newline(1:1-1:1),
Newline(28:6-28:42),
LowerIdent(29:5-29:11),OpColon(29:12-29:13),UpperIdent(29:14-29:20),Newline(1:1-1:1),
LowerIdent(30:5-30:11),OpAssign(30:12-30:13),Int(30:14-30:17),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(32:5-32:11),OpColon(32:12-32:13),UpperIdent(32:14-32:20),Newline(1:1-1:1),
LowerIdent(33:5-33:11),OpAssign(33:12-33:13),OpenCurly(33:14-33:15),LowerIdent(33:16-33:20),OpColon(33:20-33:21),StringStart(33:22-33:23),StringPart(33:23-33:28),StringEnd(33:28-33:29),Comma(33:29-33:30),LowerIdent(33:31-33:34),OpColon(33:34-33:35),Int(33:36-33:38),CloseCurly(33:39-33:40),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(35:5-35:10),OpColon(35:11-35:12),UpperIdent(35:13-35:18),Newline(1:1-1:1),
LowerIdent(36:5-36:10),OpAssign(36:11-36:12),UpperIdent(36:13-36:16),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(38:5-38:11),Newline(1:1-1:1),
CloseCurly(39:1-39:2),EndOfFile(39:2-39:2),
~~~
# PARSE
~~~clojure
(file (1:1-39:2)
	(app (1:1-1:53)
		(provides (1:6-1:12) (exposed_item (lower_ident "main!")))
		(record_field (1:15-1:53)
			"pf"
			(string (1:28-1:51) (string_part (1:29-1:50) "../basic-cli/main.roc")))
		(packages (1:13-1:53)
			(record_field (1:15-1:53)
				"pf"
				(string (1:28-1:51) (string_part (1:29-1:50) "../basic-cli/main.roc")))))
	(statements
		(type_decl (4:1-7:7)
			(header (4:1-4:7) "UserId" (args))
			(ty "U64"))
		(type_decl (7:1-10:7)
			(header (7:1-7:16)
				"Result"
				(args
					(ty_var (7:8-7:10) "ok")
					(ty_var (7:12-7:15) "err")))
			(tag_union (7:19-7:37)
				(tags
					(apply (7:20-7:26)
						(ty "Ok")
						(ty_var (7:23-7:25) "ok"))
					(apply (7:28-7:36)
						(ty "Err")
						(ty_var (7:32-7:35) "err")))))
		(type_decl (10:1-13:6)
			(header (10:1-10:7) "Person" (args))
			(record (10:10-10:35)
				(anno_record_field (10:12-10:23) "name" (ty "Str"))
				(anno_record_field (10:24-10:35) "age" (ty "U64"))))
		(type_decl (13:1-16:12)
			(header (13:1-13:12)
				"MapFn"
				(args
					(ty_var (13:7-13:8) "a")
					(ty_var (13:10-13:11) "b")))
			(fn (13:15-13:21)
				(ty_var (13:15-13:16) "a")
				(ty_var (13:20-13:21) "b")))
		(type_decl (16:1-19:6)
			(header (16:1-16:18)
				"ApiResponse"
				(args (ty_var (16:13-16:17) "data")))
			(apply (16:21-16:38)
				(ty "Result")
				(ty_var (16:28-16:32) "data")
				(ty "Str")))
		(type_decl (19:1-22:10)
			(header (19:1-19:6) "Color" (args))
			(tag_union (19:9-19:47)
				(tags
					(ty "Red")
					(ty "Green")
					(ty "Blue")
					(apply (19:28-19:46)
						(ty "Custom")
						(ty "U8")
						(ty "U8")
						(ty "U8")))))
		(type_decl (22:1-27:6)
			(header (22:1-22:16)
				"Container"
				(args (ty_var (22:11-22:15) "item")))
			(record (22:19-25:2)
				(anno_record_field (23:5-23:27)
					"contents"
					(apply (23:16-23:26)
						(ty "List")
						(ty_var (23:21-23:25) "item")))
				(anno_record_field (24:5-25:2)
					"metadata"
					(record (24:16-24:45)
						(anno_record_field (24:18-24:29) "size" (ty "U64"))
						(anno_record_field (24:30-24:45) "created" (ty "Str"))))))
		(decl (27:1-30:13)
			(ident (27:1-27:6) "main!")
			(lambda (27:9-30:13)
				(args (underscore))
				(malformed_expr (30:5-30:13) "expected_expr_close_curly_or_comma")))
		(malformed_expr (30:12-30:17) "expr_unexpected_token")
		(int (30:14-30:17) "123")
		(type_anno (32:5-33:11) "person" (ty "Person"))
		(decl (33:5-33:40)
			(ident (33:5-33:11) "person")
			(record (33:14-33:40)
				(field
					"name"
					(string (33:22-33:29) (string_part (33:23-33:28) "Alice")))
				(field "age" (int (33:36-33:38) "30"))))
		(type_anno (35:5-36:10) "color" (ty "Color"))
		(decl (36:5-36:16)
			(ident (36:5-36:10) "color")
			(tag (36:13-36:16) "Red"))
		(ident (38:5-38:11) "" "userId")
		(malformed_expr (39:1-39:2) "expr_unexpected_token")))
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

main! = |_|
	123

person : Person
person = { name: "Alice", age: 30 }

color : Color
color = Red

userId

~~~
# CANONICALIZE
~~~clojure
(can_ir
	(d_let
		(def_pattern
			(p_assign (27:1-27:6)
				(pid 131)
				(ident "main!")))
		(def_expr
			(e_lambda (27:9-30:13)
				(args (p_underscore (27:10-27:11) (pid 132)))
				(e_runtime_error (30:5-30:13) "lambda_body_not_canonicalized"))))
	(d_let
		(def_pattern
			(p_assign (33:5-33:11)
				(pid 140)
				(ident "person")))
		(def_expr (e_runtime_error (1:1-1:1) "not_implemented"))
		(annotation (33:5-33:11)
			(signature 144)
			(declared_type (ty (32:14-32:20) "Person"))))
	(d_let
		(def_pattern
			(p_assign (36:5-36:10)
				(pid 148)
				(ident "color")))
		(def_expr
			(e_tag (36:13-36:16)
				(ext_var 0)
				(name "Red")
				(args "TODO")))
		(annotation (36:5-36:10)
			(signature 152)
			(declared_type (ty (35:13-35:18) "Color"))))
	(s_type_decl (4:1-7:7)
		(type_header (4:1-4:7) "UserId")
		(ty (4:10-4:13) "U64"))
	(s_type_decl (7:1-10:7)
		(type_header (7:1-7:16)
			"Result"
			(args
				(ty_var (7:8-7:10) "ok")
				(ty_var (7:12-7:15) "err")))
		(tag_union (7:19-7:37)
			(apply (7:20-7:26)
				"Ok"
				(ty_var (7:23-7:25) "ok"))
			(apply (7:28-7:36)
				"Err"
				(ty_var (7:32-7:35) "err"))))
	(s_type_decl (10:1-13:6)
		(type_header (10:1-10:7) "Person")
		(record (10:10-10:35)
			(record_field "name" (ty (10:19-10:22) "Str"))
			(record_field "age" (ty (10:30-10:33) "U64"))))
	(s_type_decl (13:1-16:12)
		(type_header (13:1-13:12)
			"MapFn"
			(args
				(ty_var (13:7-13:8) "a")
				(ty_var (13:10-13:11) "b")))
		(fn (13:15-13:21)
			(ty_var (13:15-13:16) "a")
			(ty_var (13:20-13:21) "b")
			"false"))
	(s_type_decl (16:1-19:6)
		(type_header (16:1-16:18)
			"ApiResponse"
			(args (ty_var (16:13-16:17) "data")))
		(apply (16:21-16:38)
			"Result"
			(ty_var (16:28-16:32) "data")
			(ty (16:34-16:37) "Str")))
	(s_type_decl (19:1-22:10)
		(type_header (19:1-19:6) "Color")
		(tag_union (19:9-19:47)
			(ty (19:10-19:13) "Red")
			(ty (19:15-19:20) "Green")
			(ty (19:22-19:26) "Blue")
			(apply (19:28-19:46)
				"Custom"
				(ty (19:35-19:37) "U8")
				(ty (19:39-19:41) "U8")
				(ty (19:43-19:45) "U8"))))
	(s_type_decl (22:1-27:6)
		(type_header (22:1-22:16)
			"Container"
			(args (ty_var (22:11-22:15) "item")))
		(record (22:19-25:2)
			(record_field
				"contents"
				(apply (23:16-23:26)
					"List"
					(ty_var (23:21-23:25) "item")))
			(record_field
				"metadata"
				(record (24:16-24:45)
					(record_field "size" (ty (24:25-24:28) "U64"))
					(record_field "created" (ty (24:40-24:43) "Str")))))))
~~~
# TYPES
~~~clojure
(inferred_types
	(defs
		(def "main!" 136 (type "*"))
		(def "person" 146 (type "Error"))
		(def "color" 154 (type "[Red, * *]")))
	(expressions
		(expr (27:9-30:13) 135 (type "*"))
		(expr (33:14-33:40) 142 (type "Error"))
		(expr (36:13-36:16) 150 (type "[Red, * *]"))))
~~~