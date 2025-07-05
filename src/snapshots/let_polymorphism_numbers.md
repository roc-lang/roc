# META
~~~ini
description=Let-polymorphism with numbers
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# True let-polymorphism: identity function used at multiple types
id = |x| x

# id used with Int
int_id = id(42)

# id used with Str
str_id = id("hello")

# id used with Float
float_id = id(3.14)

# id used with List
list_id = id([1, 2, 3])

# Polymorphic numeric literal
num = 42

# num used as Int (addition with Int forces it to Int)
int_use = num + 10

# num used as Float (multiplication with Float forces it to Float)
float_use = num * 3.14

# Polymorphic function that works with any type
const = |x| |_| x

# const used to create functions that return different types
const_int = const(100)
const_str = const("world")
const_list = const([True, False])

# Using the const functions
result_int = const_int(999)  # Returns 100
result_str = const_str(999)  # Returns "world"
result_list = const_list(999)  # Returns [True, False]

main = |_| {
    # Return values showing polymorphic instantiation
    { int_id, str_id, float_id, int_use, float_use, result_int, result_str }
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:66),
LowerIdent(4:1-4:3),OpAssign(4:4-4:5),OpBar(4:6-4:7),LowerIdent(4:7-4:8),OpBar(4:8-4:9),LowerIdent(4:10-4:11),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:19),
LowerIdent(7:1-7:7),OpAssign(7:8-7:9),LowerIdent(7:10-7:12),NoSpaceOpenRound(7:12-7:13),Int(7:13-7:15),CloseRound(7:15-7:16),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(9:2-9:19),
LowerIdent(10:1-10:7),OpAssign(10:8-10:9),LowerIdent(10:10-10:12),NoSpaceOpenRound(10:12-10:13),StringStart(10:13-10:14),StringPart(10:14-10:19),StringEnd(10:19-10:20),CloseRound(10:20-10:21),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(12:2-12:21),
LowerIdent(13:1-13:9),OpAssign(13:10-13:11),LowerIdent(13:12-13:14),NoSpaceOpenRound(13:14-13:15),Float(13:15-13:19),CloseRound(13:19-13:20),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(15:2-15:20),
LowerIdent(16:1-16:8),OpAssign(16:9-16:10),LowerIdent(16:11-16:13),NoSpaceOpenRound(16:13-16:14),OpenSquare(16:14-16:15),Int(16:15-16:16),Comma(16:16-16:17),Int(16:18-16:19),Comma(16:19-16:20),Int(16:21-16:22),CloseSquare(16:22-16:23),CloseRound(16:23-16:24),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(18:2-18:30),
LowerIdent(19:1-19:4),OpAssign(19:5-19:6),Int(19:7-19:9),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(21:2-21:55),
LowerIdent(22:1-22:8),OpAssign(22:9-22:10),LowerIdent(22:11-22:14),OpPlus(22:15-22:16),Int(22:17-22:19),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(24:2-24:67),
LowerIdent(25:1-25:10),OpAssign(25:11-25:12),LowerIdent(25:13-25:16),OpStar(25:17-25:18),Float(25:19-25:23),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(27:2-27:48),
LowerIdent(28:1-28:6),OpAssign(28:7-28:8),OpBar(28:9-28:10),LowerIdent(28:10-28:11),OpBar(28:11-28:12),OpBar(28:13-28:14),Underscore(28:14-28:15),OpBar(28:15-28:16),LowerIdent(28:17-28:18),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(30:2-30:61),
LowerIdent(31:1-31:10),OpAssign(31:11-31:12),LowerIdent(31:13-31:18),NoSpaceOpenRound(31:18-31:19),Int(31:19-31:22),CloseRound(31:22-31:23),Newline(1:1-1:1),
LowerIdent(32:1-32:10),OpAssign(32:11-32:12),LowerIdent(32:13-32:18),NoSpaceOpenRound(32:18-32:19),StringStart(32:19-32:20),StringPart(32:20-32:25),StringEnd(32:25-32:26),CloseRound(32:26-32:27),Newline(1:1-1:1),
LowerIdent(33:1-33:11),OpAssign(33:12-33:13),LowerIdent(33:14-33:19),NoSpaceOpenRound(33:19-33:20),OpenSquare(33:20-33:21),UpperIdent(33:21-33:25),Comma(33:25-33:26),UpperIdent(33:27-33:32),CloseSquare(33:32-33:33),CloseRound(33:33-33:34),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(35:2-35:28),
LowerIdent(36:1-36:11),OpAssign(36:12-36:13),LowerIdent(36:14-36:23),NoSpaceOpenRound(36:23-36:24),Int(36:24-36:27),CloseRound(36:27-36:28),Newline(36:31-36:43),
LowerIdent(37:1-37:11),OpAssign(37:12-37:13),LowerIdent(37:14-37:23),NoSpaceOpenRound(37:23-37:24),Int(37:24-37:27),CloseRound(37:27-37:28),Newline(37:31-37:47),
LowerIdent(38:1-38:12),OpAssign(38:13-38:14),LowerIdent(38:15-38:25),NoSpaceOpenRound(38:25-38:26),Int(38:26-38:29),CloseRound(38:29-38:30),Newline(38:33-38:55),
Newline(1:1-1:1),
LowerIdent(40:1-40:5),OpAssign(40:6-40:7),OpBar(40:8-40:9),Underscore(40:9-40:10),OpBar(40:10-40:11),OpenCurly(40:12-40:13),Newline(1:1-1:1),
Newline(41:6-41:54),
OpenCurly(42:5-42:6),LowerIdent(42:7-42:13),Comma(42:13-42:14),LowerIdent(42:15-42:21),Comma(42:21-42:22),LowerIdent(42:23-42:31),Comma(42:31-42:32),LowerIdent(42:33-42:40),Comma(42:40-42:41),LowerIdent(42:42-42:51),Comma(42:51-42:52),LowerIdent(42:53-42:63),Comma(42:63-42:64),LowerIdent(42:65-42:75),CloseCurly(42:76-42:77),Newline(1:1-1:1),
CloseCurly(43:1-43:2),EndOfFile(43:2-43:2),
~~~
# PARSE
~~~clojure
(file @1.1-43.2
	(app @1.1-1.56
		(provides @1.6-1.11
			(exposed-lower-ident (text "main")))
		(record-field @1.14-1.56 (name "pf")
			(e-string @1.27-1.54
				(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))
		(packages @1.12-1.56
			(record-field @1.14-1.56 (name "pf")
				(e-string @1.27-1.54
					(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl @4.1-4.11
			(p-ident @4.1-4.3 (raw "id"))
			(e-lambda @4.6-4.11
				(args
					(p-ident @4.7-4.8 (raw "x")))
				(e-ident @4.10-4.11 (raw "x"))))
		(s-decl @7.1-7.16
			(p-ident @7.1-7.7 (raw "int_id"))
			(e-apply @7.10-7.16
				(e-ident @7.10-7.12 (raw "id"))
				(e-int @7.13-7.15 (raw "42"))))
		(s-decl @10.1-10.21
			(p-ident @10.1-10.7 (raw "str_id"))
			(e-apply @10.10-10.21
				(e-ident @10.10-10.12 (raw "id"))
				(e-string @10.13-10.20
					(e-string-part @10.14-10.19 (raw "hello")))))
		(s-decl @13.1-13.20
			(p-ident @13.1-13.9 (raw "float_id"))
			(e-apply @13.12-13.20
				(e-ident @13.12-13.14 (raw "id"))
				(e-frac @13.15-13.19 (raw "3.14"))))
		(s-decl @16.1-16.24
			(p-ident @16.1-16.8 (raw "list_id"))
			(e-apply @16.11-16.24
				(e-ident @16.11-16.13 (raw "id"))
				(e-list @16.14-16.23
					(e-int @16.15-16.16 (raw "1"))
					(e-int @16.18-16.19 (raw "2"))
					(e-int @16.21-16.22 (raw "3")))))
		(s-decl @19.1-19.9
			(p-ident @19.1-19.4 (raw "num"))
			(e-int @19.7-19.9 (raw "42")))
		(s-decl @22.1-25.10
			(p-ident @22.1-22.8 (raw "int_use"))
			(e-binop @22.11-25.10 (op "+")
				(e-ident @22.11-22.14 (raw "num"))
				(e-int @22.17-22.19 (raw "10"))))
		(s-decl @25.1-28.6
			(p-ident @25.1-25.10 (raw "float_use"))
			(e-binop @25.13-28.6 (op "*")
				(e-ident @25.13-25.16 (raw "num"))
				(e-frac @25.19-25.23 (raw "3.14"))))
		(s-decl @28.1-28.18
			(p-ident @28.1-28.6 (raw "const"))
			(e-lambda @28.9-28.18
				(args
					(p-ident @28.10-28.11 (raw "x")))
				(e-lambda @28.13-28.18
					(args
						(p-underscore))
					(e-ident @28.17-28.18 (raw "x")))))
		(s-decl @31.1-31.23
			(p-ident @31.1-31.10 (raw "const_int"))
			(e-apply @31.13-31.23
				(e-ident @31.13-31.18 (raw "const"))
				(e-int @31.19-31.22 (raw "100"))))
		(s-decl @32.1-32.27
			(p-ident @32.1-32.10 (raw "const_str"))
			(e-apply @32.13-32.27
				(e-ident @32.13-32.18 (raw "const"))
				(e-string @32.19-32.26
					(e-string-part @32.20-32.25 (raw "world")))))
		(s-decl @33.1-33.34
			(p-ident @33.1-33.11 (raw "const_list"))
			(e-apply @33.14-33.34
				(e-ident @33.14-33.19 (raw "const"))
				(e-list @33.20-33.33
					(e-tag @33.21-33.25 (raw "True"))
					(e-tag @33.27-33.32 (raw "False")))))
		(s-decl @36.1-36.28
			(p-ident @36.1-36.11 (raw "result_int"))
			(e-apply @36.14-36.28
				(e-ident @36.14-36.23 (raw "const_int"))
				(e-int @36.24-36.27 (raw "999"))))
		(s-decl @37.1-37.28
			(p-ident @37.1-37.11 (raw "result_str"))
			(e-apply @37.14-37.28
				(e-ident @37.14-37.23 (raw "const_str"))
				(e-int @37.24-37.27 (raw "999"))))
		(s-decl @38.1-38.30
			(p-ident @38.1-38.12 (raw "result_list"))
			(e-apply @38.15-38.30
				(e-ident @38.15-38.25 (raw "const_list"))
				(e-int @38.26-38.29 (raw "999"))))
		(s-decl @40.1-43.2
			(p-ident @40.1-40.5 (raw "main"))
			(e-lambda @40.8-43.2
				(args
					(p-underscore))
				(e-block @40.12-43.2
					(statements
						(e-record @42.5-42.77
							(field (field "int_id") (optional false))
							(field (field "str_id") (optional false))
							(field (field "float_id") (optional false))
							(field (field "int_use") (optional false))
							(field (field "float_use") (optional false))
							(field (field "result_int") (optional false))
							(field (field "result_str") (optional false)))))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# True let-polymorphism: identity function used at multiple types
id = |x| x

# id used with Int
int_id = id(42)

# id used with Str
str_id = id("hello")

# id used with Float
float_id = id(3.14)

# id used with List
list_id = id([1, 2, 3])

# Polymorphic numeric literal
num = 42

# num used as Int (addition with Int forces it to Int)
int_use = num + 10

# num used as Float (multiplication with Float forces it to Float)
float_use = num * 3.14

# Polymorphic function that works with any type
const = |x| |_| x

# const used to create functions that return different types
const_int = const(100)
const_str = const("world")
const_list = const([True, False])

# Using the const functions
result_int = const_int(999) # Returns 100
result_str = const_str(999) # Returns "world"
result_list = const_list(999) # Returns [True, False]

main = |_| {
	# Return values showing polymorphic instantiation
	{int_id, str_id, float_id, int_use, float_use, result_int, result_str}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.3 (ident "id"))
		(e-lambda @4.6-4.11
			(args
				(p-assign @4.7-4.8 (ident "x")))
			(e-lookup-local @4.10-4.11
				(pattern @4.7-4.8))))
	(d-let
		(p-assign @7.1-7.7 (ident "int_id"))
		(e-call @7.10-7.16
			(e-lookup-local @7.10-7.12
				(pattern @4.1-4.3))
			(e-int @7.13-7.15 (value "42"))))
	(d-let
		(p-assign @10.1-10.7 (ident "str_id"))
		(e-call @10.10-10.21
			(e-lookup-local @10.10-10.12
				(pattern @4.1-4.3))
			(e-string @10.13-10.20
				(e-literal @10.14-10.19 (string "hello")))))
	(d-let
		(p-assign @13.1-13.9 (ident "float_id"))
		(e-call @13.12-13.20
			(e-lookup-local @13.12-13.14
				(pattern @4.1-4.3))
			(e-dec-small @13.15-13.19 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
	(d-let
		(p-assign @16.1-16.8 (ident "list_id"))
		(e-call @16.11-16.24
			(e-lookup-local @16.11-16.13
				(pattern @4.1-4.3))
			(e-list @16.14-16.23
				(elems
					(e-int @16.15-16.16 (value "1"))
					(e-int @16.18-16.19 (value "2"))
					(e-int @16.21-16.22 (value "3"))))))
	(d-let
		(p-assign @19.1-19.4 (ident "num"))
		(e-int @19.7-19.9 (value "42")))
	(d-let
		(p-assign @22.1-22.8 (ident "int_use"))
		(e-binop @22.11-25.10 (op "add")
			(e-lookup-local @22.11-22.14
				(pattern @19.1-19.4))
			(e-int @22.17-22.19 (value "10"))))
	(d-let
		(p-assign @25.1-25.10 (ident "float_use"))
		(e-binop @25.13-28.6 (op "mul")
			(e-lookup-local @25.13-25.16
				(pattern @19.1-19.4))
			(e-dec-small @25.19-25.23 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
	(d-let
		(p-assign @28.1-28.6 (ident "const"))
		(e-lambda @28.9-28.18
			(args
				(p-assign @28.10-28.11 (ident "x")))
			(e-lambda @28.13-28.18
				(args
					(p-underscore @28.14-28.15))
				(e-lookup-local @28.17-28.18
					(pattern @28.10-28.11)))))
	(d-let
		(p-assign @31.1-31.10 (ident "const_int"))
		(e-call @31.13-31.23
			(e-lookup-local @31.13-31.18
				(pattern @28.1-28.6))
			(e-int @31.19-31.22 (value "100"))))
	(d-let
		(p-assign @32.1-32.10 (ident "const_str"))
		(e-call @32.13-32.27
			(e-lookup-local @32.13-32.18
				(pattern @28.1-28.6))
			(e-string @32.19-32.26
				(e-literal @32.20-32.25 (string "world")))))
	(d-let
		(p-assign @33.1-33.11 (ident "const_list"))
		(e-call @33.14-33.34
			(e-lookup-local @33.14-33.19
				(pattern @28.1-28.6))
			(e-list @33.20-33.33
				(elems
					(e-tag @33.21-33.25 (name "True"))
					(e-tag @33.27-33.32 (name "False"))))))
	(d-let
		(p-assign @36.1-36.11 (ident "result_int"))
		(e-call @36.14-36.28
			(e-lookup-local @36.14-36.23
				(pattern @31.1-31.10))
			(e-int @36.24-36.27 (value "999"))))
	(d-let
		(p-assign @37.1-37.11 (ident "result_str"))
		(e-call @37.14-37.28
			(e-lookup-local @37.14-37.23
				(pattern @32.1-32.10))
			(e-int @37.24-37.27 (value "999"))))
	(d-let
		(p-assign @38.1-38.12 (ident "result_list"))
		(e-call @38.15-38.30
			(e-lookup-local @38.15-38.25
				(pattern @33.1-33.11))
			(e-int @38.26-38.29 (value "999"))))
	(d-let
		(p-assign @40.1-40.5 (ident "main"))
		(e-lambda @40.8-43.2
			(args
				(p-underscore @40.9-40.10))
			(e-block @40.12-43.2
				(e-record @42.5-42.77
					(fields
						(field (name "int_id")
							(e-lookup-local @42.7-42.14
								(pattern @7.1-7.7)))
						(field (name "str_id")
							(e-lookup-local @42.15-42.22
								(pattern @10.1-10.7)))
						(field (name "float_id")
							(e-lookup-local @42.23-42.32
								(pattern @13.1-13.9)))
						(field (name "int_use")
							(e-lookup-local @42.33-42.41
								(pattern @22.1-22.8)))
						(field (name "float_use")
							(e-lookup-local @42.42-42.52
								(pattern @25.1-25.10)))
						(field (name "result_int")
							(e-lookup-local @42.53-42.64
								(pattern @36.1-36.11)))
						(field (name "result_str")
							(e-lookup-local @42.65-42.77
								(pattern @37.1-37.11)))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.3 (type "* -> *"))
		(patt @7.1-7.7 (type "*"))
		(patt @10.1-10.7 (type "*"))
		(patt @13.1-13.9 (type "*"))
		(patt @16.1-16.8 (type "*"))
		(patt @19.1-19.4 (type "Num(*)"))
		(patt @22.1-22.8 (type "*"))
		(patt @25.1-25.10 (type "*"))
		(patt @28.1-28.6 (type "* -> * -> *"))
		(patt @31.1-31.10 (type "* -> *"))
		(patt @32.1-32.10 (type "* -> *"))
		(patt @33.1-33.11 (type "* -> *"))
		(patt @36.1-36.11 (type "*"))
		(patt @37.1-37.11 (type "*"))
		(patt @38.1-38.12 (type "*"))
		(patt @40.1-40.5 (type "* -> { int_id: *, str_id: *, float_id: *, int_use: *, float_use: *, result_int: *, result_str: * }")))
	(expressions
		(expr @4.6-4.11 (type "* -> *"))
		(expr @7.10-7.16 (type "*"))
		(expr @10.10-10.21 (type "*"))
		(expr @13.12-13.20 (type "*"))
		(expr @16.11-16.24 (type "*"))
		(expr @19.7-19.9 (type "Num(*)"))
		(expr @22.11-25.10 (type "*"))
		(expr @25.13-28.6 (type "*"))
		(expr @28.9-28.18 (type "* -> * -> *"))
		(expr @31.13-31.23 (type "* -> *"))
		(expr @32.13-32.27 (type "* -> *"))
		(expr @33.14-33.34 (type "* -> *"))
		(expr @36.14-36.28 (type "*"))
		(expr @37.14-37.28 (type "*"))
		(expr @38.15-38.30 (type "*"))
		(expr @40.8-43.2 (type "* -> { int_id: *, str_id: *, float_id: *, int_use: *, float_use: *, result_int: *, result_str: * }"))))
~~~
