# META
~~~ini
description=Let-polymorphism with lists
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# True let-polymorphism: identity function
id = |x| x

# id used with different types
int_id = id(42)
str_id = id("hello")
list_id = id([1, 2, 3])

# Polymorphic function to make single-element lists
make_list = |x| [x]

# make_list used with different types
int_list = make_list(42)
str_list = make_list("hello")
bool_list = make_list(True)

# Polymorphic pair constructor
pair = |a, b| { fst: a, snd: b }

# pair used with different types
int_pair = pair(1, 2)
str_pair = pair("hello", "world")
mixed_pair = pair(42, "answer")

# Polymorphic function that returns empty list
empty = |_| []

# empty used at different types (type determined by context)
empty_ints = empty(0)
empty_strs = empty(0)

# Demonstrating polymorphism by using the same empty function
# to create lists that will be used with different types
list_with_int = [1, 2, 3]
list_with_str = ["a", "b", "c"]

main = |_| {
    # Return values demonstrating polymorphic instantiation
    { int_id, str_id, list_id, int_list, str_list, int_pair, str_pair }
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:43),
LowerIdent(4:1-4:3),OpAssign(4:4-4:5),OpBar(4:6-4:7),LowerIdent(4:7-4:8),OpBar(4:8-4:9),LowerIdent(4:10-4:11),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:31),
LowerIdent(7:1-7:7),OpAssign(7:8-7:9),LowerIdent(7:10-7:12),NoSpaceOpenRound(7:12-7:13),Int(7:13-7:15),CloseRound(7:15-7:16),Newline(1:1-1:1),
LowerIdent(8:1-8:7),OpAssign(8:8-8:9),LowerIdent(8:10-8:12),NoSpaceOpenRound(8:12-8:13),StringStart(8:13-8:14),StringPart(8:14-8:19),StringEnd(8:19-8:20),CloseRound(8:20-8:21),Newline(1:1-1:1),
LowerIdent(9:1-9:8),OpAssign(9:9-9:10),LowerIdent(9:11-9:13),NoSpaceOpenRound(9:13-9:14),OpenSquare(9:14-9:15),Int(9:15-9:16),Comma(9:16-9:17),Int(9:18-9:19),Comma(9:19-9:20),Int(9:21-9:22),CloseSquare(9:22-9:23),CloseRound(9:23-9:24),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:52),
LowerIdent(12:1-12:10),OpAssign(12:11-12:12),OpBar(12:13-12:14),LowerIdent(12:14-12:15),OpBar(12:15-12:16),OpenSquare(12:17-12:18),LowerIdent(12:18-12:19),CloseSquare(12:19-12:20),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(14:2-14:38),
LowerIdent(15:1-15:9),OpAssign(15:10-15:11),LowerIdent(15:12-15:21),NoSpaceOpenRound(15:21-15:22),Int(15:22-15:24),CloseRound(15:24-15:25),Newline(1:1-1:1),
LowerIdent(16:1-16:9),OpAssign(16:10-16:11),LowerIdent(16:12-16:21),NoSpaceOpenRound(16:21-16:22),StringStart(16:22-16:23),StringPart(16:23-16:28),StringEnd(16:28-16:29),CloseRound(16:29-16:30),Newline(1:1-1:1),
LowerIdent(17:1-17:10),OpAssign(17:11-17:12),LowerIdent(17:13-17:22),NoSpaceOpenRound(17:22-17:23),UpperIdent(17:23-17:27),CloseRound(17:27-17:28),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(19:2-19:31),
LowerIdent(20:1-20:5),OpAssign(20:6-20:7),OpBar(20:8-20:9),LowerIdent(20:9-20:10),Comma(20:10-20:11),LowerIdent(20:12-20:13),OpBar(20:13-20:14),OpenCurly(20:15-20:16),LowerIdent(20:17-20:20),OpColon(20:20-20:21),LowerIdent(20:22-20:23),Comma(20:23-20:24),LowerIdent(20:25-20:28),OpColon(20:28-20:29),LowerIdent(20:30-20:31),CloseCurly(20:32-20:33),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(22:2-22:33),
LowerIdent(23:1-23:9),OpAssign(23:10-23:11),LowerIdent(23:12-23:16),NoSpaceOpenRound(23:16-23:17),Int(23:17-23:18),Comma(23:18-23:19),Int(23:20-23:21),CloseRound(23:21-23:22),Newline(1:1-1:1),
LowerIdent(24:1-24:9),OpAssign(24:10-24:11),LowerIdent(24:12-24:16),NoSpaceOpenRound(24:16-24:17),StringStart(24:17-24:18),StringPart(24:18-24:23),StringEnd(24:23-24:24),Comma(24:24-24:25),StringStart(24:26-24:27),StringPart(24:27-24:32),StringEnd(24:32-24:33),CloseRound(24:33-24:34),Newline(1:1-1:1),
LowerIdent(25:1-25:11),OpAssign(25:12-25:13),LowerIdent(25:14-25:18),NoSpaceOpenRound(25:18-25:19),Int(25:19-25:21),Comma(25:21-25:22),StringStart(25:23-25:24),StringPart(25:24-25:30),StringEnd(25:30-25:31),CloseRound(25:31-25:32),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(27:2-27:47),
LowerIdent(28:1-28:6),OpAssign(28:7-28:8),OpBar(28:9-28:10),Underscore(28:10-28:11),OpBar(28:11-28:12),OpenSquare(28:13-28:14),CloseSquare(28:14-28:15),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(30:2-30:61),
LowerIdent(31:1-31:11),OpAssign(31:12-31:13),LowerIdent(31:14-31:19),NoSpaceOpenRound(31:19-31:20),Int(31:20-31:21),CloseRound(31:21-31:22),Newline(1:1-1:1),
LowerIdent(32:1-32:11),OpAssign(32:12-32:13),LowerIdent(32:14-32:19),NoSpaceOpenRound(32:19-32:20),Int(32:20-32:21),CloseRound(32:21-32:22),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(34:2-34:62),
Newline(35:2-35:57),
LowerIdent(36:1-36:14),OpAssign(36:15-36:16),OpenSquare(36:17-36:18),Int(36:18-36:19),Comma(36:19-36:20),Int(36:21-36:22),Comma(36:22-36:23),Int(36:24-36:25),CloseSquare(36:25-36:26),Newline(1:1-1:1),
LowerIdent(37:1-37:14),OpAssign(37:15-37:16),OpenSquare(37:17-37:18),StringStart(37:18-37:19),StringPart(37:19-37:20),StringEnd(37:20-37:21),Comma(37:21-37:22),StringStart(37:23-37:24),StringPart(37:24-37:25),StringEnd(37:25-37:26),Comma(37:26-37:27),StringStart(37:28-37:29),StringPart(37:29-37:30),StringEnd(37:30-37:31),CloseSquare(37:31-37:32),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(39:1-39:5),OpAssign(39:6-39:7),OpBar(39:8-39:9),Underscore(39:9-39:10),OpBar(39:10-39:11),OpenCurly(39:12-39:13),Newline(1:1-1:1),
Newline(40:6-40:60),
OpenCurly(41:5-41:6),LowerIdent(41:7-41:13),Comma(41:13-41:14),LowerIdent(41:15-41:21),Comma(41:21-41:22),LowerIdent(41:23-41:30),Comma(41:30-41:31),LowerIdent(41:32-41:40),Comma(41:40-41:41),LowerIdent(41:42-41:50),Comma(41:50-41:51),LowerIdent(41:52-41:60),Comma(41:60-41:61),LowerIdent(41:62-41:70),CloseCurly(41:71-41:72),Newline(1:1-1:1),
CloseCurly(42:1-42:2),EndOfFile(42:2-42:2),
~~~
# PARSE
~~~clojure
(file @1.1-42.2
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
		(s-decl @8.1-8.21
			(p-ident @8.1-8.7 (raw "str_id"))
			(e-apply @8.10-8.21
				(e-ident @8.10-8.12 (raw "id"))
				(e-string @8.13-8.20
					(e-string-part @8.14-8.19 (raw "hello")))))
		(s-decl @9.1-9.24
			(p-ident @9.1-9.8 (raw "list_id"))
			(e-apply @9.11-9.24
				(e-ident @9.11-9.13 (raw "id"))
				(e-list @9.14-9.23
					(e-int @9.15-9.16 (raw "1"))
					(e-int @9.18-9.19 (raw "2"))
					(e-int @9.21-9.22 (raw "3")))))
		(s-decl @12.1-12.20
			(p-ident @12.1-12.10 (raw "make_list"))
			(e-lambda @12.13-12.20
				(args
					(p-ident @12.14-12.15 (raw "x")))
				(e-list @12.17-12.20
					(e-ident @12.18-12.19 (raw "x")))))
		(s-decl @15.1-15.25
			(p-ident @15.1-15.9 (raw "int_list"))
			(e-apply @15.12-15.25
				(e-ident @15.12-15.21 (raw "make_list"))
				(e-int @15.22-15.24 (raw "42"))))
		(s-decl @16.1-16.30
			(p-ident @16.1-16.9 (raw "str_list"))
			(e-apply @16.12-16.30
				(e-ident @16.12-16.21 (raw "make_list"))
				(e-string @16.22-16.29
					(e-string-part @16.23-16.28 (raw "hello")))))
		(s-decl @17.1-17.28
			(p-ident @17.1-17.10 (raw "bool_list"))
			(e-apply @17.13-17.28
				(e-ident @17.13-17.22 (raw "make_list"))
				(e-tag @17.23-17.27 (raw "True"))))
		(s-decl @20.1-20.33
			(p-ident @20.1-20.5 (raw "pair"))
			(e-lambda @20.8-20.33
				(args
					(p-ident @20.9-20.10 (raw "a"))
					(p-ident @20.12-20.13 (raw "b")))
				(e-record @20.15-20.33
					(field (field "fst") (optional false)
						(e-ident @20.22-20.23 (raw "a")))
					(field (field "snd") (optional false)
						(e-ident @20.30-20.31 (raw "b"))))))
		(s-decl @23.1-23.22
			(p-ident @23.1-23.9 (raw "int_pair"))
			(e-apply @23.12-23.22
				(e-ident @23.12-23.16 (raw "pair"))
				(e-int @23.17-23.18 (raw "1"))
				(e-int @23.20-23.21 (raw "2"))))
		(s-decl @24.1-24.34
			(p-ident @24.1-24.9 (raw "str_pair"))
			(e-apply @24.12-24.34
				(e-ident @24.12-24.16 (raw "pair"))
				(e-string @24.17-24.24
					(e-string-part @24.18-24.23 (raw "hello")))
				(e-string @24.26-24.33
					(e-string-part @24.27-24.32 (raw "world")))))
		(s-decl @25.1-25.32
			(p-ident @25.1-25.11 (raw "mixed_pair"))
			(e-apply @25.14-25.32
				(e-ident @25.14-25.18 (raw "pair"))
				(e-int @25.19-25.21 (raw "42"))
				(e-string @25.23-25.31
					(e-string-part @25.24-25.30 (raw "answer")))))
		(s-decl @28.1-28.15
			(p-ident @28.1-28.6 (raw "empty"))
			(e-lambda @28.9-28.15
				(args
					(p-underscore))
				(e-list @28.13-28.15)))
		(s-decl @31.1-31.22
			(p-ident @31.1-31.11 (raw "empty_ints"))
			(e-apply @31.14-31.22
				(e-ident @31.14-31.19 (raw "empty"))
				(e-int @31.20-31.21 (raw "0"))))
		(s-decl @32.1-32.22
			(p-ident @32.1-32.11 (raw "empty_strs"))
			(e-apply @32.14-32.22
				(e-ident @32.14-32.19 (raw "empty"))
				(e-int @32.20-32.21 (raw "0"))))
		(s-decl @36.1-36.26
			(p-ident @36.1-36.14 (raw "list_with_int"))
			(e-list @36.17-36.26
				(e-int @36.18-36.19 (raw "1"))
				(e-int @36.21-36.22 (raw "2"))
				(e-int @36.24-36.25 (raw "3"))))
		(s-decl @37.1-37.32
			(p-ident @37.1-37.14 (raw "list_with_str"))
			(e-list @37.17-37.32
				(e-string @37.18-37.21
					(e-string-part @37.19-37.20 (raw "a")))
				(e-string @37.23-37.26
					(e-string-part @37.24-37.25 (raw "b")))
				(e-string @37.28-37.31
					(e-string-part @37.29-37.30 (raw "c")))))
		(s-decl @39.1-42.2
			(p-ident @39.1-39.5 (raw "main"))
			(e-lambda @39.8-42.2
				(args
					(p-underscore))
				(e-block @39.12-42.2
					(statements
						(e-record @41.5-41.72
							(field (field "int_id") (optional false))
							(field (field "str_id") (optional false))
							(field (field "list_id") (optional false))
							(field (field "int_list") (optional false))
							(field (field "str_list") (optional false))
							(field (field "int_pair") (optional false))
							(field (field "str_pair") (optional false)))))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# True let-polymorphism: identity function
id = |x| x

# id used with different types
int_id = id(42)
str_id = id("hello")
list_id = id([1, 2, 3])

# Polymorphic function to make single-element lists
make_list = |x| [x]

# make_list used with different types
int_list = make_list(42)
str_list = make_list("hello")
bool_list = make_list(True)

# Polymorphic pair constructor
pair = |a, b| {fst: a, snd: b}

# pair used with different types
int_pair = pair(1, 2)
str_pair = pair("hello", "world")
mixed_pair = pair(42, "answer")

# Polymorphic function that returns empty list
empty = |_| []

# empty used at different types (type determined by context)
empty_ints = empty(0)
empty_strs = empty(0)

# Demonstrating polymorphism by using the same empty function
# to create lists that will be used with different types
list_with_int = [1, 2, 3]
list_with_str = ["a", "b", "c"]

main = |_| {
	# Return values demonstrating polymorphic instantiation
	{int_id, str_id, list_id, int_list, str_list, int_pair, str_pair}
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
		(p-assign @8.1-8.7 (ident "str_id"))
		(e-call @8.10-8.21
			(e-lookup-local @8.10-8.12
				(pattern @4.1-4.3))
			(e-string @8.13-8.20
				(e-literal @8.14-8.19 (string "hello")))))
	(d-let
		(p-assign @9.1-9.8 (ident "list_id"))
		(e-call @9.11-9.24
			(e-lookup-local @9.11-9.13
				(pattern @4.1-4.3))
			(e-list @9.14-9.23
				(elems
					(e-int @9.15-9.16 (value "1"))
					(e-int @9.18-9.19 (value "2"))
					(e-int @9.21-9.22 (value "3"))))))
	(d-let
		(p-assign @12.1-12.10 (ident "make_list"))
		(e-lambda @12.13-12.20
			(args
				(p-assign @12.14-12.15 (ident "x")))
			(e-list @12.17-12.20
				(elems
					(e-lookup-local @12.18-12.19
						(pattern @12.14-12.15))))))
	(d-let
		(p-assign @15.1-15.9 (ident "int_list"))
		(e-call @15.12-15.25
			(e-lookup-local @15.12-15.21
				(pattern @12.1-12.10))
			(e-int @15.22-15.24 (value "42"))))
	(d-let
		(p-assign @16.1-16.9 (ident "str_list"))
		(e-call @16.12-16.30
			(e-lookup-local @16.12-16.21
				(pattern @12.1-12.10))
			(e-string @16.22-16.29
				(e-literal @16.23-16.28 (string "hello")))))
	(d-let
		(p-assign @17.1-17.10 (ident "bool_list"))
		(e-call @17.13-17.28
			(e-lookup-local @17.13-17.22
				(pattern @12.1-12.10))
			(e-tag @17.23-17.27 (name "True"))))
	(d-let
		(p-assign @20.1-20.5 (ident "pair"))
		(e-lambda @20.8-20.33
			(args
				(p-assign @20.9-20.10 (ident "a"))
				(p-assign @20.12-20.13 (ident "b")))
			(e-record @20.15-20.33
				(fields
					(field (name "fst")
						(e-lookup-local @20.22-20.23
							(pattern @20.9-20.10)))
					(field (name "snd")
						(e-lookup-local @20.30-20.31
							(pattern @20.12-20.13)))))))
	(d-let
		(p-assign @23.1-23.9 (ident "int_pair"))
		(e-call @23.12-23.22
			(e-lookup-local @23.12-23.16
				(pattern @20.1-20.5))
			(e-int @23.17-23.18 (value "1"))
			(e-int @23.20-23.21 (value "2"))))
	(d-let
		(p-assign @24.1-24.9 (ident "str_pair"))
		(e-call @24.12-24.34
			(e-lookup-local @24.12-24.16
				(pattern @20.1-20.5))
			(e-string @24.17-24.24
				(e-literal @24.18-24.23 (string "hello")))
			(e-string @24.26-24.33
				(e-literal @24.27-24.32 (string "world")))))
	(d-let
		(p-assign @25.1-25.11 (ident "mixed_pair"))
		(e-call @25.14-25.32
			(e-lookup-local @25.14-25.18
				(pattern @20.1-20.5))
			(e-int @25.19-25.21 (value "42"))
			(e-string @25.23-25.31
				(e-literal @25.24-25.30 (string "answer")))))
	(d-let
		(p-assign @28.1-28.6 (ident "empty"))
		(e-lambda @28.9-28.15
			(args
				(p-underscore @28.10-28.11))
			(e-empty_list @28.13-28.15)))
	(d-let
		(p-assign @31.1-31.11 (ident "empty_ints"))
		(e-call @31.14-31.22
			(e-lookup-local @31.14-31.19
				(pattern @28.1-28.6))
			(e-int @31.20-31.21 (value "0"))))
	(d-let
		(p-assign @32.1-32.11 (ident "empty_strs"))
		(e-call @32.14-32.22
			(e-lookup-local @32.14-32.19
				(pattern @28.1-28.6))
			(e-int @32.20-32.21 (value "0"))))
	(d-let
		(p-assign @36.1-36.14 (ident "list_with_int"))
		(e-list @36.17-36.26
			(elems
				(e-int @36.18-36.19 (value "1"))
				(e-int @36.21-36.22 (value "2"))
				(e-int @36.24-36.25 (value "3")))))
	(d-let
		(p-assign @37.1-37.14 (ident "list_with_str"))
		(e-list @37.17-37.32
			(elems
				(e-string @37.18-37.21
					(e-literal @37.19-37.20 (string "a")))
				(e-string @37.23-37.26
					(e-literal @37.24-37.25 (string "b")))
				(e-string @37.28-37.31
					(e-literal @37.29-37.30 (string "c"))))))
	(d-let
		(p-assign @39.1-39.5 (ident "main"))
		(e-lambda @39.8-42.2
			(args
				(p-underscore @39.9-39.10))
			(e-block @39.12-42.2
				(e-record @41.5-41.72
					(fields
						(field (name "int_id")
							(e-lookup-local @41.7-41.14
								(pattern @7.1-7.7)))
						(field (name "str_id")
							(e-lookup-local @41.15-41.22
								(pattern @8.1-8.7)))
						(field (name "list_id")
							(e-lookup-local @41.23-41.31
								(pattern @9.1-9.8)))
						(field (name "int_list")
							(e-lookup-local @41.32-41.41
								(pattern @15.1-15.9)))
						(field (name "str_list")
							(e-lookup-local @41.42-41.51
								(pattern @16.1-16.9)))
						(field (name "int_pair")
							(e-lookup-local @41.52-41.61
								(pattern @23.1-23.9)))
						(field (name "str_pair")
							(e-lookup-local @41.62-41.72
								(pattern @24.1-24.9)))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.3 (type "* -> *"))
		(patt @7.1-7.7 (type "*"))
		(patt @8.1-8.7 (type "*"))
		(patt @9.1-9.8 (type "*"))
		(patt @12.1-12.10 (type "* -> List(*)"))
		(patt @15.1-15.9 (type "List(*)"))
		(patt @16.1-16.9 (type "List(*)"))
		(patt @17.1-17.10 (type "List(*)"))
		(patt @20.1-20.5 (type "*, * -> { fst: *, snd: * }"))
		(patt @23.1-23.9 (type "{ fst: *, snd: * }"))
		(patt @24.1-24.9 (type "{ fst: *, snd: * }"))
		(patt @25.1-25.11 (type "{ fst: *, snd: * }"))
		(patt @28.1-28.6 (type "* -> List(*)"))
		(patt @31.1-31.11 (type "List(*)"))
		(patt @32.1-32.11 (type "List(*)"))
		(patt @36.1-36.14 (type "List(Num(*))"))
		(patt @37.1-37.14 (type "List(Str)"))
		(patt @39.1-39.5 (type "* -> { int_id: *, str_id: *, list_id: *, int_list: List(*), str_list: List(*), int_pair: { fst: *, snd: * }, str_pair: { fst: *, snd: * } }")))
	(expressions
		(expr @4.6-4.11 (type "* -> *"))
		(expr @7.10-7.16 (type "*"))
		(expr @8.10-8.21 (type "*"))
		(expr @9.11-9.24 (type "*"))
		(expr @12.13-12.20 (type "* -> List(*)"))
		(expr @15.12-15.25 (type "List(*)"))
		(expr @16.12-16.30 (type "List(*)"))
		(expr @17.13-17.28 (type "List(*)"))
		(expr @20.8-20.33 (type "*, * -> { fst: *, snd: * }"))
		(expr @23.12-23.22 (type "{ fst: *, snd: * }"))
		(expr @24.12-24.34 (type "{ fst: *, snd: * }"))
		(expr @25.14-25.32 (type "{ fst: *, snd: * }"))
		(expr @28.9-28.15 (type "* -> List(*)"))
		(expr @31.14-31.22 (type "List(*)"))
		(expr @32.14-32.22 (type "List(*)"))
		(expr @36.17-36.26 (type "List(Num(*))"))
		(expr @37.17-37.32 (type "List(Str)"))
		(expr @39.8-42.2 (type "* -> { int_id: *, str_id: *, list_id: *, int_list: List(*), str_list: List(*), int_pair: { fst: *, snd: * }, str_pair: { fst: *, snd: * } }"))))
~~~
