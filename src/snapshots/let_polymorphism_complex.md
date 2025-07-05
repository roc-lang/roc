# META
~~~ini
description=Complex let-polymorphism interactions
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# let-polymorphism: polymorphic identity function
id = |x| x

# id used at different types
id_int = id(42)
id_str = id("hello")
id_bool = id(True)
id_list = id([1, 2, 3])
id_record = id({ x: 10, y: 20 })

# Polymorphic const function
const = |x| |_| x

# const instantiated at different types
const_int = const(100)
const_str = const("world")
const_list = const([True, False])

# Using the const functions
result_int = const_int(999)
result_str = const_str(999)
result_list = const_list(999)

# Polymorphic pair constructor
make_pair = |a, b| { fst: a, snd: b }

# make_pair used with different types
int_pair = make_pair(1, 2)
str_pair = make_pair("hello", "world")
mixed_pair = make_pair(42, "answer")

# Polymorphic function to create single-element lists
make_list = |x| [x]

# make_list used with different types
list_of_int = make_list(42)
list_of_str = make_list("hello")
list_of_bool = make_list(True)

# Polymorphic compose function
compose = |f, g| |x| f(g(x))

# compose used with different function types
add_one = |x| x + 1
double = |x| x * 2
add_then_double = compose(double, add_one)
double_then_add = compose(add_one, double)

result1 = add_then_double(5)  # (5 + 1) * 2 = 12
result2 = double_then_add(5)  # (5 * 2) + 1 = 11

# Polymorphic numeric literal
num = 42

# num used as Int
int_use = num + 10

# num used as Float
float_use = num * 3.14

# Polymorphic empty function
empty = |_| []

# empty used at different types
empty_ints = empty(0)
empty_strs = empty(0)

# Demonstrating polymorphism by using empty function results
# in contexts that require specific types
list_with_ints = [1, 2, 3]
list_with_strs = ["a", "b", "c"]

main = |_| {
    # Return values demonstrating polymorphic instantiation
    { id_int, id_str, result_int, result_str, int_use, float_use, int_pair, str_pair }
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:50),
LowerIdent(4:1-4:3),OpAssign(4:4-4:5),OpBar(4:6-4:7),LowerIdent(4:7-4:8),OpBar(4:8-4:9),LowerIdent(4:10-4:11),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:29),
LowerIdent(7:1-7:7),OpAssign(7:8-7:9),LowerIdent(7:10-7:12),NoSpaceOpenRound(7:12-7:13),Int(7:13-7:15),CloseRound(7:15-7:16),Newline(1:1-1:1),
LowerIdent(8:1-8:7),OpAssign(8:8-8:9),LowerIdent(8:10-8:12),NoSpaceOpenRound(8:12-8:13),StringStart(8:13-8:14),StringPart(8:14-8:19),StringEnd(8:19-8:20),CloseRound(8:20-8:21),Newline(1:1-1:1),
LowerIdent(9:1-9:8),OpAssign(9:9-9:10),LowerIdent(9:11-9:13),NoSpaceOpenRound(9:13-9:14),UpperIdent(9:14-9:18),CloseRound(9:18-9:19),Newline(1:1-1:1),
LowerIdent(10:1-10:8),OpAssign(10:9-10:10),LowerIdent(10:11-10:13),NoSpaceOpenRound(10:13-10:14),OpenSquare(10:14-10:15),Int(10:15-10:16),Comma(10:16-10:17),Int(10:18-10:19),Comma(10:19-10:20),Int(10:21-10:22),CloseSquare(10:22-10:23),CloseRound(10:23-10:24),Newline(1:1-1:1),
LowerIdent(11:1-11:10),OpAssign(11:11-11:12),LowerIdent(11:13-11:15),NoSpaceOpenRound(11:15-11:16),OpenCurly(11:16-11:17),LowerIdent(11:18-11:19),OpColon(11:19-11:20),Int(11:21-11:23),Comma(11:23-11:24),LowerIdent(11:25-11:26),OpColon(11:26-11:27),Int(11:28-11:30),CloseCurly(11:31-11:32),CloseRound(11:32-11:33),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(13:2-13:29),
LowerIdent(14:1-14:6),OpAssign(14:7-14:8),OpBar(14:9-14:10),LowerIdent(14:10-14:11),OpBar(14:11-14:12),OpBar(14:13-14:14),Underscore(14:14-14:15),OpBar(14:15-14:16),LowerIdent(14:17-14:18),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(16:2-16:40),
LowerIdent(17:1-17:10),OpAssign(17:11-17:12),LowerIdent(17:13-17:18),NoSpaceOpenRound(17:18-17:19),Int(17:19-17:22),CloseRound(17:22-17:23),Newline(1:1-1:1),
LowerIdent(18:1-18:10),OpAssign(18:11-18:12),LowerIdent(18:13-18:18),NoSpaceOpenRound(18:18-18:19),StringStart(18:19-18:20),StringPart(18:20-18:25),StringEnd(18:25-18:26),CloseRound(18:26-18:27),Newline(1:1-1:1),
LowerIdent(19:1-19:11),OpAssign(19:12-19:13),LowerIdent(19:14-19:19),NoSpaceOpenRound(19:19-19:20),OpenSquare(19:20-19:21),UpperIdent(19:21-19:25),Comma(19:25-19:26),UpperIdent(19:27-19:32),CloseSquare(19:32-19:33),CloseRound(19:33-19:34),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(21:2-21:28),
LowerIdent(22:1-22:11),OpAssign(22:12-22:13),LowerIdent(22:14-22:23),NoSpaceOpenRound(22:23-22:24),Int(22:24-22:27),CloseRound(22:27-22:28),Newline(1:1-1:1),
LowerIdent(23:1-23:11),OpAssign(23:12-23:13),LowerIdent(23:14-23:23),NoSpaceOpenRound(23:23-23:24),Int(23:24-23:27),CloseRound(23:27-23:28),Newline(1:1-1:1),
LowerIdent(24:1-24:12),OpAssign(24:13-24:14),LowerIdent(24:15-24:25),NoSpaceOpenRound(24:25-24:26),Int(24:26-24:29),CloseRound(24:29-24:30),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(26:2-26:31),
LowerIdent(27:1-27:10),OpAssign(27:11-27:12),OpBar(27:13-27:14),LowerIdent(27:14-27:15),Comma(27:15-27:16),LowerIdent(27:17-27:18),OpBar(27:18-27:19),OpenCurly(27:20-27:21),LowerIdent(27:22-27:25),OpColon(27:25-27:26),LowerIdent(27:27-27:28),Comma(27:28-27:29),LowerIdent(27:30-27:33),OpColon(27:33-27:34),LowerIdent(27:35-27:36),CloseCurly(27:37-27:38),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(29:2-29:38),
LowerIdent(30:1-30:9),OpAssign(30:10-30:11),LowerIdent(30:12-30:21),NoSpaceOpenRound(30:21-30:22),Int(30:22-30:23),Comma(30:23-30:24),Int(30:25-30:26),CloseRound(30:26-30:27),Newline(1:1-1:1),
LowerIdent(31:1-31:9),OpAssign(31:10-31:11),LowerIdent(31:12-31:21),NoSpaceOpenRound(31:21-31:22),StringStart(31:22-31:23),StringPart(31:23-31:28),StringEnd(31:28-31:29),Comma(31:29-31:30),StringStart(31:31-31:32),StringPart(31:32-31:37),StringEnd(31:37-31:38),CloseRound(31:38-31:39),Newline(1:1-1:1),
LowerIdent(32:1-32:11),OpAssign(32:12-32:13),LowerIdent(32:14-32:23),NoSpaceOpenRound(32:23-32:24),Int(32:24-32:26),Comma(32:26-32:27),StringStart(32:28-32:29),StringPart(32:29-32:35),StringEnd(32:35-32:36),CloseRound(32:36-32:37),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(34:2-34:54),
LowerIdent(35:1-35:10),OpAssign(35:11-35:12),OpBar(35:13-35:14),LowerIdent(35:14-35:15),OpBar(35:15-35:16),OpenSquare(35:17-35:18),LowerIdent(35:18-35:19),CloseSquare(35:19-35:20),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(37:2-37:38),
LowerIdent(38:1-38:12),OpAssign(38:13-38:14),LowerIdent(38:15-38:24),NoSpaceOpenRound(38:24-38:25),Int(38:25-38:27),CloseRound(38:27-38:28),Newline(1:1-1:1),
LowerIdent(39:1-39:12),OpAssign(39:13-39:14),LowerIdent(39:15-39:24),NoSpaceOpenRound(39:24-39:25),StringStart(39:25-39:26),StringPart(39:26-39:31),StringEnd(39:31-39:32),CloseRound(39:32-39:33),Newline(1:1-1:1),
LowerIdent(40:1-40:13),OpAssign(40:14-40:15),LowerIdent(40:16-40:25),NoSpaceOpenRound(40:25-40:26),UpperIdent(40:26-40:30),CloseRound(40:30-40:31),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(42:2-42:31),
LowerIdent(43:1-43:8),OpAssign(43:9-43:10),OpBar(43:11-43:12),LowerIdent(43:12-43:13),Comma(43:13-43:14),LowerIdent(43:15-43:16),OpBar(43:16-43:17),OpBar(43:18-43:19),LowerIdent(43:19-43:20),OpBar(43:20-43:21),LowerIdent(43:22-43:23),NoSpaceOpenRound(43:23-43:24),LowerIdent(43:24-43:25),NoSpaceOpenRound(43:25-43:26),LowerIdent(43:26-43:27),CloseRound(43:27-43:28),CloseRound(43:28-43:29),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(45:2-45:45),
LowerIdent(46:1-46:8),OpAssign(46:9-46:10),OpBar(46:11-46:12),LowerIdent(46:12-46:13),OpBar(46:13-46:14),LowerIdent(46:15-46:16),OpPlus(46:17-46:18),Int(46:19-46:20),Newline(1:1-1:1),
LowerIdent(47:1-47:7),OpAssign(47:8-47:9),OpBar(47:10-47:11),LowerIdent(47:11-47:12),OpBar(47:12-47:13),LowerIdent(47:14-47:15),OpStar(47:16-47:17),Int(47:18-47:19),Newline(1:1-1:1),
LowerIdent(48:1-48:16),OpAssign(48:17-48:18),LowerIdent(48:19-48:26),NoSpaceOpenRound(48:26-48:27),LowerIdent(48:27-48:33),Comma(48:33-48:34),LowerIdent(48:35-48:42),CloseRound(48:42-48:43),Newline(1:1-1:1),
LowerIdent(49:1-49:16),OpAssign(49:17-49:18),LowerIdent(49:19-49:26),NoSpaceOpenRound(49:26-49:27),LowerIdent(49:27-49:34),Comma(49:34-49:35),LowerIdent(49:36-49:42),CloseRound(49:42-49:43),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(51:1-51:8),OpAssign(51:9-51:10),LowerIdent(51:11-51:26),NoSpaceOpenRound(51:26-51:27),Int(51:27-51:28),CloseRound(51:28-51:29),Newline(51:32-51:49),
LowerIdent(52:1-52:8),OpAssign(52:9-52:10),LowerIdent(52:11-52:26),NoSpaceOpenRound(52:26-52:27),Int(52:27-52:28),CloseRound(52:28-52:29),Newline(52:32-52:49),
Newline(1:1-1:1),
Newline(54:2-54:30),
LowerIdent(55:1-55:4),OpAssign(55:5-55:6),Int(55:7-55:9),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(57:2-57:18),
LowerIdent(58:1-58:8),OpAssign(58:9-58:10),LowerIdent(58:11-58:14),OpPlus(58:15-58:16),Int(58:17-58:19),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(60:2-60:20),
LowerIdent(61:1-61:10),OpAssign(61:11-61:12),LowerIdent(61:13-61:16),OpStar(61:17-61:18),Float(61:19-61:23),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(63:2-63:29),
LowerIdent(64:1-64:6),OpAssign(64:7-64:8),OpBar(64:9-64:10),Underscore(64:10-64:11),OpBar(64:11-64:12),OpenSquare(64:13-64:14),CloseSquare(64:14-64:15),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(66:2-66:32),
LowerIdent(67:1-67:11),OpAssign(67:12-67:13),LowerIdent(67:14-67:19),NoSpaceOpenRound(67:19-67:20),Int(67:20-67:21),CloseRound(67:21-67:22),Newline(1:1-1:1),
LowerIdent(68:1-68:11),OpAssign(68:12-68:13),LowerIdent(68:14-68:19),NoSpaceOpenRound(68:19-68:20),Int(68:20-68:21),CloseRound(68:21-68:22),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(70:2-70:61),
Newline(71:2-71:42),
LowerIdent(72:1-72:15),OpAssign(72:16-72:17),OpenSquare(72:18-72:19),Int(72:19-72:20),Comma(72:20-72:21),Int(72:22-72:23),Comma(72:23-72:24),Int(72:25-72:26),CloseSquare(72:26-72:27),Newline(1:1-1:1),
LowerIdent(73:1-73:15),OpAssign(73:16-73:17),OpenSquare(73:18-73:19),StringStart(73:19-73:20),StringPart(73:20-73:21),StringEnd(73:21-73:22),Comma(73:22-73:23),StringStart(73:24-73:25),StringPart(73:25-73:26),StringEnd(73:26-73:27),Comma(73:27-73:28),StringStart(73:29-73:30),StringPart(73:30-73:31),StringEnd(73:31-73:32),CloseSquare(73:32-73:33),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(75:1-75:5),OpAssign(75:6-75:7),OpBar(75:8-75:9),Underscore(75:9-75:10),OpBar(75:10-75:11),OpenCurly(75:12-75:13),Newline(1:1-1:1),
Newline(76:6-76:60),
OpenCurly(77:5-77:6),LowerIdent(77:7-77:13),Comma(77:13-77:14),LowerIdent(77:15-77:21),Comma(77:21-77:22),LowerIdent(77:23-77:33),Comma(77:33-77:34),LowerIdent(77:35-77:45),Comma(77:45-77:46),LowerIdent(77:47-77:54),Comma(77:54-77:55),LowerIdent(77:56-77:65),Comma(77:65-77:66),LowerIdent(77:67-77:75),Comma(77:75-77:76),LowerIdent(77:77-77:85),CloseCurly(77:86-77:87),Newline(1:1-1:1),
CloseCurly(78:1-78:2),EndOfFile(78:2-78:2),
~~~
# PARSE
~~~clojure
(file @1.1-78.2
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
			(p-ident @7.1-7.7 (raw "id_int"))
			(e-apply @7.10-7.16
				(e-ident @7.10-7.12 (raw "id"))
				(e-int @7.13-7.15 (raw "42"))))
		(s-decl @8.1-8.21
			(p-ident @8.1-8.7 (raw "id_str"))
			(e-apply @8.10-8.21
				(e-ident @8.10-8.12 (raw "id"))
				(e-string @8.13-8.20
					(e-string-part @8.14-8.19 (raw "hello")))))
		(s-decl @9.1-9.19
			(p-ident @9.1-9.8 (raw "id_bool"))
			(e-apply @9.11-9.19
				(e-ident @9.11-9.13 (raw "id"))
				(e-tag @9.14-9.18 (raw "True"))))
		(s-decl @10.1-10.24
			(p-ident @10.1-10.8 (raw "id_list"))
			(e-apply @10.11-10.24
				(e-ident @10.11-10.13 (raw "id"))
				(e-list @10.14-10.23
					(e-int @10.15-10.16 (raw "1"))
					(e-int @10.18-10.19 (raw "2"))
					(e-int @10.21-10.22 (raw "3")))))
		(s-decl @11.1-11.33
			(p-ident @11.1-11.10 (raw "id_record"))
			(e-apply @11.13-11.33
				(e-ident @11.13-11.15 (raw "id"))
				(e-record @11.16-11.32
					(field (field "x") (optional false)
						(e-int @11.21-11.23 (raw "10")))
					(field (field "y") (optional false)
						(e-int @11.28-11.30 (raw "20"))))))
		(s-decl @14.1-14.18
			(p-ident @14.1-14.6 (raw "const"))
			(e-lambda @14.9-14.18
				(args
					(p-ident @14.10-14.11 (raw "x")))
				(e-lambda @14.13-14.18
					(args
						(p-underscore))
					(e-ident @14.17-14.18 (raw "x")))))
		(s-decl @17.1-17.23
			(p-ident @17.1-17.10 (raw "const_int"))
			(e-apply @17.13-17.23
				(e-ident @17.13-17.18 (raw "const"))
				(e-int @17.19-17.22 (raw "100"))))
		(s-decl @18.1-18.27
			(p-ident @18.1-18.10 (raw "const_str"))
			(e-apply @18.13-18.27
				(e-ident @18.13-18.18 (raw "const"))
				(e-string @18.19-18.26
					(e-string-part @18.20-18.25 (raw "world")))))
		(s-decl @19.1-19.34
			(p-ident @19.1-19.11 (raw "const_list"))
			(e-apply @19.14-19.34
				(e-ident @19.14-19.19 (raw "const"))
				(e-list @19.20-19.33
					(e-tag @19.21-19.25 (raw "True"))
					(e-tag @19.27-19.32 (raw "False")))))
		(s-decl @22.1-22.28
			(p-ident @22.1-22.11 (raw "result_int"))
			(e-apply @22.14-22.28
				(e-ident @22.14-22.23 (raw "const_int"))
				(e-int @22.24-22.27 (raw "999"))))
		(s-decl @23.1-23.28
			(p-ident @23.1-23.11 (raw "result_str"))
			(e-apply @23.14-23.28
				(e-ident @23.14-23.23 (raw "const_str"))
				(e-int @23.24-23.27 (raw "999"))))
		(s-decl @24.1-24.30
			(p-ident @24.1-24.12 (raw "result_list"))
			(e-apply @24.15-24.30
				(e-ident @24.15-24.25 (raw "const_list"))
				(e-int @24.26-24.29 (raw "999"))))
		(s-decl @27.1-27.38
			(p-ident @27.1-27.10 (raw "make_pair"))
			(e-lambda @27.13-27.38
				(args
					(p-ident @27.14-27.15 (raw "a"))
					(p-ident @27.17-27.18 (raw "b")))
				(e-record @27.20-27.38
					(field (field "fst") (optional false)
						(e-ident @27.27-27.28 (raw "a")))
					(field (field "snd") (optional false)
						(e-ident @27.35-27.36 (raw "b"))))))
		(s-decl @30.1-30.27
			(p-ident @30.1-30.9 (raw "int_pair"))
			(e-apply @30.12-30.27
				(e-ident @30.12-30.21 (raw "make_pair"))
				(e-int @30.22-30.23 (raw "1"))
				(e-int @30.25-30.26 (raw "2"))))
		(s-decl @31.1-31.39
			(p-ident @31.1-31.9 (raw "str_pair"))
			(e-apply @31.12-31.39
				(e-ident @31.12-31.21 (raw "make_pair"))
				(e-string @31.22-31.29
					(e-string-part @31.23-31.28 (raw "hello")))
				(e-string @31.31-31.38
					(e-string-part @31.32-31.37 (raw "world")))))
		(s-decl @32.1-32.37
			(p-ident @32.1-32.11 (raw "mixed_pair"))
			(e-apply @32.14-32.37
				(e-ident @32.14-32.23 (raw "make_pair"))
				(e-int @32.24-32.26 (raw "42"))
				(e-string @32.28-32.36
					(e-string-part @32.29-32.35 (raw "answer")))))
		(s-decl @35.1-35.20
			(p-ident @35.1-35.10 (raw "make_list"))
			(e-lambda @35.13-35.20
				(args
					(p-ident @35.14-35.15 (raw "x")))
				(e-list @35.17-35.20
					(e-ident @35.18-35.19 (raw "x")))))
		(s-decl @38.1-38.28
			(p-ident @38.1-38.12 (raw "list_of_int"))
			(e-apply @38.15-38.28
				(e-ident @38.15-38.24 (raw "make_list"))
				(e-int @38.25-38.27 (raw "42"))))
		(s-decl @39.1-39.33
			(p-ident @39.1-39.12 (raw "list_of_str"))
			(e-apply @39.15-39.33
				(e-ident @39.15-39.24 (raw "make_list"))
				(e-string @39.25-39.32
					(e-string-part @39.26-39.31 (raw "hello")))))
		(s-decl @40.1-40.31
			(p-ident @40.1-40.13 (raw "list_of_bool"))
			(e-apply @40.16-40.31
				(e-ident @40.16-40.25 (raw "make_list"))
				(e-tag @40.26-40.30 (raw "True"))))
		(s-decl @43.1-43.29
			(p-ident @43.1-43.8 (raw "compose"))
			(e-lambda @43.11-43.29
				(args
					(p-ident @43.12-43.13 (raw "f"))
					(p-ident @43.15-43.16 (raw "g")))
				(e-lambda @43.18-43.29
					(args
						(p-ident @43.19-43.20 (raw "x")))
					(e-apply @43.22-43.29
						(e-ident @43.22-43.23 (raw "f"))
						(e-apply @43.24-43.28
							(e-ident @43.24-43.25 (raw "g"))
							(e-ident @43.26-43.27 (raw "x")))))))
		(s-decl @46.1-47.7
			(p-ident @46.1-46.8 (raw "add_one"))
			(e-lambda @46.11-47.7
				(args
					(p-ident @46.12-46.13 (raw "x")))
				(e-binop @46.15-47.7 (op "+")
					(e-ident @46.15-46.16 (raw "x"))
					(e-int @46.19-46.20 (raw "1")))))
		(s-decl @47.1-48.16
			(p-ident @47.1-47.7 (raw "double"))
			(e-lambda @47.10-48.16
				(args
					(p-ident @47.11-47.12 (raw "x")))
				(e-binop @47.14-48.16 (op "*")
					(e-ident @47.14-47.15 (raw "x"))
					(e-int @47.18-47.19 (raw "2")))))
		(s-decl @48.1-48.43
			(p-ident @48.1-48.16 (raw "add_then_double"))
			(e-apply @48.19-48.43
				(e-ident @48.19-48.26 (raw "compose"))
				(e-ident @48.27-48.33 (raw "double"))
				(e-ident @48.35-48.42 (raw "add_one"))))
		(s-decl @49.1-49.43
			(p-ident @49.1-49.16 (raw "double_then_add"))
			(e-apply @49.19-49.43
				(e-ident @49.19-49.26 (raw "compose"))
				(e-ident @49.27-49.34 (raw "add_one"))
				(e-ident @49.36-49.42 (raw "double"))))
		(s-decl @51.1-51.29
			(p-ident @51.1-51.8 (raw "result1"))
			(e-apply @51.11-51.29
				(e-ident @51.11-51.26 (raw "add_then_double"))
				(e-int @51.27-51.28 (raw "5"))))
		(s-decl @52.1-52.29
			(p-ident @52.1-52.8 (raw "result2"))
			(e-apply @52.11-52.29
				(e-ident @52.11-52.26 (raw "double_then_add"))
				(e-int @52.27-52.28 (raw "5"))))
		(s-decl @55.1-55.9
			(p-ident @55.1-55.4 (raw "num"))
			(e-int @55.7-55.9 (raw "42")))
		(s-decl @58.1-61.10
			(p-ident @58.1-58.8 (raw "int_use"))
			(e-binop @58.11-61.10 (op "+")
				(e-ident @58.11-58.14 (raw "num"))
				(e-int @58.17-58.19 (raw "10"))))
		(s-decl @61.1-64.6
			(p-ident @61.1-61.10 (raw "float_use"))
			(e-binop @61.13-64.6 (op "*")
				(e-ident @61.13-61.16 (raw "num"))
				(e-frac @61.19-61.23 (raw "3.14"))))
		(s-decl @64.1-64.15
			(p-ident @64.1-64.6 (raw "empty"))
			(e-lambda @64.9-64.15
				(args
					(p-underscore))
				(e-list @64.13-64.15)))
		(s-decl @67.1-67.22
			(p-ident @67.1-67.11 (raw "empty_ints"))
			(e-apply @67.14-67.22
				(e-ident @67.14-67.19 (raw "empty"))
				(e-int @67.20-67.21 (raw "0"))))
		(s-decl @68.1-68.22
			(p-ident @68.1-68.11 (raw "empty_strs"))
			(e-apply @68.14-68.22
				(e-ident @68.14-68.19 (raw "empty"))
				(e-int @68.20-68.21 (raw "0"))))
		(s-decl @72.1-72.27
			(p-ident @72.1-72.15 (raw "list_with_ints"))
			(e-list @72.18-72.27
				(e-int @72.19-72.20 (raw "1"))
				(e-int @72.22-72.23 (raw "2"))
				(e-int @72.25-72.26 (raw "3"))))
		(s-decl @73.1-73.33
			(p-ident @73.1-73.15 (raw "list_with_strs"))
			(e-list @73.18-73.33
				(e-string @73.19-73.22
					(e-string-part @73.20-73.21 (raw "a")))
				(e-string @73.24-73.27
					(e-string-part @73.25-73.26 (raw "b")))
				(e-string @73.29-73.32
					(e-string-part @73.30-73.31 (raw "c")))))
		(s-decl @75.1-78.2
			(p-ident @75.1-75.5 (raw "main"))
			(e-lambda @75.8-78.2
				(args
					(p-underscore))
				(e-block @75.12-78.2
					(statements
						(e-record @77.5-77.87
							(field (field "id_int") (optional false))
							(field (field "id_str") (optional false))
							(field (field "result_int") (optional false))
							(field (field "result_str") (optional false))
							(field (field "int_use") (optional false))
							(field (field "float_use") (optional false))
							(field (field "int_pair") (optional false))
							(field (field "str_pair") (optional false)))))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# let-polymorphism: polymorphic identity function
id = |x| x

# id used at different types
id_int = id(42)
id_str = id("hello")
id_bool = id(True)
id_list = id([1, 2, 3])
id_record = id({x: 10, y: 20})

# Polymorphic const function
const = |x| |_| x

# const instantiated at different types
const_int = const(100)
const_str = const("world")
const_list = const([True, False])

# Using the const functions
result_int = const_int(999)
result_str = const_str(999)
result_list = const_list(999)

# Polymorphic pair constructor
make_pair = |a, b| {fst: a, snd: b}

# make_pair used with different types
int_pair = make_pair(1, 2)
str_pair = make_pair("hello", "world")
mixed_pair = make_pair(42, "answer")

# Polymorphic function to create single-element lists
make_list = |x| [x]

# make_list used with different types
list_of_int = make_list(42)
list_of_str = make_list("hello")
list_of_bool = make_list(True)

# Polymorphic compose function
compose = |f, g| |x| f(g(x))

# compose used with different function types
add_one = |x| x + 1
double = |x| x * 2
add_then_double = compose(double, add_one)
double_then_add = compose(add_one, double)

result1 = add_then_double(5) # (5 + 1) * 2 = 12
result2 = double_then_add(5) # (5 * 2) + 1 = 11

# Polymorphic numeric literal
num = 42

# num used as Int
int_use = num + 10

# num used as Float
float_use = num * 3.14

# Polymorphic empty function
empty = |_| []

# empty used at different types
empty_ints = empty(0)
empty_strs = empty(0)

# Demonstrating polymorphism by using empty function results
# in contexts that require specific types
list_with_ints = [1, 2, 3]
list_with_strs = ["a", "b", "c"]

main = |_| {
	# Return values demonstrating polymorphic instantiation
	{id_int, id_str, result_int, result_str, int_use, float_use, int_pair, str_pair}
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
		(p-assign @7.1-7.7 (ident "id_int"))
		(e-call @7.10-7.16
			(e-lookup-local @7.10-7.12
				(pattern @4.1-4.3))
			(e-int @7.13-7.15 (value "42"))))
	(d-let
		(p-assign @8.1-8.7 (ident "id_str"))
		(e-call @8.10-8.21
			(e-lookup-local @8.10-8.12
				(pattern @4.1-4.3))
			(e-string @8.13-8.20
				(e-literal @8.14-8.19 (string "hello")))))
	(d-let
		(p-assign @9.1-9.8 (ident "id_bool"))
		(e-call @9.11-9.19
			(e-lookup-local @9.11-9.13
				(pattern @4.1-4.3))
			(e-tag @9.14-9.18 (name "True"))))
	(d-let
		(p-assign @10.1-10.8 (ident "id_list"))
		(e-call @10.11-10.24
			(e-lookup-local @10.11-10.13
				(pattern @4.1-4.3))
			(e-list @10.14-10.23
				(elems
					(e-int @10.15-10.16 (value "1"))
					(e-int @10.18-10.19 (value "2"))
					(e-int @10.21-10.22 (value "3"))))))
	(d-let
		(p-assign @11.1-11.10 (ident "id_record"))
		(e-call @11.13-11.33
			(e-lookup-local @11.13-11.15
				(pattern @4.1-4.3))
			(e-record @11.16-11.32
				(fields
					(field (name "x")
						(e-int @11.21-11.23 (value "10")))
					(field (name "y")
						(e-int @11.28-11.30 (value "20")))))))
	(d-let
		(p-assign @14.1-14.6 (ident "const"))
		(e-lambda @14.9-14.18
			(args
				(p-assign @14.10-14.11 (ident "x")))
			(e-lambda @14.13-14.18
				(args
					(p-underscore @14.14-14.15))
				(e-lookup-local @14.17-14.18
					(pattern @14.10-14.11)))))
	(d-let
		(p-assign @17.1-17.10 (ident "const_int"))
		(e-call @17.13-17.23
			(e-lookup-local @17.13-17.18
				(pattern @14.1-14.6))
			(e-int @17.19-17.22 (value "100"))))
	(d-let
		(p-assign @18.1-18.10 (ident "const_str"))
		(e-call @18.13-18.27
			(e-lookup-local @18.13-18.18
				(pattern @14.1-14.6))
			(e-string @18.19-18.26
				(e-literal @18.20-18.25 (string "world")))))
	(d-let
		(p-assign @19.1-19.11 (ident "const_list"))
		(e-call @19.14-19.34
			(e-lookup-local @19.14-19.19
				(pattern @14.1-14.6))
			(e-list @19.20-19.33
				(elems
					(e-tag @19.21-19.25 (name "True"))
					(e-tag @19.27-19.32 (name "False"))))))
	(d-let
		(p-assign @22.1-22.11 (ident "result_int"))
		(e-call @22.14-22.28
			(e-lookup-local @22.14-22.23
				(pattern @17.1-17.10))
			(e-int @22.24-22.27 (value "999"))))
	(d-let
		(p-assign @23.1-23.11 (ident "result_str"))
		(e-call @23.14-23.28
			(e-lookup-local @23.14-23.23
				(pattern @18.1-18.10))
			(e-int @23.24-23.27 (value "999"))))
	(d-let
		(p-assign @24.1-24.12 (ident "result_list"))
		(e-call @24.15-24.30
			(e-lookup-local @24.15-24.25
				(pattern @19.1-19.11))
			(e-int @24.26-24.29 (value "999"))))
	(d-let
		(p-assign @27.1-27.10 (ident "make_pair"))
		(e-lambda @27.13-27.38
			(args
				(p-assign @27.14-27.15 (ident "a"))
				(p-assign @27.17-27.18 (ident "b")))
			(e-record @27.20-27.38
				(fields
					(field (name "fst")
						(e-lookup-local @27.27-27.28
							(pattern @27.14-27.15)))
					(field (name "snd")
						(e-lookup-local @27.35-27.36
							(pattern @27.17-27.18)))))))
	(d-let
		(p-assign @30.1-30.9 (ident "int_pair"))
		(e-call @30.12-30.27
			(e-lookup-local @30.12-30.21
				(pattern @27.1-27.10))
			(e-int @30.22-30.23 (value "1"))
			(e-int @30.25-30.26 (value "2"))))
	(d-let
		(p-assign @31.1-31.9 (ident "str_pair"))
		(e-call @31.12-31.39
			(e-lookup-local @31.12-31.21
				(pattern @27.1-27.10))
			(e-string @31.22-31.29
				(e-literal @31.23-31.28 (string "hello")))
			(e-string @31.31-31.38
				(e-literal @31.32-31.37 (string "world")))))
	(d-let
		(p-assign @32.1-32.11 (ident "mixed_pair"))
		(e-call @32.14-32.37
			(e-lookup-local @32.14-32.23
				(pattern @27.1-27.10))
			(e-int @32.24-32.26 (value "42"))
			(e-string @32.28-32.36
				(e-literal @32.29-32.35 (string "answer")))))
	(d-let
		(p-assign @35.1-35.10 (ident "make_list"))
		(e-lambda @35.13-35.20
			(args
				(p-assign @35.14-35.15 (ident "x")))
			(e-list @35.17-35.20
				(elems
					(e-lookup-local @35.18-35.19
						(pattern @35.14-35.15))))))
	(d-let
		(p-assign @38.1-38.12 (ident "list_of_int"))
		(e-call @38.15-38.28
			(e-lookup-local @38.15-38.24
				(pattern @35.1-35.10))
			(e-int @38.25-38.27 (value "42"))))
	(d-let
		(p-assign @39.1-39.12 (ident "list_of_str"))
		(e-call @39.15-39.33
			(e-lookup-local @39.15-39.24
				(pattern @35.1-35.10))
			(e-string @39.25-39.32
				(e-literal @39.26-39.31 (string "hello")))))
	(d-let
		(p-assign @40.1-40.13 (ident "list_of_bool"))
		(e-call @40.16-40.31
			(e-lookup-local @40.16-40.25
				(pattern @35.1-35.10))
			(e-tag @40.26-40.30 (name "True"))))
	(d-let
		(p-assign @43.1-43.8 (ident "compose"))
		(e-lambda @43.11-43.29
			(args
				(p-assign @43.12-43.13 (ident "f"))
				(p-assign @43.15-43.16 (ident "g")))
			(e-lambda @43.18-43.29
				(args
					(p-assign @43.19-43.20 (ident "x")))
				(e-call @43.22-43.29
					(e-lookup-local @43.22-43.23
						(pattern @43.12-43.13))
					(e-call @43.24-43.28
						(e-lookup-local @43.24-43.25
							(pattern @43.15-43.16))
						(e-lookup-local @43.26-43.27
							(pattern @43.19-43.20)))))))
	(d-let
		(p-assign @46.1-46.8 (ident "add_one"))
		(e-lambda @46.11-47.7
			(args
				(p-assign @46.12-46.13 (ident "x")))
			(e-binop @46.15-47.7 (op "add")
				(e-lookup-local @46.15-46.16
					(pattern @46.12-46.13))
				(e-int @46.19-46.20 (value "1")))))
	(d-let
		(p-assign @47.1-47.7 (ident "double"))
		(e-lambda @47.10-48.16
			(args
				(p-assign @47.11-47.12 (ident "x")))
			(e-binop @47.14-48.16 (op "mul")
				(e-lookup-local @47.14-47.15
					(pattern @47.11-47.12))
				(e-int @47.18-47.19 (value "2")))))
	(d-let
		(p-assign @48.1-48.16 (ident "add_then_double"))
		(e-call @48.19-48.43
			(e-lookup-local @48.19-48.26
				(pattern @43.1-43.8))
			(e-lookup-local @48.27-48.33
				(pattern @47.1-47.7))
			(e-lookup-local @48.35-48.42
				(pattern @46.1-46.8))))
	(d-let
		(p-assign @49.1-49.16 (ident "double_then_add"))
		(e-call @49.19-49.43
			(e-lookup-local @49.19-49.26
				(pattern @43.1-43.8))
			(e-lookup-local @49.27-49.34
				(pattern @46.1-46.8))
			(e-lookup-local @49.36-49.42
				(pattern @47.1-47.7))))
	(d-let
		(p-assign @51.1-51.8 (ident "result1"))
		(e-call @51.11-51.29
			(e-lookup-local @51.11-51.26
				(pattern @48.1-48.16))
			(e-int @51.27-51.28 (value "5"))))
	(d-let
		(p-assign @52.1-52.8 (ident "result2"))
		(e-call @52.11-52.29
			(e-lookup-local @52.11-52.26
				(pattern @49.1-49.16))
			(e-int @52.27-52.28 (value "5"))))
	(d-let
		(p-assign @55.1-55.4 (ident "num"))
		(e-int @55.7-55.9 (value "42")))
	(d-let
		(p-assign @58.1-58.8 (ident "int_use"))
		(e-binop @58.11-61.10 (op "add")
			(e-lookup-local @58.11-58.14
				(pattern @55.1-55.4))
			(e-int @58.17-58.19 (value "10"))))
	(d-let
		(p-assign @61.1-61.10 (ident "float_use"))
		(e-binop @61.13-64.6 (op "mul")
			(e-lookup-local @61.13-61.16
				(pattern @55.1-55.4))
			(e-dec-small @61.19-61.23 (numerator "314") (denominator-power-of-ten "2") (value "3.14"))))
	(d-let
		(p-assign @64.1-64.6 (ident "empty"))
		(e-lambda @64.9-64.15
			(args
				(p-underscore @64.10-64.11))
			(e-empty_list @64.13-64.15)))
	(d-let
		(p-assign @67.1-67.11 (ident "empty_ints"))
		(e-call @67.14-67.22
			(e-lookup-local @67.14-67.19
				(pattern @64.1-64.6))
			(e-int @67.20-67.21 (value "0"))))
	(d-let
		(p-assign @68.1-68.11 (ident "empty_strs"))
		(e-call @68.14-68.22
			(e-lookup-local @68.14-68.19
				(pattern @64.1-64.6))
			(e-int @68.20-68.21 (value "0"))))
	(d-let
		(p-assign @72.1-72.15 (ident "list_with_ints"))
		(e-list @72.18-72.27
			(elems
				(e-int @72.19-72.20 (value "1"))
				(e-int @72.22-72.23 (value "2"))
				(e-int @72.25-72.26 (value "3")))))
	(d-let
		(p-assign @73.1-73.15 (ident "list_with_strs"))
		(e-list @73.18-73.33
			(elems
				(e-string @73.19-73.22
					(e-literal @73.20-73.21 (string "a")))
				(e-string @73.24-73.27
					(e-literal @73.25-73.26 (string "b")))
				(e-string @73.29-73.32
					(e-literal @73.30-73.31 (string "c"))))))
	(d-let
		(p-assign @75.1-75.5 (ident "main"))
		(e-lambda @75.8-78.2
			(args
				(p-underscore @75.9-75.10))
			(e-block @75.12-78.2
				(e-record @77.5-77.87
					(fields
						(field (name "id_int")
							(e-lookup-local @77.7-77.14
								(pattern @7.1-7.7)))
						(field (name "id_str")
							(e-lookup-local @77.15-77.22
								(pattern @8.1-8.7)))
						(field (name "result_int")
							(e-lookup-local @77.23-77.34
								(pattern @22.1-22.11)))
						(field (name "result_str")
							(e-lookup-local @77.35-77.46
								(pattern @23.1-23.11)))
						(field (name "int_use")
							(e-lookup-local @77.47-77.55
								(pattern @58.1-58.8)))
						(field (name "float_use")
							(e-lookup-local @77.56-77.66
								(pattern @61.1-61.10)))
						(field (name "int_pair")
							(e-lookup-local @77.67-77.76
								(pattern @30.1-30.9)))
						(field (name "str_pair")
							(e-lookup-local @77.77-77.87
								(pattern @31.1-31.9)))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.3 (type "* -> *"))
		(patt @7.1-7.7 (type "*"))
		(patt @8.1-8.7 (type "*"))
		(patt @9.1-9.8 (type "*"))
		(patt @10.1-10.8 (type "*"))
		(patt @11.1-11.10 (type "*"))
		(patt @14.1-14.6 (type "* -> * -> *"))
		(patt @17.1-17.10 (type "* -> *"))
		(patt @18.1-18.10 (type "* -> *"))
		(patt @19.1-19.11 (type "* -> *"))
		(patt @22.1-22.11 (type "*"))
		(patt @23.1-23.11 (type "*"))
		(patt @24.1-24.12 (type "*"))
		(patt @27.1-27.10 (type "*, * -> { fst: *, snd: * }"))
		(patt @30.1-30.9 (type "{ fst: *, snd: * }"))
		(patt @31.1-31.9 (type "{ fst: *, snd: * }"))
		(patt @32.1-32.11 (type "{ fst: *, snd: * }"))
		(patt @35.1-35.10 (type "* -> List(*)"))
		(patt @38.1-38.12 (type "List(*)"))
		(patt @39.1-39.12 (type "List(*)"))
		(patt @40.1-40.13 (type "List(*)"))
		(patt @43.1-43.8 (type "* -> *, * -> * -> * -> *"))
		(patt @46.1-46.8 (type "* -> *"))
		(patt @47.1-47.7 (type "* -> *"))
		(patt @48.1-48.16 (type "* -> *"))
		(patt @49.1-49.16 (type "* -> *"))
		(patt @51.1-51.8 (type "*"))
		(patt @52.1-52.8 (type "*"))
		(patt @55.1-55.4 (type "Num(*)"))
		(patt @58.1-58.8 (type "*"))
		(patt @61.1-61.10 (type "*"))
		(patt @64.1-64.6 (type "* -> List(*)"))
		(patt @67.1-67.11 (type "List(*)"))
		(patt @68.1-68.11 (type "List(*)"))
		(patt @72.1-72.15 (type "List(Num(*))"))
		(patt @73.1-73.15 (type "List(Str)"))
		(patt @75.1-75.5 (type "* -> { id_int: *, id_str: *, result_int: *, result_str: *, int_use: *, float_use: *, int_pair: { fst: *, snd: * }, str_pair: { fst: *, snd: * } }")))
	(expressions
		(expr @4.6-4.11 (type "* -> *"))
		(expr @7.10-7.16 (type "*"))
		(expr @8.10-8.21 (type "*"))
		(expr @9.11-9.19 (type "*"))
		(expr @10.11-10.24 (type "*"))
		(expr @11.13-11.33 (type "*"))
		(expr @14.9-14.18 (type "* -> * -> *"))
		(expr @17.13-17.23 (type "* -> *"))
		(expr @18.13-18.27 (type "* -> *"))
		(expr @19.14-19.34 (type "* -> *"))
		(expr @22.14-22.28 (type "*"))
		(expr @23.14-23.28 (type "*"))
		(expr @24.15-24.30 (type "*"))
		(expr @27.13-27.38 (type "*, * -> { fst: *, snd: * }"))
		(expr @30.12-30.27 (type "{ fst: *, snd: * }"))
		(expr @31.12-31.39 (type "{ fst: *, snd: * }"))
		(expr @32.14-32.37 (type "{ fst: *, snd: * }"))
		(expr @35.13-35.20 (type "* -> List(*)"))
		(expr @38.15-38.28 (type "List(*)"))
		(expr @39.15-39.33 (type "List(*)"))
		(expr @40.16-40.31 (type "List(*)"))
		(expr @43.11-43.29 (type "* -> *, * -> * -> * -> *"))
		(expr @46.11-47.7 (type "* -> *"))
		(expr @47.10-48.16 (type "* -> *"))
		(expr @48.19-48.43 (type "* -> *"))
		(expr @49.19-49.43 (type "* -> *"))
		(expr @51.11-51.29 (type "*"))
		(expr @52.11-52.29 (type "*"))
		(expr @55.7-55.9 (type "Num(*)"))
		(expr @58.11-61.10 (type "*"))
		(expr @61.13-64.6 (type "*"))
		(expr @64.9-64.15 (type "* -> List(*)"))
		(expr @67.14-67.22 (type "List(*)"))
		(expr @68.14-68.22 (type "List(*)"))
		(expr @72.18-72.27 (type "List(Num(*))"))
		(expr @73.18-73.33 (type "List(Str)"))
		(expr @75.8-78.2 (type "* -> { id_int: *, id_str: *, result_int: *, result_str: *, int_use: *, float_use: *, int_pair: { fst: *, snd: * }, str_pair: { fst: *, snd: * } }"))))
~~~
