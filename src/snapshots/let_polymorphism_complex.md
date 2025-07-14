# META
~~~ini
description=Complex let-polymorphism interactions
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic polymorphic values
num = 42
frac = 4.2
str = "hello"
bool = True

# Polymorphic empty collections
empty_list = []
empty_record = {}

# Using empty list in multiple contexts
int_list = [1, 2, 3]
str_list = ["a", "b", "c"]
bool_list = [True, False]

# Nested empty lists
nested_empty = [empty_list, empty_list, empty_list]
mixed_nested = [empty_list, [1, 2], empty_list, [3, 4]]

# Polymorphic record with empty list
poly_record = { items: empty_list, count: 0 }
use_poly_record1 = { items: [1, 2, 3], count: 0 }
use_poly_record2 = { items: ["x", "y", "z"], count: 0 }

# Complex nested structure with multiple polymorphic uses
base_config = {
    data: empty_list,
    metadata: {
        version: num,
        ratio: frac,
        description: str,
    },
}

# Different instantiations of base_config
config1 = {
    data: [1, 2, 3, 4, 5],
    metadata: {
        version: num,
        ratio: frac,
        description: str,
    },
    name: "integers",
}

config2 = {
    data: ["apple", "banana", "cherry"],
    metadata: {
        version: num,
        ratio: frac,
        description: str,
    },
    name: "fruits",
}

# Polymorphic function-like structures
make_container = |val| { value: val, wrapper: [val] }
container1 = make_container(num)
container2 = make_container(str)
container3 = make_container(frac)

# Deeply nested polymorphism
deep = {
    level1: {
        level2: {
            level3: {
                data: empty_list,
                value: num,
            },
            items: [num, num * 2, num * 3],
        },
        collection: empty_list,
    },
    results: [
        { data: [1], tag: "single" },
        { data: [1, 2], tag: "ints" },
        { data: [1, 2, 3], tag: "more" },
    ],
}

# Polymorphic values used in computations
compute1 = num + 10
compute2 = num * 2
compute3 = [num, num]
compute4 = { base: num, derived: [num, num + 1, num + 2] }

# Mixed polymorphic structures
mixed = {
    numbers: { value: num, list: [num, num], float: frac },
    strings: { value: str, list: [str, str] },
    empty_lists: {
        raw: empty_list,
        in_list: [empty_list],
        in_record: { data: empty_list },
    },
    computations: {
        from_num: num * 100,
        from_frac: frac * 10.0,
        list_from_num: [num, num, num],
    },
}

main = |_| {
    # Just type-check everything
    container1.value + 10
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),Int(4:7-4:9),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),Float(5:8-5:11),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),StringStart(6:7-6:8),StringPart(6:8-6:13),StringEnd(6:13-6:14),
LowerIdent(7:1-7:5),OpAssign(7:6-7:7),UpperIdent(7:8-7:12),
LowerIdent(10:1-10:11),OpAssign(10:12-10:13),OpenSquare(10:14-10:15),CloseSquare(10:15-10:16),
LowerIdent(11:1-11:13),OpAssign(11:14-11:15),OpenCurly(11:16-11:17),CloseCurly(11:17-11:18),
LowerIdent(14:1-14:9),OpAssign(14:10-14:11),OpenSquare(14:12-14:13),Int(14:13-14:14),Comma(14:14-14:15),Int(14:16-14:17),Comma(14:17-14:18),Int(14:19-14:20),CloseSquare(14:20-14:21),
LowerIdent(15:1-15:9),OpAssign(15:10-15:11),OpenSquare(15:12-15:13),StringStart(15:13-15:14),StringPart(15:14-15:15),StringEnd(15:15-15:16),Comma(15:16-15:17),StringStart(15:18-15:19),StringPart(15:19-15:20),StringEnd(15:20-15:21),Comma(15:21-15:22),StringStart(15:23-15:24),StringPart(15:24-15:25),StringEnd(15:25-15:26),CloseSquare(15:26-15:27),
LowerIdent(16:1-16:10),OpAssign(16:11-16:12),OpenSquare(16:13-16:14),UpperIdent(16:14-16:18),Comma(16:18-16:19),UpperIdent(16:20-16:25),CloseSquare(16:25-16:26),
LowerIdent(19:1-19:13),OpAssign(19:14-19:15),OpenSquare(19:16-19:17),LowerIdent(19:17-19:27),Comma(19:27-19:28),LowerIdent(19:29-19:39),Comma(19:39-19:40),LowerIdent(19:41-19:51),CloseSquare(19:51-19:52),
LowerIdent(20:1-20:13),OpAssign(20:14-20:15),OpenSquare(20:16-20:17),LowerIdent(20:17-20:27),Comma(20:27-20:28),OpenSquare(20:29-20:30),Int(20:30-20:31),Comma(20:31-20:32),Int(20:33-20:34),CloseSquare(20:34-20:35),Comma(20:35-20:36),LowerIdent(20:37-20:47),Comma(20:47-20:48),OpenSquare(20:49-20:50),Int(20:50-20:51),Comma(20:51-20:52),Int(20:53-20:54),CloseSquare(20:54-20:55),CloseSquare(20:55-20:56),
LowerIdent(23:1-23:12),OpAssign(23:13-23:14),OpenCurly(23:15-23:16),LowerIdent(23:17-23:22),OpColon(23:22-23:23),LowerIdent(23:24-23:34),Comma(23:34-23:35),LowerIdent(23:36-23:41),OpColon(23:41-23:42),Int(23:43-23:44),CloseCurly(23:45-23:46),
LowerIdent(24:1-24:17),OpAssign(24:18-24:19),OpenCurly(24:20-24:21),LowerIdent(24:22-24:27),OpColon(24:27-24:28),OpenSquare(24:29-24:30),Int(24:30-24:31),Comma(24:31-24:32),Int(24:33-24:34),Comma(24:34-24:35),Int(24:36-24:37),CloseSquare(24:37-24:38),Comma(24:38-24:39),LowerIdent(24:40-24:45),OpColon(24:45-24:46),Int(24:47-24:48),CloseCurly(24:49-24:50),
LowerIdent(25:1-25:17),OpAssign(25:18-25:19),OpenCurly(25:20-25:21),LowerIdent(25:22-25:27),OpColon(25:27-25:28),OpenSquare(25:29-25:30),StringStart(25:30-25:31),StringPart(25:31-25:32),StringEnd(25:32-25:33),Comma(25:33-25:34),StringStart(25:35-25:36),StringPart(25:36-25:37),StringEnd(25:37-25:38),Comma(25:38-25:39),StringStart(25:40-25:41),StringPart(25:41-25:42),StringEnd(25:42-25:43),CloseSquare(25:43-25:44),Comma(25:44-25:45),LowerIdent(25:46-25:51),OpColon(25:51-25:52),Int(25:53-25:54),CloseCurly(25:55-25:56),
LowerIdent(28:1-28:12),OpAssign(28:13-28:14),OpenCurly(28:15-28:16),
LowerIdent(29:5-29:9),OpColon(29:9-29:10),LowerIdent(29:11-29:21),Comma(29:21-29:22),
LowerIdent(30:5-30:13),OpColon(30:13-30:14),OpenCurly(30:15-30:16),
LowerIdent(31:9-31:16),OpColon(31:16-31:17),LowerIdent(31:18-31:21),Comma(31:21-31:22),
LowerIdent(32:9-32:14),OpColon(32:14-32:15),LowerIdent(32:16-32:20),Comma(32:20-32:21),
LowerIdent(33:9-33:20),OpColon(33:20-33:21),LowerIdent(33:22-33:25),Comma(33:25-33:26),
CloseCurly(34:5-34:6),Comma(34:6-34:7),
CloseCurly(35:1-35:2),
LowerIdent(38:1-38:8),OpAssign(38:9-38:10),OpenCurly(38:11-38:12),
LowerIdent(39:5-39:9),OpColon(39:9-39:10),OpenSquare(39:11-39:12),Int(39:12-39:13),Comma(39:13-39:14),Int(39:15-39:16),Comma(39:16-39:17),Int(39:18-39:19),Comma(39:19-39:20),Int(39:21-39:22),Comma(39:22-39:23),Int(39:24-39:25),CloseSquare(39:25-39:26),Comma(39:26-39:27),
LowerIdent(40:5-40:13),OpColon(40:13-40:14),OpenCurly(40:15-40:16),
LowerIdent(41:9-41:16),OpColon(41:16-41:17),LowerIdent(41:18-41:21),Comma(41:21-41:22),
LowerIdent(42:9-42:14),OpColon(42:14-42:15),LowerIdent(42:16-42:20),Comma(42:20-42:21),
LowerIdent(43:9-43:20),OpColon(43:20-43:21),LowerIdent(43:22-43:25),Comma(43:25-43:26),
CloseCurly(44:5-44:6),Comma(44:6-44:7),
LowerIdent(45:5-45:9),OpColon(45:9-45:10),StringStart(45:11-45:12),StringPart(45:12-45:20),StringEnd(45:20-45:21),Comma(45:21-45:22),
CloseCurly(46:1-46:2),
LowerIdent(48:1-48:8),OpAssign(48:9-48:10),OpenCurly(48:11-48:12),
LowerIdent(49:5-49:9),OpColon(49:9-49:10),OpenSquare(49:11-49:12),StringStart(49:12-49:13),StringPart(49:13-49:18),StringEnd(49:18-49:19),Comma(49:19-49:20),StringStart(49:21-49:22),StringPart(49:22-49:28),StringEnd(49:28-49:29),Comma(49:29-49:30),StringStart(49:31-49:32),StringPart(49:32-49:38),StringEnd(49:38-49:39),CloseSquare(49:39-49:40),Comma(49:40-49:41),
LowerIdent(50:5-50:13),OpColon(50:13-50:14),OpenCurly(50:15-50:16),
LowerIdent(51:9-51:16),OpColon(51:16-51:17),LowerIdent(51:18-51:21),Comma(51:21-51:22),
LowerIdent(52:9-52:14),OpColon(52:14-52:15),LowerIdent(52:16-52:20),Comma(52:20-52:21),
LowerIdent(53:9-53:20),OpColon(53:20-53:21),LowerIdent(53:22-53:25),Comma(53:25-53:26),
CloseCurly(54:5-54:6),Comma(54:6-54:7),
LowerIdent(55:5-55:9),OpColon(55:9-55:10),StringStart(55:11-55:12),StringPart(55:12-55:18),StringEnd(55:18-55:19),Comma(55:19-55:20),
CloseCurly(56:1-56:2),
LowerIdent(59:1-59:15),OpAssign(59:16-59:17),OpBar(59:18-59:19),LowerIdent(59:19-59:22),OpBar(59:22-59:23),OpenCurly(59:24-59:25),LowerIdent(59:26-59:31),OpColon(59:31-59:32),LowerIdent(59:33-59:36),Comma(59:36-59:37),LowerIdent(59:38-59:45),OpColon(59:45-59:46),OpenSquare(59:47-59:48),LowerIdent(59:48-59:51),CloseSquare(59:51-59:52),CloseCurly(59:53-59:54),
LowerIdent(60:1-60:11),OpAssign(60:12-60:13),LowerIdent(60:14-60:28),NoSpaceOpenRound(60:28-60:29),LowerIdent(60:29-60:32),CloseRound(60:32-60:33),
LowerIdent(61:1-61:11),OpAssign(61:12-61:13),LowerIdent(61:14-61:28),NoSpaceOpenRound(61:28-61:29),LowerIdent(61:29-61:32),CloseRound(61:32-61:33),
LowerIdent(62:1-62:11),OpAssign(62:12-62:13),LowerIdent(62:14-62:28),NoSpaceOpenRound(62:28-62:29),LowerIdent(62:29-62:33),CloseRound(62:33-62:34),
LowerIdent(65:1-65:5),OpAssign(65:6-65:7),OpenCurly(65:8-65:9),
LowerIdent(66:5-66:11),OpColon(66:11-66:12),OpenCurly(66:13-66:14),
LowerIdent(67:9-67:15),OpColon(67:15-67:16),OpenCurly(67:17-67:18),
LowerIdent(68:13-68:19),OpColon(68:19-68:20),OpenCurly(68:21-68:22),
LowerIdent(69:17-69:21),OpColon(69:21-69:22),LowerIdent(69:23-69:33),Comma(69:33-69:34),
LowerIdent(70:17-70:22),OpColon(70:22-70:23),LowerIdent(70:24-70:27),Comma(70:27-70:28),
CloseCurly(71:13-71:14),Comma(71:14-71:15),
LowerIdent(72:13-72:18),OpColon(72:18-72:19),OpenSquare(72:20-72:21),LowerIdent(72:21-72:24),Comma(72:24-72:25),LowerIdent(72:26-72:29),OpStar(72:30-72:31),Int(72:32-72:33),Comma(72:33-72:34),LowerIdent(72:35-72:38),OpStar(72:39-72:40),Int(72:41-72:42),CloseSquare(72:42-72:43),Comma(72:43-72:44),
CloseCurly(73:9-73:10),Comma(73:10-73:11),
LowerIdent(74:9-74:19),OpColon(74:19-74:20),LowerIdent(74:21-74:31),Comma(74:31-74:32),
CloseCurly(75:5-75:6),Comma(75:6-75:7),
LowerIdent(76:5-76:12),OpColon(76:12-76:13),OpenSquare(76:14-76:15),
OpenCurly(77:9-77:10),LowerIdent(77:11-77:15),OpColon(77:15-77:16),OpenSquare(77:17-77:18),Int(77:18-77:19),CloseSquare(77:19-77:20),Comma(77:20-77:21),LowerIdent(77:22-77:25),OpColon(77:25-77:26),StringStart(77:27-77:28),StringPart(77:28-77:34),StringEnd(77:34-77:35),CloseCurly(77:36-77:37),Comma(77:37-77:38),
OpenCurly(78:9-78:10),LowerIdent(78:11-78:15),OpColon(78:15-78:16),OpenSquare(78:17-78:18),Int(78:18-78:19),Comma(78:19-78:20),Int(78:21-78:22),CloseSquare(78:22-78:23),Comma(78:23-78:24),LowerIdent(78:25-78:28),OpColon(78:28-78:29),StringStart(78:30-78:31),StringPart(78:31-78:35),StringEnd(78:35-78:36),CloseCurly(78:37-78:38),Comma(78:38-78:39),
OpenCurly(79:9-79:10),LowerIdent(79:11-79:15),OpColon(79:15-79:16),OpenSquare(79:17-79:18),Int(79:18-79:19),Comma(79:19-79:20),Int(79:21-79:22),Comma(79:22-79:23),Int(79:24-79:25),CloseSquare(79:25-79:26),Comma(79:26-79:27),LowerIdent(79:28-79:31),OpColon(79:31-79:32),StringStart(79:33-79:34),StringPart(79:34-79:38),StringEnd(79:38-79:39),CloseCurly(79:40-79:41),Comma(79:41-79:42),
CloseSquare(80:5-80:6),Comma(80:6-80:7),
CloseCurly(81:1-81:2),
LowerIdent(84:1-84:9),OpAssign(84:10-84:11),LowerIdent(84:12-84:15),OpPlus(84:16-84:17),Int(84:18-84:20),
LowerIdent(85:1-85:9),OpAssign(85:10-85:11),LowerIdent(85:12-85:15),OpStar(85:16-85:17),Int(85:18-85:19),
LowerIdent(86:1-86:9),OpAssign(86:10-86:11),OpenSquare(86:12-86:13),LowerIdent(86:13-86:16),Comma(86:16-86:17),LowerIdent(86:18-86:21),CloseSquare(86:21-86:22),
LowerIdent(87:1-87:9),OpAssign(87:10-87:11),OpenCurly(87:12-87:13),LowerIdent(87:14-87:18),OpColon(87:18-87:19),LowerIdent(87:20-87:23),Comma(87:23-87:24),LowerIdent(87:25-87:32),OpColon(87:32-87:33),OpenSquare(87:34-87:35),LowerIdent(87:35-87:38),Comma(87:38-87:39),LowerIdent(87:40-87:43),OpPlus(87:44-87:45),Int(87:46-87:47),Comma(87:47-87:48),LowerIdent(87:49-87:52),OpPlus(87:53-87:54),Int(87:55-87:56),CloseSquare(87:56-87:57),CloseCurly(87:58-87:59),
LowerIdent(90:1-90:6),OpAssign(90:7-90:8),OpenCurly(90:9-90:10),
LowerIdent(91:5-91:12),OpColon(91:12-91:13),OpenCurly(91:14-91:15),LowerIdent(91:16-91:21),OpColon(91:21-91:22),LowerIdent(91:23-91:26),Comma(91:26-91:27),LowerIdent(91:28-91:32),OpColon(91:32-91:33),OpenSquare(91:34-91:35),LowerIdent(91:35-91:38),Comma(91:38-91:39),LowerIdent(91:40-91:43),CloseSquare(91:43-91:44),Comma(91:44-91:45),LowerIdent(91:46-91:51),OpColon(91:51-91:52),LowerIdent(91:53-91:57),CloseCurly(91:58-91:59),Comma(91:59-91:60),
LowerIdent(92:5-92:12),OpColon(92:12-92:13),OpenCurly(92:14-92:15),LowerIdent(92:16-92:21),OpColon(92:21-92:22),LowerIdent(92:23-92:26),Comma(92:26-92:27),LowerIdent(92:28-92:32),OpColon(92:32-92:33),OpenSquare(92:34-92:35),LowerIdent(92:35-92:38),Comma(92:38-92:39),LowerIdent(92:40-92:43),CloseSquare(92:43-92:44),CloseCurly(92:45-92:46),Comma(92:46-92:47),
LowerIdent(93:5-93:16),OpColon(93:16-93:17),OpenCurly(93:18-93:19),
LowerIdent(94:9-94:12),OpColon(94:12-94:13),LowerIdent(94:14-94:24),Comma(94:24-94:25),
LowerIdent(95:9-95:16),OpColon(95:16-95:17),OpenSquare(95:18-95:19),LowerIdent(95:19-95:29),CloseSquare(95:29-95:30),Comma(95:30-95:31),
LowerIdent(96:9-96:18),OpColon(96:18-96:19),OpenCurly(96:20-96:21),LowerIdent(96:22-96:26),OpColon(96:26-96:27),LowerIdent(96:28-96:38),CloseCurly(96:39-96:40),Comma(96:40-96:41),
CloseCurly(97:5-97:6),Comma(97:6-97:7),
LowerIdent(98:5-98:17),OpColon(98:17-98:18),OpenCurly(98:19-98:20),
LowerIdent(99:9-99:17),OpColon(99:17-99:18),LowerIdent(99:19-99:22),OpStar(99:23-99:24),Int(99:25-99:28),Comma(99:28-99:29),
LowerIdent(100:9-100:18),OpColon(100:18-100:19),LowerIdent(100:20-100:24),OpStar(100:25-100:26),Float(100:27-100:31),Comma(100:31-100:32),
LowerIdent(101:9-101:22),OpColon(101:22-101:23),OpenSquare(101:24-101:25),LowerIdent(101:25-101:28),Comma(101:28-101:29),LowerIdent(101:30-101:33),Comma(101:33-101:34),LowerIdent(101:35-101:38),CloseSquare(101:38-101:39),Comma(101:39-101:40),
CloseCurly(102:5-102:6),Comma(102:6-102:7),
CloseCurly(103:1-103:2),
LowerIdent(105:1-105:5),OpAssign(105:6-105:7),OpBar(105:8-105:9),Underscore(105:9-105:10),OpBar(105:10-105:11),OpenCurly(105:12-105:13),
LowerIdent(107:5-107:15),NoSpaceDotLowerIdent(107:15-107:21),OpPlus(107:22-107:23),Int(107:24-107:26),
CloseCurly(108:1-108:2),EndOfFile(108:2-108:2),
~~~
# PARSE
~~~clojure
(file @1.1-108.2
	(app @1.1-1.56
		(provides @1.5-1.11
			(exposed-lower-ident @1.6-1.10
				(text "main")))
		(record-field @1.14-1.54 (name "pf")
			(e-string @1.27-1.54
				(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))
		(packages @1.12-1.56
			(record-field @1.14-1.54 (name "pf")
				(e-string @1.27-1.54
					(e-string-part @1.28-1.53 (raw "../basic-cli/platform.roc"))))))
	(statements
		(s-decl @4.1-4.9
			(p-ident @4.1-4.4 (raw "num"))
			(e-int @4.7-4.9 (raw "42")))
		(s-decl @5.1-5.11
			(p-ident @5.1-5.5 (raw "frac"))
			(e-frac @5.8-5.11 (raw "4.2")))
		(s-decl @6.1-6.14
			(p-ident @6.1-6.4 (raw "str"))
			(e-string @6.7-6.14
				(e-string-part @6.8-6.13 (raw "hello"))))
		(s-decl @7.1-7.12
			(p-ident @7.1-7.5 (raw "bool"))
			(e-tag @7.8-7.12 (raw "True")))
		(s-decl @10.1-10.16
			(p-ident @10.1-10.11 (raw "empty_list"))
			(e-list @10.14-10.16))
		(s-decl @11.1-11.18
			(p-ident @11.1-11.13 (raw "empty_record"))
			(e-record @11.16-11.18))
		(s-decl @14.1-14.21
			(p-ident @14.1-14.9 (raw "int_list"))
			(e-list @14.12-14.21
				(e-int @14.13-14.14 (raw "1"))
				(e-int @14.16-14.17 (raw "2"))
				(e-int @14.19-14.20 (raw "3"))))
		(s-decl @15.1-15.27
			(p-ident @15.1-15.9 (raw "str_list"))
			(e-list @15.12-15.27
				(e-string @15.13-15.16
					(e-string-part @15.14-15.15 (raw "a")))
				(e-string @15.18-15.21
					(e-string-part @15.19-15.20 (raw "b")))
				(e-string @15.23-15.26
					(e-string-part @15.24-15.25 (raw "c")))))
		(s-decl @16.1-16.26
			(p-ident @16.1-16.10 (raw "bool_list"))
			(e-list @16.13-16.26
				(e-tag @16.14-16.18 (raw "True"))
				(e-tag @16.20-16.25 (raw "False"))))
		(s-decl @19.1-19.52
			(p-ident @19.1-19.13 (raw "nested_empty"))
			(e-list @19.16-19.52
				(e-ident @19.17-19.27 (raw "empty_list"))
				(e-ident @19.29-19.39 (raw "empty_list"))
				(e-ident @19.41-19.51 (raw "empty_list"))))
		(s-decl @20.1-20.56
			(p-ident @20.1-20.13 (raw "mixed_nested"))
			(e-list @20.16-20.56
				(e-ident @20.17-20.27 (raw "empty_list"))
				(e-list @20.29-20.35
					(e-int @20.30-20.31 (raw "1"))
					(e-int @20.33-20.34 (raw "2")))
				(e-ident @20.37-20.47 (raw "empty_list"))
				(e-list @20.49-20.55
					(e-int @20.50-20.51 (raw "3"))
					(e-int @20.53-20.54 (raw "4")))))
		(s-decl @23.1-23.46
			(p-ident @23.1-23.12 (raw "poly_record"))
			(e-record @23.15-23.46
				(field (field "items")
					(e-ident @23.24-23.34 (raw "empty_list")))
				(field (field "count")
					(e-int @23.43-23.44 (raw "0")))))
		(s-decl @24.1-24.50
			(p-ident @24.1-24.17 (raw "use_poly_record1"))
			(e-record @24.20-24.50
				(field (field "items")
					(e-list @24.29-24.38
						(e-int @24.30-24.31 (raw "1"))
						(e-int @24.33-24.34 (raw "2"))
						(e-int @24.36-24.37 (raw "3"))))
				(field (field "count")
					(e-int @24.47-24.48 (raw "0")))))
		(s-decl @25.1-25.56
			(p-ident @25.1-25.17 (raw "use_poly_record2"))
			(e-record @25.20-25.56
				(field (field "items")
					(e-list @25.29-25.44
						(e-string @25.30-25.33
							(e-string-part @25.31-25.32 (raw "x")))
						(e-string @25.35-25.38
							(e-string-part @25.36-25.37 (raw "y")))
						(e-string @25.40-25.43
							(e-string-part @25.41-25.42 (raw "z")))))
				(field (field "count")
					(e-int @25.53-25.54 (raw "0")))))
		(s-decl @28.1-35.2
			(p-ident @28.1-28.12 (raw "base_config"))
			(e-record @28.15-35.2
				(field (field "data")
					(e-ident @29.11-29.21 (raw "empty_list")))
				(field (field "metadata")
					(e-record @30.15-34.6
						(field (field "version")
							(e-ident @31.18-31.21 (raw "num")))
						(field (field "ratio")
							(e-ident @32.16-32.20 (raw "frac")))
						(field (field "description")
							(e-ident @33.22-33.25 (raw "str")))))))
		(s-decl @38.1-46.2
			(p-ident @38.1-38.8 (raw "config1"))
			(e-record @38.11-46.2
				(field (field "data")
					(e-list @39.11-39.26
						(e-int @39.12-39.13 (raw "1"))
						(e-int @39.15-39.16 (raw "2"))
						(e-int @39.18-39.19 (raw "3"))
						(e-int @39.21-39.22 (raw "4"))
						(e-int @39.24-39.25 (raw "5"))))
				(field (field "metadata")
					(e-record @40.15-44.6
						(field (field "version")
							(e-ident @41.18-41.21 (raw "num")))
						(field (field "ratio")
							(e-ident @42.16-42.20 (raw "frac")))
						(field (field "description")
							(e-ident @43.22-43.25 (raw "str")))))
				(field (field "name")
					(e-string @45.11-45.21
						(e-string-part @45.12-45.20 (raw "integers"))))))
		(s-decl @48.1-56.2
			(p-ident @48.1-48.8 (raw "config2"))
			(e-record @48.11-56.2
				(field (field "data")
					(e-list @49.11-49.40
						(e-string @49.12-49.19
							(e-string-part @49.13-49.18 (raw "apple")))
						(e-string @49.21-49.29
							(e-string-part @49.22-49.28 (raw "banana")))
						(e-string @49.31-49.39
							(e-string-part @49.32-49.38 (raw "cherry")))))
				(field (field "metadata")
					(e-record @50.15-54.6
						(field (field "version")
							(e-ident @51.18-51.21 (raw "num")))
						(field (field "ratio")
							(e-ident @52.16-52.20 (raw "frac")))
						(field (field "description")
							(e-ident @53.22-53.25 (raw "str")))))
				(field (field "name")
					(e-string @55.11-55.19
						(e-string-part @55.12-55.18 (raw "fruits"))))))
		(s-decl @59.1-59.54
			(p-ident @59.1-59.15 (raw "make_container"))
			(e-lambda @59.18-59.54
				(args
					(p-ident @59.19-59.22 (raw "val")))
				(e-record @59.24-59.54
					(field (field "value")
						(e-ident @59.33-59.36 (raw "val")))
					(field (field "wrapper")
						(e-list @59.47-59.52
							(e-ident @59.48-59.51 (raw "val")))))))
		(s-decl @60.1-60.33
			(p-ident @60.1-60.11 (raw "container1"))
			(e-apply @60.14-60.33
				(e-ident @60.14-60.28 (raw "make_container"))
				(e-ident @60.29-60.32 (raw "num"))))
		(s-decl @61.1-61.33
			(p-ident @61.1-61.11 (raw "container2"))
			(e-apply @61.14-61.33
				(e-ident @61.14-61.28 (raw "make_container"))
				(e-ident @61.29-61.32 (raw "str"))))
		(s-decl @62.1-62.34
			(p-ident @62.1-62.11 (raw "container3"))
			(e-apply @62.14-62.34
				(e-ident @62.14-62.28 (raw "make_container"))
				(e-ident @62.29-62.33 (raw "frac"))))
		(s-decl @65.1-81.2
			(p-ident @65.1-65.5 (raw "deep"))
			(e-record @65.8-81.2
				(field (field "level1")
					(e-record @66.13-75.6
						(field (field "level2")
							(e-record @67.17-73.10
								(field (field "level3")
									(e-record @68.21-71.14
										(field (field "data")
											(e-ident @69.23-69.33 (raw "empty_list")))
										(field (field "value")
											(e-ident @70.24-70.27 (raw "num")))))
								(field (field "items")
									(e-list @72.20-72.43
										(e-ident @72.21-72.24 (raw "num"))
										(e-binop @72.26-72.33 (op "*")
											(e-ident @72.26-72.29 (raw "num"))
											(e-int @72.32-72.33 (raw "2")))
										(e-binop @72.35-72.42 (op "*")
											(e-ident @72.35-72.38 (raw "num"))
											(e-int @72.41-72.42 (raw "3")))))))
						(field (field "collection")
							(e-ident @74.21-74.31 (raw "empty_list")))))
				(field (field "results")
					(e-list @76.14-80.6
						(e-record @77.9-77.37
							(field (field "data")
								(e-list @77.17-77.20
									(e-int @77.18-77.19 (raw "1"))))
							(field (field "tag")
								(e-string @77.27-77.35
									(e-string-part @77.28-77.34 (raw "single")))))
						(e-record @78.9-78.38
							(field (field "data")
								(e-list @78.17-78.23
									(e-int @78.18-78.19 (raw "1"))
									(e-int @78.21-78.22 (raw "2"))))
							(field (field "tag")
								(e-string @78.30-78.36
									(e-string-part @78.31-78.35 (raw "ints")))))
						(e-record @79.9-79.41
							(field (field "data")
								(e-list @79.17-79.26
									(e-int @79.18-79.19 (raw "1"))
									(e-int @79.21-79.22 (raw "2"))
									(e-int @79.24-79.25 (raw "3"))))
							(field (field "tag")
								(e-string @79.33-79.39
									(e-string-part @79.34-79.38 (raw "more")))))))))
		(s-decl @84.1-84.20
			(p-ident @84.1-84.9 (raw "compute1"))
			(e-binop @84.12-84.20 (op "+")
				(e-ident @84.12-84.15 (raw "num"))
				(e-int @84.18-84.20 (raw "10"))))
		(s-decl @85.1-85.19
			(p-ident @85.1-85.9 (raw "compute2"))
			(e-binop @85.12-85.19 (op "*")
				(e-ident @85.12-85.15 (raw "num"))
				(e-int @85.18-85.19 (raw "2"))))
		(s-decl @86.1-86.22
			(p-ident @86.1-86.9 (raw "compute3"))
			(e-list @86.12-86.22
				(e-ident @86.13-86.16 (raw "num"))
				(e-ident @86.18-86.21 (raw "num"))))
		(s-decl @87.1-87.59
			(p-ident @87.1-87.9 (raw "compute4"))
			(e-record @87.12-87.59
				(field (field "base")
					(e-ident @87.20-87.23 (raw "num")))
				(field (field "derived")
					(e-list @87.34-87.57
						(e-ident @87.35-87.38 (raw "num"))
						(e-binop @87.40-87.47 (op "+")
							(e-ident @87.40-87.43 (raw "num"))
							(e-int @87.46-87.47 (raw "1")))
						(e-binop @87.49-87.56 (op "+")
							(e-ident @87.49-87.52 (raw "num"))
							(e-int @87.55-87.56 (raw "2")))))))
		(s-decl @90.1-103.2
			(p-ident @90.1-90.6 (raw "mixed"))
			(e-record @90.9-103.2
				(field (field "numbers")
					(e-record @91.14-91.59
						(field (field "value")
							(e-ident @91.23-91.26 (raw "num")))
						(field (field "list")
							(e-list @91.34-91.44
								(e-ident @91.35-91.38 (raw "num"))
								(e-ident @91.40-91.43 (raw "num"))))
						(field (field "float")
							(e-ident @91.53-91.57 (raw "frac")))))
				(field (field "strings")
					(e-record @92.14-92.46
						(field (field "value")
							(e-ident @92.23-92.26 (raw "str")))
						(field (field "list")
							(e-list @92.34-92.44
								(e-ident @92.35-92.38 (raw "str"))
								(e-ident @92.40-92.43 (raw "str"))))))
				(field (field "empty_lists")
					(e-record @93.18-97.6
						(field (field "raw")
							(e-ident @94.14-94.24 (raw "empty_list")))
						(field (field "in_list")
							(e-list @95.18-95.30
								(e-ident @95.19-95.29 (raw "empty_list"))))
						(field (field "in_record")
							(e-record @96.20-96.40
								(field (field "data")
									(e-ident @96.28-96.38 (raw "empty_list")))))))
				(field (field "computations")
					(e-record @98.19-102.6
						(field (field "from_num")
							(e-binop @99.19-99.28 (op "*")
								(e-ident @99.19-99.22 (raw "num"))
								(e-int @99.25-99.28 (raw "100"))))
						(field (field "from_frac")
							(e-binop @100.20-100.31 (op "*")
								(e-ident @100.20-100.24 (raw "frac"))
								(e-frac @100.27-100.31 (raw "10.0"))))
						(field (field "list_from_num")
							(e-list @101.24-101.39
								(e-ident @101.25-101.28 (raw "num"))
								(e-ident @101.30-101.33 (raw "num"))
								(e-ident @101.35-101.38 (raw "num"))))))))
		(s-decl @105.1-108.2
			(p-ident @105.1-105.5 (raw "main"))
			(e-lambda @105.8-108.2
				(args
					(p-underscore))
				(e-block @105.12-108.2
					(statements
						(e-binop @107.5-107.26 (op "+")
							(e-field-access @107.5-107.21
								(e-ident @107.5-107.15 (raw "container1"))
								(e-ident @107.15-107.21 (raw "value")))
							(e-int @107.24-107.26 (raw "10")))))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic polymorphic values
num = 42
frac = 4.2
str = "hello"
bool = True

# Polymorphic empty collections
empty_list = []
empty_record = {}

# Using empty list in multiple contexts
int_list = [1, 2, 3]
str_list = ["a", "b", "c"]
bool_list = [True, False]

# Nested empty lists
nested_empty = [empty_list, empty_list, empty_list]
mixed_nested = [empty_list, [1, 2], empty_list, [3, 4]]

# Polymorphic record with empty list
poly_record = {items: empty_list, count: 0}
use_poly_record1 = {items: [1, 2, 3], count: 0}
use_poly_record2 = {items: ["x", "y", "z"], count: 0}

# Complex nested structure with multiple polymorphic uses
base_config = {
	data: empty_list,
	metadata: {
		version: num,
		ratio: frac,
		description: str
	}

# Different instantiations of base_config

}

# Different instantiations of base_config
config1 = {
	data: [1, 2, 3, 4, 5],
	metadata: {
		version: num,
		ratio: frac,
		description: str
	},
	name: "integers"


}

config2 = {
	data: ["apple", "banana", "cherry"],
	metadata: {
		version: num,
		ratio: frac,
		description: str
	},
	name: "fruits"

# Polymorphic function-like structures

}

# Polymorphic function-like structures
make_container = |val| {value: val, wrapper: [val]}
container1 = make_container(num)
container2 = make_container(str)
container3 = make_container(frac)

# Deeply nested polymorphism
deep = {
	level1: {
		level2: {
			level3: {
				data: empty_list,
				value: num
			},
			items: [num, num * 2, num * 3]
		},
		collection: empty_list
	},
	results: [
		{data: [1], tag: "single"},
		{data: [1, 2], tag: "ints"},
		{data: [1, 2, 3], tag: "more"},
	]

# Polymorphic values used in computations

}

# Polymorphic values used in computations
compute1 = num + 10
compute2 = num * 2
compute3 = [num, num]
compute4 = {base: num, derived: [num, num + 1, num + 2]}

# Mixed polymorphic structures
mixed = {
	numbers: {value: num, list: [num, num], float: frac},
	strings: {value: str, list: [str, str]},
	empty_lists: {
		raw: empty_list,
		in_list: [empty_list],
		in_record: {data: empty_list}
	},
	computations: {
		from_num: num * 100,
		from_frac: frac * 10.0,
		list_from_num: [num, num, num]
	}


}

main = |_| {
	# Just type-check everything
	container1.value + 10
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.4 (ident "num"))
		(e-int @4.7-4.9 (value "42")))
	(d-let
		(p-assign @5.1-5.5 (ident "frac"))
		(e-dec-small @5.8-5.11 (numerator "42") (denominator-power-of-ten "1") (value "4.2")))
	(d-let
		(p-assign @6.1-6.4 (ident "str"))
		(e-string @6.7-6.14
			(e-literal @6.8-6.13 (string "hello"))))
	(d-let
		(p-assign @7.1-7.5 (ident "bool"))
		(e-tag @7.8-7.12 (name "True")))
	(d-let
		(p-assign @10.1-10.11 (ident "empty_list"))
		(e-empty_list @10.14-10.16))
	(d-let
		(p-assign @11.1-11.13 (ident "empty_record"))
		(e-empty_record @11.16-11.18))
	(d-let
		(p-assign @14.1-14.9 (ident "int_list"))
		(e-list @14.12-14.21
			(elems
				(e-int @14.13-14.14 (value "1"))
				(e-int @14.16-14.17 (value "2"))
				(e-int @14.19-14.20 (value "3")))))
	(d-let
		(p-assign @15.1-15.9 (ident "str_list"))
		(e-list @15.12-15.27
			(elems
				(e-string @15.13-15.16
					(e-literal @15.14-15.15 (string "a")))
				(e-string @15.18-15.21
					(e-literal @15.19-15.20 (string "b")))
				(e-string @15.23-15.26
					(e-literal @15.24-15.25 (string "c"))))))
	(d-let
		(p-assign @16.1-16.10 (ident "bool_list"))
		(e-list @16.13-16.26
			(elems
				(e-tag @16.14-16.18 (name "True"))
				(e-tag @16.20-16.25 (name "False")))))
	(d-let
		(p-assign @19.1-19.13 (ident "nested_empty"))
		(e-list @19.16-19.52
			(elems
				(e-lookup-local @19.17-19.27
					(p-assign @10.1-10.11 (ident "empty_list")))
				(e-lookup-local @19.29-19.39
					(p-assign @10.1-10.11 (ident "empty_list")))
				(e-lookup-local @19.41-19.51
					(p-assign @10.1-10.11 (ident "empty_list"))))))
	(d-let
		(p-assign @20.1-20.13 (ident "mixed_nested"))
		(e-list @20.16-20.56
			(elems
				(e-lookup-local @20.17-20.27
					(p-assign @10.1-10.11 (ident "empty_list")))
				(e-list @20.29-20.35
					(elems
						(e-int @20.30-20.31 (value "1"))
						(e-int @20.33-20.34 (value "2"))))
				(e-lookup-local @20.37-20.47
					(p-assign @10.1-10.11 (ident "empty_list")))
				(e-list @20.49-20.55
					(elems
						(e-int @20.50-20.51 (value "3"))
						(e-int @20.53-20.54 (value "4")))))))
	(d-let
		(p-assign @23.1-23.12 (ident "poly_record"))
		(e-record @23.15-23.46
			(fields
				(field (name "items")
					(e-lookup-local @23.24-23.34
						(p-assign @10.1-10.11 (ident "empty_list"))))
				(field (name "count")
					(e-int @23.43-23.44 (value "0"))))))
	(d-let
		(p-assign @24.1-24.17 (ident "use_poly_record1"))
		(e-record @24.20-24.50
			(fields
				(field (name "items")
					(e-list @24.29-24.38
						(elems
							(e-int @24.30-24.31 (value "1"))
							(e-int @24.33-24.34 (value "2"))
							(e-int @24.36-24.37 (value "3")))))
				(field (name "count")
					(e-int @24.47-24.48 (value "0"))))))
	(d-let
		(p-assign @25.1-25.17 (ident "use_poly_record2"))
		(e-record @25.20-25.56
			(fields
				(field (name "items")
					(e-list @25.29-25.44
						(elems
							(e-string @25.30-25.33
								(e-literal @25.31-25.32 (string "x")))
							(e-string @25.35-25.38
								(e-literal @25.36-25.37 (string "y")))
							(e-string @25.40-25.43
								(e-literal @25.41-25.42 (string "z"))))))
				(field (name "count")
					(e-int @25.53-25.54 (value "0"))))))
	(d-let
		(p-assign @28.1-28.12 (ident "base_config"))
		(e-record @28.15-35.2
			(fields
				(field (name "data")
					(e-lookup-local @29.11-29.21
						(p-assign @10.1-10.11 (ident "empty_list"))))
				(field (name "metadata")
					(e-record @30.15-34.6
						(fields
							(field (name "version")
								(e-lookup-local @31.18-31.21
									(p-assign @4.1-4.4 (ident "num"))))
							(field (name "ratio")
								(e-lookup-local @32.16-32.20
									(p-assign @5.1-5.5 (ident "frac"))))
							(field (name "description")
								(e-lookup-local @33.22-33.25
									(p-assign @6.1-6.4 (ident "str"))))))))))
	(d-let
		(p-assign @38.1-38.8 (ident "config1"))
		(e-record @38.11-46.2
			(fields
				(field (name "data")
					(e-list @39.11-39.26
						(elems
							(e-int @39.12-39.13 (value "1"))
							(e-int @39.15-39.16 (value "2"))
							(e-int @39.18-39.19 (value "3"))
							(e-int @39.21-39.22 (value "4"))
							(e-int @39.24-39.25 (value "5")))))
				(field (name "metadata")
					(e-record @40.15-44.6
						(fields
							(field (name "version")
								(e-lookup-local @41.18-41.21
									(p-assign @4.1-4.4 (ident "num"))))
							(field (name "ratio")
								(e-lookup-local @42.16-42.20
									(p-assign @5.1-5.5 (ident "frac"))))
							(field (name "description")
								(e-lookup-local @43.22-43.25
									(p-assign @6.1-6.4 (ident "str")))))))
				(field (name "name")
					(e-string @45.11-45.21
						(e-literal @45.12-45.20 (string "integers")))))))
	(d-let
		(p-assign @48.1-48.8 (ident "config2"))
		(e-record @48.11-56.2
			(fields
				(field (name "data")
					(e-list @49.11-49.40
						(elems
							(e-string @49.12-49.19
								(e-literal @49.13-49.18 (string "apple")))
							(e-string @49.21-49.29
								(e-literal @49.22-49.28 (string "banana")))
							(e-string @49.31-49.39
								(e-literal @49.32-49.38 (string "cherry"))))))
				(field (name "metadata")
					(e-record @50.15-54.6
						(fields
							(field (name "version")
								(e-lookup-local @51.18-51.21
									(p-assign @4.1-4.4 (ident "num"))))
							(field (name "ratio")
								(e-lookup-local @52.16-52.20
									(p-assign @5.1-5.5 (ident "frac"))))
							(field (name "description")
								(e-lookup-local @53.22-53.25
									(p-assign @6.1-6.4 (ident "str")))))))
				(field (name "name")
					(e-string @55.11-55.19
						(e-literal @55.12-55.18 (string "fruits")))))))
	(d-let
		(p-assign @59.1-59.15 (ident "make_container"))
		(e-lambda @59.18-59.54
			(args
				(p-assign @59.19-59.22 (ident "val")))
			(e-record @59.24-59.54
				(fields
					(field (name "value")
						(e-lookup-local @59.33-59.36
							(p-assign @59.19-59.22 (ident "val"))))
					(field (name "wrapper")
						(e-list @59.47-59.52
							(elems
								(e-lookup-local @59.48-59.51
									(p-assign @59.19-59.22 (ident "val"))))))))))
	(d-let
		(p-assign @60.1-60.11 (ident "container1"))
		(e-call @60.14-60.33
			(e-lookup-local @60.14-60.28
				(p-assign @59.1-59.15 (ident "make_container")))
			(e-lookup-local @60.29-60.32
				(p-assign @4.1-4.4 (ident "num")))))
	(d-let
		(p-assign @61.1-61.11 (ident "container2"))
		(e-call @61.14-61.33
			(e-lookup-local @61.14-61.28
				(p-assign @59.1-59.15 (ident "make_container")))
			(e-lookup-local @61.29-61.32
				(p-assign @6.1-6.4 (ident "str")))))
	(d-let
		(p-assign @62.1-62.11 (ident "container3"))
		(e-call @62.14-62.34
			(e-lookup-local @62.14-62.28
				(p-assign @59.1-59.15 (ident "make_container")))
			(e-lookup-local @62.29-62.33
				(p-assign @5.1-5.5 (ident "frac")))))
	(d-let
		(p-assign @65.1-65.5 (ident "deep"))
		(e-record @65.8-81.2
			(fields
				(field (name "level1")
					(e-record @66.13-75.6
						(fields
							(field (name "level2")
								(e-record @67.17-73.10
									(fields
										(field (name "level3")
											(e-record @68.21-71.14
												(fields
													(field (name "data")
														(e-lookup-local @69.23-69.33
															(p-assign @10.1-10.11 (ident "empty_list"))))
													(field (name "value")
														(e-lookup-local @70.24-70.27
															(p-assign @4.1-4.4 (ident "num")))))))
										(field (name "items")
											(e-list @72.20-72.43
												(elems
													(e-lookup-local @72.21-72.24
														(p-assign @4.1-4.4 (ident "num")))
													(e-binop @72.26-72.33 (op "mul")
														(e-lookup-local @72.26-72.29
															(p-assign @4.1-4.4 (ident "num")))
														(e-int @72.32-72.33 (value "2")))
													(e-binop @72.35-72.42 (op "mul")
														(e-lookup-local @72.35-72.38
															(p-assign @4.1-4.4 (ident "num")))
														(e-int @72.41-72.42 (value "3")))))))))
							(field (name "collection")
								(e-lookup-local @74.21-74.31
									(p-assign @10.1-10.11 (ident "empty_list")))))))
				(field (name "results")
					(e-list @76.14-80.6
						(elems
							(e-record @77.9-77.37
								(fields
									(field (name "data")
										(e-list @77.17-77.20
											(elems
												(e-int @77.18-77.19 (value "1")))))
									(field (name "tag")
										(e-string @77.27-77.35
											(e-literal @77.28-77.34 (string "single"))))))
							(e-record @78.9-78.38
								(fields
									(field (name "data")
										(e-list @78.17-78.23
											(elems
												(e-int @78.18-78.19 (value "1"))
												(e-int @78.21-78.22 (value "2")))))
									(field (name "tag")
										(e-string @78.30-78.36
											(e-literal @78.31-78.35 (string "ints"))))))
							(e-record @79.9-79.41
								(fields
									(field (name "data")
										(e-list @79.17-79.26
											(elems
												(e-int @79.18-79.19 (value "1"))
												(e-int @79.21-79.22 (value "2"))
												(e-int @79.24-79.25 (value "3")))))
									(field (name "tag")
										(e-string @79.33-79.39
											(e-literal @79.34-79.38 (string "more"))))))))))))
	(d-let
		(p-assign @84.1-84.9 (ident "compute1"))
		(e-binop @84.12-84.20 (op "add")
			(e-lookup-local @84.12-84.15
				(p-assign @4.1-4.4 (ident "num")))
			(e-int @84.18-84.20 (value "10"))))
	(d-let
		(p-assign @85.1-85.9 (ident "compute2"))
		(e-binop @85.12-85.19 (op "mul")
			(e-lookup-local @85.12-85.15
				(p-assign @4.1-4.4 (ident "num")))
			(e-int @85.18-85.19 (value "2"))))
	(d-let
		(p-assign @86.1-86.9 (ident "compute3"))
		(e-list @86.12-86.22
			(elems
				(e-lookup-local @86.13-86.16
					(p-assign @4.1-4.4 (ident "num")))
				(e-lookup-local @86.18-86.21
					(p-assign @4.1-4.4 (ident "num"))))))
	(d-let
		(p-assign @87.1-87.9 (ident "compute4"))
		(e-record @87.12-87.59
			(fields
				(field (name "base")
					(e-lookup-local @87.20-87.23
						(p-assign @4.1-4.4 (ident "num"))))
				(field (name "derived")
					(e-list @87.34-87.57
						(elems
							(e-lookup-local @87.35-87.38
								(p-assign @4.1-4.4 (ident "num")))
							(e-binop @87.40-87.47 (op "add")
								(e-lookup-local @87.40-87.43
									(p-assign @4.1-4.4 (ident "num")))
								(e-int @87.46-87.47 (value "1")))
							(e-binop @87.49-87.56 (op "add")
								(e-lookup-local @87.49-87.52
									(p-assign @4.1-4.4 (ident "num")))
								(e-int @87.55-87.56 (value "2")))))))))
	(d-let
		(p-assign @90.1-90.6 (ident "mixed"))
		(e-record @90.9-103.2
			(fields
				(field (name "numbers")
					(e-record @91.14-91.59
						(fields
							(field (name "value")
								(e-lookup-local @91.23-91.26
									(p-assign @4.1-4.4 (ident "num"))))
							(field (name "list")
								(e-list @91.34-91.44
									(elems
										(e-lookup-local @91.35-91.38
											(p-assign @4.1-4.4 (ident "num")))
										(e-lookup-local @91.40-91.43
											(p-assign @4.1-4.4 (ident "num"))))))
							(field (name "float")
								(e-lookup-local @91.53-91.57
									(p-assign @5.1-5.5 (ident "frac")))))))
				(field (name "strings")
					(e-record @92.14-92.46
						(fields
							(field (name "value")
								(e-lookup-local @92.23-92.26
									(p-assign @6.1-6.4 (ident "str"))))
							(field (name "list")
								(e-list @92.34-92.44
									(elems
										(e-lookup-local @92.35-92.38
											(p-assign @6.1-6.4 (ident "str")))
										(e-lookup-local @92.40-92.43
											(p-assign @6.1-6.4 (ident "str")))))))))
				(field (name "empty_lists")
					(e-record @93.18-97.6
						(fields
							(field (name "raw")
								(e-lookup-local @94.14-94.24
									(p-assign @10.1-10.11 (ident "empty_list"))))
							(field (name "in_list")
								(e-list @95.18-95.30
									(elems
										(e-lookup-local @95.19-95.29
											(p-assign @10.1-10.11 (ident "empty_list"))))))
							(field (name "in_record")
								(e-record @96.20-96.40
									(fields
										(field (name "data")
											(e-lookup-local @96.28-96.38
												(p-assign @10.1-10.11 (ident "empty_list"))))))))))
				(field (name "computations")
					(e-record @98.19-102.6
						(fields
							(field (name "from_num")
								(e-binop @99.19-99.28 (op "mul")
									(e-lookup-local @99.19-99.22
										(p-assign @4.1-4.4 (ident "num")))
									(e-int @99.25-99.28 (value "100"))))
							(field (name "from_frac")
								(e-binop @100.20-100.31 (op "mul")
									(e-lookup-local @100.20-100.24
										(p-assign @5.1-5.5 (ident "frac")))
									(e-dec-small @100.27-100.31 (numerator "100") (denominator-power-of-ten "1") (value "10"))))
							(field (name "list_from_num")
								(e-list @101.24-101.39
									(elems
										(e-lookup-local @101.25-101.28
											(p-assign @4.1-4.4 (ident "num")))
										(e-lookup-local @101.30-101.33
											(p-assign @4.1-4.4 (ident "num")))
										(e-lookup-local @101.35-101.38
											(p-assign @4.1-4.4 (ident "num"))))))))))))
	(d-let
		(p-assign @105.1-105.5 (ident "main"))
		(e-lambda @105.8-108.2
			(args
				(p-underscore @105.9-105.10))
			(e-block @105.12-108.2
				(e-binop @107.5-107.26 (op "add")
					(e-dot-access @107.5-107.21 (field "value")
						(receiver
							(e-lookup-local @107.5-107.15
								(p-assign @60.1-60.11 (ident "container1")))))
					(e-int @107.24-107.26 (value "10")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Num(_size)"))
		(patt @5.1-5.5 (type "Frac(_size)"))
		(patt @6.1-6.4 (type "Str"))
		(patt @7.1-7.5 (type "[True]_others"))
		(patt @10.1-10.11 (type "List(Num(_size))"))
		(patt @11.1-11.13 (type "{}"))
		(patt @14.1-14.9 (type "List(Num(_size))"))
		(patt @15.1-15.9 (type "List(Str)"))
		(patt @16.1-16.10 (type "List([True, False]_others)"))
		(patt @19.1-19.13 (type "List(List(Num(_size)))"))
		(patt @20.1-20.13 (type "List(List(Num(_size)))"))
		(patt @23.1-23.12 (type "{ items: List(Num(_size)), count: Num(_size2) }"))
		(patt @24.1-24.17 (type "{ items: List(Num(_size)), count: Num(_size2) }"))
		(patt @25.1-25.17 (type "{ items: List(Str), count: Num(_size) }"))
		(patt @28.1-28.12 (type "{ data: List(Num(_size)), metadata: { version: Num(_size2), ratio: Frac(_size3), description: Str } }"))
		(patt @38.1-38.8 (type "{ data: List(Num(_size)), metadata: { version: Num(_size2), ratio: Frac(_size3), description: Str }, name: Str }"))
		(patt @48.1-48.8 (type "{ data: List(Str), metadata: { version: Num(_size), ratio: Frac(_size2), description: Str }, name: Str }"))
		(patt @59.1-59.15 (type "_arg -> { value: _field, wrapper: List(_elem) }"))
		(patt @60.1-60.11 (type "{ value: _field, wrapper: List(_elem) }"))
		(patt @61.1-61.11 (type "{ value: _field, wrapper: List(_elem) }"))
		(patt @62.1-62.11 (type "{ value: _field, wrapper: List(_elem) }"))
		(patt @65.1-65.5 (type "{ level1: { level2: { level3: { data: List(Num(_size)), value: Num(_size2) }, items: List(Num(_size3)) }, collection: List(Num(_size4)) }, results: List({ data: List(Num(_size5)), tag: Str }) }"))
		(patt @84.1-84.9 (type "_a"))
		(patt @85.1-85.9 (type "_a"))
		(patt @86.1-86.9 (type "List(Num(_size))"))
		(patt @87.1-87.9 (type "{ base: Num(_size), derived: List(Num(_size2)) }"))
		(patt @90.1-90.6 (type "{ numbers: { value: Num(_size), list: List(Num(_size2)), float: Frac(_size3) }, strings: { value: Str, list: List(Str) }, empty_lists: { raw: List(Num(_size4)), in_list: List(List(Num(_size5))), in_record: { data: List(Num(_size6)) } }, computations: { from_num: _field, from_frac: _field2, list_from_num: List(Num(_size7)) } }"))
		(patt @105.1-105.5 (type "_arg -> _ret")))
	(expressions
		(expr @4.7-4.9 (type "Num(_size)"))
		(expr @5.8-5.11 (type "Frac(_size)"))
		(expr @6.7-6.14 (type "Str"))
		(expr @7.8-7.12 (type "[True]_others"))
		(expr @10.14-10.16 (type "List(Num(_size))"))
		(expr @11.16-11.18 (type "{}"))
		(expr @14.12-14.21 (type "List(Num(_size))"))
		(expr @15.12-15.27 (type "List(Str)"))
		(expr @16.13-16.26 (type "List([True, False]_others)"))
		(expr @19.16-19.52 (type "List(List(Num(_size)))"))
		(expr @20.16-20.56 (type "List(List(Num(_size)))"))
		(expr @23.15-23.46 (type "{ items: List(Num(_size)), count: Num(_size2) }"))
		(expr @24.20-24.50 (type "{ items: List(Num(_size)), count: Num(_size2) }"))
		(expr @25.20-25.56 (type "{ items: List(Str), count: Num(_size) }"))
		(expr @28.15-35.2 (type "{ data: List(Num(_size)), metadata: { version: Num(_size2), ratio: Frac(_size3), description: Str } }"))
		(expr @38.11-46.2 (type "{ data: List(Num(_size)), metadata: { version: Num(_size2), ratio: Frac(_size3), description: Str }, name: Str }"))
		(expr @48.11-56.2 (type "{ data: List(Str), metadata: { version: Num(_size), ratio: Frac(_size2), description: Str }, name: Str }"))
		(expr @59.18-59.54 (type "_arg -> { value: _field, wrapper: List(_elem) }"))
		(expr @60.14-60.33 (type "{ value: _field, wrapper: List(_elem) }"))
		(expr @61.14-61.33 (type "{ value: _field, wrapper: List(_elem) }"))
		(expr @62.14-62.34 (type "{ value: _field, wrapper: List(_elem) }"))
		(expr @65.8-81.2 (type "{ level1: { level2: { level3: { data: List(Num(_size)), value: Num(_size2) }, items: List(Num(_size3)) }, collection: List(Num(_size4)) }, results: List({ data: List(Num(_size5)), tag: Str }) }"))
		(expr @84.12-84.20 (type "_a"))
		(expr @85.12-85.19 (type "_a"))
		(expr @86.12-86.22 (type "List(Num(_size))"))
		(expr @87.12-87.59 (type "{ base: Num(_size), derived: List(Num(_size2)) }"))
		(expr @90.9-103.2 (type "{ numbers: { value: Num(_size), list: List(Num(_size2)), float: Frac(_size3) }, strings: { value: Str, list: List(Str) }, empty_lists: { raw: List(Num(_size4)), in_list: List(List(Num(_size5))), in_record: { data: List(Num(_size6)) } }, computations: { from_num: _field, from_frac: _field2, list_from_num: List(Num(_size7)) } }"))
		(expr @105.8-108.2 (type "_arg -> _ret"))))
~~~
