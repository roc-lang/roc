# META
~~~ini
description=Let-polymorphism with records
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic values for polymorphism testing
num = 42
frac = 4.2
str = "hello"
my_empty_list = []
my_nonempty_list = [num, frac]

# Simple record with polymorphic values
simple_record = { x: num, y: frac, z: str }

# Record with empty list that gets used polymorphically
record_with_empty = { data: my_empty_list, count: 0 }

# Using the empty list from record in different contexts
use1 = [1, 2, 3]
use2 = ["a", "b", "c"]

# Nested records with polymorphic values
nested = {
    level1: {
        level2: {
            value: num,
            list: my_empty_list,
        },
        other: frac,
    },
    top: str,
}

# Record with mixed list
mixed_record = { 
    my_empty_list,
    my_nonempty_list,
    derived: [num, num],
}

# Multiple records sharing polymorphic values
record1 = { value: num, doubled: num * 2 }
record2 = { value: num, tripled: num * 3 }
record3 = { original: num, modified: num + 10 }

# Record with function using polymorphic value
record_with_fn = {
    value: num,
    transform: |x| x + num,
}

# Complex nested structure mixing all types
complex = {
    numbers: { int: num, float: frac },
    lists: { empty: my_empty_list, nonempty: my_nonempty_list },
    strings: { value: str, repeated: [str, str, str] },
    nested: {
        data: [
            { id: num, items: my_empty_list },
            { id: num + 1, items: [1, 2] },
        ],
    },
}

main = |_| {
    # Return something to type-check
    record1.value + record2.value
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:40),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),Int(4:7-4:9),Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),Float(5:8-5:11),Newline(1:1-1:1),
LowerIdent(6:1-6:4),OpAssign(6:5-6:6),StringStart(6:7-6:8),StringPart(6:8-6:13),StringEnd(6:13-6:14),Newline(1:1-1:1),
LowerIdent(7:1-7:14),OpAssign(7:15-7:16),OpenSquare(7:17-7:18),CloseSquare(7:18-7:19),Newline(1:1-1:1),
LowerIdent(8:1-8:17),OpAssign(8:18-8:19),OpenSquare(8:20-8:21),LowerIdent(8:21-8:24),Comma(8:24-8:25),LowerIdent(8:26-8:30),CloseSquare(8:30-8:31),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(10:2-10:40),
LowerIdent(11:1-11:14),OpAssign(11:15-11:16),OpenCurly(11:17-11:18),LowerIdent(11:19-11:20),OpColon(11:20-11:21),LowerIdent(11:22-11:25),Comma(11:25-11:26),LowerIdent(11:27-11:28),OpColon(11:28-11:29),LowerIdent(11:30-11:34),Comma(11:34-11:35),LowerIdent(11:36-11:37),OpColon(11:37-11:38),LowerIdent(11:39-11:42),CloseCurly(11:43-11:44),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(13:2-13:56),
LowerIdent(14:1-14:18),OpAssign(14:19-14:20),OpenCurly(14:21-14:22),LowerIdent(14:23-14:27),OpColon(14:27-14:28),LowerIdent(14:29-14:42),Comma(14:42-14:43),LowerIdent(14:44-14:49),OpColon(14:49-14:50),Int(14:51-14:52),CloseCurly(14:53-14:54),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(16:2-16:57),
LowerIdent(17:1-17:5),OpAssign(17:6-17:7),OpenSquare(17:8-17:9),Int(17:9-17:10),Comma(17:10-17:11),Int(17:12-17:13),Comma(17:13-17:14),Int(17:15-17:16),CloseSquare(17:16-17:17),Newline(1:1-1:1),
LowerIdent(18:1-18:5),OpAssign(18:6-18:7),OpenSquare(18:8-18:9),StringStart(18:9-18:10),StringPart(18:10-18:11),StringEnd(18:11-18:12),Comma(18:12-18:13),StringStart(18:14-18:15),StringPart(18:15-18:16),StringEnd(18:16-18:17),Comma(18:17-18:18),StringStart(18:19-18:20),StringPart(18:20-18:21),StringEnd(18:21-18:22),CloseSquare(18:22-18:23),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(20:2-20:41),
LowerIdent(21:1-21:7),OpAssign(21:8-21:9),OpenCurly(21:10-21:11),Newline(1:1-1:1),
LowerIdent(22:5-22:11),OpColon(22:11-22:12),OpenCurly(22:13-22:14),Newline(1:1-1:1),
LowerIdent(23:9-23:15),OpColon(23:15-23:16),OpenCurly(23:17-23:18),Newline(1:1-1:1),
LowerIdent(24:13-24:18),OpColon(24:18-24:19),LowerIdent(24:20-24:23),Comma(24:23-24:24),Newline(1:1-1:1),
LowerIdent(25:13-25:17),OpColon(25:17-25:18),LowerIdent(25:19-25:32),Comma(25:32-25:33),Newline(1:1-1:1),
CloseCurly(26:9-26:10),Comma(26:10-26:11),Newline(1:1-1:1),
LowerIdent(27:9-27:14),OpColon(27:14-27:15),LowerIdent(27:16-27:20),Comma(27:20-27:21),Newline(1:1-1:1),
CloseCurly(28:5-28:6),Comma(28:6-28:7),Newline(1:1-1:1),
LowerIdent(29:5-29:8),OpColon(29:8-29:9),LowerIdent(29:10-29:13),Comma(29:13-29:14),Newline(1:1-1:1),
CloseCurly(30:1-30:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(32:2-32:25),
LowerIdent(33:1-33:13),OpAssign(33:14-33:15),OpenCurly(33:16-33:17),Newline(1:1-1:1),
LowerIdent(34:5-34:18),Comma(34:18-34:19),Newline(1:1-1:1),
LowerIdent(35:5-35:21),Comma(35:21-35:22),Newline(1:1-1:1),
LowerIdent(36:5-36:12),OpColon(36:12-36:13),OpenSquare(36:14-36:15),LowerIdent(36:15-36:18),Comma(36:18-36:19),LowerIdent(36:20-36:23),CloseSquare(36:23-36:24),Comma(36:24-36:25),Newline(1:1-1:1),
CloseCurly(37:1-37:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(39:2-39:46),
LowerIdent(40:1-40:8),OpAssign(40:9-40:10),OpenCurly(40:11-40:12),LowerIdent(40:13-40:18),OpColon(40:18-40:19),LowerIdent(40:20-40:23),Comma(40:23-40:24),LowerIdent(40:25-40:32),OpColon(40:32-40:33),LowerIdent(40:34-40:37),OpStar(40:38-40:39),Int(40:40-40:41),CloseCurly(40:42-40:43),Newline(1:1-1:1),
LowerIdent(41:1-41:8),OpAssign(41:9-41:10),OpenCurly(41:11-41:12),LowerIdent(41:13-41:18),OpColon(41:18-41:19),LowerIdent(41:20-41:23),Comma(41:23-41:24),LowerIdent(41:25-41:32),OpColon(41:32-41:33),LowerIdent(41:34-41:37),OpStar(41:38-41:39),Int(41:40-41:41),CloseCurly(41:42-41:43),Newline(1:1-1:1),
LowerIdent(42:1-42:8),OpAssign(42:9-42:10),OpenCurly(42:11-42:12),LowerIdent(42:13-42:21),OpColon(42:21-42:22),LowerIdent(42:23-42:26),Comma(42:26-42:27),LowerIdent(42:28-42:36),OpColon(42:36-42:37),LowerIdent(42:38-42:41),OpPlus(42:42-42:43),Int(42:44-42:46),CloseCurly(42:47-42:48),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(44:2-44:47),
LowerIdent(45:1-45:15),OpAssign(45:16-45:17),OpenCurly(45:18-45:19),Newline(1:1-1:1),
LowerIdent(46:5-46:10),OpColon(46:10-46:11),LowerIdent(46:12-46:15),Comma(46:15-46:16),Newline(1:1-1:1),
LowerIdent(47:5-47:14),OpColon(47:14-47:15),OpBar(47:16-47:17),LowerIdent(47:17-47:18),OpBar(47:18-47:19),LowerIdent(47:20-47:21),OpPlus(47:22-47:23),LowerIdent(47:24-47:27),Comma(47:27-47:28),Newline(1:1-1:1),
CloseCurly(48:1-48:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(50:2-50:44),
LowerIdent(51:1-51:8),OpAssign(51:9-51:10),OpenCurly(51:11-51:12),Newline(1:1-1:1),
LowerIdent(52:5-52:12),OpColon(52:12-52:13),OpenCurly(52:14-52:15),LowerIdent(52:16-52:19),OpColon(52:19-52:20),LowerIdent(52:21-52:24),Comma(52:24-52:25),LowerIdent(52:26-52:31),OpColon(52:31-52:32),LowerIdent(52:33-52:37),CloseCurly(52:38-52:39),Comma(52:39-52:40),Newline(1:1-1:1),
LowerIdent(53:5-53:10),OpColon(53:10-53:11),OpenCurly(53:12-53:13),LowerIdent(53:14-53:19),OpColon(53:19-53:20),LowerIdent(53:21-53:34),Comma(53:34-53:35),LowerIdent(53:36-53:44),OpColon(53:44-53:45),LowerIdent(53:46-53:62),CloseCurly(53:63-53:64),Comma(53:64-53:65),Newline(1:1-1:1),
LowerIdent(54:5-54:12),OpColon(54:12-54:13),OpenCurly(54:14-54:15),LowerIdent(54:16-54:21),OpColon(54:21-54:22),LowerIdent(54:23-54:26),Comma(54:26-54:27),LowerIdent(54:28-54:36),OpColon(54:36-54:37),OpenSquare(54:38-54:39),LowerIdent(54:39-54:42),Comma(54:42-54:43),LowerIdent(54:44-54:47),Comma(54:47-54:48),LowerIdent(54:49-54:52),CloseSquare(54:52-54:53),CloseCurly(54:54-54:55),Comma(54:55-54:56),Newline(1:1-1:1),
LowerIdent(55:5-55:11),OpColon(55:11-55:12),OpenCurly(55:13-55:14),Newline(1:1-1:1),
LowerIdent(56:9-56:13),OpColon(56:13-56:14),OpenSquare(56:15-56:16),Newline(1:1-1:1),
OpenCurly(57:13-57:14),LowerIdent(57:15-57:17),OpColon(57:17-57:18),LowerIdent(57:19-57:22),Comma(57:22-57:23),LowerIdent(57:24-57:29),OpColon(57:29-57:30),LowerIdent(57:31-57:44),CloseCurly(57:45-57:46),Comma(57:46-57:47),Newline(1:1-1:1),
OpenCurly(58:13-58:14),LowerIdent(58:15-58:17),OpColon(58:17-58:18),LowerIdent(58:19-58:22),OpPlus(58:23-58:24),Int(58:25-58:26),Comma(58:26-58:27),LowerIdent(58:28-58:33),OpColon(58:33-58:34),OpenSquare(58:35-58:36),Int(58:36-58:37),Comma(58:37-58:38),Int(58:39-58:40),CloseSquare(58:40-58:41),CloseCurly(58:42-58:43),Comma(58:43-58:44),Newline(1:1-1:1),
CloseSquare(59:9-59:10),Comma(59:10-59:11),Newline(1:1-1:1),
CloseCurly(60:5-60:6),Comma(60:6-60:7),Newline(1:1-1:1),
CloseCurly(61:1-61:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(63:1-63:5),OpAssign(63:6-63:7),OpBar(63:8-63:9),Underscore(63:9-63:10),OpBar(63:10-63:11),OpenCurly(63:12-63:13),Newline(1:1-1:1),
Newline(64:6-64:37),
LowerIdent(65:5-65:12),NoSpaceDotLowerIdent(65:12-65:18),OpPlus(65:19-65:20),LowerIdent(65:21-65:28),NoSpaceDotLowerIdent(65:28-65:34),Newline(1:1-1:1),
CloseCurly(66:1-66:2),EndOfFile(66:2-66:2),
~~~
# PARSE
~~~clojure
(file @1.1-66.2
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
		(s-decl @7.1-7.19
			(p-ident @7.1-7.14 (raw "my_empty_list"))
			(e-list @7.17-7.19))
		(s-decl @8.1-8.31
			(p-ident @8.1-8.17 (raw "my_nonempty_list"))
			(e-list @8.20-8.31
				(e-ident @8.21-8.24 (qaul "") (raw "num"))
				(e-ident @8.26-8.30 (qaul "") (raw "frac"))))
		(s-decl @11.1-11.44
			(p-ident @11.1-11.14 (raw "simple_record"))
			(e-record @11.17-11.44
				(field (field "x") (optional false)
					(e-ident @11.22-11.25 (qaul "") (raw "num")))
				(field (field "y") (optional false)
					(e-ident @11.30-11.34 (qaul "") (raw "frac")))
				(field (field "z") (optional false)
					(e-ident @11.39-11.42 (qaul "") (raw "str")))))
		(s-decl @14.1-14.54
			(p-ident @14.1-14.18 (raw "record_with_empty"))
			(e-record @14.21-14.54
				(field (field "data") (optional false)
					(e-ident @14.29-14.42 (qaul "") (raw "my_empty_list")))
				(field (field "count") (optional false)
					(e-int @14.51-14.52 (raw "0")))))
		(s-decl @17.1-17.17
			(p-ident @17.1-17.5 (raw "use1"))
			(e-list @17.8-17.17
				(e-int @17.9-17.10 (raw "1"))
				(e-int @17.12-17.13 (raw "2"))
				(e-int @17.15-17.16 (raw "3"))))
		(s-decl @18.1-18.23
			(p-ident @18.1-18.5 (raw "use2"))
			(e-list @18.8-18.23
				(e-string @18.9-18.12
					(e-string-part @18.10-18.11 (raw "a")))
				(e-string @18.14-18.17
					(e-string-part @18.15-18.16 (raw "b")))
				(e-string @18.19-18.22
					(e-string-part @18.20-18.21 (raw "c")))))
		(s-decl @21.1-30.2
			(p-ident @21.1-21.7 (raw "nested"))
			(e-record @21.10-30.2
				(field (field "level1") (optional false)
					(e-record @22.13-28.6
						(field (field "level2") (optional false)
							(e-record @23.17-26.10
								(field (field "value") (optional false)
									(e-ident @24.20-24.23 (qaul "") (raw "num")))
								(field (field "list") (optional false)
									(e-ident @25.19-25.32 (qaul "") (raw "my_empty_list")))))
						(field (field "other") (optional false)
							(e-ident @27.16-27.20 (qaul "") (raw "frac")))))
				(field (field "top") (optional false)
					(e-ident @29.10-29.13 (qaul "") (raw "str")))))
		(s-decl @33.1-37.2
			(p-ident @33.1-33.13 (raw "mixed_record"))
			(e-record @33.16-37.2
				(field (field "my_empty_list") (optional false))
				(field (field "my_nonempty_list") (optional false))
				(field (field "derived") (optional false)
					(e-list @36.14-36.24
						(e-ident @36.15-36.18 (qaul "") (raw "num"))
						(e-ident @36.20-36.23 (qaul "") (raw "num"))))))
		(s-decl @40.1-40.43
			(p-ident @40.1-40.8 (raw "record1"))
			(e-record @40.11-40.43
				(field (field "value") (optional false)
					(e-ident @40.20-40.23 (qaul "") (raw "num")))
				(field (field "doubled") (optional false)
					(e-binop @40.34-40.43 (op "*")
						(e-ident @40.34-40.37 (qaul "") (raw "num"))
						(e-int @40.40-40.41 (raw "2"))))))
		(s-decl @41.1-41.43
			(p-ident @41.1-41.8 (raw "record2"))
			(e-record @41.11-41.43
				(field (field "value") (optional false)
					(e-ident @41.20-41.23 (qaul "") (raw "num")))
				(field (field "tripled") (optional false)
					(e-binop @41.34-41.43 (op "*")
						(e-ident @41.34-41.37 (qaul "") (raw "num"))
						(e-int @41.40-41.41 (raw "3"))))))
		(s-decl @42.1-42.48
			(p-ident @42.1-42.8 (raw "record3"))
			(e-record @42.11-42.48
				(field (field "original") (optional false)
					(e-ident @42.23-42.26 (qaul "") (raw "num")))
				(field (field "modified") (optional false)
					(e-binop @42.38-42.48 (op "+")
						(e-ident @42.38-42.41 (qaul "") (raw "num"))
						(e-int @42.44-42.46 (raw "10"))))))
		(s-decl @45.1-48.2
			(p-ident @45.1-45.15 (raw "record_with_fn"))
			(e-record @45.18-48.2
				(field (field "value") (optional false)
					(e-ident @46.12-46.15 (qaul "") (raw "num")))
				(field (field "transform") (optional false)
					(e-lambda @47.16-47.28
						(args
							(p-ident @47.17-47.18 (raw "x")))
						(e-binop @47.20-47.28 (op "+")
							(e-ident @47.20-47.21 (qaul "") (raw "x"))
							(e-ident @47.24-47.27 (qaul "") (raw "num")))))))
		(s-decl @51.1-61.2
			(p-ident @51.1-51.8 (raw "complex"))
			(e-record @51.11-61.2
				(field (field "numbers") (optional false)
					(e-record @52.14-52.39
						(field (field "int") (optional false)
							(e-ident @52.21-52.24 (qaul "") (raw "num")))
						(field (field "float") (optional false)
							(e-ident @52.33-52.37 (qaul "") (raw "frac")))))
				(field (field "lists") (optional false)
					(e-record @53.12-53.64
						(field (field "empty") (optional false)
							(e-ident @53.21-53.34 (qaul "") (raw "my_empty_list")))
						(field (field "nonempty") (optional false)
							(e-ident @53.46-53.62 (qaul "") (raw "my_nonempty_list")))))
				(field (field "strings") (optional false)
					(e-record @54.14-54.55
						(field (field "value") (optional false)
							(e-ident @54.23-54.26 (qaul "") (raw "str")))
						(field (field "repeated") (optional false)
							(e-list @54.38-54.53
								(e-ident @54.39-54.42 (qaul "") (raw "str"))
								(e-ident @54.44-54.47 (qaul "") (raw "str"))
								(e-ident @54.49-54.52 (qaul "") (raw "str"))))))
				(field (field "nested") (optional false)
					(e-record @55.13-60.6
						(field (field "data") (optional false)
							(e-list @56.15-59.10
								(e-record @57.13-57.46
									(field (field "id") (optional false)
										(e-ident @57.19-57.22 (qaul "") (raw "num")))
									(field (field "items") (optional false)
										(e-ident @57.31-57.44 (qaul "") (raw "my_empty_list"))))
								(e-record @58.13-58.43
									(field (field "id") (optional false)
										(e-binop @58.19-58.27 (op "+")
											(e-ident @58.19-58.22 (qaul "") (raw "num"))
											(e-int @58.25-58.26 (raw "1"))))
									(field (field "items") (optional false)
										(e-list @58.35-58.41
											(e-int @58.36-58.37 (raw "1"))
											(e-int @58.39-58.40 (raw "2")))))))))))
		(s-decl @63.1-66.2
			(p-ident @63.1-63.5 (raw "main"))
			(e-lambda @63.8-66.2
				(args
					(p-underscore))
				(e-block @63.12-66.2
					(statements
						(e-binop @65.5-66.2 (op "+")
							(e-field-access @65.5-65.20
								(e-ident @65.5-65.12 (qaul "") (raw "record1"))
								(e-ident @65.12-65.18 (qaul "") (raw ".value")))
							(e-field-access @65.21-66.2
								(e-ident @65.21-65.28 (qaul "") (raw "record2"))
								(e-ident @65.28-65.34 (qaul "") (raw ".value"))))))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic values for polymorphism testing
num = 42
frac = 4.2
str = "hello"
my_empty_list = []
my_nonempty_list = [num, frac]

# Simple record with polymorphic values
simple_record = { x: num, y: frac, z: str }

# Record with empty list that gets used polymorphically
record_with_empty = { data: my_empty_list, count: 0 }

# Using the empty list from record in different contexts
use1 = [1, 2, 3]
use2 = ["a", "b", "c"]

# Nested records with polymorphic values
nested = {
	level1: {
		level2: {
			value: num,
			list: my_empty_list,
		},
		other: frac,
	},
	top: str,
}

# Record with mixed list
mixed_record = {
	my_empty_list,
	my_nonempty_list,
	derived: [num, num],
}

# Multiple records sharing polymorphic values
record1 = { value: num, doubled: num * 2 }
record2 = { value: num, tripled: num * 3 }
record3 = { original: num, modified: num + 10 }

# Record with function using polymorphic value
record_with_fn = {
	value: num,
	transform: |x| x + num,
}

# Complex nested structure mixing all types
complex = {
	numbers: { int: num, float: frac },
	lists: { empty: my_empty_list, nonempty: my_nonempty_list },
	strings: { value: str, repeated: [str, str, str] },
	nested: {
		data: [
			{ id: num, items: my_empty_list },
			{ id: num + 1, items: [1, 2] },
		],
	},
}

main = |_| {
	# Return something to type-check
	record1.value + record2.value
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
		(p-assign @7.1-7.14 (ident "my_empty_list"))
		(e-empty_list @7.17-7.19))
	(d-let
		(p-assign @8.1-8.17 (ident "my_nonempty_list"))
		(e-list @8.20-8.31
			(elems
				(e-lookup-local @8.21-8.24
					(pattern @4.1-4.4))
				(e-lookup-local @8.26-8.30
					(pattern @5.1-5.5)))))
	(d-let
		(p-assign @11.1-11.14 (ident "simple_record"))
		(e-record @11.17-11.44
			(fields
				(field (name "x")
					(e-lookup-local @11.22-11.25
						(pattern @4.1-4.4)))
				(field (name "y")
					(e-lookup-local @11.30-11.34
						(pattern @5.1-5.5)))
				(field (name "z")
					(e-lookup-local @11.39-11.42
						(pattern @6.1-6.4))))))
	(d-let
		(p-assign @14.1-14.18 (ident "record_with_empty"))
		(e-record @14.21-14.54
			(fields
				(field (name "data")
					(e-lookup-local @14.29-14.42
						(pattern @7.1-7.14)))
				(field (name "count")
					(e-int @14.51-14.52 (value "0"))))))
	(d-let
		(p-assign @17.1-17.5 (ident "use1"))
		(e-list @17.8-17.17
			(elems
				(e-int @17.9-17.10 (value "1"))
				(e-int @17.12-17.13 (value "2"))
				(e-int @17.15-17.16 (value "3")))))
	(d-let
		(p-assign @18.1-18.5 (ident "use2"))
		(e-list @18.8-18.23
			(elems
				(e-string @18.9-18.12
					(e-literal @18.10-18.11 (string "a")))
				(e-string @18.14-18.17
					(e-literal @18.15-18.16 (string "b")))
				(e-string @18.19-18.22
					(e-literal @18.20-18.21 (string "c"))))))
	(d-let
		(p-assign @21.1-21.7 (ident "nested"))
		(e-record @21.10-30.2
			(fields
				(field (name "level1")
					(e-record @22.13-28.6
						(fields
							(field (name "level2")
								(e-record @23.17-26.10
									(fields
										(field (name "value")
											(e-lookup-local @24.20-24.23
												(pattern @4.1-4.4)))
										(field (name "list")
											(e-lookup-local @25.19-25.32
												(pattern @7.1-7.14))))))
							(field (name "other")
								(e-lookup-local @27.16-27.20
									(pattern @5.1-5.5))))))
				(field (name "top")
					(e-lookup-local @29.10-29.13
						(pattern @6.1-6.4))))))
	(d-let
		(p-assign @33.1-33.13 (ident "mixed_record"))
		(e-record @33.16-37.2
			(fields
				(field (name "my_empty_list")
					(e-lookup-local @34.5-34.19
						(pattern @7.1-7.14)))
				(field (name "my_nonempty_list")
					(e-lookup-local @35.5-35.22
						(pattern @8.1-8.17)))
				(field (name "derived")
					(e-list @36.14-36.24
						(elems
							(e-lookup-local @36.15-36.18
								(pattern @4.1-4.4))
							(e-lookup-local @36.20-36.23
								(pattern @4.1-4.4))))))))
	(d-let
		(p-assign @40.1-40.8 (ident "record1"))
		(e-record @40.11-40.43
			(fields
				(field (name "value")
					(e-lookup-local @40.20-40.23
						(pattern @4.1-4.4)))
				(field (name "doubled")
					(e-binop @40.34-40.43 (op "mul")
						(e-lookup-local @40.34-40.37
							(pattern @4.1-4.4))
						(e-int @40.40-40.41 (value "2")))))))
	(d-let
		(p-assign @41.1-41.8 (ident "record2"))
		(e-record @41.11-41.43
			(fields
				(field (name "value")
					(e-lookup-local @41.20-41.23
						(pattern @4.1-4.4)))
				(field (name "tripled")
					(e-binop @41.34-41.43 (op "mul")
						(e-lookup-local @41.34-41.37
							(pattern @4.1-4.4))
						(e-int @41.40-41.41 (value "3")))))))
	(d-let
		(p-assign @42.1-42.8 (ident "record3"))
		(e-record @42.11-42.48
			(fields
				(field (name "original")
					(e-lookup-local @42.23-42.26
						(pattern @4.1-4.4)))
				(field (name "modified")
					(e-binop @42.38-42.48 (op "add")
						(e-lookup-local @42.38-42.41
							(pattern @4.1-4.4))
						(e-int @42.44-42.46 (value "10")))))))
	(d-let
		(p-assign @45.1-45.15 (ident "record_with_fn"))
		(e-record @45.18-48.2
			(fields
				(field (name "value")
					(e-lookup-local @46.12-46.15
						(pattern @4.1-4.4)))
				(field (name "transform")
					(e-lambda @47.16-47.28
						(args
							(p-assign @47.17-47.18 (ident "x")))
						(e-binop @47.20-47.28 (op "add")
							(e-lookup-local @47.20-47.21
								(pattern @47.17-47.18))
							(e-lookup-local @47.24-47.27
								(pattern @4.1-4.4))))))))
	(d-let
		(p-assign @51.1-51.8 (ident "complex"))
		(e-record @51.11-61.2
			(fields
				(field (name "numbers")
					(e-record @52.14-52.39
						(fields
							(field (name "int")
								(e-lookup-local @52.21-52.24
									(pattern @4.1-4.4)))
							(field (name "float")
								(e-lookup-local @52.33-52.37
									(pattern @5.1-5.5))))))
				(field (name "lists")
					(e-record @53.12-53.64
						(fields
							(field (name "empty")
								(e-lookup-local @53.21-53.34
									(pattern @7.1-7.14)))
							(field (name "nonempty")
								(e-lookup-local @53.46-53.62
									(pattern @8.1-8.17))))))
				(field (name "strings")
					(e-record @54.14-54.55
						(fields
							(field (name "value")
								(e-lookup-local @54.23-54.26
									(pattern @6.1-6.4)))
							(field (name "repeated")
								(e-list @54.38-54.53
									(elems
										(e-lookup-local @54.39-54.42
											(pattern @6.1-6.4))
										(e-lookup-local @54.44-54.47
											(pattern @6.1-6.4))
										(e-lookup-local @54.49-54.52
											(pattern @6.1-6.4))))))))
				(field (name "nested")
					(e-record @55.13-60.6
						(fields
							(field (name "data")
								(e-list @56.15-59.10
									(elems
										(e-record @57.13-57.46
											(fields
												(field (name "id")
													(e-lookup-local @57.19-57.22
														(pattern @4.1-4.4)))
												(field (name "items")
													(e-lookup-local @57.31-57.44
														(pattern @7.1-7.14)))))
										(e-record @58.13-58.43
											(fields
												(field (name "id")
													(e-binop @58.19-58.27 (op "add")
														(e-lookup-local @58.19-58.22
															(pattern @4.1-4.4))
														(e-int @58.25-58.26 (value "1"))))
												(field (name "items")
													(e-list @58.35-58.41
														(elems
															(e-int @58.36-58.37 (value "1"))
															(e-int @58.39-58.40 (value "2"))))))))))))))))
	(d-let
		(p-assign @63.1-63.5 (ident "main"))
		(e-lambda @63.8-66.2
			(args
				(p-underscore @63.9-63.10))
			(e-block @63.12-66.2
				(e-binop @65.5-66.2 (op "add")
					(e-dot-access @65.5-65.20 (field "value")
						(receiver
							(e-lookup-local @65.5-65.12
								(pattern @40.1-40.8))))
					(e-dot-access @65.21-66.2 (field "value")
						(receiver
							(e-lookup-local @65.21-65.28
								(pattern @41.1-41.8)))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Frac(*)"))
		(patt @5.1-5.5 (type "Frac(*)"))
		(patt @6.1-6.4 (type "Str"))
		(patt @7.1-7.14 (type "List(Num(*))"))
		(patt @8.1-8.17 (type "List(Frac(*))"))
		(patt @11.1-11.14 (type "{ x: Frac(*), y: Frac(*), z: Str }"))
		(patt @14.1-14.18 (type "{ data: List(Num(*)), count: Num(*) }"))
		(patt @17.1-17.5 (type "List(Num(*))"))
		(patt @18.1-18.5 (type "List(Str)"))
		(patt @21.1-21.7 (type "{ level1: { level2: { value: Frac(*), list: List(Num(*)) }, other: Frac(*) }, top: Str }"))
		(patt @33.1-33.13 (type "{ my_empty_list: List(Num(*)), my_nonempty_list: List(Frac(*)), derived: List(Frac(*)) }"))
		(patt @40.1-40.8 (type "{ value: Frac(*), doubled: * }"))
		(patt @41.1-41.8 (type "{ value: Frac(*), tripled: * }"))
		(patt @42.1-42.8 (type "{ original: Frac(*), modified: * }"))
		(patt @45.1-45.15 (type "{ value: Frac(*), transform: * -> * }"))
		(patt @51.1-51.8 (type "{ numbers: { int: Frac(*), float: Frac(*) }, lists: { empty: List(Num(*)), nonempty: List(Frac(*)) }, strings: { value: Str, repeated: List(Str) }, nested: { data: List({ id: Frac(*), items: List(Num(*)) }) } }"))
		(patt @63.1-63.5 (type "* -> *")))
	(expressions
		(expr @4.7-4.9 (type "Frac(*)"))
		(expr @5.8-5.11 (type "Frac(*)"))
		(expr @6.7-6.14 (type "Str"))
		(expr @7.17-7.19 (type "List(Num(*))"))
		(expr @8.20-8.31 (type "List(Frac(*))"))
		(expr @11.17-11.44 (type "{ x: Frac(*), y: Frac(*), z: Str }"))
		(expr @14.21-14.54 (type "{ data: List(Num(*)), count: Num(*) }"))
		(expr @17.8-17.17 (type "List(Num(*))"))
		(expr @18.8-18.23 (type "List(Str)"))
		(expr @21.10-30.2 (type "{ level1: { level2: { value: Frac(*), list: List(Num(*)) }, other: Frac(*) }, top: Str }"))
		(expr @33.16-37.2 (type "{ my_empty_list: List(Num(*)), my_nonempty_list: List(Frac(*)), derived: List(Frac(*)) }"))
		(expr @40.11-40.43 (type "{ value: Frac(*), doubled: * }"))
		(expr @41.11-41.43 (type "{ value: Frac(*), tripled: * }"))
		(expr @42.11-42.48 (type "{ original: Frac(*), modified: * }"))
		(expr @45.18-48.2 (type "{ value: Frac(*), transform: * -> * }"))
		(expr @51.11-61.2 (type "{ numbers: { int: Frac(*), float: Frac(*) }, lists: { empty: List(Num(*)), nonempty: List(Frac(*)) }, strings: { value: Str, repeated: List(Str) }, nested: { data: List({ id: Frac(*), items: List(Num(*)) }) } }"))
		(expr @63.8-66.2 (type "* -> *"))))
~~~
