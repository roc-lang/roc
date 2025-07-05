# META
~~~ini
description=Let-polymorphism with lists
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic empty list polymorphism
my_empty_list = []

# Empty list used in different contexts
int_list = [1, 2, 3]
str_list = ["hello", "world"]
float_list = [1.1, 2.2, 3.3]

# Non-empty lists with let-bound values
num = 42
frac = 4.2
str = "hello"

# Lists using the same let-bound values
list1 = [num, num, num]
list2 = [frac, frac, frac]
list3 = [str, str, str]

# Nested lists with polymorphic elements
nested_empty = [my_empty_list, my_empty_list]
nested_with_ints = [[], [1, 2, 3]]
nested_with_strs = [[], ["a", "b", "c"]]

# Lists in records
record_with_lists = {
    empty: my_empty_list,
    integers: [num, num],
    strings: [str, str],
    floats: [frac, frac],
}

# Complex nested structure
complex = {
    data: [
        { id: num, values: my_empty_list },
        { id: num + 1, values: [1, 2, 3] },
    ],
    meta: { count: 0, items: my_empty_list },
}

main = |_| {
    # Return something to ensure everything type-checks
    42
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:32),
LowerIdent(4:1-4:14),OpAssign(4:15-4:16),OpenSquare(4:17-4:18),CloseSquare(4:18-4:19),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(6:2-6:40),
LowerIdent(7:1-7:9),OpAssign(7:10-7:11),OpenSquare(7:12-7:13),Int(7:13-7:14),Comma(7:14-7:15),Int(7:16-7:17),Comma(7:17-7:18),Int(7:19-7:20),CloseSquare(7:20-7:21),Newline(1:1-1:1),
LowerIdent(8:1-8:9),OpAssign(8:10-8:11),OpenSquare(8:12-8:13),StringStart(8:13-8:14),StringPart(8:14-8:19),StringEnd(8:19-8:20),Comma(8:20-8:21),StringStart(8:22-8:23),StringPart(8:23-8:28),StringEnd(8:28-8:29),CloseSquare(8:29-8:30),Newline(1:1-1:1),
LowerIdent(9:1-9:11),OpAssign(9:12-9:13),OpenSquare(9:14-9:15),Float(9:15-9:18),Comma(9:18-9:19),Float(9:20-9:23),Comma(9:23-9:24),Float(9:25-9:28),CloseSquare(9:28-9:29),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:40),
LowerIdent(12:1-12:4),OpAssign(12:5-12:6),Int(12:7-12:9),Newline(1:1-1:1),
LowerIdent(13:1-13:5),OpAssign(13:6-13:7),Float(13:8-13:11),Newline(1:1-1:1),
LowerIdent(14:1-14:4),OpAssign(14:5-14:6),StringStart(14:7-14:8),StringPart(14:8-14:13),StringEnd(14:13-14:14),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(16:2-16:40),
LowerIdent(17:1-17:6),OpAssign(17:7-17:8),OpenSquare(17:9-17:10),LowerIdent(17:10-17:13),Comma(17:13-17:14),LowerIdent(17:15-17:18),Comma(17:18-17:19),LowerIdent(17:20-17:23),CloseSquare(17:23-17:24),Newline(1:1-1:1),
LowerIdent(18:1-18:6),OpAssign(18:7-18:8),OpenSquare(18:9-18:10),LowerIdent(18:10-18:14),Comma(18:14-18:15),LowerIdent(18:16-18:20),Comma(18:20-18:21),LowerIdent(18:22-18:26),CloseSquare(18:26-18:27),Newline(1:1-1:1),
LowerIdent(19:1-19:6),OpAssign(19:7-19:8),OpenSquare(19:9-19:10),LowerIdent(19:10-19:13),Comma(19:13-19:14),LowerIdent(19:15-19:18),Comma(19:18-19:19),LowerIdent(19:20-19:23),CloseSquare(19:23-19:24),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(21:2-21:41),
LowerIdent(22:1-22:13),OpAssign(22:14-22:15),OpenSquare(22:16-22:17),LowerIdent(22:17-22:30),Comma(22:30-22:31),LowerIdent(22:32-22:45),CloseSquare(22:45-22:46),Newline(1:1-1:1),
LowerIdent(23:1-23:17),OpAssign(23:18-23:19),OpenSquare(23:20-23:21),OpenSquare(23:21-23:22),CloseSquare(23:22-23:23),Comma(23:23-23:24),OpenSquare(23:25-23:26),Int(23:26-23:27),Comma(23:27-23:28),Int(23:29-23:30),Comma(23:30-23:31),Int(23:32-23:33),CloseSquare(23:33-23:34),CloseSquare(23:34-23:35),Newline(1:1-1:1),
LowerIdent(24:1-24:17),OpAssign(24:18-24:19),OpenSquare(24:20-24:21),OpenSquare(24:21-24:22),CloseSquare(24:22-24:23),Comma(24:23-24:24),OpenSquare(24:25-24:26),StringStart(24:26-24:27),StringPart(24:27-24:28),StringEnd(24:28-24:29),Comma(24:29-24:30),StringStart(24:31-24:32),StringPart(24:32-24:33),StringEnd(24:33-24:34),Comma(24:34-24:35),StringStart(24:36-24:37),StringPart(24:37-24:38),StringEnd(24:38-24:39),CloseSquare(24:39-24:40),CloseSquare(24:40-24:41),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(26:2-26:19),
LowerIdent(27:1-27:18),OpAssign(27:19-27:20),OpenCurly(27:21-27:22),Newline(1:1-1:1),
LowerIdent(28:5-28:10),OpColon(28:10-28:11),LowerIdent(28:12-28:25),Comma(28:25-28:26),Newline(1:1-1:1),
LowerIdent(29:5-29:13),OpColon(29:13-29:14),OpenSquare(29:15-29:16),LowerIdent(29:16-29:19),Comma(29:19-29:20),LowerIdent(29:21-29:24),CloseSquare(29:24-29:25),Comma(29:25-29:26),Newline(1:1-1:1),
LowerIdent(30:5-30:12),OpColon(30:12-30:13),OpenSquare(30:14-30:15),LowerIdent(30:15-30:18),Comma(30:18-30:19),LowerIdent(30:20-30:23),CloseSquare(30:23-30:24),Comma(30:24-30:25),Newline(1:1-1:1),
LowerIdent(31:5-31:11),OpColon(31:11-31:12),OpenSquare(31:13-31:14),LowerIdent(31:14-31:18),Comma(31:18-31:19),LowerIdent(31:20-31:24),CloseSquare(31:24-31:25),Comma(31:25-31:26),Newline(1:1-1:1),
CloseCurly(32:1-32:2),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(34:2-34:27),
LowerIdent(35:1-35:8),OpAssign(35:9-35:10),OpenCurly(35:11-35:12),Newline(1:1-1:1),
LowerIdent(36:5-36:9),OpColon(36:9-36:10),OpenSquare(36:11-36:12),Newline(1:1-1:1),
OpenCurly(37:9-37:10),LowerIdent(37:11-37:13),OpColon(37:13-37:14),LowerIdent(37:15-37:18),Comma(37:18-37:19),LowerIdent(37:20-37:26),OpColon(37:26-37:27),LowerIdent(37:28-37:41),CloseCurly(37:42-37:43),Comma(37:43-37:44),Newline(1:1-1:1),
OpenCurly(38:9-38:10),LowerIdent(38:11-38:13),OpColon(38:13-38:14),LowerIdent(38:15-38:18),OpPlus(38:19-38:20),Int(38:21-38:22),Comma(38:22-38:23),LowerIdent(38:24-38:30),OpColon(38:30-38:31),OpenSquare(38:32-38:33),Int(38:33-38:34),Comma(38:34-38:35),Int(38:36-38:37),Comma(38:37-38:38),Int(38:39-38:40),CloseSquare(38:40-38:41),CloseCurly(38:42-38:43),Comma(38:43-38:44),Newline(1:1-1:1),
CloseSquare(39:5-39:6),Comma(39:6-39:7),Newline(1:1-1:1),
LowerIdent(40:5-40:9),OpColon(40:9-40:10),OpenCurly(40:11-40:12),LowerIdent(40:13-40:18),OpColon(40:18-40:19),Int(40:20-40:21),Comma(40:21-40:22),LowerIdent(40:23-40:28),OpColon(40:28-40:29),LowerIdent(40:30-40:43),CloseCurly(40:44-40:45),Comma(40:45-40:46),Newline(1:1-1:1),
CloseCurly(41:1-41:2),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(43:1-43:5),OpAssign(43:6-43:7),OpBar(43:8-43:9),Underscore(43:9-43:10),OpBar(43:10-43:11),OpenCurly(43:12-43:13),Newline(1:1-1:1),
Newline(44:6-44:56),
Int(45:5-45:7),Newline(1:1-1:1),
CloseCurly(46:1-46:2),EndOfFile(46:2-46:2),
~~~
# PARSE
~~~clojure
(file @1.1-46.2
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
		(s-decl @4.1-4.19
			(p-ident @4.1-4.14 (raw "my_empty_list"))
			(e-list @4.17-4.19))
		(s-decl @7.1-7.21
			(p-ident @7.1-7.9 (raw "int_list"))
			(e-list @7.12-7.21
				(e-int @7.13-7.14 (raw "1"))
				(e-int @7.16-7.17 (raw "2"))
				(e-int @7.19-7.20 (raw "3"))))
		(s-decl @8.1-8.30
			(p-ident @8.1-8.9 (raw "str_list"))
			(e-list @8.12-8.30
				(e-string @8.13-8.20
					(e-string-part @8.14-8.19 (raw "hello")))
				(e-string @8.22-8.29
					(e-string-part @8.23-8.28 (raw "world")))))
		(s-decl @9.1-9.29
			(p-ident @9.1-9.11 (raw "float_list"))
			(e-list @9.14-9.29
				(e-frac @9.15-9.18 (raw "1.1"))
				(e-frac @9.20-9.23 (raw "2.2"))
				(e-frac @9.25-9.28 (raw "3.3"))))
		(s-decl @12.1-12.9
			(p-ident @12.1-12.4 (raw "num"))
			(e-int @12.7-12.9 (raw "42")))
		(s-decl @13.1-13.11
			(p-ident @13.1-13.5 (raw "frac"))
			(e-frac @13.8-13.11 (raw "4.2")))
		(s-decl @14.1-14.14
			(p-ident @14.1-14.4 (raw "str"))
			(e-string @14.7-14.14
				(e-string-part @14.8-14.13 (raw "hello"))))
		(s-decl @17.1-17.24
			(p-ident @17.1-17.6 (raw "list1"))
			(e-list @17.9-17.24
				(e-ident @17.10-17.13 (qaul "") (raw "num"))
				(e-ident @17.15-17.18 (qaul "") (raw "num"))
				(e-ident @17.20-17.23 (qaul "") (raw "num"))))
		(s-decl @18.1-18.27
			(p-ident @18.1-18.6 (raw "list2"))
			(e-list @18.9-18.27
				(e-ident @18.10-18.14 (qaul "") (raw "frac"))
				(e-ident @18.16-18.20 (qaul "") (raw "frac"))
				(e-ident @18.22-18.26 (qaul "") (raw "frac"))))
		(s-decl @19.1-19.24
			(p-ident @19.1-19.6 (raw "list3"))
			(e-list @19.9-19.24
				(e-ident @19.10-19.13 (qaul "") (raw "str"))
				(e-ident @19.15-19.18 (qaul "") (raw "str"))
				(e-ident @19.20-19.23 (qaul "") (raw "str"))))
		(s-decl @22.1-22.46
			(p-ident @22.1-22.13 (raw "nested_empty"))
			(e-list @22.16-22.46
				(e-ident @22.17-22.30 (qaul "") (raw "my_empty_list"))
				(e-ident @22.32-22.45 (qaul "") (raw "my_empty_list"))))
		(s-decl @23.1-23.35
			(p-ident @23.1-23.17 (raw "nested_with_ints"))
			(e-list @23.20-23.35
				(e-list @23.21-23.23)
				(e-list @23.25-23.34
					(e-int @23.26-23.27 (raw "1"))
					(e-int @23.29-23.30 (raw "2"))
					(e-int @23.32-23.33 (raw "3")))))
		(s-decl @24.1-24.41
			(p-ident @24.1-24.17 (raw "nested_with_strs"))
			(e-list @24.20-24.41
				(e-list @24.21-24.23)
				(e-list @24.25-24.40
					(e-string @24.26-24.29
						(e-string-part @24.27-24.28 (raw "a")))
					(e-string @24.31-24.34
						(e-string-part @24.32-24.33 (raw "b")))
					(e-string @24.36-24.39
						(e-string-part @24.37-24.38 (raw "c"))))))
		(s-decl @27.1-32.2
			(p-ident @27.1-27.18 (raw "record_with_lists"))
			(e-record @27.21-32.2
				(field (field "empty") (optional false)
					(e-ident @28.12-28.25 (qaul "") (raw "my_empty_list")))
				(field (field "integers") (optional false)
					(e-list @29.15-29.25
						(e-ident @29.16-29.19 (qaul "") (raw "num"))
						(e-ident @29.21-29.24 (qaul "") (raw "num"))))
				(field (field "strings") (optional false)
					(e-list @30.14-30.24
						(e-ident @30.15-30.18 (qaul "") (raw "str"))
						(e-ident @30.20-30.23 (qaul "") (raw "str"))))
				(field (field "floats") (optional false)
					(e-list @31.13-31.25
						(e-ident @31.14-31.18 (qaul "") (raw "frac"))
						(e-ident @31.20-31.24 (qaul "") (raw "frac"))))))
		(s-decl @35.1-41.2
			(p-ident @35.1-35.8 (raw "complex"))
			(e-record @35.11-41.2
				(field (field "data") (optional false)
					(e-list @36.11-39.6
						(e-record @37.9-37.43
							(field (field "id") (optional false)
								(e-ident @37.15-37.18 (qaul "") (raw "num")))
							(field (field "values") (optional false)
								(e-ident @37.28-37.41 (qaul "") (raw "my_empty_list"))))
						(e-record @38.9-38.43
							(field (field "id") (optional false)
								(e-binop @38.15-38.23 (op "+")
									(e-ident @38.15-38.18 (qaul "") (raw "num"))
									(e-int @38.21-38.22 (raw "1"))))
							(field (field "values") (optional false)
								(e-list @38.32-38.41
									(e-int @38.33-38.34 (raw "1"))
									(e-int @38.36-38.37 (raw "2"))
									(e-int @38.39-38.40 (raw "3")))))))
				(field (field "meta") (optional false)
					(e-record @40.11-40.45
						(field (field "count") (optional false)
							(e-int @40.20-40.21 (raw "0")))
						(field (field "items") (optional false)
							(e-ident @40.30-40.43 (qaul "") (raw "my_empty_list")))))))
		(s-decl @43.1-46.2
			(p-ident @43.1-43.5 (raw "main"))
			(e-lambda @43.8-46.2
				(args
					(p-underscore))
				(e-block @43.12-46.2
					(statements
						(e-int @45.5-45.7 (raw "42"))))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic empty list polymorphism
my_empty_list = []

# Empty list used in different contexts
int_list = [1, 2, 3]
str_list = ["hello", "world"]
float_list = [1.1, 2.2, 3.3]

# Non-empty lists with let-bound values
num = 42
frac = 4.2
str = "hello"

# Lists using the same let-bound values
list1 = [num, num, num]
list2 = [frac, frac, frac]
list3 = [str, str, str]

# Nested lists with polymorphic elements
nested_empty = [my_empty_list, my_empty_list]
nested_with_ints = [[], [1, 2, 3]]
nested_with_strs = [[], ["a", "b", "c"]]

# Lists in records
record_with_lists = {
	empty: my_empty_list,
	integers: [num, num],
	strings: [str, str],
	floats: [frac, frac],
}

# Complex nested structure
complex = {
	data: [
		{ id: num, values: my_empty_list },
		{ id: num + 1, values: [1, 2, 3] },
	],
	meta: { count: 0, items: my_empty_list },
}

main = |_| {
	# Return something to ensure everything type-checks
	42
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign @4.1-4.14 (ident "my_empty_list"))
		(e-empty_list @4.17-4.19))
	(d-let
		(p-assign @7.1-7.9 (ident "int_list"))
		(e-list @7.12-7.21
			(elems
				(e-int @7.13-7.14 (value "1"))
				(e-int @7.16-7.17 (value "2"))
				(e-int @7.19-7.20 (value "3")))))
	(d-let
		(p-assign @8.1-8.9 (ident "str_list"))
		(e-list @8.12-8.30
			(elems
				(e-string @8.13-8.20
					(e-literal @8.14-8.19 (string "hello")))
				(e-string @8.22-8.29
					(e-literal @8.23-8.28 (string "world"))))))
	(d-let
		(p-assign @9.1-9.11 (ident "float_list"))
		(e-list @9.14-9.29
			(elems
				(e-dec-small @9.15-9.18 (numerator "11") (denominator-power-of-ten "1") (value "1.1"))
				(e-dec-small @9.20-9.23 (numerator "22") (denominator-power-of-ten "1") (value "2.2"))
				(e-dec-small @9.25-9.28 (numerator "33") (denominator-power-of-ten "1") (value "3.3")))))
	(d-let
		(p-assign @12.1-12.4 (ident "num"))
		(e-int @12.7-12.9 (value "42")))
	(d-let
		(p-assign @13.1-13.5 (ident "frac"))
		(e-dec-small @13.8-13.11 (numerator "42") (denominator-power-of-ten "1") (value "4.2")))
	(d-let
		(p-assign @14.1-14.4 (ident "str"))
		(e-string @14.7-14.14
			(e-literal @14.8-14.13 (string "hello"))))
	(d-let
		(p-assign @17.1-17.6 (ident "list1"))
		(e-list @17.9-17.24
			(elems
				(e-lookup-local @17.10-17.13
					(pattern @12.1-12.4))
				(e-lookup-local @17.15-17.18
					(pattern @12.1-12.4))
				(e-lookup-local @17.20-17.23
					(pattern @12.1-12.4)))))
	(d-let
		(p-assign @18.1-18.6 (ident "list2"))
		(e-list @18.9-18.27
			(elems
				(e-lookup-local @18.10-18.14
					(pattern @13.1-13.5))
				(e-lookup-local @18.16-18.20
					(pattern @13.1-13.5))
				(e-lookup-local @18.22-18.26
					(pattern @13.1-13.5)))))
	(d-let
		(p-assign @19.1-19.6 (ident "list3"))
		(e-list @19.9-19.24
			(elems
				(e-lookup-local @19.10-19.13
					(pattern @14.1-14.4))
				(e-lookup-local @19.15-19.18
					(pattern @14.1-14.4))
				(e-lookup-local @19.20-19.23
					(pattern @14.1-14.4)))))
	(d-let
		(p-assign @22.1-22.13 (ident "nested_empty"))
		(e-list @22.16-22.46
			(elems
				(e-lookup-local @22.17-22.30
					(pattern @4.1-4.14))
				(e-lookup-local @22.32-22.45
					(pattern @4.1-4.14)))))
	(d-let
		(p-assign @23.1-23.17 (ident "nested_with_ints"))
		(e-list @23.20-23.35
			(elems
				(e-empty_list @23.21-23.23)
				(e-list @23.25-23.34
					(elems
						(e-int @23.26-23.27 (value "1"))
						(e-int @23.29-23.30 (value "2"))
						(e-int @23.32-23.33 (value "3")))))))
	(d-let
		(p-assign @24.1-24.17 (ident "nested_with_strs"))
		(e-list @24.20-24.41
			(elems
				(e-empty_list @24.21-24.23)
				(e-list @24.25-24.40
					(elems
						(e-string @24.26-24.29
							(e-literal @24.27-24.28 (string "a")))
						(e-string @24.31-24.34
							(e-literal @24.32-24.33 (string "b")))
						(e-string @24.36-24.39
							(e-literal @24.37-24.38 (string "c"))))))))
	(d-let
		(p-assign @27.1-27.18 (ident "record_with_lists"))
		(e-record @27.21-32.2
			(fields
				(field (name "empty")
					(e-lookup-local @28.12-28.25
						(pattern @4.1-4.14)))
				(field (name "integers")
					(e-list @29.15-29.25
						(elems
							(e-lookup-local @29.16-29.19
								(pattern @12.1-12.4))
							(e-lookup-local @29.21-29.24
								(pattern @12.1-12.4)))))
				(field (name "strings")
					(e-list @30.14-30.24
						(elems
							(e-lookup-local @30.15-30.18
								(pattern @14.1-14.4))
							(e-lookup-local @30.20-30.23
								(pattern @14.1-14.4)))))
				(field (name "floats")
					(e-list @31.13-31.25
						(elems
							(e-lookup-local @31.14-31.18
								(pattern @13.1-13.5))
							(e-lookup-local @31.20-31.24
								(pattern @13.1-13.5))))))))
	(d-let
		(p-assign @35.1-35.8 (ident "complex"))
		(e-record @35.11-41.2
			(fields
				(field (name "data")
					(e-list @36.11-39.6
						(elems
							(e-record @37.9-37.43
								(fields
									(field (name "id")
										(e-lookup-local @37.15-37.18
											(pattern @12.1-12.4)))
									(field (name "values")
										(e-lookup-local @37.28-37.41
											(pattern @4.1-4.14)))))
							(e-record @38.9-38.43
								(fields
									(field (name "id")
										(e-binop @38.15-38.23 (op "add")
											(e-lookup-local @38.15-38.18
												(pattern @12.1-12.4))
											(e-int @38.21-38.22 (value "1"))))
									(field (name "values")
										(e-list @38.32-38.41
											(elems
												(e-int @38.33-38.34 (value "1"))
												(e-int @38.36-38.37 (value "2"))
												(e-int @38.39-38.40 (value "3"))))))))))
				(field (name "meta")
					(e-record @40.11-40.45
						(fields
							(field (name "count")
								(e-int @40.20-40.21 (value "0")))
							(field (name "items")
								(e-lookup-local @40.30-40.43
									(pattern @4.1-4.14)))))))))
	(d-let
		(p-assign @43.1-43.5 (ident "main"))
		(e-lambda @43.8-46.2
			(args
				(p-underscore @43.9-43.10))
			(e-block @43.12-46.2
				(e-int @45.5-45.7 (value "42"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.14 (type "List(Num(*))"))
		(patt @7.1-7.9 (type "List(Num(*))"))
		(patt @8.1-8.9 (type "List(Str)"))
		(patt @9.1-9.11 (type "List(Frac(*))"))
		(patt @12.1-12.4 (type "Num(*)"))
		(patt @13.1-13.5 (type "Frac(*)"))
		(patt @14.1-14.4 (type "Str"))
		(patt @17.1-17.6 (type "List(Num(*))"))
		(patt @18.1-18.6 (type "List(Frac(*))"))
		(patt @19.1-19.6 (type "List(Str)"))
		(patt @22.1-22.13 (type "List(List(Num(*)))"))
		(patt @23.1-23.17 (type "List(List(Num(*)))"))
		(patt @24.1-24.17 (type "List(List(Str))"))
		(patt @27.1-27.18 (type "{ empty: List(Num(*)), integers: List(Num(*)), strings: List(Str), floats: List(Frac(*)) }"))
		(patt @35.1-35.8 (type "{ data: List({ id: Num(*), values: List(Num(*)) }), meta: { count: Num(*), items: List(Num(*)) } }"))
		(patt @43.1-43.5 (type "* -> Num(*)")))
	(expressions
		(expr @4.17-4.19 (type "List(Num(*))"))
		(expr @7.12-7.21 (type "List(Num(*))"))
		(expr @8.12-8.30 (type "List(Str)"))
		(expr @9.14-9.29 (type "List(Frac(*))"))
		(expr @12.7-12.9 (type "Num(*)"))
		(expr @13.8-13.11 (type "Frac(*)"))
		(expr @14.7-14.14 (type "Str"))
		(expr @17.9-17.24 (type "List(Num(*))"))
		(expr @18.9-18.27 (type "List(Frac(*))"))
		(expr @19.9-19.24 (type "List(Str)"))
		(expr @22.16-22.46 (type "List(List(Num(*)))"))
		(expr @23.20-23.35 (type "List(List(Num(*)))"))
		(expr @24.20-24.41 (type "List(List(Str))"))
		(expr @27.21-32.2 (type "{ empty: List(Num(*)), integers: List(Num(*)), strings: List(Str), floats: List(Frac(*)) }"))
		(expr @35.11-41.2 (type "{ data: List({ id: Num(*), values: List(Num(*)) }), meta: { count: Num(*), items: List(Num(*)) } }"))
		(expr @43.8-46.2 (type "* -> Num(*)"))))
~~~
