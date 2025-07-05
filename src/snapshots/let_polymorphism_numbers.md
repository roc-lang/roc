# META
~~~ini
description=Let-polymorphism with numbers
type=file
~~~
# SOURCE
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic number polymorphism
num = 42
frac = 4.2

# Using polymorphic values in different contexts
int_use = num
float_use = frac

# Numbers in lists
list_with_int = [num, num]
list_with_float = [frac, frac]
mixed_num_list = [num, 100]
mixed_frac_list = [frac, 10.5]

# Numbers in records
record_with_nums = { integer: num, fraction: frac }
nested_record = { data: { x: num, y: frac }, count: 5 }

# Polymorphic empty list
empty = []
empty_in_int_context = [1, 2, 3]
empty_in_str_context = ["a", "b", "c"]

# Polymorphic values used multiple times
shared_num = 123
use1 = shared_num + 1
use2 = shared_num * 2
use3 = { value: shared_num, double: shared_num * 2 }

main = |_| {
    # Just to ensure everything type-checks
    use1
}
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp(1:1-1:4),OpenSquare(1:5-1:6),LowerIdent(1:6-1:10),CloseSquare(1:10-1:11),OpenCurly(1:12-1:13),LowerIdent(1:14-1:16),OpColon(1:16-1:17),KwPlatform(1:18-1:26),StringStart(1:27-1:28),StringPart(1:28-1:53),StringEnd(1:53-1:54),CloseCurly(1:55-1:56),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(3:2-3:28),
LowerIdent(4:1-4:4),OpAssign(4:5-4:6),Int(4:7-4:9),Newline(1:1-1:1),
LowerIdent(5:1-5:5),OpAssign(5:6-5:7),Float(5:8-5:11),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(7:2-7:49),
LowerIdent(8:1-8:8),OpAssign(8:9-8:10),LowerIdent(8:11-8:14),Newline(1:1-1:1),
LowerIdent(9:1-9:10),OpAssign(9:11-9:12),LowerIdent(9:13-9:17),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(11:2-11:19),
LowerIdent(12:1-12:14),OpAssign(12:15-12:16),OpenSquare(12:17-12:18),LowerIdent(12:18-12:21),Comma(12:21-12:22),LowerIdent(12:23-12:26),CloseSquare(12:26-12:27),Newline(1:1-1:1),
LowerIdent(13:1-13:16),OpAssign(13:17-13:18),OpenSquare(13:19-13:20),LowerIdent(13:20-13:24),Comma(13:24-13:25),LowerIdent(13:26-13:30),CloseSquare(13:30-13:31),Newline(1:1-1:1),
LowerIdent(14:1-14:15),OpAssign(14:16-14:17),OpenSquare(14:18-14:19),LowerIdent(14:19-14:22),Comma(14:22-14:23),Int(14:24-14:27),CloseSquare(14:27-14:28),Newline(1:1-1:1),
LowerIdent(15:1-15:16),OpAssign(15:17-15:18),OpenSquare(15:19-15:20),LowerIdent(15:20-15:24),Comma(15:24-15:25),Float(15:26-15:30),CloseSquare(15:30-15:31),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(17:2-17:21),
LowerIdent(18:1-18:17),OpAssign(18:18-18:19),OpenCurly(18:20-18:21),LowerIdent(18:22-18:29),OpColon(18:29-18:30),LowerIdent(18:31-18:34),Comma(18:34-18:35),LowerIdent(18:36-18:44),OpColon(18:44-18:45),LowerIdent(18:46-18:50),CloseCurly(18:51-18:52),Newline(1:1-1:1),
LowerIdent(19:1-19:14),OpAssign(19:15-19:16),OpenCurly(19:17-19:18),LowerIdent(19:19-19:23),OpColon(19:23-19:24),OpenCurly(19:25-19:26),LowerIdent(19:27-19:28),OpColon(19:28-19:29),LowerIdent(19:30-19:33),Comma(19:33-19:34),LowerIdent(19:35-19:36),OpColon(19:36-19:37),LowerIdent(19:38-19:42),CloseCurly(19:43-19:44),Comma(19:44-19:45),LowerIdent(19:46-19:51),OpColon(19:51-19:52),Int(19:53-19:54),CloseCurly(19:55-19:56),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(21:2-21:25),
LowerIdent(22:1-22:6),OpAssign(22:7-22:8),OpenSquare(22:9-22:10),CloseSquare(22:10-22:11),Newline(1:1-1:1),
LowerIdent(23:1-23:21),OpAssign(23:22-23:23),OpenSquare(23:24-23:25),Int(23:25-23:26),Comma(23:26-23:27),Int(23:28-23:29),Comma(23:29-23:30),Int(23:31-23:32),CloseSquare(23:32-23:33),Newline(1:1-1:1),
LowerIdent(24:1-24:21),OpAssign(24:22-24:23),OpenSquare(24:24-24:25),StringStart(24:25-24:26),StringPart(24:26-24:27),StringEnd(24:27-24:28),Comma(24:28-24:29),StringStart(24:30-24:31),StringPart(24:31-24:32),StringEnd(24:32-24:33),Comma(24:33-24:34),StringStart(24:35-24:36),StringPart(24:36-24:37),StringEnd(24:37-24:38),CloseSquare(24:38-24:39),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(26:2-26:41),
LowerIdent(27:1-27:11),OpAssign(27:12-27:13),Int(27:14-27:17),Newline(1:1-1:1),
LowerIdent(28:1-28:5),OpAssign(28:6-28:7),LowerIdent(28:8-28:18),OpPlus(28:19-28:20),Int(28:21-28:22),Newline(1:1-1:1),
LowerIdent(29:1-29:5),OpAssign(29:6-29:7),LowerIdent(29:8-29:18),OpStar(29:19-29:20),Int(29:21-29:22),Newline(1:1-1:1),
LowerIdent(30:1-30:5),OpAssign(30:6-30:7),OpenCurly(30:8-30:9),LowerIdent(30:10-30:15),OpColon(30:15-30:16),LowerIdent(30:17-30:27),Comma(30:27-30:28),LowerIdent(30:29-30:35),OpColon(30:35-30:36),LowerIdent(30:37-30:47),OpStar(30:48-30:49),Int(30:50-30:51),CloseCurly(30:52-30:53),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(32:1-32:5),OpAssign(32:6-32:7),OpBar(32:8-32:9),Underscore(32:9-32:10),OpBar(32:10-32:11),OpenCurly(32:12-32:13),Newline(1:1-1:1),
Newline(33:6-33:44),
LowerIdent(34:5-34:9),Newline(1:1-1:1),
CloseCurly(35:1-35:2),EndOfFile(35:2-35:2),
~~~
# PARSE
~~~clojure
(file @1.1-35.2
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
		(s-decl @8.1-8.14
			(p-ident @8.1-8.8 (raw "int_use"))
			(e-ident @8.11-8.14 (qaul "") (raw "num")))
		(s-decl @9.1-9.17
			(p-ident @9.1-9.10 (raw "float_use"))
			(e-ident @9.13-9.17 (qaul "") (raw "frac")))
		(s-decl @12.1-12.27
			(p-ident @12.1-12.14 (raw "list_with_int"))
			(e-list @12.17-12.27
				(e-ident @12.18-12.21 (qaul "") (raw "num"))
				(e-ident @12.23-12.26 (qaul "") (raw "num"))))
		(s-decl @13.1-13.31
			(p-ident @13.1-13.16 (raw "list_with_float"))
			(e-list @13.19-13.31
				(e-ident @13.20-13.24 (qaul "") (raw "frac"))
				(e-ident @13.26-13.30 (qaul "") (raw "frac"))))
		(s-decl @14.1-14.28
			(p-ident @14.1-14.15 (raw "mixed_num_list"))
			(e-list @14.18-14.28
				(e-ident @14.19-14.22 (qaul "") (raw "num"))
				(e-int @14.24-14.27 (raw "100"))))
		(s-decl @15.1-15.31
			(p-ident @15.1-15.16 (raw "mixed_frac_list"))
			(e-list @15.19-15.31
				(e-ident @15.20-15.24 (qaul "") (raw "frac"))
				(e-frac @15.26-15.30 (raw "10.5"))))
		(s-decl @18.1-18.52
			(p-ident @18.1-18.17 (raw "record_with_nums"))
			(e-record @18.20-18.52
				(field (field "integer") (optional false)
					(e-ident @18.31-18.34 (qaul "") (raw "num")))
				(field (field "fraction") (optional false)
					(e-ident @18.46-18.50 (qaul "") (raw "frac")))))
		(s-decl @19.1-19.56
			(p-ident @19.1-19.14 (raw "nested_record"))
			(e-record @19.17-19.56
				(field (field "data") (optional false)
					(e-record @19.25-19.44
						(field (field "x") (optional false)
							(e-ident @19.30-19.33 (qaul "") (raw "num")))
						(field (field "y") (optional false)
							(e-ident @19.38-19.42 (qaul "") (raw "frac")))))
				(field (field "count") (optional false)
					(e-int @19.53-19.54 (raw "5")))))
		(s-decl @22.1-22.11
			(p-ident @22.1-22.6 (raw "empty"))
			(e-list @22.9-22.11))
		(s-decl @23.1-23.33
			(p-ident @23.1-23.21 (raw "empty_in_int_context"))
			(e-list @23.24-23.33
				(e-int @23.25-23.26 (raw "1"))
				(e-int @23.28-23.29 (raw "2"))
				(e-int @23.31-23.32 (raw "3"))))
		(s-decl @24.1-24.39
			(p-ident @24.1-24.21 (raw "empty_in_str_context"))
			(e-list @24.24-24.39
				(e-string @24.25-24.28
					(e-string-part @24.26-24.27 (raw "a")))
				(e-string @24.30-24.33
					(e-string-part @24.31-24.32 (raw "b")))
				(e-string @24.35-24.38
					(e-string-part @24.36-24.37 (raw "c")))))
		(s-decl @27.1-27.17
			(p-ident @27.1-27.11 (raw "shared_num"))
			(e-int @27.14-27.17 (raw "123")))
		(s-decl @28.1-29.5
			(p-ident @28.1-28.5 (raw "use1"))
			(e-binop @28.8-29.5 (op "+")
				(e-ident @28.8-28.18 (qaul "") (raw "shared_num"))
				(e-int @28.21-28.22 (raw "1"))))
		(s-decl @29.1-30.5
			(p-ident @29.1-29.5 (raw "use2"))
			(e-binop @29.8-30.5 (op "*")
				(e-ident @29.8-29.18 (qaul "") (raw "shared_num"))
				(e-int @29.21-29.22 (raw "2"))))
		(s-decl @30.1-30.53
			(p-ident @30.1-30.5 (raw "use3"))
			(e-record @30.8-30.53
				(field (field "value") (optional false)
					(e-ident @30.17-30.27 (qaul "") (raw "shared_num")))
				(field (field "double") (optional false)
					(e-binop @30.37-30.53 (op "*")
						(e-ident @30.37-30.47 (qaul "") (raw "shared_num"))
						(e-int @30.50-30.51 (raw "2"))))))
		(s-decl @32.1-35.2
			(p-ident @32.1-32.5 (raw "main"))
			(e-lambda @32.8-35.2
				(args
					(p-underscore))
				(e-block @32.12-35.2
					(statements
						(e-ident @34.5-34.9 (qaul "") (raw "use1"))))))))
~~~
# FORMATTED
~~~roc
app [main] { pf: platform "../basic-cli/platform.roc" }

# Basic number polymorphism
num = 42
frac = 4.2

# Using polymorphic values in different contexts
int_use = num
float_use = frac

# Numbers in lists
list_with_int = [num, num]
list_with_float = [frac, frac]
mixed_num_list = [num, 100]
mixed_frac_list = [frac, 10.5]

# Numbers in records
record_with_nums = { integer: num, fraction: frac }
nested_record = { data: { x: num, y: frac }, count: 5 }

# Polymorphic empty list
empty = []
empty_in_int_context = [1, 2, 3]
empty_in_str_context = ["a", "b", "c"]

# Polymorphic values used multiple times
shared_num = 123
use1 = shared_num + 1
use2 = shared_num * 2
use3 = { value: shared_num, double: shared_num * 2 }

main = |_| {
	# Just to ensure everything type-checks
	use1
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
		(p-assign @8.1-8.8 (ident "int_use"))
		(e-lookup-local @8.11-8.14
			(pattern @4.1-4.4)))
	(d-let
		(p-assign @9.1-9.10 (ident "float_use"))
		(e-lookup-local @9.13-9.17
			(pattern @5.1-5.5)))
	(d-let
		(p-assign @12.1-12.14 (ident "list_with_int"))
		(e-list @12.17-12.27
			(elems
				(e-lookup-local @12.18-12.21
					(pattern @4.1-4.4))
				(e-lookup-local @12.23-12.26
					(pattern @4.1-4.4)))))
	(d-let
		(p-assign @13.1-13.16 (ident "list_with_float"))
		(e-list @13.19-13.31
			(elems
				(e-lookup-local @13.20-13.24
					(pattern @5.1-5.5))
				(e-lookup-local @13.26-13.30
					(pattern @5.1-5.5)))))
	(d-let
		(p-assign @14.1-14.15 (ident "mixed_num_list"))
		(e-list @14.18-14.28
			(elems
				(e-lookup-local @14.19-14.22
					(pattern @4.1-4.4))
				(e-int @14.24-14.27 (value "100")))))
	(d-let
		(p-assign @15.1-15.16 (ident "mixed_frac_list"))
		(e-list @15.19-15.31
			(elems
				(e-lookup-local @15.20-15.24
					(pattern @5.1-5.5))
				(e-dec-small @15.26-15.30 (numerator "105") (denominator-power-of-ten "1") (value "10.5")))))
	(d-let
		(p-assign @18.1-18.17 (ident "record_with_nums"))
		(e-record @18.20-18.52
			(fields
				(field (name "integer")
					(e-lookup-local @18.31-18.34
						(pattern @4.1-4.4)))
				(field (name "fraction")
					(e-lookup-local @18.46-18.50
						(pattern @5.1-5.5))))))
	(d-let
		(p-assign @19.1-19.14 (ident "nested_record"))
		(e-record @19.17-19.56
			(fields
				(field (name "data")
					(e-record @19.25-19.44
						(fields
							(field (name "x")
								(e-lookup-local @19.30-19.33
									(pattern @4.1-4.4)))
							(field (name "y")
								(e-lookup-local @19.38-19.42
									(pattern @5.1-5.5))))))
				(field (name "count")
					(e-int @19.53-19.54 (value "5"))))))
	(d-let
		(p-assign @22.1-22.6 (ident "empty"))
		(e-empty_list @22.9-22.11))
	(d-let
		(p-assign @23.1-23.21 (ident "empty_in_int_context"))
		(e-list @23.24-23.33
			(elems
				(e-int @23.25-23.26 (value "1"))
				(e-int @23.28-23.29 (value "2"))
				(e-int @23.31-23.32 (value "3")))))
	(d-let
		(p-assign @24.1-24.21 (ident "empty_in_str_context"))
		(e-list @24.24-24.39
			(elems
				(e-string @24.25-24.28
					(e-literal @24.26-24.27 (string "a")))
				(e-string @24.30-24.33
					(e-literal @24.31-24.32 (string "b")))
				(e-string @24.35-24.38
					(e-literal @24.36-24.37 (string "c"))))))
	(d-let
		(p-assign @27.1-27.11 (ident "shared_num"))
		(e-int @27.14-27.17 (value "123")))
	(d-let
		(p-assign @28.1-28.5 (ident "use1"))
		(e-binop @28.8-29.5 (op "add")
			(e-lookup-local @28.8-28.18
				(pattern @27.1-27.11))
			(e-int @28.21-28.22 (value "1"))))
	(d-let
		(p-assign @29.1-29.5 (ident "use2"))
		(e-binop @29.8-30.5 (op "mul")
			(e-lookup-local @29.8-29.18
				(pattern @27.1-27.11))
			(e-int @29.21-29.22 (value "2"))))
	(d-let
		(p-assign @30.1-30.5 (ident "use3"))
		(e-record @30.8-30.53
			(fields
				(field (name "value")
					(e-lookup-local @30.17-30.27
						(pattern @27.1-27.11)))
				(field (name "double")
					(e-binop @30.37-30.53 (op "mul")
						(e-lookup-local @30.37-30.47
							(pattern @27.1-27.11))
						(e-int @30.50-30.51 (value "2")))))))
	(d-let
		(p-assign @32.1-32.5 (ident "main"))
		(e-lambda @32.8-35.2
			(args
				(p-underscore @32.9-32.10))
			(e-block @32.12-35.2
				(e-lookup-local @34.5-34.9
					(pattern @28.1-28.5))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt @4.1-4.4 (type "Num(*)"))
		(patt @5.1-5.5 (type "Frac(*)"))
		(patt @8.1-8.8 (type "Num(*)"))
		(patt @9.1-9.10 (type "Frac(*)"))
		(patt @12.1-12.14 (type "List(Num(*))"))
		(patt @13.1-13.16 (type "List(Frac(*))"))
		(patt @14.1-14.15 (type "List(Num(*))"))
		(patt @15.1-15.16 (type "List(Frac(*))"))
		(patt @18.1-18.17 (type "{ integer: Num(*), fraction: Frac(*) }"))
		(patt @19.1-19.14 (type "{ data: { x: Num(*), y: Frac(*) }, count: Num(*) }"))
		(patt @22.1-22.6 (type "List(*)"))
		(patt @23.1-23.21 (type "List(Num(*))"))
		(patt @24.1-24.21 (type "List(Str)"))
		(patt @27.1-27.11 (type "Num(*)"))
		(patt @28.1-28.5 (type "*"))
		(patt @29.1-29.5 (type "*"))
		(patt @30.1-30.5 (type "{ value: Num(*), double: * }"))
		(patt @32.1-32.5 (type "* -> *")))
	(expressions
		(expr @4.7-4.9 (type "Num(*)"))
		(expr @5.8-5.11 (type "Frac(*)"))
		(expr @8.11-8.14 (type "Num(*)"))
		(expr @9.13-9.17 (type "Frac(*)"))
		(expr @12.17-12.27 (type "List(Num(*))"))
		(expr @13.19-13.31 (type "List(Frac(*))"))
		(expr @14.18-14.28 (type "List(Num(*))"))
		(expr @15.19-15.31 (type "List(Frac(*))"))
		(expr @18.20-18.52 (type "{ integer: Num(*), fraction: Frac(*) }"))
		(expr @19.17-19.56 (type "{ data: { x: Num(*), y: Frac(*) }, count: Num(*) }"))
		(expr @22.9-22.11 (type "List(*)"))
		(expr @23.24-23.33 (type "List(Num(*))"))
		(expr @24.24-24.39 (type "List(Str)"))
		(expr @27.14-27.17 (type "Num(*)"))
		(expr @28.8-29.5 (type "*"))
		(expr @29.8-30.5 (type "*"))
		(expr @30.8-30.53 (type "{ value: Num(*), double: * }"))
		(expr @32.8-35.2 (type "* -> *"))))
~~~
