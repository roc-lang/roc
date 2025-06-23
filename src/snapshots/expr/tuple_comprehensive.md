# META
~~~ini
description=Comprehensive tuple expression tests
type=expr
~~~
# SOURCE
~~~roc
{
    # define these to avoid runtime errors
    add_one = |_| {}
    x = 10
    y = 20
    z = 30

    # example tuples
	empty = ()
	single = (42)
	pair = (1, 2)
	triple = (1, "hello", True)
	nested = ((1, 2), (3, 4))
	mixed = (add_one(5), "world", [1, 2, 3])
	with_vars = (x, y, z)
	with_lambda = (|n| n + 1, 42)

	empty
}
~~~
# PROBLEMS
**NOT IMPLEMENTED**
This feature is not yet implemented: canonicalize record expression

**UNUSED VARIABLE**
Variable ``with_lambda`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_with_lambda` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:16:2:16:13:**
```roc
	with_lambda = (|n| n + 1, 42)
```


**UNUSED VARIABLE**
Variable ``single`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_single` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:10:2:10:8:**
```roc
	single = (42)
```


**UNUSED VARIABLE**
Variable ``pair`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_pair` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:11:2:11:6:**
```roc
	pair = (1, 2)
```


**UNUSED VARIABLE**
Variable ``nested`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_nested` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:13:2:13:8:**
```roc
	nested = ((1, 2), (3, 4))
```


**UNUSED VARIABLE**
Variable ``triple`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_triple` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:12:2:12:8:**
```roc
	triple = (1, "hello", True)
```


**UNUSED VARIABLE**
Variable ``mixed`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_mixed` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:14:2:14:7:**
```roc
	mixed = (add_one(5), "world", [1, 2, 3])
```


**UNUSED VARIABLE**
Variable ``with_vars`` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_with_vars` to suppress this warning.
The unused variable is declared here:
**tuple_comprehensive.md:15:2:15:11:**
```roc
	with_vars = (x, y, z)
```


# TOKENS
~~~zig
OpenCurly(1:1-1:2),Newline(1:1-1:1),
Newline(2:6-2:43),
LowerIdent(3:5-3:12),OpAssign(3:13-3:14),OpBar(3:15-3:16),Underscore(3:16-3:17),OpBar(3:17-3:18),OpenCurly(3:19-3:20),CloseCurly(3:20-3:21),Newline(1:1-1:1),
LowerIdent(4:5-4:6),OpAssign(4:7-4:8),Int(4:9-4:11),Newline(1:1-1:1),
LowerIdent(5:5-5:6),OpAssign(5:7-5:8),Int(5:9-5:11),Newline(1:1-1:1),
LowerIdent(6:5-6:6),OpAssign(6:7-6:8),Int(6:9-6:11),Newline(1:1-1:1),
Newline(1:1-1:1),
Newline(8:6-8:21),
LowerIdent(9:2-9:7),OpAssign(9:8-9:9),OpenRound(9:10-9:11),CloseRound(9:11-9:12),Newline(1:1-1:1),
LowerIdent(10:2-10:8),OpAssign(10:9-10:10),OpenRound(10:11-10:12),Int(10:12-10:14),CloseRound(10:14-10:15),Newline(1:1-1:1),
LowerIdent(11:2-11:6),OpAssign(11:7-11:8),OpenRound(11:9-11:10),Int(11:10-11:11),Comma(11:11-11:12),Int(11:13-11:14),CloseRound(11:14-11:15),Newline(1:1-1:1),
LowerIdent(12:2-12:8),OpAssign(12:9-12:10),OpenRound(12:11-12:12),Int(12:12-12:13),Comma(12:13-12:14),StringStart(12:15-12:16),StringPart(12:16-12:21),StringEnd(12:21-12:22),Comma(12:22-12:23),UpperIdent(12:24-12:28),CloseRound(12:28-12:29),Newline(1:1-1:1),
LowerIdent(13:2-13:8),OpAssign(13:9-13:10),OpenRound(13:11-13:12),NoSpaceOpenRound(13:12-13:13),Int(13:13-13:14),Comma(13:14-13:15),Int(13:16-13:17),CloseRound(13:17-13:18),Comma(13:18-13:19),OpenRound(13:20-13:21),Int(13:21-13:22),Comma(13:22-13:23),Int(13:24-13:25),CloseRound(13:25-13:26),CloseRound(13:26-13:27),Newline(1:1-1:1),
LowerIdent(14:2-14:7),OpAssign(14:8-14:9),OpenRound(14:10-14:11),LowerIdent(14:11-14:18),NoSpaceOpenRound(14:18-14:19),Int(14:19-14:20),CloseRound(14:20-14:21),Comma(14:21-14:22),StringStart(14:23-14:24),StringPart(14:24-14:29),StringEnd(14:29-14:30),Comma(14:30-14:31),OpenSquare(14:32-14:33),Int(14:33-14:34),Comma(14:34-14:35),Int(14:36-14:37),Comma(14:37-14:38),Int(14:39-14:40),CloseSquare(14:40-14:41),CloseRound(14:41-14:42),Newline(1:1-1:1),
LowerIdent(15:2-15:11),OpAssign(15:12-15:13),OpenRound(15:14-15:15),LowerIdent(15:15-15:16),Comma(15:16-15:17),LowerIdent(15:18-15:19),Comma(15:19-15:20),LowerIdent(15:21-15:22),CloseRound(15:22-15:23),Newline(1:1-1:1),
LowerIdent(16:2-16:13),OpAssign(16:14-16:15),OpenRound(16:16-16:17),OpBar(16:17-16:18),LowerIdent(16:18-16:19),OpBar(16:19-16:20),LowerIdent(16:21-16:22),OpPlus(16:23-16:24),Int(16:25-16:26),Comma(16:26-16:27),Int(16:28-16:30),CloseRound(16:30-16:31),Newline(1:1-1:1),
Newline(1:1-1:1),
LowerIdent(18:2-18:7),Newline(1:1-1:1),
CloseCurly(19:1-19:2),EndOfFile(19:2-19:2),
~~~
# PARSE
~~~clojure
(block (1:1-19:2)
	(statements
		(decl (3:5-3:21)
			(ident (3:5-3:12) "add_one")
			(lambda (3:15-3:21)
				(args (underscore))
				(record (3:19-3:21))))
		(decl (4:5-4:11)
			(ident (4:5-4:6) "x")
			(int (4:9-4:11) "10"))
		(decl (5:5-5:11)
			(ident (5:5-5:6) "y")
			(int (5:9-5:11) "20"))
		(decl (6:5-6:11)
			(ident (6:5-6:6) "z")
			(int (6:9-6:11) "30"))
		(decl (9:2-9:12)
			(ident (9:2-9:7) "empty")
			(tuple (9:10-9:12)))
		(decl (10:2-10:15)
			(ident (10:2-10:8) "single")
			(tuple (10:11-10:15) (int (10:12-10:14) "42")))
		(decl (11:2-11:15)
			(ident (11:2-11:6) "pair")
			(tuple (11:9-11:15)
				(int (11:10-11:11) "1")
				(int (11:13-11:14) "2")))
		(decl (12:2-12:29)
			(ident (12:2-12:8) "triple")
			(tuple (12:11-12:29)
				(int (12:12-12:13) "1")
				(string (12:15-12:22) (string_part (12:16-12:21) "hello"))
				(tag (12:24-12:28) "True")))
		(decl (13:2-13:27)
			(ident (13:2-13:8) "nested")
			(tuple (13:11-13:27)
				(tuple (13:12-13:18)
					(int (13:13-13:14) "1")
					(int (13:16-13:17) "2"))
				(tuple (13:20-13:26)
					(int (13:21-13:22) "3")
					(int (13:24-13:25) "4"))))
		(decl (14:2-14:42)
			(ident (14:2-14:7) "mixed")
			(tuple (14:10-14:42)
				(apply (14:11-14:21)
					(ident (14:11-14:18) "" "add_one")
					(int (14:19-14:20) "5"))
				(string (14:23-14:30) (string_part (14:24-14:29) "world"))
				(list (14:32-14:41)
					(int (14:33-14:34) "1")
					(int (14:36-14:37) "2")
					(int (14:39-14:40) "3"))))
		(decl (15:2-15:23)
			(ident (15:2-15:11) "with_vars")
			(tuple (15:14-15:23)
				(ident (15:15-15:16) "" "x")
				(ident (15:18-15:19) "" "y")
				(ident (15:21-15:22) "" "z")))
		(decl (16:2-16:31)
			(ident (16:2-16:13) "with_lambda")
			(tuple (16:16-16:31)
				(lambda (16:17-16:27)
					(args (ident (16:18-16:19) "n"))
					(binop (16:21-16:27)
						"+"
						(ident (16:21-16:22) "" "n")
						(int (16:25-16:26) "1")))
				(int (16:28-16:30) "42")))
		(ident (18:2-18:7) "" "empty")))
~~~
# FORMATTED
~~~roc
{
	# define these to avoid runtime errors
	add_one = |_| {}
	x = 10
	y = 20
	z = 30

	# example tuples
	empty = ()
	single = (42)
	pair = (1, 2)
	triple = (1, "hello", True)
	nested = ((1, 2), (3, 4))
	mixed = (add_one(5), "world", [1, 2, 3])
	with_vars = (x, y, z)
	with_lambda = (|n| n + 1, 42)

	empty
}
~~~
# CANONICALIZE
~~~clojure
(e_block (1:1-19:2)
	(s_let (3:5-3:21)
		(p_assign (3:5-3:12)
			(pid 72)
			(ident "add_one"))
		(e_lambda (3:15-3:21)
			(args (p_underscore (3:16-3:17) (pid 73)))
			(e_runtime_error (1:1-1:1) "not_implemented")))
	(s_let (4:5-4:11)
		(p_assign (4:5-4:6)
			(pid 78)
			(ident "x"))
		(e_int (4:9-4:11)
			(int_var 80)
			(precision_var 79)
			(literal "10")
			(value "TODO")
			(bound "u8")))
	(s_let (5:5-5:11)
		(p_assign (5:5-5:6)
			(pid 83)
			(ident "y"))
		(e_int (5:9-5:11)
			(int_var 85)
			(precision_var 84)
			(literal "20")
			(value "TODO")
			(bound "u8")))
	(s_let (6:5-6:11)
		(p_assign (6:5-6:6)
			(pid 88)
			(ident "z"))
		(e_int (6:9-6:11)
			(int_var 90)
			(precision_var 89)
			(literal "30")
			(value "TODO")
			(bound "u8")))
	(s_let (9:2-9:12)
		(p_assign (9:2-9:7)
			(pid 93)
			(ident "empty"))
		(e_tuple (9:10-9:12) (tuple_var "#94") (elems)))
	(s_let (10:2-10:15)
		(p_assign (10:2-10:8)
			(pid 97)
			(ident "single"))
		(e_tuple (10:11-10:15)
			(tuple_var "#101")
			(elems
				(e_int (10:12-10:14)
					(int_var 99)
					(precision_var 98)
					(literal "42")
					(value "TODO")
					(bound "u8")))))
	(s_let (11:2-11:15)
		(p_assign (11:2-11:6)
			(pid 104)
			(ident "pair"))
		(e_tuple (11:9-11:15)
			(tuple_var "#111")
			(elems
				(e_int (11:10-11:11)
					(int_var 106)
					(precision_var 105)
					(literal "1")
					(value "TODO")
					(bound "u8"))
				(e_int (11:13-11:14)
					(int_var 109)
					(precision_var 108)
					(literal "2")
					(value "TODO")
					(bound "u8")))))
	(s_let (12:2-12:29)
		(p_assign (12:2-12:8)
			(pid 114)
			(ident "triple"))
		(e_tuple (12:11-12:29)
			(tuple_var "#122")
			(elems
				(e_int (12:12-12:13)
					(int_var 116)
					(precision_var 115)
					(literal "1")
					(value "TODO")
					(bound "u8"))
				(e_string (12:15-12:22) (e_literal (12:16-12:21) "hello"))
				(e_tag (12:24-12:28)
					(ext_var 0)
					(name "True")
					(args "TODO")))))
	(s_let (13:2-13:27)
		(p_assign (13:2-13:8)
			(pid 125)
			(ident "nested"))
		(e_tuple (13:11-13:27)
			(tuple_var "#142")
			(elems
				(e_tuple (13:12-13:18)
					(tuple_var "#132")
					(elems
						(e_int (13:13-13:14)
							(int_var 127)
							(precision_var 126)
							(literal "1")
							(value "TODO")
							(bound "u8"))
						(e_int (13:16-13:17)
							(int_var 130)
							(precision_var 129)
							(literal "2")
							(value "TODO")
							(bound "u8"))))
				(e_tuple (13:20-13:26)
					(tuple_var "#140")
					(elems
						(e_int (13:21-13:22)
							(int_var 135)
							(precision_var 134)
							(literal "3")
							(value "TODO")
							(bound "u8"))
						(e_int (13:24-13:25)
							(int_var 138)
							(precision_var 137)
							(literal "4")
							(value "TODO")
							(bound "u8")))))))
	(s_let (14:2-14:42)
		(p_assign (14:2-14:7)
			(pid 145)
			(ident "mixed"))
		(e_tuple (14:10-14:42)
			(tuple_var "#164")
			(elems
				(e_call (14:11-14:21)
					(e_lookup (14:11-14:18) (pid 72))
					(e_int (14:19-14:20)
						(int_var 148)
						(precision_var 147)
						(literal "5")
						(value "TODO")
						(bound "u8")))
				(e_string (14:23-14:30) (e_literal (14:24-14:29) "world"))
				(e_list (14:32-14:41)
					(elem_var 162)
					(elems
						(e_int (14:33-14:34)
							(int_var 154)
							(precision_var 153)
							(literal "1")
							(value "TODO")
							(bound "u8"))
						(e_int (14:36-14:37)
							(int_var 157)
							(precision_var 156)
							(literal "2")
							(value "TODO")
							(bound "u8"))
						(e_int (14:39-14:40)
							(int_var 160)
							(precision_var 159)
							(literal "3")
							(value "TODO")
							(bound "u8")))))))
	(s_let (15:2-15:23)
		(p_assign (15:2-15:11)
			(pid 167)
			(ident "with_vars"))
		(e_tuple (15:14-15:23)
			(tuple_var "#171")
			(elems
				(e_lookup (15:15-15:16) (pid 78))
				(e_lookup (15:18-15:19) (pid 83))
				(e_lookup (15:21-15:22) (pid 88)))))
	(s_let (16:2-16:31)
		(p_assign (16:2-16:13)
			(pid 174)
			(ident "with_lambda"))
		(e_tuple (16:16-16:31)
			(tuple_var "#185")
			(elems
				(e_lambda (16:17-16:27)
					(args
						(p_assign (16:18-16:19)
							(pid 175)
							(ident "n")))
					(e_binop (16:21-16:27)
						"add"
						(e_lookup (16:21-16:22) (pid 175))
						(e_int (16:25-16:26)
							(int_var 178)
							(precision_var 177)
							(literal "1")
							(value "TODO")
							(bound "u8"))))
				(e_int (16:28-16:30)
					(int_var 183)
					(precision_var 182)
					(literal "42")
					(value "TODO")
					(bound "u8")))))
	(e_lookup (18:2-18:7) (pid 93)))
~~~
# TYPES
~~~clojure
(expr 189 (type "*"))
~~~