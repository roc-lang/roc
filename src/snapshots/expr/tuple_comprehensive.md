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
(e-block @1-1-19-2
	(statements
		(s-decl @3-5-3-21
			(p-ident @3-5-3-12 (raw "add_one"))
			(e-lambda @3-15-3-21
				(args
					(p-underscore))
				(e-record @3-19-3-21)))
		(s-decl @4-5-4-11
			(p-ident @4-5-4-6 (raw "x"))
			(e-int @4-9-4-11 (raw "10")))
		(s-decl @5-5-5-11
			(p-ident @5-5-5-6 (raw "y"))
			(e-int @5-9-5-11 (raw "20")))
		(s-decl @6-5-6-11
			(p-ident @6-5-6-6 (raw "z"))
			(e-int @6-9-6-11 (raw "30")))
		(s-decl @9-2-9-12
			(p-ident @9-2-9-7 (raw "empty"))
			(e-tuple @9-10-9-12))
		(s-decl @10-2-10-15
			(p-ident @10-2-10-8 (raw "single"))
			(e-tuple @10-11-10-15
				(e-int @10-12-10-14 (raw "42"))))
		(s-decl @11-2-11-15
			(p-ident @11-2-11-6 (raw "pair"))
			(e-tuple @11-9-11-15
				(e-int @11-10-11-11 (raw "1"))
				(e-int @11-13-11-14 (raw "2"))))
		(s-decl @12-2-12-29
			(p-ident @12-2-12-8 (raw "triple"))
			(e-tuple @12-11-12-29
				(e-int @12-12-12-13 (raw "1"))
				(e-string @12-15-12-22
					(e-string-part @12-16-12-21 (raw "hello")))
				(e-tag @12-24-12-28 (raw "True"))))
		(s-decl @13-2-13-27
			(p-ident @13-2-13-8 (raw "nested"))
			(e-tuple @13-11-13-27
				(e-tuple @13-12-13-18
					(e-int @13-13-13-14 (raw "1"))
					(e-int @13-16-13-17 (raw "2")))
				(e-tuple @13-20-13-26
					(e-int @13-21-13-22 (raw "3"))
					(e-int @13-24-13-25 (raw "4")))))
		(s-decl @14-2-14-42
			(p-ident @14-2-14-7 (raw "mixed"))
			(e-tuple @14-10-14-42
				(e-apply @14-11-14-21
					(e-ident @14-11-14-18 (qaul "") (raw "add_one"))
					(e-int @14-19-14-20 (raw "5")))
				(e-string @14-23-14-30
					(e-string-part @14-24-14-29 (raw "world")))
				(e-list @14-32-14-41
					(e-int @14-33-14-34 (raw "1"))
					(e-int @14-36-14-37 (raw "2"))
					(e-int @14-39-14-40 (raw "3")))))
		(s-decl @15-2-15-23
			(p-ident @15-2-15-11 (raw "with_vars"))
			(e-tuple @15-14-15-23
				(e-ident @15-15-15-16 (qaul "") (raw "x"))
				(e-ident @15-18-15-19 (qaul "") (raw "y"))
				(e-ident @15-21-15-22 (qaul "") (raw "z"))))
		(s-decl @16-2-16-31
			(p-ident @16-2-16-13 (raw "with_lambda"))
			(e-tuple @16-16-16-31
				(e-lambda @16-17-16-27
					(args
						(p-ident @16-18-16-19 (raw "n")))
					(e-binop @16-21-16-27 (op "+")
						(e-ident @16-21-16-22 (qaul "") (raw "n"))
						(e-int @16-25-16-26 (raw "1"))))
				(e-int @16-28-16-30 (raw "42"))))
		(e-ident @18-2-18-7 (qaul "") (raw "empty"))))
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
(e-block @1-1-19-2 (id 161)
	(s-let @3-5-3-21
		(p-assign @3-5-3-12 (ident "add_one") (id 72))
		(e-lambda @3-15-3-21 (id 75)
			(args
				(p-underscore @3-16-3-17 (id 73)))
			(e-empty_record @3-19-3-21)))
	(s-let @4-5-4-11
		(p-assign @4-5-4-6 (ident "x") (id 77))
		(e-int @4-9-4-11 (num-var 79) (value "10") (id 79)))
	(s-let @5-5-5-11
		(p-assign @5-5-5-6 (ident "y") (id 81))
		(e-int @5-9-5-11 (num-var 83) (value "20") (id 83)))
	(s-let @6-5-6-11
		(p-assign @6-5-6-6 (ident "z") (id 85))
		(e-int @6-9-6-11 (num-var 87) (value "30") (id 87)))
	(s-let @9-2-9-12
		(p-assign @9-2-9-7 (ident "empty") (id 89))
		(e-tuple @9-10-9-12 (id 90)
			(elems)))
	(s-let @10-2-10-15
		(p-assign @10-2-10-8 (ident "single") (id 92))
		(e-tuple @10-11-10-15 (id 95)
			(elems
				(e-int @10-12-10-14 (num-var 94) (value "42")))))
	(s-let @11-2-11-15
		(p-assign @11-2-11-6 (ident "pair") (id 97))
		(e-tuple @11-9-11-15 (id 102)
			(elems
				(e-int @11-10-11-11 (num-var 99) (value "1"))
				(e-int @11-13-11-14 (num-var 101) (value "2")))))
	(s-let @12-2-12-29
		(p-assign @12-2-12-8 (ident "triple") (id 104))
		(e-tuple @12-11-12-29 (id 111)
			(elems
				(e-int @12-12-12-13 (num-var 106) (value "1"))
				(e-string @12-15-12-22
					(e-literal @12-16-12-21 (string "hello")))
				(e-tag @12-24-12-28 (ext-var 0) (name "True") (args "TODO")))))
	(s-let @13-2-13-27
		(p-assign @13-2-13-8 (ident "nested") (id 113))
		(e-tuple @13-11-13-27 (id 124)
			(elems
				(e-tuple @13-12-13-18
					(elems
						(e-int @13-13-13-14 (num-var 115) (value "1"))
						(e-int @13-16-13-17 (num-var 117) (value "2"))))
				(e-tuple @13-20-13-26
					(elems
						(e-int @13-21-13-22 (num-var 120) (value "3"))
						(e-int @13-24-13-25 (num-var 122) (value "4")))))))
	(s-let @14-2-14-42
		(p-assign @14-2-14-7 (ident "mixed") (id 126))
		(e-tuple @14-10-14-42 (id 141)
			(elems
				(e-call @14-11-14-21
					(e-lookup-local @14-11-14-18
						(pattern (id 72)))
					(e-int @14-19-14-20 (num-var 129) (value "5")))
				(e-string @14-23-14-30
					(e-literal @14-24-14-29 (string "world")))
				(e-list @14-32-14-41 (elem-var 139)
					(elems
						(e-int @14-33-14-34 (num-var 134) (value "1"))
						(e-int @14-36-14-37 (num-var 136) (value "2"))
						(e-int @14-39-14-40 (num-var 138) (value "3")))))))
	(s-let @15-2-15-23
		(p-assign @15-2-15-11 (ident "with_vars") (id 143))
		(e-tuple @15-14-15-23 (id 147)
			(elems
				(e-lookup-local @15-15-15-16
					(pattern (id 77)))
				(e-lookup-local @15-18-15-19
					(pattern (id 81)))
				(e-lookup-local @15-21-15-22
					(pattern (id 85))))))
	(s-let @16-2-16-31
		(p-assign @16-2-16-13 (ident "with_lambda") (id 149))
		(e-tuple @16-16-16-31 (id 158)
			(elems
				(e-lambda @16-17-16-27
					(args
						(p-assign @16-18-16-19 (ident "n") (id 150)))
					(e-binop @16-21-16-27 (op "add")
						(e-lookup-local @16-21-16-22
							(pattern (id 150)))
						(e-int @16-25-16-26 (num-var 153) (value "1"))))
				(e-int @16-28-16-30 (num-var 157) (value "42")))))
	(e-lookup-local @18-2-18-7
		(pattern (id 89))))
~~~
# TYPES
~~~clojure
(expr (id 161) (type "*"))
~~~