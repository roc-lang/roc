# META
~~~ini
description=Binops collection
type=expr
~~~
# SOURCE
~~~roc
(
    4 + 2,
    4 - 2,
    4 * 2,
    4 / 2,
    4 % 2,
    4 < 2,
    4 > 2,
    4 <= 2,
    4 >= 2,
    4 == 2,
    4 != 2,
    4 // 2,
    True and True,
    True or True,
    None ?? 0,
)
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),Newline(1:1-1:1),
Int(2:5-2:6),OpPlus(2:7-2:8),Int(2:9-2:10),Comma(2:10-2:11),Newline(1:1-1:1),
Int(3:5-3:6),OpBinaryMinus(3:7-3:8),Int(3:9-3:10),Comma(3:10-3:11),Newline(1:1-1:1),
Int(4:5-4:6),OpStar(4:7-4:8),Int(4:9-4:10),Comma(4:10-4:11),Newline(1:1-1:1),
Int(5:5-5:6),OpSlash(5:7-5:8),Int(5:9-5:10),Comma(5:10-5:11),Newline(1:1-1:1),
Int(6:5-6:6),OpPercent(6:7-6:8),Int(6:9-6:10),Comma(6:10-6:11),Newline(1:1-1:1),
Int(7:5-7:6),OpLessThan(7:7-7:8),Int(7:9-7:10),Comma(7:10-7:11),Newline(1:1-1:1),
Int(8:5-8:6),OpGreaterThan(8:7-8:8),Int(8:9-8:10),Comma(8:10-8:11),Newline(1:1-1:1),
Int(9:5-9:6),OpLessThanOrEq(9:7-9:9),Int(9:10-9:11),Comma(9:11-9:12),Newline(1:1-1:1),
Int(10:5-10:6),OpGreaterThanOrEq(10:7-10:9),Int(10:10-10:11),Comma(10:11-10:12),Newline(1:1-1:1),
Int(11:5-11:6),OpEquals(11:7-11:9),Int(11:10-11:11),Comma(11:11-11:12),Newline(1:1-1:1),
Int(12:5-12:6),OpNotEquals(12:7-12:9),Int(12:10-12:11),Comma(12:11-12:12),Newline(1:1-1:1),
Int(13:5-13:6),OpDoubleSlash(13:7-13:9),Int(13:10-13:11),Comma(13:11-13:12),Newline(1:1-1:1),
UpperIdent(14:5-14:9),OpAnd(14:10-14:13),UpperIdent(14:14-14:18),Comma(14:18-14:19),Newline(1:1-1:1),
UpperIdent(15:5-15:9),OpOr(15:10-15:12),UpperIdent(15:13-15:17),Comma(15:17-15:18),Newline(1:1-1:1),
UpperIdent(16:5-16:9),OpDoubleQuestion(16:10-16:12),Int(16:13-16:14),Comma(16:14-16:15),Newline(1:1-1:1),
CloseRound(17:1-17:2),EndOfFile(17:2-17:2),
~~~
# PARSE
~~~clojure
(e-tuple @1-1-17-2
	(e-binop @2-5-2-11 (op "+")
		(e-int @2-5-2-6 (raw "4"))
		(e-int @2-9-2-10 (raw "2")))
	(e-binop @3-5-3-11 (op "-")
		(e-int @3-5-3-6 (raw "4"))
		(e-int @3-9-3-10 (raw "2")))
	(e-binop @4-5-4-11 (op "*")
		(e-int @4-5-4-6 (raw "4"))
		(e-int @4-9-4-10 (raw "2")))
	(e-binop @5-5-5-11 (op "/")
		(e-int @5-5-5-6 (raw "4"))
		(e-int @5-9-5-10 (raw "2")))
	(e-binop @6-5-6-11 (op "%")
		(e-int @6-5-6-6 (raw "4"))
		(e-int @6-9-6-10 (raw "2")))
	(e-binop @7-5-7-11 (op "<")
		(e-int @7-5-7-6 (raw "4"))
		(e-int @7-9-7-10 (raw "2")))
	(e-binop @8-5-8-11 (op ">")
		(e-int @8-5-8-6 (raw "4"))
		(e-int @8-9-8-10 (raw "2")))
	(e-binop @9-5-9-12 (op "<=")
		(e-int @9-5-9-6 (raw "4"))
		(e-int @9-10-9-11 (raw "2")))
	(e-binop @10-5-10-12 (op ">=")
		(e-int @10-5-10-6 (raw "4"))
		(e-int @10-10-10-11 (raw "2")))
	(e-binop @11-5-11-12 (op "==")
		(e-int @11-5-11-6 (raw "4"))
		(e-int @11-10-11-11 (raw "2")))
	(e-binop @12-5-12-12 (op "!=")
		(e-int @12-5-12-6 (raw "4"))
		(e-int @12-10-12-11 (raw "2")))
	(e-binop @13-5-13-12 (op "//")
		(e-int @13-5-13-6 (raw "4"))
		(e-int @13-10-13-11 (raw "2")))
	(e-binop @14-5-14-19 (op "and")
		(e-tag @14-5-14-9 (raw "True"))
		(e-tag @14-14-14-18 (raw "True")))
	(e-binop @15-5-15-18 (op "or")
		(e-tag @15-5-15-9 (raw "True"))
		(e-tag @15-13-15-17 (raw "True")))
	(e-binop @16-5-16-15 (op "??")
		(e-tag @16-5-16-9 (raw "None"))
		(e-int @16-13-16-14 (raw "0"))))
~~~
# FORMATTED
~~~roc
(
	4 + 2,
	4 - 2,
	4 * 2,
	4 / 2,
	4 % 2,
	4 < 2,
	4 > 2,
	4 <= 2,
	4 >= 2,
	4 == 2,
	4 != 2,
	4 // 2,
	True and True,
	True or True,
	None ?? 0,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1-1-17-2 (id 122)
	(elems
		(e-binop @2-5-2-11 (op "add")
			(e-int @2-5-2-6 (value "4"))
			(e-int @2-9-2-10 (value "2")))
		(e-binop @3-5-3-11 (op "sub")
			(e-int @3-5-3-6 (value "4"))
			(e-int @3-9-3-10 (value "2")))
		(e-binop @4-5-4-11 (op "mul")
			(e-int @4-5-4-6 (value "4"))
			(e-int @4-9-4-10 (value "2")))
		(e-binop @5-5-5-11 (op "div")
			(e-int @5-5-5-6 (value "4"))
			(e-int @5-9-5-10 (value "2")))
		(e-binop @6-5-6-11 (op "rem")
			(e-int @6-5-6-6 (value "4"))
			(e-int @6-9-6-10 (value "2")))
		(e-binop @7-5-7-11 (op "lt")
			(e-int @7-5-7-6 (value "4"))
			(e-int @7-9-7-10 (value "2")))
		(e-binop @8-5-8-11 (op "gt")
			(e-int @8-5-8-6 (value "4"))
			(e-int @8-9-8-10 (value "2")))
		(e-binop @9-5-9-12 (op "le")
			(e-int @9-5-9-6 (value "4"))
			(e-int @9-10-9-11 (value "2")))
		(e-binop @10-5-10-12 (op "ge")
			(e-int @10-5-10-6 (value "4"))
			(e-int @10-10-10-11 (value "2")))
		(e-binop @11-5-11-12 (op "eq")
			(e-int @11-5-11-6 (value "4"))
			(e-int @11-10-11-11 (value "2")))
		(e-binop @12-5-12-12 (op "ne")
			(e-int @12-5-12-6 (value "4"))
			(e-int @12-10-12-11 (value "2")))
		(e-binop @13-5-13-12 (op "div_trunc")
			(e-int @13-5-13-6 (value "4"))
			(e-int @13-10-13-11 (value "2")))
		(e-binop @14-5-14-19 (op "and")
			(e-tag @14-5-14-9 (ext-var 0) (name "True") (args "TODO"))
			(e-tag @14-14-14-18 (ext-var 0) (name "True") (args "TODO")))
		(e-binop @15-5-15-18 (op "or")
			(e-tag @15-5-15-9 (ext-var 0) (name "True") (args "TODO"))
			(e-tag @15-13-15-17 (ext-var 0) (name "True") (args "TODO")))
		(e-binop @16-5-16-15 (op "null_coalesce")
			(e-tag @16-5-16-9 (ext-var 0) (name "None") (args "TODO"))
			(e-int @16-13-16-14 (value "0")))))
~~~
# TYPES
~~~clojure
(expr (id 122) (type "(*, *, *, *, *, *, *, *, *, *, *, *, *, *, *)"))
~~~
