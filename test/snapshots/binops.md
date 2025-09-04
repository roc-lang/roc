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
    Bool.True and Bool.False,
    Bool.False or Bool.True,
    None ?? 0,
)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound(1:1-1:2),
Int(2:5-2:6),OpPlus(2:7-2:8),Int(2:9-2:10),Comma(2:10-2:11),
Int(3:5-3:6),OpBinaryMinus(3:7-3:8),Int(3:9-3:10),Comma(3:10-3:11),
Int(4:5-4:6),OpStar(4:7-4:8),Int(4:9-4:10),Comma(4:10-4:11),
Int(5:5-5:6),OpSlash(5:7-5:8),Int(5:9-5:10),Comma(5:10-5:11),
Int(6:5-6:6),OpPercent(6:7-6:8),Int(6:9-6:10),Comma(6:10-6:11),
Int(7:5-7:6),OpLessThan(7:7-7:8),Int(7:9-7:10),Comma(7:10-7:11),
Int(8:5-8:6),OpGreaterThan(8:7-8:8),Int(8:9-8:10),Comma(8:10-8:11),
Int(9:5-9:6),OpLessThanOrEq(9:7-9:9),Int(9:10-9:11),Comma(9:11-9:12),
Int(10:5-10:6),OpGreaterThanOrEq(10:7-10:9),Int(10:10-10:11),Comma(10:11-10:12),
Int(11:5-11:6),OpEquals(11:7-11:9),Int(11:10-11:11),Comma(11:11-11:12),
Int(12:5-12:6),OpNotEquals(12:7-12:9),Int(12:10-12:11),Comma(12:11-12:12),
Int(13:5-13:6),OpDoubleSlash(13:7-13:9),Int(13:10-13:11),Comma(13:11-13:12),
UpperIdent(14:5-14:9),NoSpaceDotUpperIdent(14:9-14:14),OpAnd(14:15-14:18),UpperIdent(14:19-14:23),NoSpaceDotUpperIdent(14:23-14:29),Comma(14:29-14:30),
UpperIdent(15:5-15:9),NoSpaceDotUpperIdent(15:9-15:15),OpOr(15:16-15:18),UpperIdent(15:19-15:23),NoSpaceDotUpperIdent(15:23-15:28),Comma(15:28-15:29),
UpperIdent(16:5-16:9),OpDoubleQuestion(16:10-16:12),Int(16:13-16:14),Comma(16:14-16:15),
CloseRound(17:1-17:2),
EndOfFile(18:1-18:1),
~~~
# PARSE
~~~clojure
(e-tuple @1.1-17.2
	(e-binop @2.5-2.10 (op "+")
		(e-int @2.5-2.6 (raw "4"))
		(e-int @2.9-2.10 (raw "2")))
	(e-binop @3.5-3.10 (op "-")
		(e-int @3.5-3.6 (raw "4"))
		(e-int @3.9-3.10 (raw "2")))
	(e-binop @4.5-4.10 (op "*")
		(e-int @4.5-4.6 (raw "4"))
		(e-int @4.9-4.10 (raw "2")))
	(e-binop @5.5-5.10 (op "/")
		(e-int @5.5-5.6 (raw "4"))
		(e-int @5.9-5.10 (raw "2")))
	(e-binop @6.5-6.10 (op "%")
		(e-int @6.5-6.6 (raw "4"))
		(e-int @6.9-6.10 (raw "2")))
	(e-binop @7.5-7.10 (op "<")
		(e-int @7.5-7.6 (raw "4"))
		(e-int @7.9-7.10 (raw "2")))
	(e-binop @8.5-8.10 (op ">")
		(e-int @8.5-8.6 (raw "4"))
		(e-int @8.9-8.10 (raw "2")))
	(e-binop @9.5-9.11 (op "<=")
		(e-int @9.5-9.6 (raw "4"))
		(e-int @9.10-9.11 (raw "2")))
	(e-binop @10.5-10.11 (op ">=")
		(e-int @10.5-10.6 (raw "4"))
		(e-int @10.10-10.11 (raw "2")))
	(e-binop @11.5-11.11 (op "==")
		(e-int @11.5-11.6 (raw "4"))
		(e-int @11.10-11.11 (raw "2")))
	(e-binop @12.5-12.11 (op "!=")
		(e-int @12.5-12.6 (raw "4"))
		(e-int @12.10-12.11 (raw "2")))
	(e-binop @13.5-13.11 (op "//")
		(e-int @13.5-13.6 (raw "4"))
		(e-int @13.10-13.11 (raw "2")))
	(e-binop @14.5-14.29 (op "and")
		(e-tag @14.5-14.14 (raw "Bool.True"))
		(e-tag @14.19-14.29 (raw "Bool.False")))
	(e-binop @15.5-15.28 (op "or")
		(e-tag @15.5-15.15 (raw "Bool.False"))
		(e-tag @15.19-15.28 (raw "Bool.True")))
	(e-binop @16.5-16.14 (op "??")
		(e-tag @16.5-16.9 (raw "None"))
		(e-int @16.13-16.14 (raw "0"))))
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
	Bool.True and Bool.False,
	Bool.False or Bool.True,
	None ?? 0,
)
~~~
# CANONICALIZE
~~~clojure
(e-tuple @1.1-17.2
	(elems
		(e-binop @2.5-2.10 (op "add")
			(e-int @2.5-2.6 (value "4"))
			(e-int @2.9-2.10 (value "2")))
		(e-binop @3.5-3.10 (op "sub")
			(e-int @3.5-3.6 (value "4"))
			(e-int @3.9-3.10 (value "2")))
		(e-binop @4.5-4.10 (op "mul")
			(e-int @4.5-4.6 (value "4"))
			(e-int @4.9-4.10 (value "2")))
		(e-binop @5.5-5.10 (op "div")
			(e-int @5.5-5.6 (value "4"))
			(e-int @5.9-5.10 (value "2")))
		(e-binop @6.5-6.10 (op "rem")
			(e-int @6.5-6.6 (value "4"))
			(e-int @6.9-6.10 (value "2")))
		(e-binop @7.5-7.10 (op "lt")
			(e-int @7.5-7.6 (value "4"))
			(e-int @7.9-7.10 (value "2")))
		(e-binop @8.5-8.10 (op "gt")
			(e-int @8.5-8.6 (value "4"))
			(e-int @8.9-8.10 (value "2")))
		(e-binop @9.5-9.11 (op "le")
			(e-int @9.5-9.6 (value "4"))
			(e-int @9.10-9.11 (value "2")))
		(e-binop @10.5-10.11 (op "ge")
			(e-int @10.5-10.6 (value "4"))
			(e-int @10.10-10.11 (value "2")))
		(e-binop @11.5-11.11 (op "eq")
			(e-int @11.5-11.6 (value "4"))
			(e-int @11.10-11.11 (value "2")))
		(e-binop @12.5-12.11 (op "ne")
			(e-int @12.5-12.6 (value "4"))
			(e-int @12.10-12.11 (value "2")))
		(e-binop @13.5-13.11 (op "div_trunc")
			(e-int @13.5-13.6 (value "4"))
			(e-int @13.10-13.11 (value "2")))
		(e-binop @14.5-14.29 (op "and")
			(e-nominal @14.5-14.14 (nominal "Bool")
				(e-tag @14.5-14.14 (name "True")))
			(e-nominal @14.19-14.29 (nominal "Bool")
				(e-tag @14.19-14.29 (name "False"))))
		(e-binop @15.5-15.28 (op "or")
			(e-nominal @15.5-15.15 (nominal "Bool")
				(e-tag @15.5-15.15 (name "False")))
			(e-nominal @15.19-15.28 (nominal "Bool")
				(e-tag @15.19-15.28 (name "True"))))
		(e-binop @16.5-16.14 (op "null_coalesce")
			(e-tag @16.5-16.9 (name "None"))
			(e-int @16.13-16.14 (value "0")))))
~~~
# TYPES
~~~clojure
(expr @1.1-17.2 (type "(Num(_size), Num(_size2), Num(_size3), Num(_size4), Num(_size5), Bool, Bool, Bool, Bool, Bool, Bool, Num(_size6), _field, _field2, _field3)"))
~~~
