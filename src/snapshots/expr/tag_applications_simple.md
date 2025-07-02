# META
~~~ini
description=Various tag applications
type=expr
~~~
# SOURCE
~~~roc
[
    Some(42),
    None,
    Ok("hello"),
    Err("oops"),
    Just(100),
    Nothing,
    Left(1),
    Right(2),
    Some(Ok(Just(42))),
    Result(Ok(Some(True))),
]
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenSquare(1:1-1:2),Newline(1:1-1:1),
UpperIdent(2:5-2:9),NoSpaceOpenRound(2:9-2:10),Int(2:10-2:12),CloseRound(2:12-2:13),Comma(2:13-2:14),Newline(1:1-1:1),
UpperIdent(3:5-3:9),Comma(3:9-3:10),Newline(1:1-1:1),
UpperIdent(4:5-4:7),NoSpaceOpenRound(4:7-4:8),StringStart(4:8-4:9),StringPart(4:9-4:14),StringEnd(4:14-4:15),CloseRound(4:15-4:16),Comma(4:16-4:17),Newline(1:1-1:1),
UpperIdent(5:5-5:8),NoSpaceOpenRound(5:8-5:9),StringStart(5:9-5:10),StringPart(5:10-5:14),StringEnd(5:14-5:15),CloseRound(5:15-5:16),Comma(5:16-5:17),Newline(1:1-1:1),
UpperIdent(6:5-6:9),NoSpaceOpenRound(6:9-6:10),Int(6:10-6:13),CloseRound(6:13-6:14),Comma(6:14-6:15),Newline(1:1-1:1),
UpperIdent(7:5-7:12),Comma(7:12-7:13),Newline(1:1-1:1),
UpperIdent(8:5-8:9),NoSpaceOpenRound(8:9-8:10),Int(8:10-8:11),CloseRound(8:11-8:12),Comma(8:12-8:13),Newline(1:1-1:1),
UpperIdent(9:5-9:10),NoSpaceOpenRound(9:10-9:11),Int(9:11-9:12),CloseRound(9:12-9:13),Comma(9:13-9:14),Newline(1:1-1:1),
UpperIdent(10:5-10:9),NoSpaceOpenRound(10:9-10:10),UpperIdent(10:10-10:12),NoSpaceOpenRound(10:12-10:13),UpperIdent(10:13-10:17),NoSpaceOpenRound(10:17-10:18),Int(10:18-10:20),CloseRound(10:20-10:21),CloseRound(10:21-10:22),CloseRound(10:22-10:23),Comma(10:23-10:24),Newline(1:1-1:1),
UpperIdent(11:5-11:11),NoSpaceOpenRound(11:11-11:12),UpperIdent(11:12-11:14),NoSpaceOpenRound(11:14-11:15),UpperIdent(11:15-11:19),NoSpaceOpenRound(11:19-11:20),UpperIdent(11:20-11:24),CloseRound(11:24-11:25),CloseRound(11:25-11:26),CloseRound(11:26-11:27),Comma(11:27-11:28),Newline(1:1-1:1),
CloseSquare(12:1-12:2),EndOfFile(12:2-12:2),
~~~
# PARSE
~~~clojure
(e-list @1.1-12.2
	(e-apply @2.5-2.13
		(e-tag @2.5-2.9 (raw "Some"))
		(e-int @2.10-2.12 (raw "42")))
	(e-tag @3.5-3.9 (raw "None"))
	(e-apply @4.5-4.16
		(e-tag @4.5-4.7 (raw "Ok"))
		(e-string @4.8-4.15
			(e-string-part @4.9-4.14 (raw "hello"))))
	(e-apply @5.5-5.16
		(e-tag @5.5-5.8 (raw "Err"))
		(e-string @5.9-5.15
			(e-string-part @5.10-5.14 (raw "oops"))))
	(e-apply @6.5-6.14
		(e-tag @6.5-6.9 (raw "Just"))
		(e-int @6.10-6.13 (raw "100")))
	(e-tag @7.5-7.12 (raw "Nothing"))
	(e-apply @8.5-8.12
		(e-tag @8.5-8.9 (raw "Left"))
		(e-int @8.10-8.11 (raw "1")))
	(e-apply @9.5-9.13
		(e-tag @9.5-9.10 (raw "Right"))
		(e-int @9.11-9.12 (raw "2")))
	(e-apply @10.5-10.23
		(e-tag @10.5-10.9 (raw "Some"))
		(e-apply @10.10-10.22
			(e-tag @10.10-10.12 (raw "Ok"))
			(e-apply @10.13-10.21
				(e-tag @10.13-10.17 (raw "Just"))
				(e-int @10.18-10.20 (raw "42")))))
	(e-apply @11.5-11.27
		(e-tag @11.5-11.11 (raw "Result"))
		(e-apply @11.12-11.26
			(e-tag @11.12-11.14 (raw "Ok"))
			(e-apply @11.15-11.25
				(e-tag @11.15-11.19 (raw "Some"))
				(e-tag @11.20-11.24 (raw "True"))))))
~~~
# FORMATTED
~~~roc
[
	Some(42),
	None,
	Ok("hello"),
	Err("oops"),
	Just(100),
	Nothing,
	Left(1),
	Right(2),
	Some(Ok(Just(42))),
	Result(Ok(Some(True))),
]
~~~
# CANONICALIZE
~~~clojure
(e-list @1.1-12.2
	(elems
		(e-tag @2.5-2.13 (name "Some") (args "TODO"))
		(e-tag @3.5-3.9 (name "None") (args "TODO"))
		(e-tag @4.5-4.16 (name "Ok") (args "TODO"))
		(e-tag @5.5-5.16 (name "Err") (args "TODO"))
		(e-tag @6.5-6.14 (name "Just") (args "TODO"))
		(e-tag @7.5-7.12 (name "Nothing") (args "TODO"))
		(e-tag @8.5-8.12 (name "Left") (args "TODO"))
		(e-tag @9.5-9.13 (name "Right") (args "TODO"))
		(e-tag @10.5-10.23 (name "Some") (args "TODO"))
		(e-tag @11.5-11.27 (name "Result") (args "TODO"))))
~~~
# TYPES
~~~clojure
(expr @1.1-12.2 (type "List([None, Nothing]*)"))
~~~
