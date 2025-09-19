# META
~~~ini
description=Tag applications vs function calls
type=expr
~~~
# SOURCE
~~~roc
{
    someTag: Some(42),
    noneTag: None,
    okTag: Ok("hello"),
    errTag: Err("oops"),
    addOne: |x| x + 1,
    result: addOne(5),
    nested: Some(Ok(Just(42))),
    tagList: [Some(1), Some(2), None, Some(3)],
}
~~~
# EXPECTED
UNDEFINED VARIABLE - tag_vs_function_calls.md:7:13:7:19
# PROBLEMS
**UNDEFINED VARIABLE**
Nothing is named `addOne` in this scope.
Is there an `import` or `exposing` missing up-top?

**tag_vs_function_calls.md:7:13:7:19:**
```roc
    result: addOne(5),
```
            ^^^^^^


# TOKENS
~~~zig
OpenCurly(1:1-1:2),
LowerIdent(2:5-2:12),OpColon(2:12-2:13),UpperIdent(2:14-2:18),NoSpaceOpenRound(2:18-2:19),Int(2:19-2:21),CloseRound(2:21-2:22),Comma(2:22-2:23),
LowerIdent(3:5-3:12),OpColon(3:12-3:13),UpperIdent(3:14-3:18),Comma(3:18-3:19),
LowerIdent(4:5-4:10),OpColon(4:10-4:11),UpperIdent(4:12-4:14),NoSpaceOpenRound(4:14-4:15),StringStart(4:15-4:16),StringPart(4:16-4:21),StringEnd(4:21-4:22),CloseRound(4:22-4:23),Comma(4:23-4:24),
LowerIdent(5:5-5:11),OpColon(5:11-5:12),UpperIdent(5:13-5:16),NoSpaceOpenRound(5:16-5:17),StringStart(5:17-5:18),StringPart(5:18-5:22),StringEnd(5:22-5:23),CloseRound(5:23-5:24),Comma(5:24-5:25),
LowerIdent(6:5-6:11),OpColon(6:11-6:12),OpBar(6:13-6:14),LowerIdent(6:14-6:15),OpBar(6:15-6:16),LowerIdent(6:17-6:18),OpPlus(6:19-6:20),Int(6:21-6:22),Comma(6:22-6:23),
LowerIdent(7:5-7:11),OpColon(7:11-7:12),LowerIdent(7:13-7:19),NoSpaceOpenRound(7:19-7:20),Int(7:20-7:21),CloseRound(7:21-7:22),Comma(7:22-7:23),
LowerIdent(8:5-8:11),OpColon(8:11-8:12),UpperIdent(8:13-8:17),NoSpaceOpenRound(8:17-8:18),UpperIdent(8:18-8:20),NoSpaceOpenRound(8:20-8:21),UpperIdent(8:21-8:25),NoSpaceOpenRound(8:25-8:26),Int(8:26-8:28),CloseRound(8:28-8:29),CloseRound(8:29-8:30),CloseRound(8:30-8:31),Comma(8:31-8:32),
LowerIdent(9:5-9:12),OpColon(9:12-9:13),OpenSquare(9:14-9:15),UpperIdent(9:15-9:19),NoSpaceOpenRound(9:19-9:20),Int(9:20-9:21),CloseRound(9:21-9:22),Comma(9:22-9:23),UpperIdent(9:24-9:28),NoSpaceOpenRound(9:28-9:29),Int(9:29-9:30),CloseRound(9:30-9:31),Comma(9:31-9:32),UpperIdent(9:33-9:37),Comma(9:37-9:38),UpperIdent(9:39-9:43),NoSpaceOpenRound(9:43-9:44),Int(9:44-9:45),CloseRound(9:45-9:46),CloseSquare(9:46-9:47),Comma(9:47-9:48),
CloseCurly(10:1-10:2),
EndOfFile(11:1-11:1),
~~~
# PARSE
~~~clojure
(e-record @1.1-10.2
	(field (field "someTag")
		(e-apply @2.14-2.22
			(e-tag @2.14-2.18 (raw "Some"))
			(e-int @2.19-2.21 (raw "42"))))
	(field (field "noneTag")
		(e-tag @3.14-3.18 (raw "None")))
	(field (field "okTag")
		(e-apply @4.12-4.23
			(e-tag @4.12-4.14 (raw "Ok"))
			(e-string @4.15-4.22
				(e-string-part @4.16-4.21 (raw "hello")))))
	(field (field "errTag")
		(e-apply @5.13-5.24
			(e-tag @5.13-5.16 (raw "Err"))
			(e-string @5.17-5.23
				(e-string-part @5.18-5.22 (raw "oops")))))
	(field (field "addOne")
		(e-lambda @6.13-6.22
			(args
				(p-ident @6.14-6.15 (raw "x")))
			(e-binop @6.17-6.22 (op "+")
				(e-ident @6.17-6.18 (raw "x"))
				(e-int @6.21-6.22 (raw "1")))))
	(field (field "result")
		(e-apply @7.13-7.22
			(e-ident @7.13-7.19 (raw "addOne"))
			(e-int @7.20-7.21 (raw "5"))))
	(field (field "nested")
		(e-apply @8.13-8.31
			(e-tag @8.13-8.17 (raw "Some"))
			(e-apply @8.18-8.30
				(e-tag @8.18-8.20 (raw "Ok"))
				(e-apply @8.21-8.29
					(e-tag @8.21-8.25 (raw "Just"))
					(e-int @8.26-8.28 (raw "42"))))))
	(field (field "tagList")
		(e-list @9.14-9.47
			(e-apply @9.15-9.22
				(e-tag @9.15-9.19 (raw "Some"))
				(e-int @9.20-9.21 (raw "1")))
			(e-apply @9.24-9.31
				(e-tag @9.24-9.28 (raw "Some"))
				(e-int @9.29-9.30 (raw "2")))
			(e-tag @9.33-9.37 (raw "None"))
			(e-apply @9.39-9.46
				(e-tag @9.39-9.43 (raw "Some"))
				(e-int @9.44-9.45 (raw "3"))))))
~~~
# FORMATTED
~~~roc
{
	someTag: Some(42),
	noneTag: None,
	okTag: Ok("hello"),
	errTag: Err("oops"),
	addOne: |x| x + 1,
	result: addOne(5),
	nested: Some(Ok(Just(42))),
	tagList: [Some(1), Some(2), None, Some(3)],
}
~~~
# CANONICALIZE
~~~clojure
(e-record @1.1-10.2
	(fields
		(field (name "someTag")
			(e-tag @2.14-2.22 (name "Some")
				(args
					(e-num @2.19-2.21 (value "42")))))
		(field (name "noneTag")
			(e-tag @3.14-3.18 (name "None")))
		(field (name "okTag")
			(e-nominal @4.12-4.23 (nominal "Result")
				(e-tag @4.12-4.23 (name "Ok")
					(args
						(e-string @4.15-4.22
							(e-literal @4.16-4.21 (string "hello")))))))
		(field (name "errTag")
			(e-nominal @5.13-5.24 (nominal "Result")
				(e-tag @5.13-5.24 (name "Err")
					(args
						(e-string @5.17-5.23
							(e-literal @5.18-5.22 (string "oops")))))))
		(field (name "addOne")
			(e-lambda @6.13-6.22
				(args
					(p-assign @6.14-6.15 (ident "x")))
				(e-binop @6.17-6.22 (op "add")
					(e-lookup-local @6.17-6.18
						(p-assign @6.14-6.15 (ident "x")))
					(e-num @6.21-6.22 (value "1")))))
		(field (name "result")
			(e-call @7.13-7.22
				(e-num @7.20-7.21 (value "5"))))
		(field (name "nested")
			(e-tag @8.13-8.31 (name "Some")
				(args
					(e-nominal @8.18-8.30 (nominal "Result")
						(e-tag @8.18-8.30 (name "Ok")
							(args
								(e-tag @8.21-8.29 (name "Just")
									(args
										(e-num @8.26-8.28 (value "42"))))))))))
		(field (name "tagList")
			(e-list @9.14-9.47
				(elems
					(e-tag @9.15-9.22 (name "Some")
						(args
							(e-num @9.20-9.21 (value "1"))))
					(e-tag @9.24-9.31 (name "Some")
						(args
							(e-num @9.29-9.30 (value "2"))))
					(e-tag @9.33-9.37 (name "None"))
					(e-tag @9.39-9.46 (name "Some")
						(args
							(e-num @9.44-9.45 (value "3")))))))))
~~~
# TYPES
~~~clojure
(expr @1.1-10.2 (type "{ someTag: [Some(Num(_size))]_others, noneTag: [None]_others2, okTag: Error, errTag: Error, addOne: Num(_size2) -> Num(_size3), result: _field, nested: [Some(Error)]_others3, tagList: List([Some(Num(_size4))][None]_others4) }"))
~~~
