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
    Try(Ok(Some(True))),
]
~~~
# EXPECTED
TYPE DOES NOT HAVE METHODS - tag_applications_simple.md:2:10:2:12
# PROBLEMS
**TYPE DOES NOT HAVE METHODS**
You're calling the method `from_numeral` on a type that doesn't support methods:
**tag_applications_simple.md:2:10:2:12:**
```roc
    Some(42),
```
         ^^

This type doesn't support methods:
    _[Ok([Just(_a)]_others)]_others2 where [[Ok([Just(b)]c)]d.from_numeral : [Ok([Just(b)]c)]d]_



# TOKENS
~~~zig
OpenSquare,
UpperIdent,NoSpaceOpenRound,Int,CloseRound,Comma,
UpperIdent,Comma,
UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,
UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,
UpperIdent,NoSpaceOpenRound,Int,CloseRound,Comma,
UpperIdent,Comma,
UpperIdent,NoSpaceOpenRound,Int,CloseRound,Comma,
UpperIdent,NoSpaceOpenRound,Int,CloseRound,Comma,
UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,CloseRound,Comma,
UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseRound,CloseRound,Comma,
CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-list
	(e-apply
		(e-tag (raw "Some"))
		(e-int (raw "42")))
	(e-tag (raw "None"))
	(e-apply
		(e-tag (raw "Ok"))
		(e-string
			(e-string-part (raw "hello"))))
	(e-apply
		(e-tag (raw "Err"))
		(e-string
			(e-string-part (raw "oops"))))
	(e-apply
		(e-tag (raw "Just"))
		(e-int (raw "100")))
	(e-tag (raw "Nothing"))
	(e-apply
		(e-tag (raw "Left"))
		(e-int (raw "1")))
	(e-apply
		(e-tag (raw "Right"))
		(e-int (raw "2")))
	(e-apply
		(e-tag (raw "Some"))
		(e-apply
			(e-tag (raw "Ok"))
			(e-apply
				(e-tag (raw "Just"))
				(e-int (raw "42")))))
	(e-apply
		(e-tag (raw "Try"))
		(e-apply
			(e-tag (raw "Ok"))
			(e-apply
				(e-tag (raw "Some"))
				(e-tag (raw "True"))))))
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
	Try(Ok(Some(True))),
]
~~~
# CANONICALIZE
~~~clojure
(e-list
	(elems
		(e-tag (name "Some")
			(args
				(e-num (value "42"))))
		(e-tag (name "None"))
		(e-tag (name "Ok")
			(args
				(e-string
					(e-literal (string "hello")))))
		(e-tag (name "Err")
			(args
				(e-string
					(e-literal (string "oops")))))
		(e-tag (name "Just")
			(args
				(e-num (value "100"))))
		(e-tag (name "Nothing"))
		(e-tag (name "Left")
			(args
				(e-num (value "1"))))
		(e-tag (name "Right")
			(args
				(e-num (value "2"))))
		(e-tag (name "Some")
			(args
				(e-tag (name "Ok")
					(args
						(e-tag (name "Just")
							(args
								(e-num (value "42"))))))))
		(e-tag (name "Try")
			(args
				(e-tag (name "Ok")
					(args
						(e-tag (name "Some")
							(args
								(e-tag (name "True"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "List([Err(Str), Just(_a), Left(_b), None, Nothing, Ok(Str), Right(_c), Some([Ok([Just(_d)]_others)]_others2), Try([Ok([Some([True]_others3)]_others4)]_others5)]_others6) where [_e.from_numeral : _arg -> _ret, _f.from_numeral : _arg2 -> _ret2, _g.from_numeral : _arg3 -> _ret3, _h.from_numeral : _arg4 -> _ret4]"))
~~~
