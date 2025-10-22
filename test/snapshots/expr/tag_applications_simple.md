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
# EXPECTED
INCOMPATIBLE LIST ELEMENTS - tag_applications_simple.md:9:5:9:5
# PROBLEMS
**INCOMPATIBLE LIST ELEMENTS**
The eighth and ninth elements in this list have incompatible types:
**tag_applications_simple.md:9:5:**
```roc
    Right(2),
    Some(Ok(Just(42))),
```
    ^^^^^^^^
    ^^^^^^^^^^^^^^^^^^

The eighth element has this type:
    _[Err(Error), Just(Num(_size)), Left(Num(_size2)), None, Nothing, Ok(Error), Some(Num(_size3)), Right(Num(_size4))]_others_

However, the ninth element has this type:
    _[Some([Ok([Just(Num(_size))]_others)]_others2)][Err(Error), Just(Num(_size2)), Left(Num(_size3)), None, Nothing, Ok(Error), Right(Num(_size4))]_others3_

All elements in a list must have compatible types.

Note: You can wrap each element in a tag to make them compatible.
To learn about tags, see <https://www.roc-lang.org/tutorial#tags>

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
		(e-tag (raw "Result"))
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
	Result(Ok(Some(True))),
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
		(e-tag (name "Result")
			(args
				(e-tag (name "Ok")
					(args
						(e-tag (name "Some")
							(args
								(e-tag (name "True"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "List(Error)"))
~~~
