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
OpenCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,Int,CloseRound,Comma,
LowerIdent,OpColon,UpperIdent,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,Comma,
LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,Comma,
LowerIdent,OpColon,LowerIdent,NoSpaceOpenRound,Int,CloseRound,Comma,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,CloseRound,Comma,
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,Int,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,Int,CloseRound,Comma,UpperIdent,Comma,UpperIdent,NoSpaceOpenRound,Int,CloseRound,CloseSquare,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "someTag")
		(e-apply
			(e-tag (raw "Some"))
			(e-int (raw "42"))))
	(field (field "noneTag")
		(e-tag (raw "None")))
	(field (field "okTag")
		(e-apply
			(e-tag (raw "Ok"))
			(e-string
				(e-string-part (raw "hello")))))
	(field (field "errTag")
		(e-apply
			(e-tag (raw "Err"))
			(e-string
				(e-string-part (raw "oops")))))
	(field (field "addOne")
		(e-lambda
			(args
				(p-ident (raw "x")))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-int (raw "1")))))
	(field (field "result")
		(e-apply
			(e-ident (raw "addOne"))
			(e-int (raw "5"))))
	(field (field "nested")
		(e-apply
			(e-tag (raw "Some"))
			(e-apply
				(e-tag (raw "Ok"))
				(e-apply
					(e-tag (raw "Just"))
					(e-int (raw "42"))))))
	(field (field "tagList")
		(e-list
			(e-apply
				(e-tag (raw "Some"))
				(e-int (raw "1")))
			(e-apply
				(e-tag (raw "Some"))
				(e-int (raw "2")))
			(e-tag (raw "None"))
			(e-apply
				(e-tag (raw "Some"))
				(e-int (raw "3"))))))
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
(e-record
	(fields
		(field (name "someTag")
			(e-tag (name "Some")
				(args
					(e-num (value "42")))))
		(field (name "noneTag")
			(e-tag (name "None")))
		(field (name "okTag")
			(e-tag (name "Ok")
				(args
					(e-string
						(e-literal (string "hello"))))))
		(field (name "errTag")
			(e-tag (name "Err")
				(args
					(e-string
						(e-literal (string "oops"))))))
		(field (name "addOne")
			(e-lambda
				(args
					(p-assign (ident "x")))
				(e-binop (op "add")
					(e-lookup-local
						(p-assign (ident "x")))
					(e-num (value "1")))))
		(field (name "result")
			(e-call
				(e-runtime-error (tag "ident_not_in_scope"))
				(e-num (value "5"))))
		(field (name "nested")
			(e-tag (name "Some")
				(args
					(e-tag (name "Ok")
						(args
							(e-tag (name "Just")
								(args
									(e-num (value "42")))))))))
		(field (name "tagList")
			(e-list
				(elems
					(e-tag (name "Some")
						(args
							(e-num (value "1"))))
					(e-tag (name "Some")
						(args
							(e-num (value "2"))))
					(e-tag (name "None"))
					(e-tag (name "Some")
						(args
							(e-num (value "3")))))))))
~~~
# TYPES
~~~clojure
(expr (type "{ addOne: Num(_size) -> Num(_size2), errTag: [Err(Str)]_others, nested: [Some([Ok([Just(Num(_size3))]_others2)]_others3)]_others4, noneTag: [None]_others5, okTag: [Ok(Str)]_others6, result: Error, someTag: [Some(Num(_size4))]_others7, tagList: List([Some(Num(_size5))][None]_others8) }"))
~~~
