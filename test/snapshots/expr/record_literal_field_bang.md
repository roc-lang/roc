# META
~~~ini
description=record_literal_field_bang
type=expr
~~~
# SOURCE
~~~roc
{
    answer: 42,
    launchTheNukes!: |{}| ...,
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenCurly,
LowerIdent,OpColon,Int,Comma,
LowerIdent,OpColon,OpBar,OpenCurly,CloseCurly,OpBar,TripleDot,Comma,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-record
	(field (field "answer")
		(e-int (raw "42")))
	(field (field "launchTheNukes!")
		(e-lambda
			(args
				(p-record))
			(e-ellipsis))))
~~~
# FORMATTED
~~~roc
{
	answer: 42,
	launchTheNukes!: |{}| ...,
}
~~~
# CANONICALIZE
~~~clojure
(e-record
	(fields
		(field (name "answer")
			(e-num (value "42")))
		(field (name "launchTheNukes!")
			(e-lambda
				(args
					(p-record-destructure
						(destructs)))
				(e-not-implemented)))))
~~~
# TYPES
~~~clojure
(expr (type "{ answer: Num(_size), launchTheNukes!: {} -> _ret }"))
~~~
