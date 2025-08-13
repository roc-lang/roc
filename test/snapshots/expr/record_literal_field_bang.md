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
OpenCurly(1:1-1:2),
LowerIdent(2:5-2:11),OpColon(2:11-2:12),Int(2:13-2:15),Comma(2:15-2:16),
LowerIdent(3:5-3:20),OpColon(3:20-3:21),OpBar(3:22-3:23),OpenCurly(3:23-3:24),CloseCurly(3:24-3:25),OpBar(3:25-3:26),TripleDot(3:27-3:30),Comma(3:30-3:31),
CloseCurly(4:1-4:2),EndOfFile(4:2-4:2),
~~~
# PARSE
~~~clojure
(e-record @1.1-4.2
	(field (field "answer")
		(e-int @2.13-2.15 (raw "42")))
	(field (field "launchTheNukes!")
		(e-lambda @3.22-3.30
			(args
				(p-record @3.23-3.25))
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
(e-record @1.1-4.2
	(fields
		(field (name "answer")
			(e-int @2.13-2.15 (value "42")))
		(field (name "launchTheNukes!")
			(e-lambda @3.22-3.30
				(args
					(p-record-destructure @3.23-3.25
						(destructs)))
				(e-not-implemented @1.1-1.1)))))
~~~
# TYPES
~~~clojure
(expr @1.1-4.2 (type "{ answer: Num(_size), launchTheNukes!: _arg -> _ret }"))
~~~
