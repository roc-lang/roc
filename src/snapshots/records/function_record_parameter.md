# META
~~~ini
description=Function with record parameter destructuring and string interpolation
type=expr
~~~
# SOURCE
~~~roc
|{ name, age }| "Hello ${name}, you are ${age.to_str()} years old"
~~~
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:4-1:8),Comma(1:8-1:9),LowerIdent(1:10-1:13),CloseCurly(1:14-1:15),OpBar(1:15-1:16),StringStart(1:17-1:18),StringPart(1:18-1:24),OpenStringInterpolation(1:24-1:26),LowerIdent(1:26-1:30),CloseStringInterpolation(1:30-1:31),StringPart(1:31-1:41),OpenStringInterpolation(1:41-1:43),LowerIdent(1:43-1:46),NoSpaceDotLowerIdent(1:46-1:53),NoSpaceOpenRound(1:53-1:54),CloseRound(1:54-1:55),CloseStringInterpolation(1:55-1:56),StringPart(1:56-1:66),StringEnd(1:66-1:67),EndOfFile(1:67-1:67),
~~~
# PARSE
~~~clojure
(e-lambda @1.1-1.67
	(args
		(p-record @1.2-1.15
			(field @1.4-1.9 (name "name") (rest false))
			(field @1.10-1.15 (name "age") (rest false))))
	(e-string @1.17-1.67
		(e-string-part @1.18-1.24 (raw "Hello "))
		(e-ident @1.26-1.30 (qaul "") (raw "name"))
		(e-string-part @1.31-1.41 (raw ", you are "))
		(e-field-access @1.43-1.56
			(e-ident @1.43-1.46 (qaul "") (raw "age"))
			(e-apply @1.46-1.55
				(e-ident @1.46-1.53 (qaul "") (raw ".to_str"))))
		(e-string-part @1.56-1.66 (raw " years old"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda @1.1-1.67
	(args
		(p-record-destructure @1.2-1.15
			(destructs
				(record-destruct @1.4-1.9 (label "name") (ident "name")
					(required))
				(record-destruct @1.10-1.15 (label "age") (ident "age")
					(required)))))
	(e-string @1.17-1.67
		(e-literal @1.18-1.24 (string "Hello "))
		(e-lookup-local @1.26-1.30
			(pattern @1.4-1.9))
		(e-literal @1.31-1.41 (string ", you are "))
		(e-dot-access @1.43-1.56 (field "to_str")
			(receiver
				(e-lookup-local @1.43-1.46
					(pattern @1.10-1.15)))
			(args))
		(e-literal @1.56-1.66 (string " years old"))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.67 (type "a -> Str"))
~~~
