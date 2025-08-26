# META
~~~ini
description=Function with record parameter and rest pattern
type=expr
~~~
# SOURCE
~~~roc
|{ first_name, ..rest }| "Hello ${first_name} ${rest.last_name}"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar(1:1-1:2),OpenCurly(1:2-1:3),LowerIdent(1:4-1:14),Comma(1:14-1:15),DoubleDot(1:16-1:18),LowerIdent(1:18-1:22),CloseCurly(1:23-1:24),OpBar(1:24-1:25),StringStart(1:26-1:27),StringPart(1:27-1:33),OpenStringInterpolation(1:33-1:35),LowerIdent(1:35-1:45),CloseStringInterpolation(1:45-1:46),StringPart(1:46-1:47),OpenStringInterpolation(1:47-1:49),LowerIdent(1:49-1:53),NoSpaceDotLowerIdent(1:53-1:63),CloseStringInterpolation(1:63-1:64),StringPart(1:64-1:64),StringEnd(1:64-1:65),
EndOfFile(2:1-2:1),
~~~
# PARSE
~~~clojure
(e-lambda @1.1-1.65
	(args
		(p-record @1.2-1.24
			(field @1.4-1.14 (name "first_name") (rest false))
			(field @1.16-1.22 (name "rest") (rest true))))
	(e-string @1.26-1.65
		(e-string-part @1.27-1.33 (raw "Hello "))
		(e-ident @1.35-1.45 (raw "first_name"))
		(e-string-part @1.46-1.47 (raw " "))
		(e-field-access @1.49-1.63
			(e-ident @1.49-1.53 (raw "rest"))
			(e-ident @1.53-1.63 (raw "last_name")))
		(e-string-part @1.64-1.64 (raw ""))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda @1.1-1.65
	(args
		(p-record-destructure @1.2-1.24
			(destructs
				(record-destruct @1.4-1.14 (label "first_name") (ident "first_name")
					(required
						(p-assign @1.4-1.14 (ident "first_name"))))
				(record-destruct @1.16-1.22 (label "rest") (ident "rest")
					(required
						(p-assign @1.16-1.22 (ident "rest")))))))
	(e-string @1.26-1.65
		(e-literal @1.27-1.33 (string "Hello "))
		(e-lookup-local @1.35-1.45
			(p-assign @1.4-1.14 (ident "first_name")))
		(e-literal @1.46-1.47 (string " "))
		(e-dot-access @1.49-1.63 (field "last_name")
			(receiver
				(e-lookup-local @1.49-1.53
					(p-assign @1.16-1.22 (ident "rest")))))
		(e-literal @1.64-1.64 (string ""))))
~~~
# TYPES
~~~clojure
(expr @1.1-1.65 (type "_arg -> Str"))
~~~
