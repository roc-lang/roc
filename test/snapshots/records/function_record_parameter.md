# META
~~~ini
description=Function with record parameter destructuring and string interpolation
type=expr
~~~
# SOURCE
~~~roc
|{ name, age }| "Hello ${name}, you are ${age.to_str()} years old"
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-record
			(field (name "name") (rest false))
			(field (name "age") (rest false))))
	(e-string
		(e-string-part (raw "Hello "))
		(e-ident (raw "name"))
		(e-string-part (raw ", you are "))
		(e-field-access
			(e-ident (raw "age"))
			(e-apply
				(e-ident (raw "to_str"))))
		(e-string-part (raw " years old"))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-record-destructure
			(destructs
				(record-destruct (label "name") (ident "name")
					(required
						(p-assign (ident "name"))))
				(record-destruct (label "age") (ident "age")
					(required
						(p-assign (ident "age")))))))
	(e-string
		(e-literal (string "Hello "))
		(e-lookup-local
			(p-assign (ident "name")))
		(e-literal (string ", you are "))
		(e-dot-access (field "to_str")
			(receiver
				(e-lookup-local
					(p-assign (ident "age"))))
			(args))
		(e-literal (string " years old"))))
~~~
# TYPES
~~~clojure
(expr (type "{ age: _field, name: _field2 } -> Error"))
~~~
