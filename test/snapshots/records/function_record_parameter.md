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
		(e-method-call (method ".to_str")
			(receiver
				(e-ident (raw "age")))
			(args))
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
	(e-block
		(s-let
			(p-assign (ident "#interp_0"))
			(e-lookup-local
				(p-assign (ident "name"))))
		(s-let
			(p-assign (ident "#interp_1"))
			(e-dispatch-call (method "to_str") (constraint-fn-var 28)
				(receiver
					(e-lookup-local
						(p-assign (ident "age"))))
				(args)))
		(e-interpolation (constraint-fn-var 87)
			(first
				(e-literal (string "Hello ")))
			(parts
				(e-lookup-local
					(p-assign (ident "#interp_0")))
				(e-literal (string ", you are "))
				(e-lookup-local
					(p-assign (ident "#interp_1")))
				(e-literal (string " years old"))))))
~~~
# TYPES
~~~clojure
(expr (type "{ age: a, name: _field } -> b where [a.to_str : a -> _ret, b.from_interpolation : Str, Iter((_field, Str)) -> b]"))
~~~
