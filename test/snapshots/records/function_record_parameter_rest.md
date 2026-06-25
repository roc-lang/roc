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
OpBar,OpenCurly,LowerIdent,Comma,DoubleDot,LowerIdent,CloseCurly,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,CloseStringInterpolation,StringPart,StringEnd,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-record
			(field (name "first_name") (rest false))
			(field (name "rest") (rest true))))
	(e-string
		(e-string-part (raw "Hello "))
		(e-ident (raw "first_name"))
		(e-string-part (raw " "))
		(e-field-access
			(e-ident (raw "rest"))
			(e-ident (raw "last_name")))
		(e-string-part (raw ""))))
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
				(record-destruct (label "first_name") (ident "first_name")
					(required
						(p-assign (ident "first_name"))))
				(record-destruct (label "rest") (ident "rest")
					(rest-pattern
						(p-assign (ident "rest")))))))
	(e-block
		(s-let
			(p-assign (ident "#interp_0"))
			(e-lookup-local
				(p-assign (ident "first_name"))))
		(s-let
			(p-assign (ident "#interp_1"))
			(e-field-access (field "last_name")
				(receiver
					(e-lookup-local
						(p-assign (ident "rest"))))))
		(e-interpolation (constraint-fn-var 86)
			(first
				(e-literal (string "Hello ")))
			(parts
				(e-lookup-local
					(p-assign (ident "#interp_0")))
				(e-literal (string " "))
				(e-lookup-local
					(p-assign (ident "#interp_1")))
				(e-literal (string ""))))))
~~~
# TYPES
~~~clojure
(expr (type "{ first_name: _field, last_name: _field2, .. } -> a where [a.from_interpolation : Str, Iter((_field, Str)) -> a]"))
~~~
