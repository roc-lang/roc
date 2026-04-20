# META
~~~ini
description=Function with record parameter destructuring and rest pattern, capture whole record using `as`
type=expr
~~~
# SOURCE
~~~roc
|{ name, age, ..a } as person| { greeting: "Hello ${name}", full_record: person, is_adult: age >= 18 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,Comma,DoubleDot,LowerIdent,CloseCurly,KwAs,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,Comma,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,OpGreaterThanOrEq,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-lambda
	(args
		(p-as (name "person")
			(p-record
				(field (name "name") (rest false))
				(field (name "age") (rest false))
				(field (name "a") (rest true)))))
	(e-record
		(field (field "greeting")
			(e-string
				(e-string-part (raw "Hello "))
				(e-ident (raw "name"))
				(e-string-part (raw ""))))
		(field (field "full_record")
			(e-ident (raw "person")))
		(field (field "is_adult")
			(e-binop (op ">=")
				(e-ident (raw "age"))
				(e-int (raw "18"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(e-lambda
	(args
		(p-as (as "person")
			(p-record-destructure
				(destructs
					(record-destruct (label "name") (ident "name")
						(required
							(p-assign (ident "name"))))
					(record-destruct (label "age") (ident "age")
						(required
							(p-assign (ident "age"))))
					(record-destruct (label "a") (ident "a")
						(rest-pattern
							(p-assign (ident "a"))))))))
	(e-record
		(fields
			(field (name "greeting")
				(e-string
					(e-literal (string "Hello "))
					(e-lookup-local
						(p-assign (ident "name")))
					(e-literal (string ""))))
			(field (name "full_record")
				(e-lookup-local
					(p-as (as "person")
						(p-record-destructure
							(destructs
								(record-destruct (label "name") (ident "name")
									(required
										(p-assign (ident "name"))))
								(record-destruct (label "age") (ident "age")
									(required
										(p-assign (ident "age"))))
								(record-destruct (label "a") (ident "a")
									(rest-pattern
										(p-assign (ident "a")))))))))
			(field (name "is_adult")
				(e-binop (op "ge")
					(e-lookup-local
						(p-assign (ident "age")))
					(e-num (value "18")))))))
~~~
# TYPES
~~~clojure
(expr (type "{ age: Dec, name: Str, .. } -> { full_record: { age: Dec, name: Str, .. }, greeting: Str, is_adult: Bool }"))
~~~
