# META
~~~ini
description=Match expression with record destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { name, age } => "${name} is ${age.to_str()} years old"
    { name, address: { city } } => "${city} is the city of ${name}"
    {} => "empty"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,TripleDot,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
OpenCurly,CloseCurly,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ellipsis)
	(branches
		(branch
			(p-record
				(field (name "name") (rest false))
				(field (name "age") (rest false)))
			(e-string
				(e-string-part (raw ""))
				(e-ident (raw "name"))
				(e-string-part (raw " is "))
				(e-method-call (method ".to_str")
					(receiver
						(e-ident (raw "age")))
					(args))
				(e-string-part (raw " years old"))))
		(branch
			(p-record
				(field (name "name") (rest false))
				(field (name "address") (rest false)
					(p-record
						(field (name "city") (rest false)))))
			(e-string
				(e-string-part (raw ""))
				(e-ident (raw "city"))
				(e-string-part (raw " is the city of "))
				(e-ident (raw "name"))
				(e-string-part (raw ""))))
		(branch
			(p-record)
			(e-string
				(e-string-part (raw "empty"))))))
~~~
# FORMATTED
~~~roc
match ... {
	{ name, age } => "${name} is ${age.to_str()} years old"
	{ name, address: { city } } => "${city} is the city of ${name}"
	{} => "empty"
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-not-implemented))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "name") (ident "name")
									(required
										(p-assign (ident "name"))))
								(record-destruct (label "age") (ident "age")
									(required
										(p-assign (ident "age"))))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-lookup-local
								(p-assign (ident "name"))))
						(s-let
							(p-assign (ident "#interp_1"))
							(e-dispatch-call (method "to_str") (constraint-fn-var 74)
								(receiver
									(e-lookup-local
										(p-assign (ident "age"))))
								(args)))
						(e-dispatch-call (method "from_interpolation") (constraint-fn-var 259)
							(receiver
								(e-string
									(e-literal (string ""))))
							(args
								(e-dispatch-call (method "prepended") (constraint-fn-var 217)
									(receiver
										(e-dispatch-call (method "prepended") (constraint-fn-var 159)
											(receiver
												(e-dispatch-call (method "iter") (constraint-fn-var 95)
													(receiver
														(e-empty_list))
													(args)))
											(args
												(e-tuple
													(elems
														(e-lookup-local
															(p-assign (ident "#interp_1")))
														(e-string
															(e-literal (string " years old"))))))))
									(args
										(e-tuple
											(elems
												(e-lookup-local
													(p-assign (ident "#interp_0")))
												(e-string
													(e-literal (string " is "))))))))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "name") (ident "name")
									(required
										(p-assign (ident "name"))))
								(record-destruct (label "address") (ident "address")
									(sub-pattern
										(p-record-destructure
											(destructs
												(record-destruct (label "city") (ident "city")
													(required
														(p-assign (ident "city"))))))))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_2"))
							(e-lookup-local
								(p-assign (ident "city"))))
						(s-let
							(p-assign (ident "#interp_3"))
							(e-lookup-local
								(p-assign (ident "name"))))
						(e-dispatch-call (method "from_interpolation") (constraint-fn-var 444)
							(receiver
								(e-string
									(e-literal (string ""))))
							(args
								(e-dispatch-call (method "prepended") (constraint-fn-var 402)
									(receiver
										(e-dispatch-call (method "prepended") (constraint-fn-var 344)
											(receiver
												(e-dispatch-call (method "iter") (constraint-fn-var 280)
													(receiver
														(e-empty_list))
													(args)))
											(args
												(e-tuple
													(elems
														(e-lookup-local
															(p-assign (ident "#interp_3")))
														(e-string
															(e-literal (string ""))))))))
									(args
										(e-tuple
											(elems
												(e-lookup-local
													(p-assign (ident "#interp_2")))
												(e-string
													(e-literal (string " is the city of "))))))))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs))))
				(value
					(e-string
						(e-literal (string "empty"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
