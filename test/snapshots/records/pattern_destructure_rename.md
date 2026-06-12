# META
~~~ini
description=Record destructuring with field renaming
type=expr
~~~
# SOURCE
~~~roc
match person {
    { name: userName, age: userAge } => "User ${userName} is ${userAge.to_str()} years old"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "person"))
	(branches
		(branch
			(p-record
				(field (name "name") (rest false)
					(p-ident (raw "userName")))
				(field (name "age") (rest false)
					(p-ident (raw "userAge"))))
			(e-string
				(e-string-part (raw "User "))
				(e-ident (raw "userName"))
				(e-string-part (raw " is "))
				(e-method-call (method ".to_str")
					(receiver
						(e-ident (raw "userAge")))
					(args))
				(e-string-part (raw " years old"))))))
~~~
# FORMATTED
~~~roc
match person {
	{ name: userName, age: userAge } => "User ${userName} is ${userAge.to_str()} years old"
}
~~~
# CANONICALIZE
~~~clojure
(e-match
	(match
		(cond
			(e-runtime-error (tag "ident_not_in_scope")))
		(branches
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "name") (ident "name")
									(sub-pattern
										(p-assign (ident "userName"))))
								(record-destruct (label "age") (ident "age")
									(sub-pattern
										(p-assign (ident "userAge"))))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-lookup-local
								(p-assign (ident "userName"))))
						(s-let
							(p-assign (ident "#interp_1"))
							(e-dispatch-call (method "to_str") (constraint-fn-var 36)
								(receiver
									(e-lookup-local
										(p-assign (ident "userAge"))))
								(args)))
						(e-interpolation (constraint-fn-var 182)
							(first
								(e-literal (string "User ")))
							(rest
								(e-dispatch-call (method "prepended") (constraint-fn-var 140)
									(receiver
										(e-dispatch-call (method "prepended") (constraint-fn-var 95)
											(receiver
												(e-dispatch-call (method "iter") (constraint-fn-var 44)
													(receiver
														(e-empty_list))
													(args)))
											(args
												(e-tuple
													(elems
														(e-lookup-local
															(p-assign (ident "#interp_1")))
														(e-literal (string " years old")))))))
									(args
										(e-tuple
											(elems
												(e-lookup-local
													(p-assign (ident "#interp_0")))
												(e-literal (string " is "))))))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
