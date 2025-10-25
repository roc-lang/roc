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
				(e-field-access
					(e-ident (raw "age"))
					(e-apply
						(e-ident (raw "to_str"))))
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
					(e-string
						(e-literal (string ""))
						(e-lookup-local
							(p-assign (ident "name")))
						(e-literal (string " is "))
						(e-dot-access (field "to_str")
							(receiver
								(e-lookup-local
									(p-assign (ident "age"))))
							(args))
						(e-literal (string " years old")))))
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
					(e-string
						(e-literal (string ""))
						(e-lookup-local
							(p-assign (ident "city")))
						(e-literal (string " is the city of "))
						(e-lookup-local
							(p-assign (ident "name")))
						(e-literal (string "")))))
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
