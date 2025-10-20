# META
~~~ini
description=Edge cases for nested record patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { a: { b: { c } } } => "deeply nested: ${c}"
    { x, y: {} } => "mixed with empty: ${x}"
    { outer: { inner }, simple } => "mixed: ${inner} and ${simple}"
    { a: { b }, c: { d } } => "multiple nested: ${b}, ${d}"
    { name: x } => "renamed: ${x}"
    { person: { name: firstName, age: userAge } } => "renamed nested: ${firstName} (${userAge.to_str()})"
    {} => "empty record"
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwMatch,TripleDot,OpenCurly,
OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,CloseCurly,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,OpenCurly,CloseCurly,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,Comma,LowerIdent,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,Comma,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
OpenCurly,LowerIdent,OpColon,LowerIdent,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,CloseCurly,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
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
				(field (name "a") (rest false)
					(p-record
						(field (name "b") (rest false)
							(p-record
								(field (name "c") (rest false)))))))
			(e-string
				(e-string-part (raw "deeply nested: "))
				(e-ident (raw "c"))
				(e-string-part (raw ""))))
		(branch
			(p-record
				(field (name "x") (rest false))
				(field (name "y") (rest false)
					(p-record)))
			(e-string
				(e-string-part (raw "mixed with empty: "))
				(e-ident (raw "x"))
				(e-string-part (raw ""))))
		(branch
			(p-record
				(field (name "outer") (rest false)
					(p-record
						(field (name "inner") (rest false))))
				(field (name "simple") (rest false)))
			(e-string
				(e-string-part (raw "mixed: "))
				(e-ident (raw "inner"))
				(e-string-part (raw " and "))
				(e-ident (raw "simple"))
				(e-string-part (raw ""))))
		(branch
			(p-record
				(field (name "a") (rest false)
					(p-record
						(field (name "b") (rest false))))
				(field (name "c") (rest false)
					(p-record
						(field (name "d") (rest false)))))
			(e-string
				(e-string-part (raw "multiple nested: "))
				(e-ident (raw "b"))
				(e-string-part (raw ", "))
				(e-ident (raw "d"))
				(e-string-part (raw ""))))
		(branch
			(p-record
				(field (name "name") (rest false)
					(p-ident (raw "x"))))
			(e-string
				(e-string-part (raw "renamed: "))
				(e-ident (raw "x"))
				(e-string-part (raw ""))))
		(branch
			(p-record
				(field (name "person") (rest false)
					(p-record
						(field (name "name") (rest false)
							(p-ident (raw "firstName")))
						(field (name "age") (rest false)
							(p-ident (raw "userAge"))))))
			(e-string
				(e-string-part (raw "renamed nested: "))
				(e-ident (raw "firstName"))
				(e-string-part (raw " ("))
				(e-field-access
					(e-ident (raw "userAge"))
					(e-apply
						(e-ident (raw "to_str"))))
				(e-string-part (raw ")"))))
		(branch
			(p-record)
			(e-string
				(e-string-part (raw "empty record"))))))
~~~
# FORMATTED
~~~roc
match ... {
	{ a: { b: { c } } } => "deeply nested: ${c}"
	{ x, y: {} } => "mixed with empty: ${x}"
	{ outer: { inner }, simple } => "mixed: ${inner} and ${simple}"
	{ a: { b }, c: { d } } => "multiple nested: ${b}, ${d}"
	{ name: x } => "renamed: ${x}"
	{ person: { name: firstName, age: userAge } } => "renamed nested: ${firstName} (${userAge.to_str()})"
	{} => "empty record"
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
								(record-destruct (label "a") (ident "a")
									(sub-pattern
										(p-record-destructure
											(destructs
												(record-destruct (label "b") (ident "b")
													(sub-pattern
														(p-record-destructure
															(destructs
																(record-destruct (label "c") (ident "c")
																	(required
																		(p-assign (ident "c"))))))))))))))))
				(value
					(e-string
						(e-literal (string "deeply nested: "))
						(e-lookup-local
							(p-assign (ident "c")))
						(e-literal (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "x") (ident "x")
									(required
										(p-assign (ident "x"))))
								(record-destruct (label "y") (ident "y")
									(sub-pattern
										(p-record-destructure
											(destructs))))))))
				(value
					(e-string
						(e-literal (string "mixed with empty: "))
						(e-lookup-local
							(p-assign (ident "x")))
						(e-literal (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "outer") (ident "outer")
									(sub-pattern
										(p-record-destructure
											(destructs
												(record-destruct (label "inner") (ident "inner")
													(required
														(p-assign (ident "inner"))))))))
								(record-destruct (label "simple") (ident "simple")
									(required
										(p-assign (ident "simple"))))))))
				(value
					(e-string
						(e-literal (string "mixed: "))
						(e-lookup-local
							(p-assign (ident "inner")))
						(e-literal (string " and "))
						(e-lookup-local
							(p-assign (ident "simple")))
						(e-literal (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "a") (ident "a")
									(sub-pattern
										(p-record-destructure
											(destructs
												(record-destruct (label "b") (ident "b")
													(required
														(p-assign (ident "b"))))))))
								(record-destruct (label "c") (ident "c")
									(sub-pattern
										(p-record-destructure
											(destructs
												(record-destruct (label "d") (ident "d")
													(required
														(p-assign (ident "d"))))))))))))
				(value
					(e-string
						(e-literal (string "multiple nested: "))
						(e-lookup-local
							(p-assign (ident "b")))
						(e-literal (string ", "))
						(e-lookup-local
							(p-assign (ident "d")))
						(e-literal (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "name") (ident "name")
									(sub-pattern
										(p-assign (ident "x"))))))))
				(value
					(e-string
						(e-literal (string "renamed: "))
						(e-lookup-local
							(p-assign (ident "x")))
						(e-literal (string "")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "person") (ident "person")
									(sub-pattern
										(p-record-destructure
											(destructs
												(record-destruct (label "name") (ident "name")
													(sub-pattern
														(p-assign (ident "firstName"))))
												(record-destruct (label "age") (ident "age")
													(sub-pattern
														(p-assign (ident "userAge"))))))))))))
				(value
					(e-string
						(e-literal (string "renamed nested: "))
						(e-lookup-local
							(p-assign (ident "firstName")))
						(e-literal (string " ("))
						(e-dot-access (field "to_str")
							(receiver
								(e-lookup-local
									(p-assign (ident "userAge"))))
							(args))
						(e-literal (string ")")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs))))
				(value
					(e-string
						(e-literal (string "empty record"))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
