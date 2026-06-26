# META
~~~ini
description=Match expression with deeply nested record patterns
type=expr
~~~
# SOURCE
~~~roc
match ... {
    { name, address: { city, country } } => "${name} lives in ${city}, ${country}"
    { person: { name, age }, location: { city } } => "${name} (${age.to_str()}) from ${city}"
    { data: { info: { value } } } => "Deep nested: ${value}"
    { simple } => "Simple: ${simple}"
    {} => "empty"
}
~~~
# EXPECTED
UNCONDITIONAL CONDITION - nested_record_patterns.md:1:1:1:1
# PROBLEMS

┌─────────────────────────┐
│ UNCONDITIONAL CONDITION ├─ This match value is known at compile time, so ───┐
└┬────────────────────────┘  this match will always inspect the same value.   │
 │                                                                            │
 │  match ... {                                                               │
 │  ‾                                                                         │
 └───────────────────────────────────────────── nested_record_patterns.md:1:1 ┘


# TOKENS
~~~zig
KwMatch,TripleDot,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,Comma,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,CloseCurly,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
OpenCurly,LowerIdent,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
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
				(field (name "address") (rest false)
					(p-record
						(field (name "city") (rest false))
						(field (name "country") (rest false)))))
			(e-string
				(e-string-part (raw ""))
				(e-ident (raw "name"))
				(e-string-part (raw " lives in "))
				(e-ident (raw "city"))
				(e-string-part (raw ", "))
				(e-ident (raw "country"))
				(e-string-part (raw ""))))
		(branch
			(p-record
				(field (name "person") (rest false)
					(p-record
						(field (name "name") (rest false))
						(field (name "age") (rest false))))
				(field (name "location") (rest false)
					(p-record
						(field (name "city") (rest false)))))
			(e-string
				(e-string-part (raw ""))
				(e-ident (raw "name"))
				(e-string-part (raw " ("))
				(e-method-call (method ".to_str")
					(receiver
						(e-ident (raw "age")))
					(args))
				(e-string-part (raw ") from "))
				(e-ident (raw "city"))
				(e-string-part (raw ""))))
		(branch
			(p-record
				(field (name "data") (rest false)
					(p-record
						(field (name "info") (rest false)
							(p-record
								(field (name "value") (rest false)))))))
			(e-string
				(e-string-part (raw "Deep nested: "))
				(e-ident (raw "value"))
				(e-string-part (raw ""))))
		(branch
			(p-record
				(field (name "simple") (rest false)))
			(e-string
				(e-string-part (raw "Simple: "))
				(e-ident (raw "simple"))
				(e-string-part (raw ""))))
		(branch
			(p-record)
			(e-string
				(e-string-part (raw "empty"))))))
~~~
# FORMATTED
~~~roc
match ... {
	{ name, address: { city, country } } => "${name} lives in ${city}, ${country}"
	{ person: { name, age }, location: { city } } => "${name} (${age.to_str()}) from ${city}"
	{ data: { info: { value } } } => "Deep nested: ${value}"
	{ simple } => "Simple: ${simple}"
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
								(record-destruct (label "address") (ident "address")
									(sub-pattern
										(p-record-destructure
											(destructs
												(record-destruct (label "city") (ident "city")
													(required
														(p-assign (ident "city"))))
												(record-destruct (label "country") (ident "country")
													(required
														(p-assign (ident "country"))))))))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-lookup-local
								(p-assign (ident "name"))))
						(s-let
							(p-assign (ident "#interp_1"))
							(e-lookup-local
								(p-assign (ident "city"))))
						(s-let
							(p-assign (ident "#interp_2"))
							(e-lookup-local
								(p-assign (ident "country"))))
						(e-interpolation (constraint-fn-var 167)
							(first
								(e-literal (string "")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_0")))
								(e-literal (string " lives in "))
								(e-lookup-local
									(p-assign (ident "#interp_1")))
								(e-literal (string ", "))
								(e-lookup-local
									(p-assign (ident "#interp_2")))
								(e-literal (string "")))))))
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
													(required
														(p-assign (ident "name"))))
												(record-destruct (label "age") (ident "age")
													(required
														(p-assign (ident "age"))))))))
								(record-destruct (label "location") (ident "location")
									(sub-pattern
										(p-record-destructure
											(destructs
												(record-destruct (label "city") (ident "city")
													(required
														(p-assign (ident "city"))))))))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_3"))
							(e-lookup-local
								(p-assign (ident "name"))))
						(s-let
							(p-assign (ident "#interp_4"))
							(e-dispatch-call (method "to_str") (constraint-fn-var 172)
								(receiver
									(e-lookup-local
										(p-assign (ident "age"))))
								(args)))
						(s-let
							(p-assign (ident "#interp_5"))
							(e-lookup-local
								(p-assign (ident "city"))))
						(e-interpolation (constraint-fn-var 234)
							(first
								(e-literal (string "")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_3")))
								(e-literal (string " ("))
								(e-lookup-local
									(p-assign (ident "#interp_4")))
								(e-literal (string ") from "))
								(e-lookup-local
									(p-assign (ident "#interp_5")))
								(e-literal (string "")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "data") (ident "data")
									(sub-pattern
										(p-record-destructure
											(destructs
												(record-destruct (label "info") (ident "info")
													(sub-pattern
														(p-record-destructure
															(destructs
																(record-destruct (label "value") (ident "value")
																	(required
																		(p-assign (ident "value"))))))))))))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_6"))
							(e-lookup-local
								(p-assign (ident "value"))))
						(e-interpolation (constraint-fn-var 293)
							(first
								(e-literal (string "Deep nested: ")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_6")))
								(e-literal (string "")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-record-destructure
							(destructs
								(record-destruct (label "simple") (ident "simple")
									(required
										(p-assign (ident "simple"))))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_7"))
							(e-lookup-local
								(p-assign (ident "simple"))))
						(e-interpolation (constraint-fn-var 350)
							(first
								(e-literal (string "Simple: ")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_7")))
								(e-literal (string "")))))))
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
