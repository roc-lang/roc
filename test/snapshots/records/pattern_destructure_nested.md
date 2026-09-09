# META
~~~ini
description=Nested record destructuring pattern in a match expression
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
match person {
    { name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
}
~~~
# EXPECTED
NAME NOT IN SCOPE - pattern_destructure_nested.md:1:7:1:13
UNUSED VARIABLE - pattern_destructure_nested.md:2:38:2:45
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 7) (end 1 13))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "person")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "pattern_destructure_nested.md") (start 1 7) (end 1 13) (annotation error) (line-text "match person {"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 2 38) (end 2 45))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "zipCode")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_zipCode")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "pattern_destructure_nested.md") (start 2 38) (end 2 45) (annotation error) (line-text "    { name, address: { street, city, zipCode } } => \"${name} lives on ${street} in ${city}\"")))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,CloseCurly,OpFatArrow,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
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
				(field (name "name") (rest false))
				(field (name "address") (rest false)
					(p-record
						(field (name "street") (rest false))
						(field (name "city") (rest false))
						(field (name "zipCode") (rest false)))))
			(e-string
				(e-string-part (raw ""))
				(e-ident (raw "name"))
				(e-string-part (raw " lives on "))
				(e-ident (raw "street"))
				(e-string-part (raw " in "))
				(e-ident (raw "city"))
				(e-string-part (raw ""))))))
~~~
# FORMATTED
~~~roc
match person {
	{ name, address: { street, city, zipCode } } => "${name} lives on ${street} in ${city}"
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
									(required
										(p-assign (ident "name"))))
								(record-destruct (label "address") (ident "address")
									(sub-pattern
										(p-record-destructure
											(destructs
												(record-destruct (label "street") (ident "street")
													(required
														(p-assign (ident "street"))))
												(record-destruct (label "city") (ident "city")
													(required
														(p-assign (ident "city"))))
												(record-destruct (label "zipCode") (ident "zipCode")
													(required
														(p-assign (ident "zipCode"))))))))))))
				(value
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-lookup-local
								(p-assign (ident "name"))))
						(s-let
							(p-assign (ident "#interp_1"))
							(e-lookup-local
								(p-assign (ident "street"))))
						(s-let
							(p-assign (ident "#interp_2"))
							(e-lookup-local
								(p-assign (ident "city"))))
						(e-interpolation (constraint-fn-var 266) (dispatcher-var 30)
							(first
								(e-literal (string "")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_0")))
								(e-literal (string " lives on "))
								(e-lookup-local
									(p-assign (ident "#interp_1")))
								(e-literal (string " in "))
								(e-lookup-local
									(p-assign (ident "#interp_2")))
								(e-literal (string ""))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Str"))
~~~
