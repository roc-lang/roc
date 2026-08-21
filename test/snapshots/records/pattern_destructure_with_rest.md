# META
~~~ini
description=Record destructuring with rest pattern
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
match person {
    { first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)
}
~~~
# EXPECTED
NAME NOT IN SCOPE - pattern_destructure_with_rest.md:1:7:1:13
DOES NOT EXIST - pattern_destructure_with_rest.md:2:33:2:40
DOES NOT EXIST - pattern_destructure_with_rest.md:2:55:2:62
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
			(source-region (file "pattern_destructure_with_rest.md") (start 1 7) (end 1 13) (annotation error) (line-text "match person {"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 2 33) (end 2 40))
		(headline
			(annotated code "Str.len")
			(reflow " does not exist."))
		(document
			(annotated code "Str")
			(reflow " is in scope, but it has no associated ")
			(annotated code "len")
			(reflow ".")
			(line-break)
			(line-break)
			(source-region (file "pattern_destructure_with_rest.md") (start 2 33) (end 2 40) (annotation error) (line-text "    { first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)"))))
	(report
		(severity runtime_error)
		(title "Does Not Exist")
		(region (start 2 55) (end 2 62))
		(headline
			(annotated code "Str.len")
			(reflow " does not exist."))
		(document
			(annotated code "Str")
			(reflow " is in scope, but it has no associated ")
			(annotated code "len")
			(reflow ".")
			(line-break)
			(line-break)
			(source-region (file "pattern_destructure_with_rest.md") (start 2 55) (end 2 62) (annotation error) (line-text "    { first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)")))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,DoubleDot,LowerIdent,CloseCurly,OpFatArrow,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpGreaterThan,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,
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
				(field (name "first_name") (rest false))
				(field (name "others") (rest true)))
			(e-binop (op ">")
				(e-apply
					(e-ident (raw "Str.len"))
					(e-ident (raw "first_name")))
				(e-apply
					(e-ident (raw "Str.len"))
					(e-field-access
						(receiver
							(e-ident (raw "others")))
						(segment (mode "required") (field "last_name"))))))))
~~~
# FORMATTED
~~~roc
match person {
	{ first_name, ..others } => Str.len(first_name) > Str.len(others.last_name)
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
								(record-destruct (label "first_name") (ident "first_name")
									(required
										(p-assign (ident "first_name"))))
								(record-destruct (label "others") (ident "others")
									(rest-pattern
										(p-assign (ident "others"))))))))
				(value
					(e-binop (op "gt")
						(e-call
							(e-runtime-error (tag "nested_value_not_found"))
							(e-lookup-local
								(p-assign (ident "first_name"))))
						(e-call
							(e-runtime-error (tag "nested_value_not_found"))
							(e-field-access
								(receiver
									(e-lookup-local
										(p-assign (ident "others"))))
								(segments
									(segment (name "last_name") (mode "required")))))))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
