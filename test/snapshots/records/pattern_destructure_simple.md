# META
~~~ini
description=Simple record destructuring pattern
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
match person {
    { name, age } => name
}
~~~
# EXPECTED
NAME NOT IN SCOPE - pattern_destructure_simple.md:1:7:1:13
UNUSED VARIABLE - pattern_destructure_simple.md:2:13:2:16
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
			(source-region (file "pattern_destructure_simple.md") (start 1 7) (end 1 13) (annotation error) (line-text "match person {"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 2 13) (end 2 16))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "age")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_age")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "pattern_destructure_simple.md") (start 2 13) (end 2 16) (annotation error) (line-text "    { name, age } => name")))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpFatArrow,LowerIdent,
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
				(field (name "age") (rest false)))
			(e-ident (raw "name")))))
~~~
# FORMATTED
~~~roc
match person {
	{ name, age } => name
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
								(record-destruct (label "age") (ident "age")
									(required
										(p-assign (ident "age"))))))))
				(value
					(e-lookup-local
						(p-assign (ident "name"))))))))
~~~
# TYPES
~~~clojure
(expr (type "_a"))
~~~
