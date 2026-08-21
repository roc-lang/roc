# META
~~~ini
description=Match expression with list patterns including invalid rest pattern
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
match numbers {
    [] => acc
    [first, ..rest] => 0 # invalid rest pattern should error
}
~~~
# EXPECTED
OLD LIST REST PATTERN - list_patterns.md:3:13:3:19
NAME NOT IN SCOPE - list_patterns.md:1:7:1:14
NAME NOT IN SCOPE - list_patterns.md:2:11:2:14
UNUSED VARIABLE - list_patterns.md:3:6:3:11
UNUSED VARIABLE - list_patterns.md:3:15:3:15
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Old List Rest Pattern")
		(region (start 3 13) (end 3 19))
		(headline
			(reflow "I was parsing a list pattern, and this uses the old rest syntax."))
		(document
			(reflow "List rest patterns now use ")
			(annotated code ".. as name")
			(reflow ". The name is optional, but if it is present it must come after ")
			(annotated code "as")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[first, .. as rest]")
			(annotation-end)
			(line-break)
			(line-break)
			(source-region (file "list_patterns.md") (start 3 13) (end 3 19) (annotation error) (line-text "    [first, ..rest] => 0 # invalid rest pattern should error"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 7) (end 1 14))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "numbers")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "list_patterns.md") (start 1 7) (end 1 14) (annotation error) (line-text "match numbers {"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 2 11) (end 2 14))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "acc")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "list_patterns.md") (start 2 11) (end 2 14) (annotation error) (line-text "    [] => acc"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 3 6) (end 3 11))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "first")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_first")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "list_patterns.md") (start 3 6) (end 3 11) (annotation error) (line-text "    [first, ..rest] => 0 # invalid rest pattern should error"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 3 15) (end 3 15))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "list_patterns.md") (start 3 15) (end 3 15) (annotation error) (line-text "    [first, ..rest] => 0 # invalid rest pattern should error")))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "numbers"))
	(branches
		(branch
			(p-list)
			(e-ident (raw "acc")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "rest")))
			(e-int (raw "0")))))
~~~
# FORMATTED
~~~roc
match numbers {
	[] => acc
	[first, .. as rest] => 0
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
						(p-list
							(patterns))))
				(value
					(e-runtime-error (tag "ident_not_in_scope"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "0")))))))
~~~
# TYPES
~~~clojure
(expr (type "Error"))
~~~
