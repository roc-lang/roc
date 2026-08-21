# META
~~~ini
description=Match expression with invalid (old style) list rest patterns should error
type=expr
canonicalize_diagnostics=true
~~~
# SOURCE
~~~roc
match items {
    [first, ..rest] => 0 # invalid rest pattern should error
    [..rest, last] => 1 # invalid rest pattern should error
    [x, ..rest, y] => 2 # invalid rest pattern should error
}
~~~
# EXPECTED
OLD LIST REST PATTERN - list_rest_invalid.md:2:13:2:19
OLD LIST REST PATTERN - list_rest_invalid.md:3:6:3:12
OLD LIST REST PATTERN - list_rest_invalid.md:4:9:4:15
NAME NOT IN SCOPE - list_rest_invalid.md:1:7:1:12
UNUSED VARIABLE - list_rest_invalid.md:2:6:2:11
UNUSED VARIABLE - list_rest_invalid.md:2:15:2:15
UNUSED VARIABLE - list_rest_invalid.md:3:8:3:8
UNUSED VARIABLE - list_rest_invalid.md:3:14:3:18
UNUSED VARIABLE - list_rest_invalid.md:4:6:4:7
UNUSED VARIABLE - list_rest_invalid.md:4:11:4:11
UNUSED VARIABLE - list_rest_invalid.md:4:17:4:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Old List Rest Pattern")
		(region (start 2 13) (end 2 19))
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
			(source-region (file "list_rest_invalid.md") (start 2 13) (end 2 19) (annotation error) (line-text "    [first, ..rest] => 0 # invalid rest pattern should error"))))
	(report
		(severity runtime_error)
		(title "Old List Rest Pattern")
		(region (start 3 6) (end 3 12))
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
			(source-region (file "list_rest_invalid.md") (start 3 6) (end 3 12) (annotation error) (line-text "    [..rest, last] => 1 # invalid rest pattern should error"))))
	(report
		(severity runtime_error)
		(title "Old List Rest Pattern")
		(region (start 4 9) (end 4 15))
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
			(source-region (file "list_rest_invalid.md") (start 4 9) (end 4 15) (annotation error) (line-text "    [x, ..rest, y] => 2 # invalid rest pattern should error"))))
	(report
		(severity runtime_error)
		(title "Name Not In Scope")
		(region (start 1 7) (end 1 12))
		(headline
			(reflow "Nothing is named ")
			(annotated symbol-unqualified "items")
			(reflow " in this scope."))
		(document
			(reflow "Is it misspelled, or is there an import missing?")
			(line-break)
			(line-break)
			(source-region (file "list_rest_invalid.md") (start 1 7) (end 1 12) (annotation error) (line-text "match items {"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 2 6) (end 2 11))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "first")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_first")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "list_rest_invalid.md") (start 2 6) (end 2 11) (annotation error) (line-text "    [first, ..rest] => 0 # invalid rest pattern should error"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 2 15) (end 2 15))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "list_rest_invalid.md") (start 2 15) (end 2 15) (annotation error) (line-text "    [first, ..rest] => 0 # invalid rest pattern should error"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 3 8) (end 3 8))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "list_rest_invalid.md") (start 3 8) (end 3 8) (annotation error) (line-text "    [..rest, last] => 1 # invalid rest pattern should error"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 3 14) (end 3 18))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "last")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_last")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "list_rest_invalid.md") (start 3 14) (end 3 18) (annotation error) (line-text "    [..rest, last] => 1 # invalid rest pattern should error"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 4 6) (end 4 7))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "x")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_x")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "list_rest_invalid.md") (start 4 6) (end 4 7) (annotation error) (line-text "    [x, ..rest, y] => 2 # invalid rest pattern should error"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 4 11) (end 4 11))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "rest")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_rest")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "list_rest_invalid.md") (start 4 11) (end 4 11) (annotation error) (line-text "    [x, ..rest, y] => 2 # invalid rest pattern should error"))))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 4 17) (end 4 18))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "y")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_y")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "list_rest_invalid.md") (start 4 17) (end 4 18) (annotation error) (line-text "    [x, ..rest, y] => 2 # invalid rest pattern should error")))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,LowerIdent,Comma,DoubleDot,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,Int,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "items"))
	(branches
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-list-rest (name "rest")))
			(e-int (raw "0")))
		(branch
			(p-list
				(p-list-rest (name "rest"))
				(p-ident (raw "last")))
			(e-int (raw "1")))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-list-rest (name "rest"))
				(p-ident (raw "y")))
			(e-int (raw "2")))))
~~~
# FORMATTED
~~~roc
match items {
	[first, .. as rest] => 0 # invalid rest pattern should error
	[.. as rest, last] => 1 # invalid rest pattern should error
	[x, .. as rest, y] => 2
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
							(patterns
								(p-assign (ident "first")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "0"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "last")))
							(rest-at (index 0)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "1"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-assign (ident "y")))
							(rest-at (index 1)
								(p-assign (ident "rest"))))))
				(value
					(e-num (value "2")))))))
~~~
# TYPES
~~~clojure
(expr (type "Dec"))
~~~
