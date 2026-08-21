# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [] => 0
    [x] => x
    [first, second] => first + second
    [head, .. as tail] => head
    [One, Two, .. as rest] => 3
    [x, y, z, .. as more] => x + y + z
}
~~~
# EXPECTED
MISSING METHOD - list_destructure_variations.md:4:24:4:38
MISSING METHOD - list_destructure_variations.md:2:11:2:12
MISSING METHOD - list_destructure_variations.md:6:31:6:32
MISSING METHOD - list_destructure_variations.md:7:30:7:35
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 4 24) (end 4 38))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "plus")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "list_destructure_variations.md") (start 4 24) (end 4 38) (annotation error) (line-text "    [first, second] => first + second"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "plus")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[One, Two, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 2 11) (end 2 12))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "list_destructure_variations.md") (start 2 11) (end 2 12) (annotation error) (line-text "    [] => 0"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_numeral")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[One, Two, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 6 31) (end 6 32))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "list_destructure_variations.md") (start 6 31) (end 6 32) (annotation error) (line-text "    [One, Two, .. as rest] => 3"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "from_numeral")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[One, Two, ..]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 7 30) (end 7 35))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "plus")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "list_destructure_variations.md") (start 7 30) (end 7 35) (annotation error) (line-text "    [x, y, z, .. as more] => x + y + z"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "plus")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[One, Two, ..]")
			(annotation-end))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,CloseSquare,OpFatArrow,Int,
OpenSquare,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
OpenSquare,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,UpperIdent,Comma,UpperIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,Int,
OpenSquare,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,Comma,DoubleDot,KwAs,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "list"))
	(branches
		(branch
			(p-list)
			(e-int (raw "0")))
		(branch
			(p-list
				(p-ident (raw "x")))
			(e-ident (raw "x")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-ident (raw "second")))
			(e-binop (op "+")
				(e-ident (raw "first"))
				(e-ident (raw "second"))))
		(branch
			(p-list
				(p-ident (raw "head"))
				(p-list-rest (name "tail")))
			(e-ident (raw "head")))
		(branch
			(p-list
				(p-tag (raw "One"))
				(p-tag (raw "Two"))
				(p-list-rest (name "rest")))
			(e-int (raw "3")))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-ident (raw "y"))
				(p-ident (raw "z"))
				(p-list-rest (name "more")))
			(e-binop (op "+")
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "y")))
				(e-ident (raw "z"))))))
~~~
# FORMATTED
~~~roc
match list {
	[] => 0
	[x] => x
	[first, second] => first + second
	[head, .. as tail] => head
	[One, Two, .. as rest] => 3
	[x, y, z, .. as more] => x + y + z
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
					(e-runtime-error (tag "erroneous_value_expr"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))))))
				(value
					(e-lookup-local
						(p-assign (ident "x")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first"))
								(p-assign (ident "second"))))))
				(value
					(e-runtime-error (tag "erroneous_value_expr"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "head")))
							(rest-at (index 1)
								(p-assign (ident "tail"))))))
				(value
					(e-lookup-local
						(p-assign (ident "head")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-applied-tag)
								(p-applied-tag))
							(rest-at (index 2)
								(p-assign (ident "rest"))))))
				(value
					(e-runtime-error (tag "erroneous_value_expr"))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-assign (ident "y"))
								(p-assign (ident "z")))
							(rest-at (index 3)
								(p-assign (ident "more"))))))
				(value
					(e-runtime-error (tag "erroneous_value_expr")))))))
~~~
# TYPES
~~~clojure
(expr (type "[One, Two, ..]"))
~~~
