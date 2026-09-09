# META
~~~ini
description=Match expression with various list destructuring patterns
type=expr
~~~
# SOURCE
~~~roc
match list {
    [first] => first
    [first, second] => first + second
}
~~~
# EXPECTED
POLYMORPHIC VALUE - list_destructure_scoping.md:1:1:4:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Polymorphic Value")
		(region (start 1 1) (end 4 2))
		(headline
			(reflow "This top-level value still has an unresolved polymorphic type."))
		(document
			(source-region (file "list_destructure_scoping.md") (start 1 1) (end 4 2) (annotation error) (line-text "match list {\n    [first] => first\n    [first, second] => first + second\n}"))
			(line-break)
			(line-break)
			(reflow "Its type is:")
			(line-break)
			(annotated code-block "a where [a.plus : a, a -> a]")
			(line-break)
			(reflow "Add an annotation or use this value in a way that fixes its concrete type."))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,
OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "list"))
	(branches
		(branch
			(p-list
				(p-ident (raw "first")))
			(e-ident (raw "first")))
		(branch
			(p-list
				(p-ident (raw "first"))
				(p-ident (raw "second")))
			(e-binop (op "+")
				(e-ident (raw "first"))
				(e-ident (raw "second"))))))
~~~
# FORMATTED
~~~roc
match list {
	[first] => first
	[first, second] => first + second
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
								(p-assign (ident "first"))))))
				(value
					(e-lookup-local
						(p-assign (ident "first")))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "first"))
								(p-assign (ident "second"))))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 217)
						(receiver
							(e-lookup-local
								(p-assign (ident "first"))))
						(args
							(e-lookup-local
								(p-assign (ident "second"))))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.plus : a, a -> a]"))
~~~
