# META
~~~ini
description=Match expression with nested list patterns testing variable scoping
type=expr
~~~
# SOURCE
~~~roc
match nestedList {
    [[x], [y]] => x + y
    [[x, y]] => x - y
    [x, [y]] => x * y
}
~~~
# EXPECTED
MISSING METHOD - nested_list_scoping.md:4:17:4:22
POLYMORPHIC VALUE - nested_list_scoping.md:1:1:5:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 4 17) (end 4 22))
		(headline
			(reflow "The value before this")
			(reflow " ")
			(annotated operator "*")
			(reflow " ")
			(reflow "operator has a type that doesn't have a")
			(reflow " ")
			(annotated code "times")
			(reflow " ")
			(reflow "method."))
		(document
			(source-region (file "nested_list_scoping.md") (start 4 17) (end 4 22) (annotation error) (line-text "    [x, [y]] => x * y"))
			(line-break)
			(reflow "The value's type, which does not have a method named ")
			(annotated code "times")
			(reflow ",")
			(reflow " ")
			(reflow "is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "List(a) where [a.minus : a, a -> a, a.plus : a, a -> a]")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "The")
			(reflow " ")
			(annotated operator "*")
			(reflow " ")
			(reflow "operator calls a method named")
			(reflow " ")
			(annotated code "times")
			(reflow " ")
			(reflow "on the value preceding it, passing the value after the operator as the one argument.")))
	(report
		(severity runtime_error)
		(title "Polymorphic Value")
		(region (start 1 1) (end 5 2))
		(headline
			(reflow "This top-level value still has an unresolved polymorphic type."))
		(document
			(source-region (file "nested_list_scoping.md") (start 1 1) (end 5 2) (annotation error) (line-text "match nestedList {\n    [[x], [y]] => x + y\n    [[x, y]] => x - y\n    [x, [y]] => x * y\n}"))
			(line-break)
			(line-break)
			(reflow "Its type is:")
			(line-break)
			(annotated code-block "a where [a.minus : a, a -> a, a.plus : a, a -> a]")
			(line-break)
			(reflow "Add an annotation or use this value in a way that fixes its concrete type."))))
~~~
# TOKENS
~~~zig
KwMatch,LowerIdent,OpenCurly,
OpenSquare,OpenSquare,LowerIdent,CloseSquare,Comma,OpenSquare,LowerIdent,CloseSquare,CloseSquare,OpFatArrow,LowerIdent,OpPlus,LowerIdent,
OpenSquare,OpenSquare,LowerIdent,Comma,LowerIdent,CloseSquare,CloseSquare,OpFatArrow,LowerIdent,OpBinaryMinus,LowerIdent,
OpenSquare,LowerIdent,Comma,OpenSquare,LowerIdent,CloseSquare,CloseSquare,OpFatArrow,LowerIdent,OpStar,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-match
	(e-ident (raw "nestedList"))
	(branches
		(branch
			(p-list
				(p-list
					(p-ident (raw "x")))
				(p-list
					(p-ident (raw "y"))))
			(e-binop (op "+")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))
		(branch
			(p-list
				(p-list
					(p-ident (raw "x"))
					(p-ident (raw "y"))))
			(e-binop (op "-")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))
		(branch
			(p-list
				(p-ident (raw "x"))
				(p-list
					(p-ident (raw "y"))))
			(e-binop (op "*")
				(e-ident (raw "x"))
				(e-ident (raw "y"))))))
~~~
# FORMATTED
~~~roc
match nestedList {
	[[x], [y]] => x + y
	[[x, y]] => x - y
	[x, [y]] => x * y
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
								(p-list
									(patterns
										(p-assign (ident "x"))))
								(p-list
									(patterns
										(p-assign (ident "y"))))))))
				(value
					(e-dispatch-call (method "plus") (constraint-fn-var 232)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-lookup-local
								(p-assign (ident "y")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-list
									(patterns
										(p-assign (ident "x"))
										(p-assign (ident "y"))))))))
				(value
					(e-dispatch-call (method "minus") (constraint-fn-var 234)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-lookup-local
								(p-assign (ident "y")))))))
			(branch
				(patterns
					(pattern (degenerate false)
						(p-list
							(patterns
								(p-assign (ident "x"))
								(p-list
									(patterns
										(p-assign (ident "y"))))))))
				(value
					(e-binop (op "mul")
						(e-lookup-local
							(p-assign (ident "x")))
						(e-lookup-local
							(p-assign (ident "y")))))))))
~~~
# TYPES
~~~clojure
(expr (type "a where [a.minus : a, a -> a, a.plus : a, a -> a]"))
~~~
