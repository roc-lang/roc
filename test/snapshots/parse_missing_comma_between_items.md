# META
~~~ini
description=A missing comma between call arguments or list items is reported, not silently accepted
type=snippet
~~~
# SOURCE
~~~roc
add = |a, b| a + b

sum = add(1 2)

items = [1 2, 3]
~~~
# EXPECTED
EXPECTED CALL ARGUMENT END - parse_missing_comma_between_items.md:3:13:3:14
EXPECTED LIST SEPARATOR - parse_missing_comma_between_items.md:5:12:5:13
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Expected Call Argument End")
		(region (start 3 13) (end 3 14))
		(headline
			(reflow "I was parsing function or method call arguments, and I expected `,` or `)`."))
		(document
			(reflow "Function call arguments go inside parentheses and are separated with commas.")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "add(1, 2)")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "2")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "parse_missing_comma_between_items.md") (start 3 13) (end 3 14) (annotation error) (line-text "sum = add(1 2)"))))
	(report
		(severity runtime_error)
		(title "Expected List Separator")
		(region (start 5 12) (end 5 13))
		(headline
			(reflow "I was parsing a list expression, and I expected `,` or `]`."))
		(document
			(reflow "Separate list elements with commas and close the list with ")
			(annotated code "]")
			(reflow ".")
			(line-break)
			(line-break)
			(text "For example:")
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[1, 2, 3]")
			(annotation-end)
			(line-break)
			(line-break)
			(text "I found ")
			(annotated code "2")
			(text " here.")
			(line-break)
			(line-break)
			(source-region (file "parse_missing_comma_between_items.md") (start 5 12) (end 5 13) (annotation error) (line-text "items = [1 2, 3]")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,Int,CloseRound,
LowerIdent,OpAssign,OpenSquare,Int,Int,Comma,Int,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "add"))
			(e-lambda
				(args
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-binop (op "+")
					(e-ident (raw "a"))
					(e-ident (raw "b")))))
		(s-decl
			(p-ident (raw "sum"))
			(e-apply
				(e-ident (raw "add"))
				(e-int (raw "1"))
				(e-int (raw "2"))))
		(s-decl
			(p-ident (raw "items"))
			(e-list
				(e-int (raw "1"))
				(e-int (raw "2"))
				(e-int (raw "3"))))))
~~~
# FORMATTED
~~~roc
add = |a, b| a + b

sum = add(1, 2)

items = [1, 2, 3]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "add"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-dispatch-call (method "plus") (constraint-fn-var 224)
				(receiver
					(e-lookup-local
						(p-assign (ident "a"))))
				(args
					(e-lookup-local
						(p-assign (ident "b")))))))
	(d-let
		(p-assign (ident "sum"))
		(e-call (constraint-fn-var 244)
			(e-lookup-local
				(p-assign (ident "add")))
			(e-num (value "1"))
			(e-num (value "2"))))
	(d-let
		(p-assign (ident "items"))
		(e-list
			(elems
				(e-num (value "1"))
				(e-num (value "2"))
				(e-num (value "3"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c, d -> c where [c.plus : c, d -> c]"))
		(patt (type "Dec"))
		(patt (type "List(Dec)")))
	(expressions
		(expr (type "c, d -> c where [c.plus : c, d -> c]"))
		(expr (type "Dec"))
		(expr (type "List(Dec)"))))
~~~
