# META
~~~ini
description=Record destructure assignment without `..` is closed: an extra field in the value is a type mismatch
type=snippet
~~~
# SOURCE
~~~roc
compute : U64
compute = {
    { x, y } = { x: 1, y: 2, z: 3 }
    x + y
}
~~~
# EXPECTED
TYPE MISMATCH - destructure_closed_assignment.md:3:16:3:36
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 16) (end 3 36))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "destructure_closed_assignment.md") (start 3 16) (end 3 36) (annotation error) (line-text "    { x, y } = { x: 1, y: 2, z: 3 }"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ x: a, y: b, z: c }")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "    b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "    c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "  ]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ x: a, y: b }")
			(line-break)
			(indent 1)
			(text "  where [")
			(line-break)
			(indent 1)
			(text "    a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "    b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]),")
			(line-break)
			(indent 1)
			(text "  ]")
			(annotation-end)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This pattern doesn't bind the")
			(reflow " ")
			(annotated code "z")
			(reflow " ")
			(reflow "field. Match it explicitly with")
			(reflow " ")
			(annotated code "z: _")
			(reflow ",")
			(reflow " ")
			(reflow "or add")
			(reflow " ")
			(annotated code "..")
			(reflow " ")
			(reflow "to match all the remaining fields."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "compute")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "compute"))
			(e-block
				(statements
					(s-decl
						(p-record
							(field (name "x") (rest false))
							(field (name "y") (rest false)))
						(e-record
							(field (field "x")
								(e-int (raw "1")))
							(field (field "y")
								(e-int (raw "2")))
							(field (field "z")
								(e-int (raw "3")))))
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-ident (raw "y"))))))))
~~~
# FORMATTED
~~~roc
compute : U64
compute = {
	{ x, y } = { x: 1, y: 2, z: 3 }
	x + y
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "compute"))
		(e-block
			(s-let
				(p-record-destructure
					(destructs
						(record-destruct (label "x") (ident "x")
							(required
								(p-assign (ident "x"))))
						(record-destruct (label "y") (ident "y")
							(required
								(p-assign (ident "y"))))))
				(e-record
					(fields
						(field (name "x")
							(e-num (value "1")))
						(field (name "y")
							(e-num (value "2")))
						(field (name "z")
							(e-num (value "3"))))))
			(e-dispatch-call (method "plus") (constraint-fn-var 257)
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args
					(e-lookup-local
						(p-assign (ident "y"))))))
		(annotation
			(ty-lookup (name "U64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
