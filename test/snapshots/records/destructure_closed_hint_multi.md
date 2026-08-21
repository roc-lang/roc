# META
~~~ini
description=A too-narrow record destructure lists every unbound field and suggests `field: _` or `..`
type=snippet
~~~
# SOURCE
~~~roc
compute : U64
compute = {
    { x } = { x: 1, y: 2, z: 3 }
    x
}
~~~
# EXPECTED
TYPE MISMATCH - destructure_closed_hint_multi.md:3:13:3:33
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 13) (end 3 33))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "destructure_closed_hint_multi.md") (start 3 13) (end 3 33) (annotation error) (line-text "    { x } = { x: 1, y: 2, z: 3 }"))
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
			(text "{ x: a } where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]")
			(annotation-end)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This pattern doesn't bind these fields:")
			(line-break)
			(reflow " -")
			(reflow " ")
			(annotated code "y")
			(line-break)
			(reflow " -")
			(reflow " ")
			(annotated code "z")
			(line-break)
			(reflow "Match them explicitly with")
			(reflow " ")
			(annotated code "y: _")
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
OpenCurly,LowerIdent,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,
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
							(field (name "x") (rest false)))
						(e-record
							(field (field "x")
								(e-int (raw "1")))
							(field (field "y")
								(e-int (raw "2")))
							(field (field "z")
								(e-int (raw "3")))))
					(e-ident (raw "x")))))))
~~~
# FORMATTED
~~~roc
compute : U64
compute = {
	{ x } = { x: 1, y: 2, z: 3 }
	x
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
								(p-assign (ident "x"))))))
				(e-record
					(fields
						(field (name "x")
							(e-num (value "1")))
						(field (name "y")
							(e-num (value "2")))
						(field (name "z")
							(e-num (value "3"))))))
			(e-lookup-local
				(p-assign (ident "x"))))
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
