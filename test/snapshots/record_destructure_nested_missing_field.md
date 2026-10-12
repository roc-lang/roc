# META
~~~ini
description=A nested record destructure missing a field rejects its binding statement
type=snippet
~~~
# SOURCE
~~~roc
run = || {
	{ a: { b } } = { a: {} }
	b
}
~~~
# EXPECTED
TYPE MISMATCH - record_destructure_nested_missing_field.md:2:4:2:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 4) (end 2 12))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "record_destructure_nested_missing_field.md") (start 2 4) (end 2 12) (annotation error) (line-text "\t{ a: { b } } = { a: {} }"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{}")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ b: _field }")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "This record is missing the field:")
			(reflow " ")
			(annotated code "b"))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,OpenCurly,
OpenCurly,LowerIdent,OpColon,OpenCurly,LowerIdent,CloseCurly,CloseCurly,OpAssign,OpenCurly,LowerIdent,OpColon,OpenCurly,CloseCurly,CloseCurly,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "run"))
			(e-lambda
				(args)
				(e-block
					(statements
						(s-decl
							(p-record
								(field (name "a") (rest false)
									(p-record
										(field (name "b") (rest false)))))
							(e-record
								(field (field "a")
									(e-record))))
						(e-ident (raw "b"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "run"))
		(e-lambda
			(args)
			(e-block
				(s-runtime-error (tag "erroneous_value_expr"))
				(e-runtime-error (tag "erroneous_value_use"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> _ret")))
	(expressions
		(expr (type "({}) -> _ret"))))
~~~
