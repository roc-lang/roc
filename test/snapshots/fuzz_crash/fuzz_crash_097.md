# META
~~~ini
description=Parser formatting instability (multiline tuple vs lambda)
type=file
~~~
# SOURCE
~~~roc
a=(0(0->X)
->X .a)
~~~
# EXPECTED
MISSING METHOD - fuzz_crash_097.md:1:4:1:5
TYPE MISMATCH - fuzz_crash_097.md:1:4:2:4
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Missing Method")
		(region (start 1 4) (end 1 5))
		(headline
			(reflow "This")
			(reflow " ")
			(annotated code "from_numeral")
			(reflow " ")
			(reflow "method is being called on a value whose type doesn't have that method."))
		(document
			(source-region (file "fuzz_crash_097.md") (start 1 4) (end 1 5) (annotation error) (line-text "a=(0(0->X)"))
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
			(text "[X(b), ..] -> _ret")
			(line-break)
			(indent 1)
			(text "  where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 4) (end 2 4))
		(headline
			(reflow "This is not a record, so it does not have any fields to access."))
		(document
			(source-region (file "fuzz_crash_097.md") (start 1 4) (end 2 4) (annotation error) (line-text "a=(0(0->X)\n->X .a)"))
			(line-break)
			(reflow "It is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[X(_b), ..]")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But I need a record with a")
			(reflow " ")
			(annotated code "a")
			(reflow " ")
			(reflow "field."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,NoSpaceOpenRound,Int,NoSpaceOpenRound,Int,OpArrow,UpperIdent,CloseRound,
OpArrow,UpperIdent,DotLowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "a"))
			(e-tuple
				(e-field-access
					(receiver
						(e-arrow-call
							(e-apply
								(e-int (raw "0"))
								(e-arrow-call
									(e-int (raw "0"))
									(e-tag (raw "X"))))
							(e-tag (raw "X"))))
					(segment (mode "required") (field "a")))))))
~~~
# FORMATTED
~~~roc
a = (
	0(0 |> X)
		|> X
		.a
)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "a"))
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
