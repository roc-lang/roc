# META
~~~ini
description=Issue #10056: Unstable formatting with escaped record literal header
type=file
~~~
# SOURCE
~~~roc
d=(0->X .a)
~~~
# EXPECTED
TYPE MISMATCH - fuzz_crash_092.md:1:4:1:8
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 4) (end 1 8))
		(headline
			(reflow "This is not a record, so it does not have any fields to access."))
		(document
			(source-region (file "fuzz_crash_092.md") (start 1 4) (end 1 8) (annotation error) (line-text "d=(0->X .a)"))
			(line-break)
			(reflow "It is:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[X(b), ..] where [b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)])]")
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
LowerIdent,OpAssign,NoSpaceOpenRound,Int,OpArrow,UpperIdent,DotLowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "d"))
			(e-tuple
				(e-field-access
					(receiver
						(e-arrow-call
							(e-int (raw "0"))
							(e-tag (raw "X"))))
					(segment (mode "required") (field "a")))))))
~~~
# FORMATTED
~~~roc
d = ((0 |> X).a)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "d"))
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
