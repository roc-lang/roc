# META
~~~ini
description=A lowercase builtin number type name accessed on a number literal reports the number used as a record and hints at the capitalized type suffix
type=snippet
~~~
# SOURCE
~~~roc
direct = 1.u64
parenthesized = (1).u64
fraction = 1.5.dec
other_field = 1.foo
through_name = {
	n = 5
	n.u64
}
~~~
# EXPECTED
TYPE MISMATCH - lowercase_number_type_suffix.md:1:10:1:11
TYPE MISMATCH - lowercase_number_type_suffix.md:2:18:2:19
TYPE MISMATCH - lowercase_number_type_suffix.md:3:12:3:15
TYPE MISMATCH - lowercase_number_type_suffix.md:4:15:4:16
TYPE MISMATCH - lowercase_number_type_suffix.md:6:6:6:7
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 10) (end 1 11))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "lowercase_number_type_suffix.md") (start 1 10) (end 1 11) (annotation error) (line-text "direct = 1.u64"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ u64: _field, .. }")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Type suffixes on number literals are capitalized and go directly after the number. To give this number the type")
			(reflow " ")
			(annotated type "U64")
			(reflow ", write")
			(reflow " ")
			(annotated code "1.U64")
			(reflow " ")
			(reflow "instead.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 18) (end 2 19))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "lowercase_number_type_suffix.md") (start 2 18) (end 2 19) (annotation error) (line-text "parenthesized = (1).u64"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ u64: _field, .. }")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Type suffixes on number literals are capitalized and go directly after the number. To give this number the type")
			(reflow " ")
			(annotated type "U64")
			(reflow ", write")
			(reflow " ")
			(annotated code "1.U64")
			(reflow " ")
			(reflow "instead.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 12) (end 3 15))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "lowercase_number_type_suffix.md") (start 3 12) (end 3 15) (annotation error) (line-text "fraction = 1.5.dec"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ dec: _field, .. }")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Type suffixes on number literals are capitalized and go directly after the number. To give this number the type")
			(reflow " ")
			(annotated type "Dec")
			(reflow ", write")
			(reflow " ")
			(annotated code "1.5.Dec")
			(reflow " ")
			(reflow "instead.")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 15) (end 4 16))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "lowercase_number_type_suffix.md") (start 4 15) (end 4 16) (annotation error) (line-text "other_field = 1.foo"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ foo: _field, .. }")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 6) (end 6 7))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "lowercase_number_type_suffix.md") (start 6 6) (end 6 7) (annotation error) (line-text "\tn = 5"))
			(line-break)
			(reflow "The type was determined to be non-numeric here:")
			(line-break)
			(source-region (file "lowercase_number_type_suffix.md") (start 7 2) (end 7 3) (annotation error) (line-text "\tn.u64"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ u64: _field, .. }")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,Int,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,OpenRound,Int,CloseRound,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,Float,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,Int,NoSpaceDotLowerIdent,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,NoSpaceDotLowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "direct"))
			(e-field-access
				(receiver
					(e-int (raw "1")))
				(segment (mode "required") (field "u64"))))
		(s-decl
			(p-ident (raw "parenthesized"))
			(e-field-access
				(receiver
					(e-tuple
						(e-int (raw "1"))))
				(segment (mode "required") (field "u64"))))
		(s-decl
			(p-ident (raw "fraction"))
			(e-field-access
				(receiver
					(e-frac (raw "1.5")))
				(segment (mode "required") (field "dec"))))
		(s-decl
			(p-ident (raw "other_field"))
			(e-field-access
				(receiver
					(e-int (raw "1")))
				(segment (mode "required") (field "foo"))))
		(s-decl
			(p-ident (raw "through_name"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "n"))
						(e-int (raw "5")))
					(e-field-access
						(receiver
							(e-ident (raw "n")))
						(segment (mode "required") (field "u64"))))))))
~~~
# FORMATTED
~~~roc
direct = (1).u64

parenthesized = (1).u64

fraction = (1.5).dec

other_field = (1).foo

through_name = {
	n = 5
	n.u64
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "direct"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(segments
				(segment (name "u64") (mode "required")))))
	(d-let
		(p-assign (ident "parenthesized"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(segments
				(segment (name "u64") (mode "required")))))
	(d-let
		(p-assign (ident "fraction"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(segments
				(segment (name "dec") (mode "required")))))
	(d-let
		(p-assign (ident "other_field"))
		(e-field-access
			(receiver
				(e-runtime-error (tag "erroneous_value_expr")))
			(segments
				(segment (name "foo") (mode "required")))))
	(d-let
		(p-assign (ident "through_name"))
		(e-block
			(s-let
				(p-assign (ident "n"))
				(e-runtime-error (tag "erroneous_value_expr")))
			(e-field-access
				(receiver
					(e-lookup-local
						(p-assign (ident "n"))))
				(segments
					(segment (name "u64") (mode "required")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a"))
		(patt (type "_a")))
	(expressions
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))
		(expr (type "_a"))))
~~~
