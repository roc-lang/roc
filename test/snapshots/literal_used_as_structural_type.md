# META
~~~ini
description=Number and string literals used as records, functions, or tag unions report the literal mismatch rather than the implicit conversion method
type=snippet
~~~
# SOURCE
~~~roc
number_as_function = {
	n = 5
	n(1)
}
number_as_tag = {
	n = 5
	n == Foo
}
string_as_record = {
	s = "hi"
	s.a
}
interpolation_as_record = {
	name = "Ada"
	s = "Hi ${name}"
	s.a
}
~~~
# EXPECTED
TYPE MISMATCH - literal_used_as_structural_type.md:2:6:2:7
TYPE MISMATCH - literal_used_as_structural_type.md:6:6:6:7
TYPE MISMATCH - literal_used_as_structural_type.md:10:6:10:10
TYPE MISMATCH - literal_used_as_structural_type.md:15:6:15:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 6) (end 2 7))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "literal_used_as_structural_type.md") (start 2 6) (end 2 7) (annotation error) (line-text "\tn = 5"))
			(line-break)
			(reflow "The type was determined to be non-numeric here:")
			(line-break)
			(source-region (file "literal_used_as_structural_type.md") (start 3 2) (end 3 6) (annotation error) (line-text "\tn(1)"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "_arg -> _ret")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 6) (end 6 7))
		(headline
			(reflow "This number is being used where a non-number type is needed."))
		(document
			(source-region (file "literal_used_as_structural_type.md") (start 6 6) (end 6 7) (annotation error) (line-text "\tn = 5"))
			(line-break)
			(reflow "The type was determined to be non-numeric here:")
			(line-break)
			(source-region (file "literal_used_as_structural_type.md") (start 7 7) (end 7 10) (annotation error) (line-text "\tn == Foo"))
			(line-break)
			(reflow "Other code expects this to have the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Foo]")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 10 6) (end 10 10))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "literal_used_as_structural_type.md") (start 10 6) (end 10 10) (annotation error) (line-text "\ts = \"hi\""))
			(line-break)
			(reflow "The type was determined to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ a: _field, .. }")
			(annotation-end)))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 15 6) (end 15 18))
		(headline
			(reflow "This string literal is being used where a non-string type is needed."))
		(document
			(source-region (file "literal_used_as_structural_type.md") (start 15 6) (end 15 18) (annotation error) (line-text "\ts = \"Hi ${name}\""))
			(line-break)
			(reflow "The type was determined to be:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "{ a: _field, .. }")
			(annotation-end))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,Int,
LowerIdent,OpEquals,UpperIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,NoSpaceDotLowerIdent,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,CloseStringInterpolation,StringPart,StringEnd,
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
			(p-ident (raw "number_as_function"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "n"))
						(e-int (raw "5")))
					(e-apply
						(e-ident (raw "n"))
						(e-int (raw "1"))))))
		(s-decl
			(p-ident (raw "number_as_tag"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "n"))
						(e-int (raw "5")))
					(e-binop (op "==")
						(e-ident (raw "n"))
						(e-tag (raw "Foo"))))))
		(s-decl
			(p-ident (raw "string_as_record"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "s"))
						(e-string
							(e-string-part (raw "hi"))))
					(e-field-access
						(receiver
							(e-ident (raw "s")))
						(segment (mode "required") (field "a"))))))
		(s-decl
			(p-ident (raw "interpolation_as_record"))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "name"))
						(e-string
							(e-string-part (raw "Ada"))))
					(s-decl
						(p-ident (raw "s"))
						(e-string
							(e-string-part (raw "Hi "))
							(e-ident (raw "name"))
							(e-string-part (raw ""))))
					(e-field-access
						(receiver
							(e-ident (raw "s")))
						(segment (mode "required") (field "a"))))))))
~~~
# FORMATTED
~~~roc
number_as_function = {
	n = 5
	n(1)
}

number_as_tag = {
	n = 5
	n == Foo
}

string_as_record = {
	s = "hi"
	s.a
}

interpolation_as_record = {
	name = "Ada"
	s = "Hi ${name}"
	s.a
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "number_as_function"))
		(e-block
			(s-let
				(p-assign (ident "n"))
				(e-runtime-error (tag "erroneous_value_expr")))
			(e-call (constraint-fn-var 272)
				(e-lookup-local
					(p-assign (ident "n")))
				(e-num (value "1")))))
	(d-let
		(p-assign (ident "number_as_tag"))
		(e-block
			(s-let
				(p-assign (ident "n"))
				(e-runtime-error (tag "erroneous_value_expr")))
			(e-structural-eq (negated "false")
				(lhs
					(e-lookup-local
						(p-assign (ident "n"))))
				(rhs
					(e-tag (name "Foo"))))))
	(d-let
		(p-assign (ident "string_as_record"))
		(e-block
			(s-let
				(p-assign (ident "s"))
				(e-runtime-error (tag "erroneous_value_expr")))
			(e-field-access
				(receiver
					(e-lookup-local
						(p-assign (ident "s"))))
				(segments
					(segment (name "a") (mode "required"))))))
	(d-let
		(p-assign (ident "interpolation_as_record"))
		(e-block
			(s-let
				(p-assign (ident "name"))
				(e-string
					(e-literal (string "Ada"))))
			(s-let
				(p-assign (ident "s"))
				(e-block
					(s-let
						(p-assign (ident "#interp_0"))
						(e-lookup-local
							(p-assign (ident "name"))))
					(e-runtime-error (tag "erroneous_value_expr")
						(e-literal (string "Hi "))
						(e-lookup-local
							(p-assign (ident "#interp_0")))
						(e-literal (string "")))))
			(e-field-access
				(receiver
					(e-lookup-local
						(p-assign (ident "s"))))
				(segments
					(segment (name "a") (mode "required")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_b"))
		(patt (type "Bool"))
		(patt (type "_b"))
		(patt (type "_b")))
	(expressions
		(expr (type "_b"))
		(expr (type "Bool"))
		(expr (type "_b"))
		(expr (type "_b"))))
~~~
