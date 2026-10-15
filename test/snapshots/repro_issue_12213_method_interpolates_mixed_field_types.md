# META
~~~ini
description=A method that interpolates a Str field beside a U64 field reports that the interpolation's values must share one type, pointing at the interpolation and the call
type=file
~~~
# SOURCE
~~~roc
Person := { name : Str, age : U64 }.{
	describe = |person| "${person.name} is ${person.age} years old"
}

main! = |_args| {
	ada = Person.{ name: "Ada", age: 36 }
	echo!(ada.describe())
	Ok({})
}
~~~
# EXPECTED
TYPE MISMATCH - repro_issue_12213_method_interpolates_mixed_field_types.md:2:22:2:65
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 2 22) (end 2 65))
		(headline
			(reflow "The values interpolated into this string must all have the same type."))
		(document
			(source-region (file "repro_issue_12213_method_interpolates_mixed_field_types.md") (start 2 22) (end 2 65) (annotation error) (line-text "\tdescribe = |person| \"${person.name} is ${person.age} years old\""))
			(line-break)
			(reflow "But this use gives them different types:")
			(line-break)
			(source-region (file "repro_issue_12213_method_interpolates_mixed_field_types.md") (start 7 8) (end 7 22) (annotation error) (line-text "\techo!(ada.describe())"))
			(line-break)
			(line-break)
			(reflow "One of them has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U64")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But another has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "Str")
			(annotation-end)
			(line-break)
			(line-break)
			(annotated emphasis "Hint:")
			(reflow " ")
			(reflow "Convert the values to one type before interpolating them, for example with")
			(reflow " ")
			(annotated code ".to_str()")
			(reflow "."))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,CloseStringInterpolation,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,Dot,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Person")
				(args))
			(ty-record
				(anno-record-field (name "name")
					(ty (name "Str")))
				(anno-record-field (name "age")
					(ty (name "U64"))))
			(associated
				(s-decl
					(p-ident (raw "describe"))
					(e-lambda
						(args
							(p-ident (raw "person")))
						(e-string
							(e-string-part (raw ""))
							(e-field-access
								(receiver
									(e-ident (raw "person")))
								(segment (mode "required") (field "name")))
							(e-string-part (raw " is "))
							(e-field-access
								(receiver
									(e-ident (raw "person")))
								(segment (mode "required") (field "age")))
							(e-string-part (raw " years old")))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "_args")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "ada"))
							(e-nominal-record
								(mapper (e-tag (raw "Person")))
								(backing (e-record
										(field (field "name")
											(e-string
												(e-string-part (raw "Ada"))))
										(field (field "age")
											(e-int (raw "36")))))))
						(e-apply
							(e-ident (raw "echo!"))
							(e-method-call (method ".describe")
								(receiver
									(e-ident (raw "ada")))
								(args)))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "echo!"))
		(e-hosted-lambda (symbol "echo!")
			(args
				(p-assign (ident "_echo_arg"))))
		(annotation
			(ty-fn (effectful true)
				(ty-lookup (name "Str") (builtin))
				(ty-record))))
	(d-let
		(p-assign (ident "repro_issue_12213_method_interpolates_mixed_field_types.Person.describe"))
		(e-lambda
			(args
				(p-assign (ident "person")))
			(e-block
				(s-let
					(p-assign (ident "#interp_0"))
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "person"))))
						(segments
							(segment (name "name") (mode "required")))))
				(s-let
					(p-assign (ident "#interp_1"))
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "person"))))
						(segments
							(segment (name "age") (mode "required")))))
				(e-interpolation (constraint-fn-var 299) (dispatcher-var 33)
					(first
						(e-literal (string "")))
					(parts
						(e-lookup-local
							(p-assign (ident "#interp_0")))
						(e-literal (string " is "))
						(e-lookup-local
							(p-assign (ident "#interp_1")))
						(e-literal (string " years old")))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "_args")))
			(e-block
				(s-let
					(p-assign (ident "ada"))
					(e-nominal (nominal "Person")
						(e-record
							(fields
								(field (name "name")
									(e-string
										(e-literal (string "Ada"))))
								(field (name "age")
									(e-num (value "36")))))))
				(s-expr
					(e-call (constraint-fn-var 351)
						(e-lookup-local
							(p-assign (ident "echo!")))
						(e-runtime-error (tag "erroneous_value_expr")
							(e-lookup-local
								(p-assign (ident "ada"))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record))))))
	(s-nominal-decl
		(ty-header (name "Person"))
		(ty-record
			(field (field "name")
				(ty-lookup (name "Str") (builtin)))
			(field (field "age")
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "{ age: a, name: a, .. } -> b where [b.from_interpolation : List(Str) -> Try(List(a) -> b, [InvalidInterpolation(Str)])]"))
		(patt (type "_arg => [Ok({})]")))
	(type_decls
		(nominal (type "Person")
			(ty-header (name "Person"))))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "{ age: a, name: a, .. } -> b where [b.from_interpolation : List(Str) -> Try(List(a) -> b, [InvalidInterpolation(Str)])]"))
		(expr (type "_arg => [Ok({})]"))))
~~~
