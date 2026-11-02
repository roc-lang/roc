# META
~~~ini
description=A Str interpolation checks each value against Str, so a U64 value never retypes another value's string literal
type=file
~~~
# SOURCE
~~~roc
main! = |_args| {
	p = { name: "Ada", age: 36.U64 }
	echo!("${p.name} is ${p.age} years old")
	Ok({})
}
~~~
# EXPECTED
TYPE MISMATCH - interpolation_str_target_decides_parts.md:3:24:3:29
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 3 24) (end 3 29))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "interpolation_str_target_decides_parts.md") (start 3 24) (end 3 29) (annotation error) (line-text "\techo!(\"${p.name} is ${p.age} years old\")"))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "U64")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "But you are trying to use it as:")
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
			(reflow "Convert it to a")
			(reflow " ")
			(annotated code "Str")
			(reflow " ")
			(reflow "with")
			(reflow " ")
			(annotated code ".to_str()")
			(reflow "."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,Comma,LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,CloseCurly,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,CloseStringInterpolation,StringPart,StringEnd,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "_args")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "p"))
							(e-record
								(field (field "name")
									(e-string
										(e-string-part (raw "Ada"))))
								(field (field "age")
									(e-typed-int (raw "36") (type "U64")))))
						(e-apply
							(e-ident (raw "echo!"))
							(e-string
								(e-string-part (raw ""))
								(e-field-access
									(receiver
										(e-ident (raw "p")))
									(segment (mode "required") (field "name")))
								(e-string-part (raw " is "))
								(e-field-access
									(receiver
										(e-ident (raw "p")))
									(segment (mode "required") (field "age")))
								(e-string-part (raw " years old"))))
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
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "_args")))
			(e-block
				(s-let
					(p-assign (ident "p"))
					(e-record
						(fields
							(field (name "name")
								(e-string
									(e-literal (string "Ada"))))
							(field (name "age")
								(e-typed-int (value "36") (type "U64"))))))
				(s-expr
					(e-call (constraint-fn-var 301)
						(e-lookup-local
							(p-assign (ident "echo!")))
						(e-block
							(s-let
								(p-assign (ident "#interp_0"))
								(e-field-access
									(receiver
										(e-lookup-local
											(p-assign (ident "p"))))
									(segments
										(segment (name "name") (mode "required")))))
							(s-let
								(p-assign (ident "#interp_1"))
								(e-field-access
									(receiver
										(e-lookup-local
											(p-assign (ident "p"))))
									(segments
										(segment (name "age") (mode "required")))))
							(e-runtime-error (tag "erroneous_value_expr")
								(e-literal (string ""))
								(e-lookup-local
									(p-assign (ident "#interp_0")))
								(e-literal (string " is "))
								(e-lookup-local
									(p-assign (ident "#interp_1")))
								(e-literal (string " years old"))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "_arg => [Ok({})]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "_arg => [Ok({})]"))))
~~~
