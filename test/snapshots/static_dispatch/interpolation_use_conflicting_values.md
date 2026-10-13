# META
~~~ini
description=A use that gives a generalized interpolation's values different types reports the interpolation and the use once, not each value at the definition
type=file
~~~
# SOURCE
~~~roc
describe = |person| "${person.name} is ${person.age} years old"

main! = |_args| {
	name : Str
	name = "Ada"
	echo!(describe({ name, age: 36.U64 }))
	Ok({})
}
~~~
# EXPECTED
TYPE MISMATCH - interpolation_use_conflicting_values.md:1:21:1:64
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 1 21) (end 1 64))
		(headline
			(reflow "The values interpolated into this string must all have the same type."))
		(document
			(source-region (file "interpolation_use_conflicting_values.md") (start 1 21) (end 1 64) (annotation error) (line-text "describe = |person| \"${person.name} is ${person.age} years old\""))
			(line-break)
			(reflow "But this use gives them different types:")
			(line-break)
			(source-region (file "interpolation_use_conflicting_values.md") (start 6 8) (end 6 16) (annotation error) (line-text "\techo!(describe({ name, age: 36.U64 }))"))
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,StringStart,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,CloseStringInterpolation,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,Comma,LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,CloseCurly,CloseRound,CloseRound,
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
					(e-string-part (raw " years old")))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "_args")))
				(e-block
					(statements
						(s-type-anno (name "name")
							(ty (name "Str")))
						(s-decl
							(p-ident (raw "name"))
							(e-string
								(e-string-part (raw "Ada"))))
						(e-apply
							(e-ident (raw "echo!"))
							(e-apply
								(e-ident (raw "describe"))
								(e-record
									(field (field "name"))
									(field (field "age")
										(e-typed-int (raw "36") (type "U64"))))))
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
		(p-assign (ident "describe"))
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
				(e-interpolation (constraint-fn-var 290) (dispatcher-var 26)
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
					(p-assign (ident "name"))
					(e-string
						(e-literal (string "Ada"))))
				(s-runtime-error (tag "erroneous_value_expr")
					(e-runtime-error (tag "erroneous_value_expr")
						(e-lookup-local
							(p-assign (ident "echo!")))
						(e-runtime-error (tag "erroneous_value_expr"))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "{ age: a, name: a, .. } -> b where [b.from_interpolation : List(Str) -> Try(List(a) -> b, [InvalidInterpolation(Str)])]"))
		(patt (type "_arg -> [Ok({})]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "{ age: a, name: a, .. } -> b where [b.from_interpolation : List(Str) -> Try(List(a) -> b, [InvalidInterpolation(Str)])]"))
		(expr (type "_arg -> [Ok({})]"))))
~~~
