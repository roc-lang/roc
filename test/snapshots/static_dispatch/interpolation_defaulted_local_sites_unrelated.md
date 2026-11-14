# META
~~~ini
description=Interpolations assigned to one local that defaults to Str are each checked against Str, so each rejected value is reported where it appears
type=file
~~~
# SOURCE
~~~roc
f : I64 -> Str
f = |n| {
	var $o = ""
	$o = "a ${n.to_str()}"
	$o = "b ${Ok(n)}"
	$o = "c ${Err(n)}"
	"done"
}

main! = |_args| {
	echo!(f(1))
	Ok({})
}
~~~
# EXPECTED
UNUSED VARIABLE - interpolation_defaulted_local_sites_unrelated.md:3:6:3:8
TYPE MISMATCH - interpolation_defaulted_local_sites_unrelated.md:5:12:5:17
TYPE MISMATCH - interpolation_defaulted_local_sites_unrelated.md:6:12:6:18
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 3 6) (end 3 8))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "$o")
			(reflow " is defined here and then never used."))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_$o")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "interpolation_defaulted_local_sites_unrelated.md") (start 3 6) (end 3 8) (annotation error) (line-text "\tvar $o = \"\""))))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 5 12) (end 5 17))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "interpolation_defaulted_local_sites_unrelated.md") (start 5 12) (end 5 17) (annotation error) (line-text "\t$o = \"b ${Ok(n)}\""))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Ok(I64)]")
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
			(reflow "To get a representation of this value for debugging, use")
			(reflow " ")
			(annotated code "Str.inspect")
			(reflow ".")))
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 6 12) (end 6 18))
		(headline
			(reflow "This expression is used in an unexpected way."))
		(document
			(source-region (file "interpolation_defaulted_local_sites_unrelated.md") (start 6 12) (end 6 18) (annotation error) (line-text "\t$o = \"c ${Err(n)}\""))
			(line-break)
			(reflow "It has the type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "[Err(I64)]")
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
			(reflow "To get a representation of this value for debugging, use")
			(reflow " ")
			(annotated code "Str.inspect")
			(reflow "."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,StringStart,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
LowerIdent,OpAssign,StringStart,StringPart,OpenStringInterpolation,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseStringInterpolation,StringPart,StringEnd,
StringStart,StringPart,StringEnd,
CloseCurly,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,Int,CloseRound,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "f")
			(ty-fn
				(ty (name "I64"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-block
					(statements
						(s-var (name "$o")
							(e-string
								(e-string-part (raw ""))))
						(s-decl
							(p-ident (raw "$o"))
							(e-string
								(e-string-part (raw "a "))
								(e-method-call (method ".to_str")
									(receiver
										(e-ident (raw "n")))
									(args))
								(e-string-part (raw ""))))
						(s-decl
							(p-ident (raw "$o"))
							(e-string
								(e-string-part (raw "b "))
								(e-apply
									(e-tag (raw "Ok"))
									(e-ident (raw "n")))
								(e-string-part (raw ""))))
						(s-decl
							(p-ident (raw "$o"))
							(e-string
								(e-string-part (raw "c "))
								(e-apply
									(e-tag (raw "Err"))
									(e-ident (raw "n")))
								(e-string-part (raw ""))))
						(e-string
							(e-string-part (raw "done")))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-ident (raw "_args")))
				(e-block
					(statements
						(e-apply
							(e-ident (raw "echo!"))
							(e-apply
								(e-ident (raw "f"))
								(e-int (raw "1"))))
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
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-block
				(s-var
					(p-var-assign (ident "$o"))
					(e-string
						(e-literal (string ""))))
				(s-reassign
					(p-var-assign (ident "$o"))
					(e-block
						(s-let
							(p-assign (ident "#interp_0"))
							(e-dispatch-call (method "to_str") (constraint-fn-var 296)
								(receiver
									(e-lookup-local
										(p-assign (ident "n"))))
								(args)))
						(e-interpolation (constraint-fn-var 317) (dispatcher-var 26)
							(first
								(e-literal (string "a ")))
							(parts
								(e-lookup-local
									(p-assign (ident "#interp_0")))
								(e-literal (string ""))))))
				(s-reassign
					(p-var-assign (ident "$o"))
					(e-block
						(s-let
							(p-assign (ident "#interp_1"))
							(e-tag (name "Ok")
								(args
									(e-lookup-local
										(p-assign (ident "n"))))))
						(e-runtime-error (tag "erroneous_value_expr")
							(e-literal (string "b "))
							(e-lookup-local
								(p-assign (ident "#interp_1")))
							(e-literal (string "")))))
				(s-reassign
					(p-var-assign (ident "$o"))
					(e-block
						(s-let
							(p-assign (ident "#interp_2"))
							(e-tag (name "Err")
								(args
									(e-lookup-local
										(p-assign (ident "n"))))))
						(e-runtime-error (tag "erroneous_value_expr")
							(e-literal (string "c "))
							(e-lookup-local
								(p-assign (ident "#interp_2")))
							(e-literal (string "")))))
				(e-string
					(e-literal (string "done")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "_args")))
			(e-block
				(s-expr
					(e-call (constraint-fn-var 417)
						(e-lookup-local
							(p-assign (ident "echo!")))
						(e-call (constraint-fn-var 416)
							(e-lookup-local
								(p-assign (ident "f")))
							(e-num (value "1")))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "I64 -> Str"))
		(patt (type "_arg => [Ok({})]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "I64 -> Str"))
		(expr (type "_arg => [Ok({})]"))))
~~~
