# META
~~~ini
description=repro for https://github.com/roc-lang/roc/issues/9962 - effectful functions should warn without bang names
type=file
~~~
# SOURCE
~~~roc
topThunk = || echo!("top")

main! = |_| {
    thunk = || echo!("local")
    thunk()
    topThunk()
    Ok({})
}
~~~
# EXPECTED
EFFECTFUL FUNCTION NAME - repro_issue_9962_effectful_local_function_name.md:1:1:1:9
EFFECTFUL FUNCTION NAME - repro_issue_9962_effectful_local_function_name.md:4:5:4:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Effectful Function Name")
		(region (start 1 1) (end 1 9))
		(headline
			(reflow "This function performs an effect, so its name must end in `!`."))
		(document
			(source-region (file "repro_issue_9962_effectful_local_function_name.md") (start 1 1) (end 1 9) (annotation warning) (line-text "topThunk = || echo!(\"top\")"))
			(line-break)
			(line-break)
			(reflow "Add a trailing")
			(reflow " ")
			(annotated code "!")
			(reflow " ")
			(reflow "to this function name.")))
	(report
		(severity warning)
		(title "Effectful Function Name")
		(region (start 4 5) (end 4 10))
		(headline
			(reflow "This function performs an effect, so its name must end in `!`."))
		(document
			(source-region (file "repro_issue_9962_effectful_local_function_name.md") (start 4 5) (end 4 10) (annotation warning) (line-text "    thunk = || echo!(\"local\")"))
			(line-break)
			(line-break)
			(reflow "Add a trailing")
			(reflow " ")
			(annotated code "!")
			(reflow " ")
			(reflow "to this function name."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,OpBar,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,OpBar,LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,NoSpaceOpenRound,CloseRound,
LowerIdent,NoSpaceOpenRound,CloseRound,
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
			(p-ident (raw "topThunk"))
			(e-lambda
				(args)
				(e-apply
					(e-ident (raw "echo!"))
					(e-string
						(e-string-part (raw "top"))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "thunk"))
							(e-lambda
								(args)
								(e-apply
									(e-ident (raw "echo!"))
									(e-string
										(e-string-part (raw "local"))))))
						(e-apply
							(e-ident (raw "thunk")))
						(e-apply
							(e-ident (raw "topThunk")))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
topThunk = || echo!("top")

main! = |_| {
	thunk = || echo!("local")
	thunk()
	topThunk()
	Ok({})
}
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
		(p-assign (ident "topThunk"))
		(e-lambda
			(args)
			(e-call (constraint-fn-var 257)
				(e-lookup-local
					(p-assign (ident "echo!")))
				(e-string
					(e-literal (string "top"))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-assign (ident "thunk"))
					(e-lambda
						(args)
						(e-call (constraint-fn-var 266)
							(e-lookup-local
								(p-assign (ident "echo!")))
							(e-string
								(e-literal (string "local"))))))
				(s-expr
					(e-call (constraint-fn-var 267)
						(e-lookup-local
							(p-assign (ident "thunk")))))
				(s-expr
					(e-call (constraint-fn-var 269)
						(e-lookup-local
							(p-assign (ident "topThunk")))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "({}) => {}"))
		(patt (type "_arg => [Ok({}), ..]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "({}) => {}"))
		(expr (type "_arg => [Ok({}), ..]"))))
~~~
