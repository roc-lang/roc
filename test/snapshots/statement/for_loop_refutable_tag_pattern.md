# META
~~~ini
description=A for loop tag pattern must cover every tag in the iterator item type
type=file
~~~
# SOURCE
~~~roc
main! = |_args| {
    for Ok(_value) in [Ok(1), Err("bad")] {}
    Ok({})
}
~~~
# EXPECTED
NON EXHAUSTIVE DESTRUCTURE - for_loop_refutable_tag_pattern.md:2:9:2:19
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Non Exhaustive Destructure")
		(region (start 2 9) (end 2 19))
		(headline
			(reflow "This destructuring pattern doesn't cover all possible cases."))
		(document
			(source-region (file "for_loop_refutable_tag_pattern.md") (start 2 9) (end 2 19) (annotation error) (line-text "    for Ok(_value) in [Ok(1), Err(\"bad\")] {}"))
			(line-break)
			(reflow "The value being destructured has type:")
			(line-break)
			(text "        ")
			(annotated type "[Err(Str), Ok(Dec), ..]")
			(line-break)
			(line-break)
			(reflow "Missing patterns:")
			(line-break)
			(text "    ")
			(annotation-start code-block)
			(indent 1)
			(text "Err _")
			(annotation-end)
			(line-break))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
KwFor,UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,KwIn,OpenSquare,UpperIdent,NoSpaceOpenRound,Int,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,CloseSquare,OpenCurly,CloseCurly,
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
						(s-for
							(p-tag (raw "Ok")
								(p-ident (raw "_value")))
							(e-list
								(e-apply
									(e-tag (raw "Ok"))
									(e-int (raw "1")))
								(e-apply
									(e-tag (raw "Err"))
									(e-string
										(e-string-part (raw "bad")))))
							(e-record))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
main! = |_args| {
	for Ok(_value) in [Ok(1), Err("bad")] {}
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
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-assign (ident "_args")))
			(e-block
				(s-for
					(p-applied-tag)
					(e-list
						(elems
							(e-tag (name "Ok")
								(args
									(e-num (value "1"))))
							(e-tag (name "Err")
								(args
									(e-string
										(e-literal (string "bad")))))))
					(e-empty_record))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "_arg -> [Ok({}), ..]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "_arg -> [Ok({}), ..]"))))
~~~
