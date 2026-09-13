# META
~~~ini
description=A for loop pattern must be exhaustive for every item produced by its iterator
type=file
~~~
# SOURCE
~~~roc
main! = |_args| {
    for 1 in [1, 2] {}
    Ok({})
}
~~~
# EXPECTED
NON EXHAUSTIVE DESTRUCTURE - for_loop_refutable_literal_pattern.md:2:9:2:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Non Exhaustive Destructure")
		(region (start 2 9) (end 2 10))
		(headline
			(reflow "This destructuring pattern doesn't cover all possible cases."))
		(document
			(source-region (file "for_loop_refutable_literal_pattern.md") (start 2 9) (end 2 10) (annotation error) (line-text "    for 1 in [1, 2] {}"))
			(line-break)
			(reflow "The value being destructured has type:")
			(line-break)
			(text "        ")
			(annotated type "Dec")
			(line-break)
			(line-break)
			(reflow "Missing patterns:")
			(line-break)
			(text "    ")
			(annotation-start code-block)
			(indent 1)
			(text "_")
			(annotation-end)
			(line-break))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
KwFor,Int,KwIn,OpenSquare,Int,Comma,Int,CloseSquare,OpenCurly,CloseCurly,
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
							(p-int (raw "1"))
							(e-list
								(e-int (raw "1"))
								(e-int (raw "2")))
							(e-record))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
main! = |_args| {
	for 1 in [1, 2] {}
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
					(p-num (value "1"))
					(e-list
						(elems
							(e-num (value "1"))
							(e-num (value "2"))))
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
