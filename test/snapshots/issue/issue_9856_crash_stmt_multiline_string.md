# META
~~~ini
description=Crash statements accept multiline string expressions
type=file
~~~
# SOURCE
~~~roc
main! = |_| {
    crash
         \\This does not
         \\work.

    Ok({})
}
~~~
# EXPECTED
UNREACHABLE CODE - issue_9856_crash_stmt_multiline_string.md:6:5:6:11
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Unreachable Code")
		(region (start 6 5) (end 6 11))
		(headline
			(reflow "This code will never run, because the code before it always ends with a ")
			(annotated code "return")
			(reflow ", ")
			(annotated code "crash")
			(reflow ", or ")
			(annotated code "break")
			(reflow ", or loops forever."))
		(document
			(source-region (file "issue_9856_crash_stmt_multiline_string.md") (start 6 5) (end 6 11) (annotation error) (line-text "    Ok({})"))
			(reflow "Remove this code, or change the code before it so that it can finish."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
KwCrash,
MultilineStringStart,StringPart,
MultilineStringStart,StringPart,
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
					(p-underscore))
				(e-block
					(statements
						(s-crash
							(e-multiline-string
								(e-string-part (raw "This does not"))
								(e-string-part (raw "work."))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
main! = |_| {
	crash
		\\This does not
		\\work.

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
				(p-underscore))
			(e-block
				(s-expr
					(e-run-low-level (op "crash")
						(args
							(e-string
								(e-literal (string "This does not
work."))))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "_arg -> [Ok({})]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "_arg -> [Ok({})]"))))
~~~
