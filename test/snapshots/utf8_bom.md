# META
~~~ini
description=A leading UTF-8 BOM produces an actionable encoding diagnostic (issue 11869)
type=file
~~~
# SOURCE
~~~roc
﻿
main! = |_args| {
    echo!("ok")
    Ok({})
}
~~~
# EXPECTED
UTF-8 BYTE ORDER MARK - utf8_bom.md:1:1:1:4
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "UTF-8 Byte Order Mark")
		(region (start 1 1) (end 1 4))
		(headline
			(reflow "This file starts with a UTF-8 byte order mark (BOM), an invisible encoding marker. Roc source files must use UTF-8 without a BOM. Remove the BOM or save the file as UTF-8 without BOM in your editor."))
		(document
			(source-region (file "utf8_bom.md") (start 1 1) (end 1 4) (annotation error) (line-text "﻿")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
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
						(e-apply
							(e-ident (raw "echo!"))
							(e-string
								(e-string-part (raw "ok"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
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
				(s-expr
					(e-call (constraint-fn-var 250)
						(e-lookup-local
							(p-assign (ident "echo!")))
						(e-string
							(e-literal (string "ok")))))
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
