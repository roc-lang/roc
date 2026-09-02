# META
~~~ini
description=An app header with no platform canonicalizes as a default app, so echo! is in scope
type=file
~~~
# SOURCE
~~~roc
app [main!] { unicode: "https://example.com/unicode.tar.zst" }

main! = |_args| {
    echo!("hello")
    Ok({})
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
LowerIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(packages
			(record-field (name "unicode")
				(e-string
					(e-string-part (raw "https://example.com/unicode.tar.zst"))))))
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
								(e-string-part (raw "hello"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
app [main!] { unicode: "https://example.com/unicode.tar.zst" }

main! = |_args| {
	echo!("hello")
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
				(s-expr
					(e-call (constraint-fn-var 242)
						(e-lookup-local
							(p-assign (ident "echo!")))
						(e-string
							(e-literal (string "hello")))))
				(e-tag (name "Ok")
					(args
						(e-empty_record)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "_arg => [Ok({}), ..]")))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "_arg => [Ok({}), ..]"))))
~~~
