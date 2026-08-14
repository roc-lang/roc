# META
~~~ini
description=Polarity - a tag union in an output position is implicitly open, so a function body may produce tags the annotation does not list, and `[E]` means the same as `[E, ..]`
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

parse : Str -> Try(U8, [InvalidU8])
parse = |input| {
    if Str.is_empty(input) {
        Err(EmptyInput)
    } else {
        Err(InvalidU8)
    }
}

main! = |_| {}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwIf,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpenCurly,
UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
CloseCurly,KwElse,OpenCurly,
UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(app
		(provides
			(exposed-lower-ident
				(text "main!")))
		(record-field (name "pf")
			(e-string
				(e-string-part (raw "../basic-cli/main.roc"))))
		(packages
			(record-field (name "pf")
				(e-string
					(e-string-part (raw "../basic-cli/main.roc"))))))
	(statements
		(s-type-anno (name "parse")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U8"))
					(ty-tag-union
						(tags
							(ty (name "InvalidU8")))))))
		(s-decl
			(p-ident (raw "parse"))
			(e-lambda
				(args
					(p-ident (raw "input")))
				(e-block
					(statements
						(e-if-then-else
							(e-apply
								(e-ident (raw "Str.is_empty"))
								(e-ident (raw "input")))
							(e-block
								(statements
									(e-apply
										(e-tag (raw "Err"))
										(e-tag (raw "EmptyInput")))))
							(e-block
								(statements
									(e-apply
										(e-tag (raw "Err"))
										(e-tag (raw "InvalidU8"))))))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-record)))))
~~~
# FORMATTED
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

parse : Str -> Try(U8, [InvalidU8])
parse = |input| {
	if Str.is_empty(input) {
		Err(EmptyInput)
	} else {
		Err(InvalidU8)
	}
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parse"))
		(e-lambda
			(args
				(p-assign (ident "input")))
			(e-block
				(e-if
					(if-branches
						(if-branch
							(e-call (constraint-fn-var 262)
								(e-lookup-external
									(builtin))
								(e-lookup-local
									(p-assign (ident "input"))))
							(e-block
								(e-tag (name "Err")
									(args
										(e-tag (name "EmptyInput")))))))
					(if-else
						(e-block
							(e-tag (name "Err")
								(args
									(e-tag (name "InvalidU8")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U8") (builtin))
					(ty-tag-union
						(ty-tag-name (name "InvalidU8")))))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(U8, [EmptyInput, InvalidU8])"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "Str -> Try(U8, [EmptyInput, InvalidU8])"))
		(expr (type "_arg -> {}"))))
~~~
