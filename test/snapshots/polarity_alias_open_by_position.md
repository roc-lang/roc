# META
~~~ini
description=Polarity - an alias of a closed tag union defers its openness to each use site - open in output positions (extra tags absorbed), closed in input positions (foreign tags rejected)
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

ParseErrs : [InvalidUtf8, InvalidU8]

parse : Str -> Try(U8, ParseErrs)
parse = |_| Err(SomethingElse)

handle : ParseErrs -> Str
handle = |err| {
    match err {
        InvalidUtf8 => "utf8"
        InvalidU8 => "u8"
    }
}

bad = handle(SomethingElse)

main! = |_| {}
~~~
# EXPECTED
TYPE MISMATCH - polarity_alias_open_by_position.md:16:7:16:7
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ The first argument being passed to this function has the ──┐
└┬──────────────┘  wrong type.                                                │
 │                                                                            │
 │  bad = handle(SomethingElse)                                               │
 │               ‾‾‾‾‾‾‾‾‾‾‾‾‾                                                │
 └────────────────────────────────── polarity_alias_open_by_position.md:16:14 ┘

    This argument has the type:

        [SomethingElse]

    But `handle` needs the first argument to be:

        ParseErrs

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
CloseCurly,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
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
		(s-type-decl
			(header (name "ParseErrs")
				(args))
			(ty-tag-union
				(tags
					(ty (name "InvalidUtf8"))
					(ty (name "InvalidU8")))))
		(s-type-anno (name "parse")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U8"))
					(ty (name "ParseErrs")))))
		(s-decl
			(p-ident (raw "parse"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-tag (raw "Err"))
					(e-tag (raw "SomethingElse")))))
		(s-type-anno (name "handle")
			(ty-fn
				(ty (name "ParseErrs"))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "handle"))
			(e-lambda
				(args
					(p-ident (raw "err")))
				(e-block
					(statements
						(e-match
							(e-ident (raw "err"))
							(branches
								(branch
									(p-tag (raw "InvalidUtf8"))
									(e-string
										(e-string-part (raw "utf8"))))
								(branch
									(p-tag (raw "InvalidU8"))
									(e-string
										(e-string-part (raw "u8"))))))))))
		(s-decl
			(p-ident (raw "bad"))
			(e-apply
				(e-ident (raw "handle"))
				(e-tag (raw "SomethingElse"))))
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

ParseErrs : [InvalidUtf8, InvalidU8]

parse : Str -> Try(U8, ParseErrs)
parse = |_| Err(SomethingElse)

handle : ParseErrs -> Str
handle = |err| {
	match err {
		InvalidUtf8 => "utf8"
		InvalidU8 => "u8"
	}
}

bad = handle(SomethingElse)

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "parse"))
		(e-lambda
			(args
				(p-underscore))
			(e-tag (name "Err")
				(args
					(e-tag (name "SomethingElse")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U8") (builtin))
					(ty-lookup (name "ParseErrs") (local))))))
	(d-let
		(p-assign (ident "handle"))
		(e-lambda
			(args
				(p-assign (ident "err")))
			(e-block
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "err"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-string
										(e-literal (string "utf8")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-string
										(e-literal (string "u8"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "ParseErrs") (local))
				(ty-lookup (name "Str") (builtin)))))
	(d-let
		(p-assign (ident "bad"))
		(e-runtime-error (tag "erroneous_value_expr")))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-empty_record)))
	(s-alias-decl
		(ty-header (name "ParseErrs"))
		(ty-tag-union
			(ty-tag-name (name "InvalidUtf8"))
			(ty-tag-name (name "InvalidU8")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str -> Try(U8, ParseErrs)"))
		(patt (type "ParseErrs -> Str"))
		(patt (type "Error"))
		(patt (type "_arg -> {}")))
	(type_decls
		(alias (type "ParseErrs")
			(ty-header (name "ParseErrs"))))
	(expressions
		(expr (type "Str -> Try(U8, ParseErrs)"))
		(expr (type "ParseErrs -> Str"))
		(expr (type "Error"))
		(expr (type "_arg -> {}"))))
~~~
