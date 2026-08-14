# META
~~~ini
description=Polarity - an explicit `..` on a tag union in an output position is redundant (they are implicitly open) and warns; a named ext (`..others`) and an input-position `..` do not warn
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

parse : Str -> Try(U8, [InvalidU8, ..])
parse = |_| Err(InvalidU8)

parse_named : Str -> Try(U8, [InvalidU8, ..others])
parse_named = |_| Err(InvalidU8)

handle : [Known, ..] -> Str
handle = |input| {
    match input {
        Known => "known"
        _ => "other"
    }
}

main! = |_| {}
~~~
# EXPECTED
REDUNDANT OPEN TAG UNION - polarity_redundant_open_ext_warning.md:3:36:3:38
# PROBLEMS
── ● redundant open tag union ────── polarity_redundant_open_ext_warning.md:3:36

This tag union has an explicit `..`, but it is already implicitly open.

parse : Str -> Try(U8, [InvalidU8, ..])
                                   ^^

Tag unions in output positions, like the return type of a function, are
automatically open. Remove the .. or bind it to a named type variable like
..others if you want to refer to the extension elsewhere.

# TOKENS
~~~zig
KwApp,OpenSquare,LowerIdent,CloseSquare,OpenCurly,LowerIdent,OpColon,KwPlatform,StringStart,StringPart,StringEnd,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,Comma,DoubleDot,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,OpenSquare,UpperIdent,Comma,DoubleDot,LowerIdent,CloseSquare,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpColon,OpenSquare,UpperIdent,Comma,DoubleDot,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwMatch,LowerIdent,OpenCurly,
UpperIdent,OpFatArrow,StringStart,StringPart,StringEnd,
Underscore,OpFatArrow,StringStart,StringPart,StringEnd,
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
							(ty (name "InvalidU8")))
						..))))
		(s-decl
			(p-ident (raw "parse"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-tag (raw "Err"))
					(e-tag (raw "InvalidU8")))))
		(s-type-anno (name "parse_named")
			(ty-fn
				(ty (name "Str"))
				(ty-apply
					(ty (name "Try"))
					(ty (name "U8"))
					(ty-tag-union
						(tags
							(ty (name "InvalidU8")))
						(ty-var (raw "others"))))))
		(s-decl
			(p-ident (raw "parse_named"))
			(e-lambda
				(args
					(p-underscore))
				(e-apply
					(e-tag (raw "Err"))
					(e-tag (raw "InvalidU8")))))
		(s-type-anno (name "handle")
			(ty-fn
				(ty-tag-union
					(tags
						(ty (name "Known")))
					..)
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "handle"))
			(e-lambda
				(args
					(p-ident (raw "input")))
				(e-block
					(statements
						(e-match
							(e-ident (raw "input"))
							(branches
								(branch
									(p-tag (raw "Known"))
									(e-string
										(e-string-part (raw "known"))))
								(branch
									(p-underscore)
									(e-string
										(e-string-part (raw "other"))))))))))
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

parse : Str -> Try(U8, [InvalidU8, ..])
parse = |_| Err(InvalidU8)

parse_named : Str -> Try(U8, [InvalidU8, ..others])
parse_named = |_| Err(InvalidU8)

handle : [Known, ..] -> Str
handle = |input| {
	match input {
		Known => "known"
		_ => "other"
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
				(p-underscore))
			(e-tag (name "Err")
				(args
					(e-tag (name "InvalidU8")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U8") (builtin))
					(ty-tag-union
						(ty-tag-name (name "InvalidU8"))
						(ty-rigid-var (name "#others")))))))
	(d-let
		(p-assign (ident "parse_named"))
		(e-lambda
			(args
				(p-underscore))
			(e-tag (name "Err")
				(args
					(e-tag (name "InvalidU8")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Str") (builtin))
				(ty-apply (name "Try") (builtin)
					(ty-lookup (name "U8") (builtin))
					(ty-tag-union
						(ty-tag-name (name "InvalidU8"))
						(ty-rigid-var (name "others")))))))
	(d-let
		(p-assign (ident "handle"))
		(e-lambda
			(args
				(p-assign (ident "input")))
			(e-block
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "input"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-string
										(e-literal (string "known")))))
							(branch
								(patterns
									(pattern (degenerate false)
										(p-underscore)))
								(value
									(e-string
										(e-literal (string "other"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "Known"))
					(ty-rigid-var (name "#others")))
				(ty-lookup (name "Str") (builtin)))))
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
		(patt (type "Str -> Try(U8, [InvalidU8])"))
		(patt (type "Str -> Try(U8, [InvalidU8, ..others])"))
		(patt (type "[Known, ..] -> Str"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "Str -> Try(U8, [InvalidU8])"))
		(expr (type "Str -> Try(U8, [InvalidU8, ..others])"))
		(expr (type "[Known, ..] -> Str"))
		(expr (type "_arg -> {}"))))
~~~
