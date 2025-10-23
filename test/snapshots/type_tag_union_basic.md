# META
~~~ini
description=Basic tag union type canonicalization
type=file
~~~
# SOURCE
~~~roc
app [main!] { pf: platform "../basic-cli/main.roc" }

process : [Some(Str), None] -> Str
process = |_maybe| "result"

is_ok_ret_unqualified_bool : [Ok2(_ok), Err2(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result {
    Ok2(_) => True
    Err2(_) => False
}

is_ok_ret_bool : [Ok2(_ok2), Err2(_err2)] -> Bool
is_ok_ret_bool = |result| match result {
    Ok2(_) => Bool.True
    Err2(_) => Bool.False
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
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,Comma,UpperIdent,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,StringStart,StringPart,StringEnd,
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,UpperIdent,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,UpperIdent,
CloseCurly,
LowerIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,CloseSquare,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
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
		(s-type-anno (name "process")
			(ty-fn
				(ty-tag-union
					(tags
						(ty-apply
							(ty (name "Some"))
							(ty (name "Str")))
						(ty (name "None"))))
				(ty (name "Str"))))
		(s-decl
			(p-ident (raw "process"))
			(e-lambda
				(args
					(p-ident (raw "_maybe")))
				(e-string
					(e-string-part (raw "result")))))
		(s-type-anno (name "is_ok_ret_unqualified_bool")
			(ty-fn
				(ty-tag-union
					(tags
						(ty-apply
							(ty (name "Ok2"))
							(underscore-ty-var (raw "_ok")))
						(ty-apply
							(ty (name "Err2"))
							(underscore-ty-var (raw "_err")))))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "is_ok_ret_unqualified_bool"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-match
					(e-ident (raw "result"))
					(branches
						(branch
							(p-tag (raw "Ok2")
								(p-underscore))
							(e-tag (raw "True")))
						(branch
							(p-tag (raw "Err2")
								(p-underscore))
							(e-tag (raw "False")))))))
		(s-type-anno (name "is_ok_ret_bool")
			(ty-fn
				(ty-tag-union
					(tags
						(ty-apply
							(ty (name "Ok2"))
							(underscore-ty-var (raw "_ok2")))
						(ty-apply
							(ty (name "Err2"))
							(underscore-ty-var (raw "_err2")))))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "is_ok_ret_bool"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-match
					(e-ident (raw "result"))
					(branches
						(branch
							(p-tag (raw "Ok2")
								(p-underscore))
							(e-tag (raw "Bool.True")))
						(branch
							(p-tag (raw "Err2")
								(p-underscore))
							(e-tag (raw "Bool.False")))))))
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

process : [Some(Str), None] -> Str
process = |_maybe| "result"

is_ok_ret_unqualified_bool : [Ok2(_ok), Err2(_err)] -> Bool
is_ok_ret_unqualified_bool = |result| match result {
	Ok2(_) => True
	Err2(_) => False
}

is_ok_ret_bool : [Ok2(_ok2), Err2(_err2)] -> Bool
is_ok_ret_bool = |result| match result {
	Ok2(_) => Bool.True
	Err2(_) => Bool.False
}

main! = |_| {}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "process"))
		(e-lambda
			(args
				(p-assign (ident "_maybe")))
			(e-string
				(e-literal (string "result"))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "Some")
						(ty-lookup (name "Str") (external-module "Str")))
					(ty-tag-name (name "None")))
				(ty-lookup (name "Str") (external-module "Str")))))
	(d-let
		(p-assign (ident "is_ok_ret_unqualified_bool"))
		(e-lambda
			(args
				(p-assign (ident "result")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-tag (name "True"))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-tag (name "False"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "Ok2")
						(ty-rigid-var (name "_ok")))
					(ty-tag-name (name "Err2")
						(ty-rigid-var (name "_err"))))
				(ty-lookup (name "Bool") (external-module "Bool")))))
	(d-let
		(p-assign (ident "is_ok_ret_bool"))
		(e-lambda
			(args
				(p-assign (ident "result")))
			(e-match
				(match
					(cond
						(e-lookup-local
							(p-assign (ident "result"))))
					(branches
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-nominal-external
									(external-module "Bool")
									(e-tag (name "True")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-applied-tag)))
							(value
								(e-nominal-external
									(external-module "Bool")
									(e-tag (name "False")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-tag-union
					(ty-tag-name (name "Ok2")
						(ty-rigid-var (name "_ok2")))
					(ty-tag-name (name "Err2")
						(ty-rigid-var (name "_err2"))))
				(ty-lookup (name "Bool") (external-module "Bool")))))
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
		(patt (type "[None, Some(Str)] -> Str"))
		(patt (type "[Err2(_err), Ok2(_ok)] -> Bool"))
		(patt (type "[Err2(_err2), Ok2(_ok2)] -> Bool"))
		(patt (type "_arg -> {}")))
	(expressions
		(expr (type "[None, Some(Str)] -> Str"))
		(expr (type "[Err2(_err), Ok2(_ok)] -> Bool"))
		(expr (type "[Err2(_err2), Ok2(_ok2)] -> Bool"))
		(expr (type "_arg -> {}"))))
~~~
