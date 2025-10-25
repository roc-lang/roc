# META
~~~ini
description=Example of a more complex nominal tag union with two payload types
type=snippet
~~~
# SOURCE
~~~roc
MyResult(ok, err) := [Ok(ok), Err(err)]

ok : ok -> MyResult(ok, _)
ok = |a| MyResult.Ok(a)

is_ok : MyResult(_ok, _err) -> Bool
is_ok = |result| match result {
    MyResult.Ok(_) => Bool.True
    MyResult.Err(_) => Bool.False
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,Underscore,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,NamedUnderscore,Comma,NamedUnderscore,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,NoSpaceDotUpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "MyResult")
				(args
					(ty-var (raw "ok"))
					(ty-var (raw "err"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "ok")))
					(ty-apply
						(ty (name "Err"))
						(ty-var (raw "err"))))))
		(s-type-anno (name "ok")
			(ty-fn
				(ty-var (raw "ok"))
				(ty-apply
					(ty (name "MyResult"))
					(ty-var (raw "ok"))
					(_))))
		(s-decl
			(p-ident (raw "ok"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-apply
					(e-tag (raw "MyResult.Ok"))
					(e-ident (raw "a")))))
		(s-type-anno (name "is_ok")
			(ty-fn
				(ty-apply
					(ty (name "MyResult"))
					(underscore-ty-var (raw "_ok"))
					(underscore-ty-var (raw "_err")))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "is_ok"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-match
					(e-ident (raw "result"))
					(branches
						(branch
							(p-tag (raw ".Ok")
								(p-underscore))
							(e-tag (raw "Bool.True")))
						(branch
							(p-tag (raw ".Err")
								(p-underscore))
							(e-tag (raw "Bool.False")))))))))
~~~
# FORMATTED
~~~roc
MyResult(ok, err) := [Ok(ok), Err(err)]

ok : ok -> MyResult(ok, _)
ok = |a| MyResult.Ok(a)

is_ok : MyResult(_ok, _err) -> Bool
is_ok = |result| match result {
	MyResult.Ok(_) => Bool.True
	MyResult.Err(_) => Bool.False
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "ok"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-nominal (nominal "MyResult")
				(e-tag (name "Ok")
					(args
						(e-lookup-local
							(p-assign (ident "a")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "ok"))
				(ty-apply (name "MyResult") (local)
					(ty-rigid-var-lookup (ty-rigid-var (name "ok")))
					(ty-underscore)))))
	(d-let
		(p-assign (ident "is_ok"))
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
									(p-nominal
										(p-applied-tag))))
							(value
								(e-nominal-external
									(external-module "Bool")
									(e-tag (name "True")))))
						(branch
							(patterns
								(pattern (degenerate false)
									(p-nominal
										(p-applied-tag))))
							(value
								(e-nominal-external
									(external-module "Bool")
									(e-tag (name "False")))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "MyResult") (local)
					(ty-rigid-var (name "_ok"))
					(ty-rigid-var (name "_err")))
				(ty-lookup (name "Bool") (external-module "Bool")))))
	(s-nominal-decl
		(ty-header (name "MyResult")
			(ty-args
				(ty-rigid-var (name "ok"))
				(ty-rigid-var (name "err"))))
		(ty-tag-union
			(ty-tag-name (name "Ok")
				(ty-rigid-var-lookup (ty-rigid-var (name "ok"))))
			(ty-tag-name (name "Err")
				(ty-rigid-var-lookup (ty-rigid-var (name "err")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "ok -> MyResult(ok, err)"))
		(patt (type "MyResult(_ok, _err) -> Bool")))
	(type_decls
		(nominal (type "MyResult(ok, err)")
			(ty-header (name "MyResult")
				(ty-args
					(ty-rigid-var (name "ok"))
					(ty-rigid-var (name "err"))))))
	(expressions
		(expr (type "ok -> MyResult(ok, err)"))
		(expr (type "MyResult(_ok, _err) -> Bool"))))
~~~
