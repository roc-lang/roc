# META
~~~ini
description=Unqualified tags with payloads on polymorphic nominal types
type=snippet
~~~
# SOURCE
~~~roc
MyResult(ok, err) := [Ok(ok), Err(err)]

myResult : MyResult(Str, I32)
myResult = Ok("success")

isOk : MyResult(ok, err) -> Bool
isOk = |result| match result {
    Ok(_) => Bool.True
    Err(_) => Bool.False
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColonEqual,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseSquare,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,UpperIdent,NoSpaceOpenRound,StringStart,StringPart,StringEnd,CloseRound,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,KwMatch,LowerIdent,OpenCurly,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
UpperIdent,NoSpaceOpenRound,Underscore,CloseRound,OpFatArrow,UpperIdent,NoSpaceDotUpperIdent,
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
		(s-type-anno (name "myResult")
			(ty-apply
				(ty (name "MyResult"))
				(ty (name "Str"))
				(ty (name "I32"))))
		(s-decl
			(p-ident (raw "myResult"))
			(e-apply
				(e-tag (raw "Ok"))
				(e-string
					(e-string-part (raw "success")))))
		(s-type-anno (name "isOk")
			(ty-fn
				(ty-apply
					(ty (name "MyResult"))
					(ty-var (raw "ok"))
					(ty-var (raw "err")))
				(ty (name "Bool"))))
		(s-decl
			(p-ident (raw "isOk"))
			(e-lambda
				(args
					(p-ident (raw "result")))
				(e-match
					(e-ident (raw "result"))
					(branches
						(branch
							(p-tag (raw "Ok")
								(p-underscore))
							(e-tag (raw "Bool.True")))
						(branch
							(p-tag (raw "Err")
								(p-underscore))
							(e-tag (raw "Bool.False")))))))))
~~~
# FORMATTED
~~~roc
MyResult(ok, err) := [Ok(ok), Err(err)]

myResult : MyResult(Str, I32)
myResult = Ok("success")

isOk : MyResult(ok, err) -> Bool
isOk = |result| match result {
	Ok(_) => Bool.True
	Err(_) => Bool.False
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "myResult"))
		(e-tag (name "Ok")
			(args
				(e-string
					(e-literal (string "success")))))
		(annotation
			(declared-type
				(ty-apply (name "MyResult") (local)
					(ty-lookup (name "Str") (builtin))
					(ty-lookup (name "I32") (builtin))))))
	(d-let
		(p-assign (ident "isOk"))
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
			(declared-type
				(ty-fn (effectful false)
					(ty-apply (name "MyResult") (local)
						(ty-rigid-var (name "ok"))
						(ty-rigid-var (name "err")))
					(ty-lookup (name "Bool") (external-module "Bool"))))))
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
		(patt (type "MyResult(Str, Num(Int(Signed32)))"))
		(patt (type "MyResult(ok, err) -> Bool")))
	(type_decls
		(nominal (type "MyResult(ok, err)")
			(ty-header (name "MyResult")
				(ty-args
					(ty-rigid-var (name "ok"))
					(ty-rigid-var (name "err"))))))
	(expressions
		(expr (type "MyResult(Str, Num(Int(Signed32)))"))
		(expr (type "MyResult(ok, err) -> Bool"))))
~~~
