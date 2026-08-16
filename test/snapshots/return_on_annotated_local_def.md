# META
~~~ini
description=return on the rhs of an annotated local def inside an unannotated function checks against the function body, not the local annotation
type=snippet
~~~
# SOURCE
~~~roc
f = |take_early_return| {
	n : U64
	n = if take_early_return {
		return Err(BadInput)
	} else {
		1
	}
	Ok(n)
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,KwIf,LowerIdent,OpenCurly,
KwReturn,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
CloseCurly,KwElse,OpenCurly,
Int,
CloseCurly,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "take_early_return")))
				(e-block
					(statements
						(s-type-anno (name "n")
							(ty (name "U64")))
						(s-decl
							(p-ident (raw "n"))
							(e-if-then-else
								(e-ident (raw "take_early_return"))
								(e-block
									(statements
										(s-return
											(e-apply
												(e-tag (raw "Err"))
												(e-tag (raw "BadInput"))))))
								(e-block
									(statements
										(e-int (raw "1"))))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-ident (raw "n")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "take_early_return")))
			(e-block
				(s-let
					(p-assign (ident "n"))
					(e-if
						(if-branches
							(if-branch
								(e-lookup-local
									(p-assign (ident "take_early_return")))
								(e-block
									(e-return
										(e-tag (name "Err")
											(args
												(e-tag (name "BadInput"))))))))
						(if-else
							(e-block
								(e-num (value "1"))))))
				(e-tag (name "Ok")
					(args
						(e-lookup-local
							(p-assign (ident "n")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool -> [Err([BadInput, ..]), Ok(U64), ..]")))
	(expressions
		(expr (type "Bool -> [Err([BadInput, ..]), Ok(U64), ..]"))))
~~~
