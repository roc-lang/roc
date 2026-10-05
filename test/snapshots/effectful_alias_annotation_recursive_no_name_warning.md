# META
~~~ini
description=A recursive call through the group's own `=>` annotation written as an alias must not trigger the effectful-name warning, the same as when the annotation is written directly
type=file
~~~
# SOURCE
~~~roc
Eff : U64 => U64

recurse : Eff
recurse = |n|
    1 + recurse(n + 1)

main! = |_| {
    _ = recurse(0)
    Ok({})
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,OpFatArrow,UpperIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,
Int,OpPlus,LowerIdent,NoSpaceOpenRound,LowerIdent,OpPlus,Int,CloseRound,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
UpperIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Eff")
				(args))
			(ty-fn
				(ty (name "U64"))
				(ty (name "U64"))))
		(s-type-anno (name "recurse")
			(ty (name "Eff")))
		(s-decl
			(p-ident (raw "recurse"))
			(e-lambda
				(args
					(p-ident (raw "n")))
				(e-binop (op "+")
					(e-int (raw "1"))
					(e-apply
						(e-ident (raw "recurse"))
						(e-binop (op "+")
							(e-ident (raw "n"))
							(e-int (raw "1")))))))
		(s-decl
			(p-ident (raw "main!"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-apply
								(e-ident (raw "recurse"))
								(e-int (raw "0"))))
						(e-apply
							(e-tag (raw "Ok"))
							(e-record))))))))
~~~
# FORMATTED
~~~roc
Eff : U64 => U64

recurse : Eff
recurse = |n|
	1 + recurse(n + 1)

main! = |_| {
	_ = recurse(0)
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
		(p-assign (ident "recurse"))
		(e-lambda
			(args
				(p-assign (ident "n")))
			(e-dispatch-call (method "plus") (constraint-fn-var 290)
				(receiver
					(e-num (value "1")))
				(args
					(e-call (constraint-fn-var 289)
						(e-lookup-local
							(p-assign (ident "recurse")))
						(e-dispatch-call (method "plus") (constraint-fn-var 283)
							(receiver
								(e-lookup-local
									(p-assign (ident "n"))))
							(args
								(e-num (value "1"))))))))
		(annotation
			(ty-lookup (name "Eff") (local))))
	(d-let
		(p-assign (ident "main!"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-let
					(p-underscore)
					(e-call (constraint-fn-var 305)
						(e-lookup-local
							(p-assign (ident "recurse")))
						(e-num (value "0"))))
				(e-tag (name "Ok")
					(args
						(e-empty_record))))))
	(s-alias-decl
		(ty-header (name "Eff"))
		(ty-fn (effectful true)
			(ty-lookup (name "U64") (builtin))
			(ty-lookup (name "U64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Str => {}"))
		(patt (type "Eff"))
		(patt (type "_arg => [Ok({})]")))
	(type_decls
		(alias (type "Eff")
			(ty-header (name "Eff"))))
	(expressions
		(expr (type "Str => {}"))
		(expr (type "Eff"))
		(expr (type "_arg => [Ok({})]"))))
~~~
