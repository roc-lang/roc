# META
~~~ini
description=A newline before .method() still resolves the associated item of the type name it qualifies (issue 10708)
type=file:Blub.roc
~~~
# SOURCE
~~~roc
Blub :: [].{
	go : () -> U8
	go = || 5
}

expect 5 == Blub.go()

expect 5 ==
	Blub
		.go()
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpDoubleColon,OpenSquare,CloseSquare,Dot,OpenCurly,
LowerIdent,OpColon,OpenRound,CloseRound,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,OpBar,Int,
CloseCurly,
KwExpect,Int,OpEquals,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
KwExpect,Int,OpEquals,
UpperIdent,
DotLowerIdent,NoSpaceOpenRound,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Blub")
				(args))
			(ty-tag-union
				(tags))
			(associated
				(s-type-anno (name "go")
					(ty-fn
						(ty (name "U8"))))
				(s-decl
					(p-ident (raw "go"))
					(e-lambda
						(args)
						(e-int (raw "5"))))))
		(s-expect
			(e-binop (op "==")
				(e-int (raw "5"))
				(e-apply
					(e-ident (raw "Blub.go")))))
		(s-expect
			(e-binop (op "==")
				(e-int (raw "5"))
				(e-apply
					(e-ident (raw "Blub.go")))))))
~~~
# FORMATTED
~~~roc
Blub :: [].{
	go : () -> U8
	go = || 5
}

expect 5 == Blub.go()

expect 5 ==
	Blub.go()
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Blub.go"))
		(e-lambda
			(args)
			(e-num (value "5")))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "U8") (builtin)))))
	(s-nominal-decl
		(ty-header (name "Blub"))
		(ty-tag-union))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-num (value "5")))
			(rhs
				(e-call (constraint-fn-var 240)
					(e-lookup-local
						(p-assign (ident "Blub.go")))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-num (value "5")))
			(rhs
				(e-call (constraint-fn-var 258)
					(e-lookup-local
						(p-assign (ident "Blub.go"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "({}) -> U8")))
	(type_decls
		(nominal (type "Blub")
			(ty-header (name "Blub"))))
	(expressions
		(expr (type "({}) -> U8"))))
~~~
