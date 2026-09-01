# META
~~~ini
description=Infers independent same-name method requirements and discharges them against one concrete method scheme
type=file
~~~
# SOURCE
~~~roc
f1 = |l| l.map(|i| U32.to_u64(i))
f2 = |l| l.map(|i| U32.to_u128(i))

g = |l| {
    _a1 = f1(l)
    _a2 = f2(l)

    0
}

main = g([1.U32, 2.U32])
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
NamedUnderscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
Int,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenSquare,Int,NoSpaceDotUpperIdent,Comma,Int,NoSpaceDotUpperIdent,CloseSquare,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "f1"))
			(e-lambda
				(args
					(p-ident (raw "l")))
				(e-method-call (method ".map")
					(receiver
						(e-ident (raw "l")))
					(args
						(e-lambda
							(args
								(p-ident (raw "i")))
							(e-apply
								(e-ident (raw "U32.to_u64"))
								(e-ident (raw "i"))))))))
		(s-decl
			(p-ident (raw "f2"))
			(e-lambda
				(args
					(p-ident (raw "l")))
				(e-method-call (method ".map")
					(receiver
						(e-ident (raw "l")))
					(args
						(e-lambda
							(args
								(p-ident (raw "i")))
							(e-apply
								(e-ident (raw "U32.to_u128"))
								(e-ident (raw "i"))))))))
		(s-decl
			(p-ident (raw "g"))
			(e-lambda
				(args
					(p-ident (raw "l")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "_a1"))
							(e-apply
								(e-ident (raw "f1"))
								(e-ident (raw "l"))))
						(s-decl
							(p-ident (raw "_a2"))
							(e-apply
								(e-ident (raw "f2"))
								(e-ident (raw "l"))))
						(e-int (raw "0"))))))
		(s-decl
			(p-ident (raw "main"))
			(e-apply
				(e-ident (raw "g"))
				(e-list
					(e-typed-int (raw "1") (type "U32"))
					(e-typed-int (raw "2") (type "U32")))))))
~~~
# FORMATTED
~~~roc
f1 = |l| l.map(|i| U32.to_u64(i))

f2 = |l| l.map(|i| U32.to_u128(i))

g = |l| {
	_a1 = f1(l)
	_a2 = f2(l)

	0
}

main = g([1.U32, 2.U32])
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f1"))
		(e-lambda
			(args
				(p-assign (ident "l")))
			(e-dispatch-call (method "map") (constraint-fn-var 252)
				(receiver
					(e-lookup-local
						(p-assign (ident "l"))))
				(args
					(e-lambda
						(args
							(p-assign (ident "i")))
						(e-call (constraint-fn-var 251)
							(e-lookup-external
								(builtin))
							(e-lookup-local
								(p-assign (ident "i")))))))))
	(d-let
		(p-assign (ident "f2"))
		(e-lambda
			(args
				(p-assign (ident "l")))
			(e-dispatch-call (method "map") (constraint-fn-var 261)
				(receiver
					(e-lookup-local
						(p-assign (ident "l"))))
				(args
					(e-lambda
						(args
							(p-assign (ident "i")))
						(e-call (constraint-fn-var 260)
							(e-lookup-external
								(builtin))
							(e-lookup-local
								(p-assign (ident "i")))))))))
	(d-let
		(p-assign (ident "g"))
		(e-lambda
			(args
				(p-assign (ident "l")))
			(e-block
				(s-let
					(p-assign (ident "_a1"))
					(e-call (constraint-fn-var 270)
						(e-lookup-local
							(p-assign (ident "f1")))
						(e-lookup-local
							(p-assign (ident "l")))))
				(s-let
					(p-assign (ident "_a2"))
					(e-call (constraint-fn-var 278)
						(e-lookup-local
							(p-assign (ident "f2")))
						(e-lookup-local
							(p-assign (ident "l")))))
				(e-num (value "0")))))
	(d-let
		(p-assign (ident "main"))
		(e-call (constraint-fn-var 352)
			(e-lookup-local
				(p-assign (ident "g")))
			(e-list
				(elems
					(e-typed-int (value "1") (type "U32"))
					(e-typed-int (value "2") (type "U32")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c -> d where [c.map : c, (U32 -> U64) -> d]"))
		(patt (type "c -> d where [c.map : c, (U32 -> U128) -> d]"))
		(patt (type "c -> d where [c.map : c, (U32 -> U128) -> _ret, c.map : c, (U32 -> U64) -> _ret2, d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(patt (type "Dec")))
	(expressions
		(expr (type "c -> d where [c.map : c, (U32 -> U64) -> d]"))
		(expr (type "c -> d where [c.map : c, (U32 -> U128) -> d]"))
		(expr (type "c -> d where [c.map : c, (U32 -> U128) -> _ret, c.map : c, (U32 -> U64) -> _ret2, d.from_numeral : Numeral -> Try(d, [InvalidNumeral(Str)])]"))
		(expr (type "Dec"))))
~~~
