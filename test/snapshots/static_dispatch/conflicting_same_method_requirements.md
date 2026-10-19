# META
~~~ini
description=Rejects a generalized type whose uses require two incompatible same-name methods
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
TYPE MISMATCH - conflicting_same_method_requirements.md:4:1:4:2
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Type Mismatch")
		(region (start 4 1) (end 4 2))
		(headline
			(reflow "This is used in a way that requires its type to have two different, incompatible")
			(reflow " ")
			(annotated code "map")
			(reflow " ")
			(reflow "methods, but no type can have more than one")
			(reflow " ")
			(annotated code "map")
			(reflow " ")
			(reflow "method."))
		(document
			(source-region (file "conflicting_same_method_requirements.md") (start 4 1) (end 4 2) (annotation error) (line-text "g = |l| {"))
			(line-break)
			(reflow "One part of this use needs")
			(reflow " ")
			(annotated code "map")
			(reflow " ")
			(reflow "to have this type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a, (U32 -> U128) -> _ret")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "Another part needs it to have this type:")
			(line-break)
			(line-break)
			(annotation-start code-block)
			(indent 1)
			(text "a, (U32 -> U64) -> _ret")
			(annotation-end)
			(line-break)
			(line-break)
			(reflow "A type has only one")
			(reflow " ")
			(annotated code "map")
			(reflow " ")
			(reflow "method, so no type can satisfy both."))))
~~~
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
			(e-dispatch-call (method "map") (constraint-fn-var 259)
				(receiver
					(e-lookup-local
						(p-assign (ident "l"))))
				(args
					(e-lambda
						(args
							(p-assign (ident "i")))
						(e-call (constraint-fn-var 258)
							(e-lookup-external
								(builtin))
							(e-lookup-local
								(p-assign (ident "i")))))))))
	(d-let
		(p-assign (ident "f2"))
		(e-lambda
			(args
				(p-assign (ident "l")))
			(e-dispatch-call (method "map") (constraint-fn-var 268)
				(receiver
					(e-lookup-local
						(p-assign (ident "l"))))
				(args
					(e-lambda
						(args
							(p-assign (ident "i")))
						(e-call (constraint-fn-var 267)
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
					(e-call (constraint-fn-var 274)
						(e-lookup-local
							(p-assign (ident "f1")))
						(e-runtime-error (tag "erroneous_value_use"))))
				(s-let
					(p-assign (ident "_a2"))
					(e-call (constraint-fn-var 279)
						(e-lookup-local
							(p-assign (ident "f2")))
						(e-runtime-error (tag "erroneous_value_use"))))
				(e-num (value "0")))))
	(d-let
		(p-assign (ident "main"))
		(e-call (constraint-fn-var 300)
			(e-runtime-error (tag "erroneous_value_expr"))
			(e-list
				(elems
					(e-typed-int (value "1") (type "U32"))
					(e-typed-int (value "2") (type "U32")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> b where [a.map : a, (U32 -> U64) -> b]"))
		(patt (type "a -> b where [a.map : a, (U32 -> U128) -> b]"))
		(patt (type "Error -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(patt (type "Dec")))
	(expressions
		(expr (type "a -> b where [a.map : a, (U32 -> U64) -> b]"))
		(expr (type "a -> b where [a.map : a, (U32 -> U128) -> b]"))
		(expr (type "Error -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)])]"))
		(expr (type "Dec"))))
~~~
