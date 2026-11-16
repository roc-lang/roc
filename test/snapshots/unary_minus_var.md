# META
~~~ini
description=Unary minus directly before a var name, and minus without spaces between names
type=snippet
~~~
# SOURCE
~~~roc
negated : I64 -> I64
negated = |start| {
	var $n = start
	$n = -$n
	$n = $n-start
	$n = start-$n
	$n = $n - -$n
	$n
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,OpUnaryMinus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,OpBinaryMinus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,OpBinaryMinus,LowerIdent,
LowerIdent,OpAssign,LowerIdent,OpBinaryMinus,OpUnaryMinus,LowerIdent,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "negated")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "negated"))
			(e-lambda
				(args
					(p-ident (raw "start")))
				(e-block
					(statements
						(s-var (name "$n")
							(e-ident (raw "start")))
						(s-decl
							(p-ident (raw "$n"))
							(unary "-"
								(e-ident (raw "$n"))))
						(s-decl
							(p-ident (raw "$n"))
							(e-binop (op "-")
								(e-ident (raw "$n"))
								(e-ident (raw "start"))))
						(s-decl
							(p-ident (raw "$n"))
							(e-binop (op "-")
								(e-ident (raw "start"))
								(e-ident (raw "$n"))))
						(s-decl
							(p-ident (raw "$n"))
							(e-binop (op "-")
								(e-ident (raw "$n"))
								(unary "-"
									(e-ident (raw "$n")))))
						(e-ident (raw "$n"))))))))
~~~
# FORMATTED
~~~roc
negated : I64 -> I64
negated = |start| {
	var $n = start
	$n = -$n
	$n = $n - start
	$n = start - $n
	$n = $n - -$n
	$n
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "negated"))
		(e-lambda
			(args
				(p-assign (ident "start")))
			(e-block
				(s-var
					(p-var-assign (ident "$n"))
					(e-lookup-local
						(p-assign (ident "start"))))
				(s-reassign
					(p-var-assign (ident "$n"))
					(e-dispatch-call (method "negate") (constraint-fn-var 243)
						(receiver
							(e-lookup-local
								(p-var-assign (ident "$n"))))
						(args)))
				(s-reassign
					(p-var-assign (ident "$n"))
					(e-dispatch-call (method "minus") (constraint-fn-var 249)
						(receiver
							(e-lookup-local
								(p-var-assign (ident "$n"))))
						(args
							(e-lookup-local
								(p-assign (ident "start"))))))
				(s-reassign
					(p-var-assign (ident "$n"))
					(e-dispatch-call (method "minus") (constraint-fn-var 249)
						(receiver
							(e-lookup-local
								(p-assign (ident "start"))))
						(args
							(e-lookup-local
								(p-var-assign (ident "$n"))))))
				(s-reassign
					(p-var-assign (ident "$n"))
					(e-dispatch-call (method "minus") (constraint-fn-var 249)
						(receiver
							(e-lookup-local
								(p-var-assign (ident "$n"))))
						(args
							(e-dispatch-call (method "negate") (constraint-fn-var 251)
								(receiver
									(e-lookup-local
										(p-var-assign (ident "$n"))))
								(args)))))
				(e-lookup-local
					(p-var-assign (ident "$n")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64 -> I64")))
	(expressions
		(expr (type "I64 -> I64"))))
~~~
