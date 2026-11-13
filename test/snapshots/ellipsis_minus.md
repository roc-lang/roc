# META
~~~ini
description=Minus before and after an ellipsis follows the same spacing rules as minus with a number literal
type=snippet
~~~
# SOURCE
~~~roc
negated = |_| -...
negated_in_list = |_| [-..., -...]
negated_statement = |a| {
    _ = a
    -...
}
subtract_no_spaces = |a| a-...
subtract_spaced = |a| a - ...
subtract_from_ellipsis = |_| ...-1
subtract_ellipses = |_| ...-...
subtract_negated = |a| a - -...
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpUnaryMinus,TripleDot,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenSquare,OpUnaryMinus,TripleDot,Comma,OpUnaryMinus,TripleDot,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
Underscore,OpAssign,LowerIdent,
OpUnaryMinus,TripleDot,
CloseCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpBinaryMinus,TripleDot,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpBinaryMinus,TripleDot,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,TripleDot,OpBinaryMinus,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,TripleDot,OpBinaryMinus,TripleDot,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpBinaryMinus,OpUnaryMinus,TripleDot,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "negated"))
			(e-lambda
				(args
					(p-underscore))
				(unary "-"
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "negated_in_list"))
			(e-lambda
				(args
					(p-underscore))
				(e-list
					(unary "-"
						(e-ellipsis))
					(unary "-"
						(e-ellipsis)))))
		(s-decl
			(p-ident (raw "negated_statement"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-block
					(statements
						(s-decl
							(p-underscore)
							(e-ident (raw "a")))
						(unary "-"
							(e-ellipsis))))))
		(s-decl
			(p-ident (raw "subtract_no_spaces"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-binop (op "-")
					(e-ident (raw "a"))
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "subtract_spaced"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-binop (op "-")
					(e-ident (raw "a"))
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "subtract_from_ellipsis"))
			(e-lambda
				(args
					(p-underscore))
				(e-binop (op "-")
					(e-ellipsis)
					(e-int (raw "1")))))
		(s-decl
			(p-ident (raw "subtract_ellipses"))
			(e-lambda
				(args
					(p-underscore))
				(e-binop (op "-")
					(e-ellipsis)
					(e-ellipsis))))
		(s-decl
			(p-ident (raw "subtract_negated"))
			(e-lambda
				(args
					(p-ident (raw "a")))
				(e-binop (op "-")
					(e-ident (raw "a"))
					(unary "-"
						(e-ellipsis)))))))
~~~
# FORMATTED
~~~roc
negated = |_| -...

negated_in_list = |_| [-..., -...]

negated_statement = |a| {
	_ = a
	-...
}

subtract_no_spaces = |a| a - ...

subtract_spaced = |a| a - ...

subtract_from_ellipsis = |_| ... - 1

subtract_ellipses = |_| ... - ...

subtract_negated = |a| a - -...
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "negated"))
		(e-lambda
			(args
				(p-underscore))
			(e-dispatch-call (method "negate") (constraint-fn-var 268)
				(receiver
					(e-not-implemented))
				(args))))
	(d-let
		(p-assign (ident "negated_in_list"))
		(e-lambda
			(args
				(p-underscore))
			(e-list
				(elems
					(e-dispatch-call (method "negate") (constraint-fn-var 270)
						(receiver
							(e-not-implemented))
						(args))
					(e-dispatch-call (method "negate") (constraint-fn-var 272)
						(receiver
							(e-not-implemented))
						(args))))))
	(d-let
		(p-assign (ident "negated_statement"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-block
				(s-let
					(p-underscore)
					(e-lookup-local
						(p-assign (ident "a"))))
				(e-dispatch-call (method "negate") (constraint-fn-var 274)
					(receiver
						(e-not-implemented))
					(args)))))
	(d-let
		(p-assign (ident "subtract_no_spaces"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-dispatch-call (method "minus") (constraint-fn-var 276)
				(receiver
					(e-lookup-local
						(p-assign (ident "a"))))
				(args
					(e-not-implemented)))))
	(d-let
		(p-assign (ident "subtract_spaced"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-dispatch-call (method "minus") (constraint-fn-var 278)
				(receiver
					(e-lookup-local
						(p-assign (ident "a"))))
				(args
					(e-not-implemented)))))
	(d-let
		(p-assign (ident "subtract_from_ellipsis"))
		(e-lambda
			(args
				(p-underscore))
			(e-dispatch-call (method "minus") (constraint-fn-var 287)
				(receiver
					(e-not-implemented))
				(args
					(e-num (value "1"))))))
	(d-let
		(p-assign (ident "subtract_ellipses"))
		(e-lambda
			(args
				(p-underscore))
			(e-dispatch-call (method "minus") (constraint-fn-var 289)
				(receiver
					(e-not-implemented))
				(args
					(e-not-implemented)))))
	(d-let
		(p-assign (ident "subtract_negated"))
		(e-lambda
			(args
				(p-assign (ident "a")))
			(e-dispatch-call (method "minus") (constraint-fn-var 293)
				(receiver
					(e-lookup-local
						(p-assign (ident "a"))))
				(args
					(e-dispatch-call (method "negate") (constraint-fn-var 291)
						(receiver
							(e-not-implemented))
						(args)))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> b where [b.negate : b -> b]"))
		(patt (type "_arg -> List(b) where [b.negate : b -> b]"))
		(patt (type "_arg -> b where [b.negate : b -> b]"))
		(patt (type "b -> b where [b.minus : b, c -> b]"))
		(patt (type "b -> b where [b.minus : b, c -> b]"))
		(patt (type "_arg -> b where [b.minus : b, c -> b, c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(patt (type "_arg -> b where [b.minus : b, _arg2 -> b]"))
		(patt (type "b -> b where [b.minus : b, c -> b, c.negate : c -> c]")))
	(expressions
		(expr (type "_arg -> b where [b.negate : b -> b]"))
		(expr (type "_arg -> List(b) where [b.negate : b -> b]"))
		(expr (type "_arg -> b where [b.negate : b -> b]"))
		(expr (type "b -> b where [b.minus : b, c -> b]"))
		(expr (type "b -> b where [b.minus : b, c -> b]"))
		(expr (type "_arg -> b where [b.minus : b, c -> b, c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))
		(expr (type "_arg -> b where [b.minus : b, _arg2 -> b]"))
		(expr (type "b -> b where [b.minus : b, c -> b, c.negate : c -> c]"))))
~~~
