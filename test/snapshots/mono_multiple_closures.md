# META
~~~ini
description=Mono test: multiple different closures with captures
type=mono
~~~
# SOURCE
~~~roc
func = |x, y| {
	add_x = |a| a + x
	add_y = |b| b + y
	add_x(5) + add_y(5)
}

result = func(10, 20)
~~~
# MONO
~~~roc
func = |x, y| {
	add_x = |a| a + x
	add_y = |b| b + y
	add_x(5) + add_y(5)
}

result : Dec
result = func(10, 20)
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,OpPlus,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,Comma,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-decl
			(p-ident (raw "func"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "y")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "add_x"))
							(e-lambda
								(args
									(p-ident (raw "a")))
								(e-binop (op "+")
									(e-ident (raw "a"))
									(e-ident (raw "x")))))
						(s-decl
							(p-ident (raw "add_y"))
							(e-lambda
								(args
									(p-ident (raw "b")))
								(e-binop (op "+")
									(e-ident (raw "b"))
									(e-ident (raw "y")))))
						(e-binop (op "+")
							(e-apply
								(e-ident (raw "add_x"))
								(e-int (raw "5")))
							(e-apply
								(e-ident (raw "add_y"))
								(e-int (raw "5"))))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "func"))
				(e-int (raw "10"))
				(e-int (raw "20"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "func"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "y")))
			(e-block
				(s-let
					(p-assign (ident "add_x"))
					(e-closure
						(captures
							(capture (ident "x")))
						(e-lambda
							(args
								(p-assign (ident "a")))
							(e-dispatch-call (method "plus") (constraint-fn-var 43)
								(receiver
									(e-lookup-local
										(p-assign (ident "a"))))
								(args
									(e-lookup-local
										(p-assign (ident "x"))))))))
				(s-let
					(p-assign (ident "add_y"))
					(e-closure
						(captures
							(capture (ident "y")))
						(e-lambda
							(args
								(p-assign (ident "b")))
							(e-dispatch-call (method "plus") (constraint-fn-var 45)
								(receiver
									(e-lookup-local
										(p-assign (ident "b"))))
								(args
									(e-lookup-local
										(p-assign (ident "y"))))))))
				(e-dispatch-call (method "plus") (constraint-fn-var 121)
					(receiver
						(e-call (constraint-fn-var 83)
							(e-lookup-local
								(p-assign (ident "add_x")))
							(e-num (value "5"))))
					(args
						(e-call (constraint-fn-var 120)
							(e-lookup-local
								(p-assign (ident "add_y")))
							(e-num (value "5"))))))))
	(d-let
		(p-assign (ident "result"))
		(e-call (constraint-fn-var 201)
			(e-lookup-local
				(p-assign (ident "func")))
			(e-num (value "10"))
			(e-num (value "20")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c, d -> e where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), c.plus : c, d -> c, e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)]), e.plus : e, c -> e]"))
		(patt (type "Dec")))
	(expressions
		(expr (type "c, d -> e where [c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)]), c.plus : c, d -> c, e.from_numeral : Numeral -> Try(e, [InvalidNumeral(Str)]), e.plus : e, c -> e]"))
		(expr (type "Dec"))))
~~~
