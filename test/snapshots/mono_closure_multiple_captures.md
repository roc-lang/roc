# META
~~~ini
description=Mono test: closure with multiple captured variables
type=mono
~~~
# SOURCE
~~~roc
func = |a, b| {
	add_ab = |x| a + b + x
	add_ab(10)
}
result = func(1, 2)
~~~
# MONO
~~~roc
func : c, Dec -> c where [c.plus : c, Dec -> c]
func = |a, b| {
	add_ab = |x| a + b + x
	add_ab(10)
}

result : Dec
result = 13
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
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
					(p-ident (raw "a"))
					(p-ident (raw "b")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "add_ab"))
							(e-lambda
								(args
									(p-ident (raw "x")))
								(e-binop (op "+")
									(e-binop (op "+")
										(e-ident (raw "a"))
										(e-ident (raw "b")))
									(e-ident (raw "x")))))
						(e-apply
							(e-ident (raw "add_ab"))
							(e-int (raw "10")))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "func"))
				(e-int (raw "1"))
				(e-int (raw "2"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "func"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-block
				(s-let
					(p-assign (ident "add_ab"))
					(e-closure
						(captures
							(capture (ident "a"))
							(capture (ident "b")))
						(e-lambda
							(args
								(p-assign (ident "x")))
							(e-binop (op "add")
								(e-binop (op "add")
									(e-lookup-local
										(p-assign (ident "a")))
									(e-lookup-local
										(p-assign (ident "b"))))
								(e-lookup-local
									(p-assign (ident "x")))))))
				(e-call
					(e-lookup-local
						(p-assign (ident "add_ab")))
					(e-num (value "10"))))))
	(d-let
		(p-assign (ident "result"))
		(e-num (value "13"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c, Dec -> c where [c.plus : c, Dec -> c]"))
		(patt (type "Dec")))
	(expressions
		(expr (type "c, Dec -> c where [c.plus : c, Dec -> c]"))
		(expr (type "Dec"))))
~~~
