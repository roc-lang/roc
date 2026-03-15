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
func : Dec, Dec -> Dec
func = |x, y| {
	add_x = |a| a + x
	add_y = |b| b + y
	add_x(5) + add_y(5)
}

result : Dec
result = 40
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
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "a")))
								(e-lookup-local
									(p-assign (ident "x")))))))
				(s-let
					(p-assign (ident "add_y"))
					(e-closure
						(captures
							(capture (ident "y")))
						(e-lambda
							(args
								(p-assign (ident "b")))
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "b")))
								(e-lookup-local
									(p-assign (ident "y")))))))
				(e-binop (op "add")
					(e-call
						(e-lookup-local
							(p-assign (ident "add_x")))
						(e-num (value "5")))
					(e-call
						(e-lookup-local
							(p-assign (ident "add_y")))
						(e-num (value "5")))))))
	(d-let
		(p-assign (ident "result"))
		(e-num (value "40"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec, Dec -> Dec"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec, Dec -> Dec"))
		(expr (type "Dec"))))
~~~
