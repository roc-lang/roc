# META
~~~ini
description=Mono test: dispatch for closure call with captured variable
type=mono
~~~
# SOURCE
~~~roc
func = |offset| {
	condition = True
	f = if condition |x| x + offset else |x| x * 2
	f(10)
}
result = func(1)
~~~
# MONO
~~~roc
func : Dec -> Dec
func = |offset| {
	condition = True
	f = if (condition) |x| x + offset else |x| x * 2
	f(10)
}

result : Dec
result = 11
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,UpperIdent,
LowerIdent,OpAssign,KwIf,LowerIdent,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,KwElse,OpBar,LowerIdent,OpBar,LowerIdent,OpStar,Int,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,Int,CloseRound,
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
					(p-ident (raw "offset")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "condition"))
							(e-tag (raw "True")))
						(s-decl
							(p-ident (raw "f"))
							(e-if-then-else
								(e-ident (raw "condition"))
								(e-lambda
									(args
										(p-ident (raw "x")))
									(e-binop (op "+")
										(e-ident (raw "x"))
										(e-ident (raw "offset"))))
								(e-lambda
									(args
										(p-ident (raw "x")))
									(e-binop (op "*")
										(e-ident (raw "x"))
										(e-int (raw "2"))))))
						(e-apply
							(e-ident (raw "f"))
							(e-int (raw "10")))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "func"))
				(e-int (raw "1"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "func"))
		(e-lambda
			(args
				(p-assign (ident "offset")))
			(e-block
				(s-let
					(p-assign (ident "condition"))
					(e-tag (name "True")))
				(s-let
					(p-assign (ident "f"))
					(e-if
						(if-branches
							(if-branch
								(e-lookup-local
									(p-assign (ident "condition")))
								(e-closure
									(captures
										(capture (ident "offset")))
									(e-lambda
										(args
											(p-assign (ident "x")))
										(e-binop (op "add")
											(e-lookup-local
												(p-assign (ident "x")))
											(e-lookup-local
												(p-assign (ident "offset"))))))))
						(if-else
							(e-lambda
								(args
									(p-assign (ident "x")))
								(e-binop (op "mul")
									(e-lookup-local
										(p-assign (ident "x")))
									(e-num (value "2")))))))
				(e-call
					(e-lookup-local
						(p-assign (ident "f")))
					(e-num (value "10"))))))
	(d-let
		(p-assign (ident "result"))
		(e-num (value "11"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec -> Dec"))
		(patt (type "Dec")))
	(expressions
		(expr (type "Dec -> Dec"))
		(expr (type "Dec"))))
~~~
