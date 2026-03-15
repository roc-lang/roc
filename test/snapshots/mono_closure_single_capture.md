# META
~~~ini
description=Mono test: closure with single captured variable
type=mono
~~~
# SOURCE
~~~roc
func = |x| {
	add_x = |y| x + y
	add_x(10)
}
result = func(42)
~~~
# MONO
~~~roc
result : Dec
result = 52
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
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
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
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-decl
							(p-ident (raw "add_x"))
							(e-lambda
								(args
									(p-ident (raw "y")))
								(e-binop (op "+")
									(e-ident (raw "x"))
									(e-ident (raw "y")))))
						(e-apply
							(e-ident (raw "add_x"))
							(e-int (raw "10")))))))
		(s-decl
			(p-ident (raw "result"))
			(e-apply
				(e-ident (raw "func"))
				(e-int (raw "42"))))))
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "func"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-block
				(s-let
					(p-assign (ident "add_x"))
					(e-closure
						(captures
							(capture (ident "x")))
						(e-lambda
							(args
								(p-assign (ident "y")))
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "x")))
								(e-lookup-local
									(p-assign (ident "y")))))))
				(e-call
					(e-lookup-local
						(p-assign (ident "add_x")))
					(e-num (value "10"))))))
	(d-let
		(p-assign (ident "result"))
		(e-num (value "52"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> a where [a.plus : a, b -> a]"))
		(patt (type "Dec")))
	(expressions
		(expr (type "a -> a where [a.plus : a, b -> a]"))
		(expr (type "Dec"))))
~~~
