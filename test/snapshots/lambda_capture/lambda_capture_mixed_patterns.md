# META
~~~ini
description=Mixed capture patterns in block expression
type=expr
~~~
# SOURCE
~~~roc
(|base| {
		simple = |x| base + x + 1
		simple(1)
})(1)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,Int,
LowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-lambda
			(args
				(p-ident (raw "base")))
			(e-block
				(statements
					(s-decl
						(p-ident (raw "simple"))
						(e-lambda
							(args
								(p-ident (raw "x")))
							(e-binop (op "+")
								(e-binop (op "+")
									(e-ident (raw "base"))
									(e-ident (raw "x")))
								(e-int (raw "1")))))
					(e-apply
						(e-ident (raw "simple"))
						(e-int (raw "1")))))))
	(e-int (raw "1")))
~~~
# FORMATTED
~~~roc
(
	|base| {
		simple = |x| base + x + 1
		simple(1)
	},
)(1)
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-lambda
		(args
			(p-assign (ident "base")))
		(e-block
			(s-let
				(p-assign (ident "simple"))
				(e-closure
					(captures
						(capture (ident "base")))
					(e-lambda
						(args
							(p-assign (ident "x")))
						(e-binop (op "add")
							(e-binop (op "add")
								(e-lookup-local
									(p-assign (ident "base")))
								(e-lookup-local
									(p-assign (ident "x"))))
							(e-num (value "1"))))))
			(e-call
				(e-lookup-local
					(p-assign (ident "simple")))
				(e-num (value "1")))))
	(e-num (value "1")))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
