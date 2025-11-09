# META
~~~ini
description="The test case from the original request, involving multiple levels of nesting and local assignments. This will be the ultimate validation."
type=expr
~~~
# SOURCE
~~~roc
(((|a| {
    a_loc = a * 2
    |b| {
        b_loc = a_loc + b
        |c| b_loc + c
    }
})(100))(20))(3)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
OpenRound,NoSpaceOpenRound,NoSpaceOpenRound,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
CloseCurly,CloseRound,NoSpaceOpenRound,Int,CloseRound,CloseRound,NoSpaceOpenRound,Int,CloseRound,CloseRound,NoSpaceOpenRound,Int,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(e-apply
	(e-tuple
		(e-apply
			(e-tuple
				(e-apply
					(e-tuple
						(e-lambda
							(args
								(p-ident (raw "a")))
							(e-block
								(statements
									(s-decl
										(p-ident (raw "a_loc"))
										(e-binop (op "*")
											(e-ident (raw "a"))
											(e-int (raw "2"))))
									(e-lambda
										(args
											(p-ident (raw "b")))
										(e-block
											(statements
												(s-decl
													(p-ident (raw "b_loc"))
													(e-binop (op "+")
														(e-ident (raw "a_loc"))
														(e-ident (raw "b"))))
												(e-lambda
													(args
														(p-ident (raw "c")))
													(e-binop (op "+")
														(e-ident (raw "b_loc"))
														(e-ident (raw "c")))))))))))
					(e-int (raw "100"))))
			(e-int (raw "20"))))
	(e-int (raw "3")))
~~~
# FORMATTED
~~~roc
(
	(
		(
			|a| {
				a_loc = a * 2
				|b| {
					b_loc = a_loc + b
					|c| b_loc + c
				}
			},
		)(100),
	)(20),
)(3)
~~~
# CANONICALIZE
~~~clojure
(e-call
	(e-call
		(e-call
			(e-lambda
				(args
					(p-assign (ident "a")))
				(e-block
					(s-let
						(p-assign (ident "a_loc"))
						(e-binop (op "mul")
							(e-lookup-local
								(p-assign (ident "a")))
							(e-num (value "2"))))
					(e-closure
						(captures
							(capture (ident "a_loc")))
						(e-lambda
							(args
								(p-assign (ident "b")))
							(e-block
								(s-let
									(p-assign (ident "b_loc"))
									(e-binop (op "add")
										(e-lookup-local
											(p-assign (ident "a_loc")))
										(e-lookup-local
											(p-assign (ident "b")))))
								(e-closure
									(captures
										(capture (ident "b_loc")))
									(e-lambda
										(args
											(p-assign (ident "c")))
										(e-binop (op "add")
											(e-lookup-local
												(p-assign (ident "b_loc")))
											(e-lookup-local
												(p-assign (ident "c")))))))))))
			(e-num (value "100")))
		(e-num (value "20")))
	(e-num (value "3")))
~~~
# TYPES
~~~clojure
(expr (type "Num(_size)"))
~~~
