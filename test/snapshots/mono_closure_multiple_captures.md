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
c1_add_ab = |x, captures| captures.a + captures.b + x

func = |a, b| {
	add_ab = C1_add_ab({ a, b })
	match add_ab {
		C1_add_ab(captures) => c1_add_ab(10, captures)
	}
}

result = func(1, 2)
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
					(e-tag (name "#1_add_ab")
						(args
							(e-record
								(fields
									(field (name "a")
										(e-lookup-local
											(p-assign (ident "a"))))
									(field (name "b")
										(e-lookup-local
											(p-assign (ident "b")))))))))
				(e-match
					(match
						(cond
							(e-lookup-local
								(p-assign (ident "add_ab"))))
						(branches
							(branch
								(patterns
									(pattern (degenerate false)
										(p-applied-tag)))
								(value
									(e-call
										(e-lookup-local
											(p-assign (ident "c1_add_ab")))
										(e-num (value "10"))
										(e-lookup-local
											(p-assign (ident "captures"))))))))))))
	(d-let
		(p-assign (ident "result"))
		(e-call
			(e-lookup-local
				(p-assign (ident "func")))
			(e-num (value "1"))
			(e-num (value "2")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "c, Dec -> c where [c.plus : c, Dec -> c]"))
		(patt (type "Dec")))
	(expressions
		(expr (type "c, Dec -> _ret where [c.plus : c, Dec -> c]"))
		(expr (type "_c where [_d.plus : e, Dec -> e]"))))
~~~
