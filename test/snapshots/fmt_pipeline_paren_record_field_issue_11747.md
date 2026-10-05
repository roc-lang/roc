# META
~~~ini
description=Issue 11747 - Preserve field-value application in pipe targets while removing redundant grouping around bare field targets
type=snippet
~~~
# SOURCE
~~~roc
expect {
	rec = { func1: |x| x + 3, func2: |x, y| x + y }
	result1 = 2 |> (rec.func1)
	result2 = 2 |> (rec.func2)(3)
	result1 == result2
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
KwExpect,OpenCurly,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,OpBar,LowerIdent,OpBar,LowerIdent,OpPlus,Int,Comma,LowerIdent,OpColon,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,OpPlus,LowerIdent,CloseCurly,
LowerIdent,OpAssign,Int,OpPizza,OpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,
LowerIdent,OpAssign,Int,OpPizza,OpenRound,LowerIdent,NoSpaceDotLowerIdent,CloseRound,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,OpEquals,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-expect
			(e-block
				(statements
					(s-decl
						(p-ident (raw "rec"))
						(e-record
							(field (field "func1")
								(e-lambda
									(args
										(p-ident (raw "x")))
									(e-binop (op "+")
										(e-ident (raw "x"))
										(e-int (raw "3")))))
							(field (field "func2")
								(e-lambda
									(args
										(p-ident (raw "x"))
										(p-ident (raw "y")))
									(e-binop (op "+")
										(e-ident (raw "x"))
										(e-ident (raw "y")))))))
					(s-decl
						(p-ident (raw "result1"))
						(e-arrow-call
							(e-int (raw "2"))
							(e-field-access
								(receiver
									(e-ident (raw "rec")))
								(segment (mode "required") (field "func1")))))
					(s-decl
						(p-ident (raw "result2"))
						(e-arrow-call
							(e-int (raw "2"))
							(e-apply
								(e-field-access
									(receiver
										(e-ident (raw "rec")))
									(segment (mode "required") (field "func2")))
								(e-int (raw "3")))))
					(e-binop (op "==")
						(e-ident (raw "result1"))
						(e-ident (raw "result2"))))))))
~~~
# FORMATTED
~~~roc
expect {
	rec = { func1: |x| x + 3, func2: |x, y| x + y }
	result1 = 2 |> rec.func1
	result2 = 2 |> (rec.func2)(3)
	result1 == result2
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-expect
		(e-block
			(s-let
				(p-assign (ident "rec"))
				(e-record
					(fields
						(field (name "func1")
							(e-lambda
								(args
									(p-assign (ident "x")))
								(e-dispatch-call (method "plus") (constraint-fn-var 248)
									(receiver
										(e-lookup-local
											(p-assign (ident "x"))))
									(args
										(e-num (value "3"))))))
						(field (name "func2")
							(e-lambda
								(args
									(p-assign (ident "x"))
									(p-assign (ident "y")))
								(e-dispatch-call (method "plus") (constraint-fn-var 257)
									(receiver
										(e-lookup-local
											(p-assign (ident "x"))))
									(args
										(e-lookup-local
											(p-assign (ident "y"))))))))))
			(s-let
				(p-assign (ident "result1"))
				(e-call (constraint-fn-var 276)
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "rec"))))
						(segments
							(segment (name "func1") (mode "required"))))
					(e-num (value "2"))))
			(s-let
				(p-assign (ident "result2"))
				(e-call (constraint-fn-var 295)
					(e-field-access
						(receiver
							(e-lookup-local
								(p-assign (ident "rec"))))
						(segments
							(segment (name "func2") (mode "required"))))
					(e-num (value "2"))
					(e-num (value "3"))))
			(e-method-eq (negated "false")
				(lhs
					(e-lookup-local
						(p-assign (ident "result1"))))
				(rhs
					(e-lookup-local
						(p-assign (ident "result2"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(expressions))
~~~
