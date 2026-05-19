# META
~~~ini
description=For loop iterating over List Bool
type=snippet
~~~
# SOURCE
~~~roc
result : Bool
result = {
	var allTrue_ = Bool.True
	for b in [Bool.True, Bool.True, Bool.False] {
		if b == Bool.False {
			allTrue_ = Bool.False
		} else {
			{}
		}
	}
	allTrue_
}

expect result == Bool.False
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
KwFor,LowerIdent,KwIn,OpenSquare,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,Comma,UpperIdent,NoSpaceDotUpperIdent,CloseSquare,OpenCurly,
KwIf,LowerIdent,OpEquals,UpperIdent,NoSpaceDotUpperIdent,OpenCurly,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotUpperIdent,
CloseCurly,KwElse,OpenCurly,
OpenCurly,CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,UpperIdent,NoSpaceDotUpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "result")
			(ty (name "Bool")))
		(s-decl
			(p-ident (raw "result"))
			(e-block
				(statements
					(s-var (name "allTrue_")
						(e-tag (raw "Bool.True")))
					(s-for
						(p-ident (raw "b"))
						(e-list
							(e-tag (raw "Bool.True"))
							(e-tag (raw "Bool.True"))
							(e-tag (raw "Bool.False")))
						(e-block
							(statements
								(e-if-then-else
									(e-binop (op "==")
										(e-ident (raw "b"))
										(e-tag (raw "Bool.False")))
									(e-block
										(statements
											(s-decl
												(p-ident (raw "allTrue_"))
												(e-tag (raw "Bool.False")))))
									(e-block
										(statements
											(e-record)))))))
					(e-ident (raw "allTrue_")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-tag (raw "Bool.False"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "result"))
		(e-block
			(s-var
				(p-assign (ident "allTrue_"))
				(e-nominal-external
					(builtin)
					(e-tag (name "True"))))
			(s-expr
				(e-block
					(s-var
						(p-assign (ident "#for_iter_1"))
						(e-dispatch-call (method "iter") (constraint-fn-var 111)
							(receiver
								(e-list
									(elems
										(e-nominal-external
											(builtin)
											(e-tag (name "True")))
										(e-nominal-external
											(builtin)
											(e-tag (name "True")))
										(e-nominal-external
											(builtin)
											(e-tag (name "False"))))))
							(args)))
					(s-while
						(e-tag (name "True"))
						(e-match
							(match
								(cond
									(e-dispatch-call (method "next") (constraint-fn-var 166)
										(receiver
											(e-lookup-local
												(p-assign (ident "#for_iter_1"))))
										(args)))
								(branches
									(branch
										(patterns
											(pattern (degenerate false)
												(p-applied-tag)))
										(value
											(e-block
												(s-reassign
													(p-assign (ident "#for_iter_1"))
													(e-lookup-local
														(p-assign (ident "#for_rest_2"))))
												(s-expr
													(e-block
														(e-if
															(if-branches
																(if-branch
																	(e-method-eq (negated "false")
																		(lhs
																			(e-lookup-local
																				(p-assign (ident "b"))))
																		(rhs
																			(e-nominal-external
																				(builtin)
																				(e-tag (name "False")))))
																	(e-block
																		(s-reassign
																			(p-assign (ident "allTrue_"))
																			(e-nominal-external
																				(builtin)
																				(e-tag (name "False"))))
																		(e-empty_record))))
															(if-else
																(e-block
																	(e-empty_record))))))
												(e-empty_record))))
									(branch
										(patterns
											(pattern (degenerate false)
												(p-applied-tag)))
										(value
											(e-block
												(s-break)
												(e-empty_record))))
									(branch
										(patterns
											(pattern (degenerate false)
												(p-applied-tag)))
										(value
											(e-block
												(s-break)
												(e-empty_record))))))))
					(e-empty_record)))
			(e-lookup-local
				(p-assign (ident "allTrue_"))))
		(annotation
			(ty-lookup (name "Bool") (builtin))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "result"))))
			(rhs
				(e-nominal-external
					(builtin)
					(e-tag (name "False")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Bool")))
	(expressions
		(expr (type "Bool"))))
~~~
