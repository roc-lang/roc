# META
~~~ini
description=return directly inside a top-level or inline expect is a compile error, and break cannot exit a loop enclosing the expect
type=snippet
~~~
# SOURCE
~~~roc
expect {
	return 1
}

f : I64 -> I64
f = |x| {
	expect {
		if x == 1 {
			return x
		}
		x == 2
	}
	x
}

g : List(I64) -> List(I64)
g = |xs| {
	for x in xs {
		expect {
			if x == 1 {
				break
			}
			x != 2
		}
	}
	xs
}
~~~
# EXPECTED
RETURN IN EXPECT - return_in_expect.md:2:2:2:10
RETURN IN EXPECT - return_in_expect.md:9:4:9:12
BREAK IN EXPECT - return_in_expect.md:21:5:21:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Return In Expect")
		(region (start 2 2) (end 2 10))
		(headline
			(reflow "The ")
			(annotated code "return")
			(reflow " keyword cannot be used directly inside an ")
			(annotated code "expect")
			(reflow "."))
		(document
			(source-region (file "return_in_expect.md") (start 2 2) (end 2 10) (annotation error) (line-text "\treturn 1"))
			(line-break)
			(reflow "Optimized builds remove inline ")
			(annotated code "expect")
			(reflow "s, so an ")
			(annotated code "expect")
			(reflow " must not move control flow outside of itself, or the program would behave differently in optimized builds.")))
	(report
		(severity runtime_error)
		(title "Return In Expect")
		(region (start 9 4) (end 9 12))
		(headline
			(reflow "The ")
			(annotated code "return")
			(reflow " keyword cannot be used directly inside an ")
			(annotated code "expect")
			(reflow "."))
		(document
			(source-region (file "return_in_expect.md") (start 9 4) (end 9 12) (annotation error) (line-text "\t\t\treturn x"))
			(line-break)
			(reflow "Optimized builds remove inline ")
			(annotated code "expect")
			(reflow "s, so an ")
			(annotated code "expect")
			(reflow " must not move control flow outside of itself, or the program would behave differently in optimized builds.")))
	(report
		(severity runtime_error)
		(title "Break In Expect")
		(region (start 21 5) (end 21 10))
		(headline
			(reflow "The ")
			(annotated code "break")
			(reflow " statement cannot exit a loop from inside an ")
			(annotated code "expect")
			(reflow "."))
		(document
			(source-region (file "return_in_expect.md") (start 21 5) (end 21 10) (annotation error) (line-text "\t\t\t\tbreak"))
			(line-break)
			(reflow "Optimized builds remove inline ")
			(annotated code "expect")
			(reflow "s, so an ")
			(annotated code "expect")
			(reflow " must not move control flow outside of itself, or the program would behave differently in optimized builds."))))
~~~
# TOKENS
~~~zig
KwExpect,OpenCurly,
KwReturn,Int,
CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwExpect,OpenCurly,
KwIf,LowerIdent,OpEquals,Int,OpenCurly,
KwReturn,LowerIdent,
CloseCurly,
LowerIdent,OpEquals,Int,
CloseCurly,
LowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwFor,LowerIdent,KwIn,LowerIdent,OpenCurly,
KwExpect,OpenCurly,
KwIf,LowerIdent,OpEquals,Int,OpenCurly,
KwBreak,
CloseCurly,
LowerIdent,OpNotEquals,Int,
CloseCurly,
CloseCurly,
LowerIdent,
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
					(s-return
						(e-int (raw "1"))))))
		(s-type-anno (name "f")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "f"))
			(e-lambda
				(args
					(p-ident (raw "x")))
				(e-block
					(statements
						(s-expect
							(e-block
								(statements
									(e-if-without-else
										(e-binop (op "==")
											(e-ident (raw "x"))
											(e-int (raw "1")))
										(e-block
											(statements
												(s-return
													(e-ident (raw "x"))))))
									(e-binop (op "==")
										(e-ident (raw "x"))
										(e-int (raw "2"))))))
						(e-ident (raw "x"))))))
		(s-type-anno (name "g")
			(ty-fn
				(ty-apply
					(ty (name "List"))
					(ty (name "I64")))
				(ty-apply
					(ty (name "List"))
					(ty (name "I64")))))
		(s-decl
			(p-ident (raw "g"))
			(e-lambda
				(args
					(p-ident (raw "xs")))
				(e-block
					(statements
						(s-for
							(p-ident (raw "x"))
							(e-ident (raw "xs"))
							(e-block
								(statements
									(s-expect
										(e-block
											(statements
												(e-if-without-else
													(e-binop (op "==")
														(e-ident (raw "x"))
														(e-int (raw "1")))
													(e-block
														(statements
															(s-break))))
												(e-binop (op "!=")
													(e-ident (raw "x"))
													(e-int (raw "2")))))))))
						(e-ident (raw "xs"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "f"))
		(e-lambda
			(args
				(p-assign (ident "x")))
			(e-block
				(s-expect
					(e-block
						(s-runtime-error (tag "erroneous_value_expr"))
						(e-method-eq (negated "false")
							(lhs
								(e-lookup-local
									(p-assign (ident "x"))))
							(rhs
								(e-num (value "2"))))))
				(e-lookup-local
					(p-assign (ident "x")))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin)))))
	(d-let
		(p-assign (ident "g"))
		(e-lambda
			(args
				(p-assign (ident "xs")))
			(e-block
				(s-for
					(p-assign (ident "x"))
					(e-lookup-local
						(p-assign (ident "xs")))
					(e-block
						(s-expect
							(e-block
								(s-expr
									(e-if
										(if-branches
											(if-branch
												(e-method-eq (negated "false")
													(lhs
														(e-lookup-local
															(p-assign (ident "x"))))
													(rhs
														(e-num (value "1"))))
												(e-block
													(s-runtime-error (tag "control_flow_in_expect"))
													(e-empty_record))))
										(if-else
											(e-empty_record))))
								(e-method-eq (negated "true")
									(lhs
										(e-lookup-local
											(p-assign (ident "x"))))
									(rhs
										(e-num (value "2"))))))
						(e-empty_record)))
				(e-lookup-local
					(p-assign (ident "xs")))))
		(annotation
			(ty-fn (effectful false)
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "I64") (builtin)))
				(ty-apply (name "List") (builtin)
					(ty-lookup (name "I64") (builtin))))))
	(s-expect
		(e-runtime-error (tag "erroneous_value_expr"))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64 -> I64"))
		(patt (type "List(I64) -> List(I64)")))
	(expressions
		(expr (type "I64 -> I64"))
		(expr (type "List(I64) -> List(I64)"))))
~~~
