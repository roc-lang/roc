# META
~~~ini
description=An expect cannot reassign a var declared outside it, but can reassign vars declared inside it, and for/match binders reusing an outer var's name are not reassignments
type=snippet
~~~
# SOURCE
~~~roc
withdraw : I64, I64 -> I64
withdraw = |balance, amount| {
	var $remaining = balance
	var $count = 0

	expect {
		$remaining = $remaining - amount
		$remaining >= 0
	}

	expect {
		($count, extra) = (1, 2)
		$count + extra == 3
	}

	expect {
		var $left = balance
		$left = $left - amount
		for x in [1, 2] {
			$left = $left + x
		}
		$left >= 0
	}

	expect {
		for $count in [1, 2] {
			_y = $count
		}
		True
	}

	$count = $count + 1
	$remaining - amount + $count
}
~~~
# EXPECTED
VAR REASSIGNED IN EXPECT - var_reassigned_in_expect.md:7:3:7:13
VAR REASSIGNED IN EXPECT - var_reassigned_in_expect.md:12:4:12:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Var Reassigned In Expect")
		(region (start 7 3) (end 7 13))
		(headline
			(reflow "This ")
			(annotated code "expect")
			(reflow " reassigns ")
			(annotated symbol-unqualified "$remaining")
			(reflow ", which was declared outside of it:"))
		(document
			(source-region (file "var_reassigned_in_expect.md") (start 7 3) (end 7 13) (annotation error) (line-text "\t\t$remaining = $remaining - amount"))
			(line-break)
			(annotated symbol-unqualified "$remaining")
			(reflow " was declared here:")
			(line-break)
			(source-region (file "var_reassigned_in_expect.md") (start 3 6) (end 3 16) (annotation dim) (line-text "\tvar $remaining = balance"))
			(line-break)
			(reflow "Optimized builds remove inline ")
			(annotated code "expect")
			(reflow "s, so an ")
			(annotated code "expect")
			(reflow " must not change variables declared outside of it, or the program would behave differently in optimized builds. Variables declared inside the ")
			(annotated code "expect")
			(reflow " can be reassigned freely.")))
	(report
		(severity runtime_error)
		(title "Var Reassigned In Expect")
		(region (start 12 4) (end 12 10))
		(headline
			(reflow "This ")
			(annotated code "expect")
			(reflow " reassigns ")
			(annotated symbol-unqualified "$count")
			(reflow ", which was declared outside of it:"))
		(document
			(source-region (file "var_reassigned_in_expect.md") (start 12 4) (end 12 10) (annotation error) (line-text "\t\t($count, extra) = (1, 2)"))
			(line-break)
			(annotated symbol-unqualified "$count")
			(reflow " was declared here:")
			(line-break)
			(source-region (file "var_reassigned_in_expect.md") (start 4 6) (end 4 12) (annotation dim) (line-text "\tvar $count = 0"))
			(line-break)
			(reflow "Optimized builds remove inline ")
			(annotated code "expect")
			(reflow "s, so an ")
			(annotated code "expect")
			(reflow " must not change variables declared outside of it, or the program would behave differently in optimized builds. Variables declared inside the ")
			(annotated code "expect")
			(reflow " can be reassigned freely."))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,LowerIdent,
KwVar,LowerIdent,OpAssign,Int,
KwExpect,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpBinaryMinus,LowerIdent,
LowerIdent,OpGreaterThanOrEq,Int,
CloseCurly,
KwExpect,OpenCurly,
OpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpAssign,OpenRound,Int,Comma,Int,CloseRound,
LowerIdent,OpPlus,LowerIdent,OpEquals,Int,
CloseCurly,
KwExpect,OpenCurly,
KwVar,LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,OpBinaryMinus,LowerIdent,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,OpGreaterThanOrEq,Int,
CloseCurly,
KwExpect,OpenCurly,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,CloseSquare,OpenCurly,
NamedUnderscore,OpAssign,LowerIdent,
CloseCurly,
UpperIdent,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpBinaryMinus,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "withdraw")
			(ty-fn
				(ty (name "I64"))
				(ty (name "I64"))
				(ty (name "I64"))))
		(s-decl
			(p-ident (raw "withdraw"))
			(e-lambda
				(args
					(p-ident (raw "balance"))
					(p-ident (raw "amount")))
				(e-block
					(statements
						(s-var (name "$remaining")
							(e-ident (raw "balance")))
						(s-var (name "$count")
							(e-int (raw "0")))
						(s-expect
							(e-block
								(statements
									(s-decl
										(p-ident (raw "$remaining"))
										(e-binop (op "-")
											(e-ident (raw "$remaining"))
											(e-ident (raw "amount"))))
									(e-binop (op ">=")
										(e-ident (raw "$remaining"))
										(e-int (raw "0"))))))
						(s-expect
							(e-block
								(statements
									(s-decl
										(p-tuple
											(p-ident (raw "$count"))
											(p-ident (raw "extra")))
										(e-tuple
											(e-int (raw "1"))
											(e-int (raw "2"))))
									(e-binop (op "==")
										(e-binop (op "+")
											(e-ident (raw "$count"))
											(e-ident (raw "extra")))
										(e-int (raw "3"))))))
						(s-expect
							(e-block
								(statements
									(s-var (name "$left")
										(e-ident (raw "balance")))
									(s-decl
										(p-ident (raw "$left"))
										(e-binop (op "-")
											(e-ident (raw "$left"))
											(e-ident (raw "amount"))))
									(s-for
										(p-ident (raw "x"))
										(e-list
											(e-int (raw "1"))
											(e-int (raw "2")))
										(e-block
											(statements
												(s-decl
													(p-ident (raw "$left"))
													(e-binop (op "+")
														(e-ident (raw "$left"))
														(e-ident (raw "x")))))))
									(e-binop (op ">=")
										(e-ident (raw "$left"))
										(e-int (raw "0"))))))
						(s-expect
							(e-block
								(statements
									(s-for
										(p-ident (raw "$count"))
										(e-list
											(e-int (raw "1"))
											(e-int (raw "2")))
										(e-block
											(statements
												(s-decl
													(p-ident (raw "_y"))
													(e-ident (raw "$count"))))))
									(e-tag (raw "True")))))
						(s-decl
							(p-ident (raw "$count"))
							(e-binop (op "+")
								(e-ident (raw "$count"))
								(e-int (raw "1"))))
						(e-binop (op "+")
							(e-binop (op "-")
								(e-ident (raw "$remaining"))
								(e-ident (raw "amount")))
							(e-ident (raw "$count")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "withdraw"))
		(e-lambda
			(args
				(p-assign (ident "balance"))
				(p-assign (ident "amount")))
			(e-block
				(s-var
					(p-var-assign (ident "$remaining"))
					(e-lookup-local
						(p-assign (ident "balance"))))
				(s-var
					(p-var-assign (ident "$count"))
					(e-num (value "0")))
				(s-expect
					(e-block
						(s-runtime-error (tag "var_reassigned_in_expect"))
						(e-dispatch-call (method "is_gte") (constraint-fn-var 315)
							(receiver
								(e-lookup-local
									(p-var-assign (ident "$remaining"))))
							(args
								(e-num (value "0"))))))
				(s-expect
					(e-block
						(s-let
							(p-tuple
								(patterns
									(p-runtime-error (tag "var_reassigned_in_expect"))
									(p-assign (ident "extra"))))
							(e-runtime-error (tag "erroneous_value_expr")))
						(e-runtime-error (tag "erroneous_value_expr")
							(e-runtime-error (tag "erroneous_value_expr")
								(e-lookup-local
									(p-var-assign (ident "$count")))
								(e-runtime-error (tag "erroneous_value_expr"))))))
				(s-expect
					(e-block
						(s-var
							(p-var-assign (ident "$left"))
							(e-lookup-local
								(p-assign (ident "balance"))))
						(s-reassign
							(p-var-assign (ident "$left"))
							(e-dispatch-call (method "minus") (constraint-fn-var 346)
								(receiver
									(e-lookup-local
										(p-var-assign (ident "$left"))))
								(args
									(e-lookup-local
										(p-assign (ident "amount"))))))
						(s-for
							(p-assign (ident "x"))
							(e-list
								(elems
									(e-num (value "1"))
									(e-num (value "2"))))
							(e-block
								(s-reassign
									(p-var-assign (ident "$left"))
									(e-dispatch-call (method "plus") (constraint-fn-var 410)
										(receiver
											(e-lookup-local
												(p-var-assign (ident "$left"))))
										(args
											(e-lookup-local
												(p-assign (ident "x"))))))
								(e-empty_record)))
						(e-dispatch-call (method "is_gte") (constraint-fn-var 425)
							(receiver
								(e-lookup-local
									(p-var-assign (ident "$left"))))
							(args
								(e-num (value "0"))))))
				(s-expect
					(e-block
						(s-for
							(p-var-assign (ident "$count"))
							(e-list
								(elems
									(e-num (value "1"))
									(e-num (value "2"))))
							(e-block
								(s-let
									(p-assign (ident "_y"))
									(e-lookup-local
										(p-var-assign (ident "$count"))))
								(e-empty_record)))
						(e-tag (name "True"))))
				(s-reassign
					(p-var-assign (ident "$count"))
					(e-dispatch-call (method "plus") (constraint-fn-var 487)
						(receiver
							(e-lookup-local
								(p-var-assign (ident "$count"))))
						(args
							(e-num (value "1")))))
				(e-dispatch-call (method "plus") (constraint-fn-var 493)
					(receiver
						(e-dispatch-call (method "minus") (constraint-fn-var 489)
							(receiver
								(e-lookup-local
									(p-var-assign (ident "$remaining"))))
							(args
								(e-lookup-local
									(p-assign (ident "amount"))))))
					(args
						(e-lookup-local
							(p-var-assign (ident "$count")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin))
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "I64, I64 -> I64")))
	(expressions
		(expr (type "I64, I64 -> I64"))))
~~~
