# META
~~~ini
description=Variable scoping with var keyword
type=snippet
~~~
# SOURCE
~~~roc
# Regular function with var usage
processItems = |items| {
	var count_ = 0
	var total_ = 0

	# Reassign vars within same function - should work
	count_ = count_ + 1
	total_ = total_ + 10

	# Nested function - var reassignment should fail across function boundary
	nestedFunc = |_| {
		count_ = count_ + 5 # Should cause error - different function
		total_ = total_ * 2 # Should cause error - different function
		count_
	}

	result = nestedFunc({})
	total_ + result
}
~~~
# EXPECTED
VAR REASSIGNMENT ERROR - :0:0:0:0
VAR REASSIGNMENT ERROR - :0:0:0:0
UNUSED VARIABLE - can_var_scoping_regular_var.md:2:17:2:22
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Var Reassignment Error")
		(headline
			(reflow "Cannot reassign a ")
			(annotated keyword "var")
			(reflow " from outside the function where it was declared."))
		(document
			(reflow "Variables declared with ")
			(annotated keyword "var")
			(reflow " can only be reassigned within the same function scope.")))
	(report
		(severity runtime_error)
		(title "Var Reassignment Error")
		(headline
			(reflow "Cannot reassign a ")
			(annotated keyword "var")
			(reflow " from outside the function where it was declared."))
		(document
			(reflow "Variables declared with ")
			(annotated keyword "var")
			(reflow " can only be reassigned within the same function scope.")))
	(report
		(severity warning)
		(title "Unused Variable")
		(region (start 2 17) (end 2 22))
		(headline
			(reflow "Variable ")
			(annotated symbol-unqualified "items")
			(reflow " is defined here and then never used:"))
		(document
			(reflow "If you don't need this variable, prefix it with an underscore like ")
			(annotated symbol-unqualified "_items")
			(reflow " to suppress this warning.")
			(line-break)
			(source-region (file "can_var_scoping_regular_var.md") (start 2 17) (end 2 22) (annotation error) (line-text "processItems = |items| {")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwVar,LowerIdent,OpAssign,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,OpStar,Int,
LowerIdent,
CloseCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
LowerIdent,OpPlus,LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-decl
			(p-ident (raw "processItems"))
			(e-lambda
				(args
					(p-ident (raw "items")))
				(e-block
					(statements
						(s-var (name "count_")
							(e-int (raw "0")))
						(s-var (name "total_")
							(e-int (raw "0")))
						(s-decl
							(p-ident (raw "count_"))
							(e-binop (op "+")
								(e-ident (raw "count_"))
								(e-int (raw "1"))))
						(s-decl
							(p-ident (raw "total_"))
							(e-binop (op "+")
								(e-ident (raw "total_"))
								(e-int (raw "10"))))
						(s-decl
							(p-ident (raw "nestedFunc"))
							(e-lambda
								(args
									(p-underscore))
								(e-block
									(statements
										(s-decl
											(p-ident (raw "count_"))
											(e-binop (op "+")
												(e-ident (raw "count_"))
												(e-int (raw "5"))))
										(s-decl
											(p-ident (raw "total_"))
											(e-binop (op "*")
												(e-ident (raw "total_"))
												(e-int (raw "2"))))
										(e-ident (raw "count_"))))))
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "nestedFunc"))
								(e-record)))
						(e-binop (op "+")
							(e-ident (raw "total_"))
							(e-ident (raw "result")))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "processItems"))
		(e-lambda
			(args
				(p-assign (ident "items")))
			(e-block
				(s-var
					(p-assign (ident "count_"))
					(e-num (value "0")))
				(s-var
					(p-assign (ident "total_"))
					(e-num (value "0")))
				(s-reassign
					(p-assign (ident "count_"))
					(e-dispatch-call (method "plus") (constraint-fn-var 263)
						(receiver
							(e-lookup-local
								(p-assign (ident "count_"))))
						(args
							(e-num (value "1")))))
				(s-reassign
					(p-assign (ident "total_"))
					(e-dispatch-call (method "plus") (constraint-fn-var 272)
						(receiver
							(e-lookup-local
								(p-assign (ident "total_"))))
						(args
							(e-num (value "10")))))
				(s-let
					(p-assign (ident "nestedFunc"))
					(e-closure
						(captures
							(capture (ident "count_")))
						(e-lambda
							(args
								(p-underscore))
							(e-block
								(s-reassign
									(p-assign (ident "count_"))
									(e-runtime-error (tag "var_across_function_boundary")))
								(s-reassign
									(p-assign (ident "total_"))
									(e-runtime-error (tag "var_across_function_boundary")))
								(e-lookup-local
									(p-assign (ident "count_")))))))
				(s-let
					(p-assign (ident "result"))
					(e-call (constraint-fn-var 276)
						(e-lookup-local
							(p-assign (ident "nestedFunc")))
						(e-empty_record)))
				(e-dispatch-call (method "plus") (constraint-fn-var 277)
					(receiver
						(e-lookup-local
							(p-assign (ident "total_"))))
					(args
						(e-lookup-local
							(p-assign (ident "result")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, c -> b, c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]")))
	(expressions
		(expr (type "_arg -> a where [a.from_numeral : Numeral -> Try(a, [InvalidNumeral(Str)]), a.plus : a, b -> a, b.from_numeral : Numeral -> Try(b, [InvalidNumeral(Str)]), b.plus : b, c -> b, c.from_numeral : Numeral -> Try(c, [InvalidNumeral(Str)])]"))))
~~~
