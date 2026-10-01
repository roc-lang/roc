# META
~~~ini
description=For loop with var reassignment tracking across iterations
type=snippet
~~~
# SOURCE
~~~roc
result : U64
result = {
	var sum_ = 0
	var max_ = 0
	for n in [3, 7, 2, 9, 1] {
		sum_ = sum_ + n
		if n > max_ {
			max_ = n
		} else {
			{}
		}
	}
	sum_ + max_
}

expect result == 31
~~~
# EXPECTED
VAR NAME MISSING `$` - for_loop_var_reassign_tracking.md:3:6:3:10
VAR NAME MISSING `$` - for_loop_var_reassign_tracking.md:4:6:4:10
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 3 6) (end 3 10))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "sum_")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$sum_")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "for_loop_var_reassign_tracking.md") (start 3 6) (end 3 10) (annotation warning) (line-text "\tvar sum_ = 0"))))
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 4 6) (end 4 10))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "max_")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$max_")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "for_loop_var_reassign_tracking.md") (start 4 6) (end 4 10) (annotation warning) (line-text "\tvar max_ = 0")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwVar,LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
KwIf,LowerIdent,OpGreaterThan,LowerIdent,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
CloseCurly,KwElse,OpenCurly,
OpenCurly,CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpPlus,LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "result")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "result"))
			(e-block
				(statements
					(s-var (name "sum_")
						(e-int (raw "0")))
					(s-var (name "max_")
						(e-int (raw "0")))
					(s-for
						(p-ident (raw "n"))
						(e-list
							(e-int (raw "3"))
							(e-int (raw "7"))
							(e-int (raw "2"))
							(e-int (raw "9"))
							(e-int (raw "1")))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "sum_"))
									(e-binop (op "+")
										(e-ident (raw "sum_"))
										(e-ident (raw "n"))))
								(e-if-then-else
									(e-binop (op ">")
										(e-ident (raw "n"))
										(e-ident (raw "max_")))
									(e-block
										(statements
											(s-decl
												(p-ident (raw "max_"))
												(e-ident (raw "n")))))
									(e-block
										(statements
											(e-record)))))))
					(e-binop (op "+")
						(e-ident (raw "sum_"))
						(e-ident (raw "max_"))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-int (raw "31"))))))
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
				(p-var-assign (ident "sum_"))
				(e-num (value "0")))
			(s-var
				(p-var-assign (ident "max_"))
				(e-num (value "0")))
			(s-for
				(p-assign (ident "n"))
				(e-list
					(elems
						(e-num (value "3"))
						(e-num (value "7"))
						(e-num (value "2"))
						(e-num (value "9"))
						(e-num (value "1"))))
				(e-block
					(s-reassign
						(p-var-assign (ident "sum_"))
						(e-dispatch-call (method "plus") (constraint-fn-var 344)
							(receiver
								(e-lookup-local
									(p-var-assign (ident "sum_"))))
							(args
								(e-lookup-local
									(p-assign (ident "n"))))))
					(e-if
						(if-branches
							(if-branch
								(e-dispatch-call (method "is_gt") (constraint-fn-var 347)
									(receiver
										(e-lookup-local
											(p-assign (ident "n"))))
									(args
										(e-lookup-local
											(p-var-assign (ident "max_")))))
								(e-block
									(s-reassign
										(p-var-assign (ident "max_"))
										(e-lookup-local
											(p-assign (ident "n"))))
									(e-empty_record))))
						(if-else
							(e-block
								(e-empty_record))))))
			(e-dispatch-call (method "plus") (constraint-fn-var 353)
				(receiver
					(e-lookup-local
						(p-var-assign (ident "sum_"))))
				(args
					(e-lookup-local
						(p-var-assign (ident "max_"))))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "result"))))
			(rhs
				(e-num (value "31"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
