# META
~~~ini
description=For loop with var reassignment on every iteration
type=snippet
~~~
# SOURCE
~~~roc
result : U64
result = {
	var prev_ = 0
	var count_ = 0
	for n in [10, 20, 30, 40, 50] {
		count_ = count_ + 1
		prev_ = n
	}
	prev_ + count_
}

expect result == 55
~~~
# EXPECTED
VAR NAME MISSING `$` - for_loop_var_every_iteration.md:3:6:3:11
VAR NAME MISSING `$` - for_loop_var_every_iteration.md:4:6:4:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 3 6) (end 3 11))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "prev_")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$prev_")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "for_loop_var_every_iteration.md") (start 3 6) (end 3 11) (annotation warning) (line-text "\tvar prev_ = 0"))))
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 4 6) (end 4 12))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "count_")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$count_")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "for_loop_var_every_iteration.md") (start 4 6) (end 4 12) (annotation warning) (line-text "\tvar count_ = 0")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwVar,LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,
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
					(s-var (name "prev_")
						(e-int (raw "0")))
					(s-var (name "count_")
						(e-int (raw "0")))
					(s-for
						(p-ident (raw "n"))
						(e-list
							(e-int (raw "10"))
							(e-int (raw "20"))
							(e-int (raw "30"))
							(e-int (raw "40"))
							(e-int (raw "50")))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "count_"))
									(e-binop (op "+")
										(e-ident (raw "count_"))
										(e-int (raw "1"))))
								(s-decl
									(p-ident (raw "prev_"))
									(e-ident (raw "n"))))))
					(e-binop (op "+")
						(e-ident (raw "prev_"))
						(e-ident (raw "count_"))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-int (raw "55"))))))
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
				(p-var-assign (ident "prev_"))
				(e-num (value "0")))
			(s-var
				(p-var-assign (ident "count_"))
				(e-num (value "0")))
			(s-for
				(p-assign (ident "n"))
				(e-list
					(elems
						(e-num (value "10"))
						(e-num (value "20"))
						(e-num (value "30"))
						(e-num (value "40"))
						(e-num (value "50"))))
				(e-block
					(s-reassign
						(p-var-assign (ident "count_"))
						(e-dispatch-call (method "plus") (constraint-fn-var 343)
							(receiver
								(e-lookup-local
									(p-var-assign (ident "count_"))))
							(args
								(e-num (value "1")))))
					(s-reassign
						(p-var-assign (ident "prev_"))
						(e-lookup-local
							(p-assign (ident "n"))))
					(e-empty_record)))
			(e-dispatch-call (method "plus") (constraint-fn-var 348)
				(receiver
					(e-lookup-local
						(p-var-assign (ident "prev_"))))
				(args
					(e-lookup-local
						(p-var-assign (ident "count_"))))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "result"))))
			(rhs
				(e-num (value "55"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
