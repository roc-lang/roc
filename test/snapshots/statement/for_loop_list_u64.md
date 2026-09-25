# META
~~~ini
description=For loop iterating over List U64
type=snippet
~~~
# SOURCE
~~~roc
sum : U64
sum = {
	var total_ = 0
	for n in [1, 2, 3, 4, 5] {
		total_ = total_ + n
	}
	total_
}

expect sum == 15
~~~
# EXPECTED
VAR NAME MISSING `$` - for_loop_list_u64.md:3:6:3:12
# PROBLEMS
~~~clojure
(reports
	(report
		(severity warning)
		(title "Var Name Missing `$`")
		(region (start 3 6) (end 3 12))
		(headline
			(reflow "The mutable binding ")
			(annotated symbol-unqualified "total_")
			(reflow " is declared with ")
			(annotated keyword "var")
			(reflow " but its name does not start with ")
			(annotated code "$")
			(reflow "."))
		(document
			(reflow "Rename this binding and all of its uses to ")
			(annotated symbol-unqualified "$total_")
			(reflow ". The name is only a convention; mutability comes from the ")
			(annotated keyword "var")
			(reflow " declaration.")
			(line-break)
			(line-break)
			(source-region (file "for_loop_list_u64.md") (start 3 6) (end 3 12) (annotation warning) (line-text "\tvar total_ = 0")))))
~~~
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,
LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "sum")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "sum"))
			(e-block
				(statements
					(s-var (name "total_")
						(e-int (raw "0")))
					(s-for
						(p-ident (raw "n"))
						(e-list
							(e-int (raw "1"))
							(e-int (raw "2"))
							(e-int (raw "3"))
							(e-int (raw "4"))
							(e-int (raw "5")))
						(e-block
							(statements
								(s-decl
									(p-ident (raw "total_"))
									(e-binop (op "+")
										(e-ident (raw "total_"))
										(e-ident (raw "n")))))))
					(e-ident (raw "total_")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "sum"))
				(e-int (raw "15"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "sum"))
		(e-block
			(s-var
				(p-var-assign (ident "total_"))
				(e-num (value "0")))
			(s-for
				(p-assign (ident "n"))
				(e-list
					(elems
						(e-num (value "1"))
						(e-num (value "2"))
						(e-num (value "3"))
						(e-num (value "4"))
						(e-num (value "5"))))
				(e-block
					(s-reassign
						(p-var-assign (ident "total_"))
						(e-dispatch-call (method "plus") (constraint-fn-var 321)
							(receiver
								(e-lookup-local
									(p-var-assign (ident "total_"))))
							(args
								(e-lookup-local
									(p-assign (ident "n"))))))
					(e-empty_record)))
			(e-lookup-local
				(p-var-assign (ident "total_"))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-lookup-local
					(p-assign (ident "sum"))))
			(rhs
				(e-num (value "15"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
