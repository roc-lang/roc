# META
~~~ini
description=For loop with complex var mutation
type=snippet
~~~
# SOURCE
~~~roc
countEvens : U64
countEvens = {
	var count_ = 0
	var sum_ = 0
	for n in [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] {
		if n % 2 == 0 {
			count_ = count_ + 1
			sum_ = sum_ + n
		} else {
			{}
		}
	}
	count_ * sum_
}

expect countEvens == 150
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,
KwVar,LowerIdent,OpAssign,Int,
KwVar,LowerIdent,OpAssign,Int,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,OpenCurly,
KwIf,LowerIdent,OpPercent,Int,OpEquals,Int,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
CloseCurly,KwElse,OpenCurly,
OpenCurly,CloseCurly,
CloseCurly,
CloseCurly,
LowerIdent,OpStar,LowerIdent,
CloseCurly,
KwExpect,LowerIdent,OpEquals,Int,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "countEvens")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "countEvens"))
			(e-block
				(statements
					(s-var (name "count_")
						(e-int (raw "0")))
					(s-var (name "sum_")
						(e-int (raw "0")))
					(s-for
						(p-ident (raw "n"))
						(e-list
							(e-int (raw "1"))
							(e-int (raw "2"))
							(e-int (raw "3"))
							(e-int (raw "4"))
							(e-int (raw "5"))
							(e-int (raw "6"))
							(e-int (raw "7"))
							(e-int (raw "8"))
							(e-int (raw "9"))
							(e-int (raw "10")))
						(e-block
							(statements
								(e-if-then-else
									(e-binop (op "==")
										(e-binop (op "%")
											(e-ident (raw "n"))
											(e-int (raw "2")))
										(e-int (raw "0")))
									(e-block
										(statements
											(s-decl
												(p-ident (raw "count_"))
												(e-binop (op "+")
													(e-ident (raw "count_"))
													(e-int (raw "1"))))
											(s-decl
												(p-ident (raw "sum_"))
												(e-binop (op "+")
													(e-ident (raw "sum_"))
													(e-ident (raw "n"))))))
									(e-block
										(statements
											(e-record)))))))
					(e-binop (op "*")
						(e-ident (raw "count_"))
						(e-ident (raw "sum_"))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "countEvens"))
				(e-int (raw "150"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "countEvens"))
		(e-block
			(s-var
				(p-assign (ident "count_"))
				(e-num (value "0")))
			(s-var
				(p-assign (ident "sum_"))
				(e-num (value "0")))
			(s-for
				(p-assign (ident "n"))
				(e-list
					(elems
						(e-num (value "1"))
						(e-num (value "2"))
						(e-num (value "3"))
						(e-num (value "4"))
						(e-num (value "5"))
						(e-num (value "6"))
						(e-num (value "7"))
						(e-num (value "8"))
						(e-num (value "9"))
						(e-num (value "10"))))
				(e-block
					(e-if
						(if-branches
							(if-branch
								(e-binop (op "eq")
									(e-binop (op "rem")
										(e-lookup-local
											(p-assign (ident "n")))
										(e-num (value "2")))
									(e-num (value "0")))
								(e-block
									(s-reassign
										(p-assign (ident "count_"))
										(e-binop (op "add")
											(e-lookup-local
												(p-assign (ident "count_")))
											(e-num (value "1"))))
									(s-reassign
										(p-assign (ident "sum_"))
										(e-binop (op "add")
											(e-lookup-local
												(p-assign (ident "sum_")))
											(e-lookup-local
												(p-assign (ident "n")))))
									(e-empty_record))))
						(if-else
							(e-block
								(e-empty_record))))))
			(e-binop (op "mul")
				(e-lookup-local
					(p-assign (ident "count_")))
				(e-lookup-local
					(p-assign (ident "sum_")))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "countEvens")))
			(e-num (value "150")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(Int(Unsigned64))")))
	(expressions
		(expr (type "Num(Int(Unsigned64))"))))
~~~
