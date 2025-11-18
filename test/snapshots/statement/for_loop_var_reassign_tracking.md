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
TYPE MISMATCH - for_loop_var_reassign_tracking.md:13:2:13:6
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**for_loop_var_reassign_tracking.md:13:2:13:6:**
```roc
	sum_ + max_
```
	^^^^

It has the type:
    _Num(_size)_

But the type annotation says it should have the type:
    _U64_

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
	(type-module)
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
				(p-assign (ident "sum_"))
				(e-num (value "0")))
			(s-var
				(p-assign (ident "max_"))
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
						(p-assign (ident "sum_"))
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "sum_")))
							(e-lookup-local
								(p-assign (ident "n")))))
					(e-if
						(if-branches
							(if-branch
								(e-binop (op "gt")
									(e-lookup-local
										(p-assign (ident "n")))
									(e-lookup-local
										(p-assign (ident "max_"))))
								(e-block
									(s-reassign
										(p-assign (ident "max_"))
										(e-lookup-local
											(p-assign (ident "n"))))
									(e-empty_record))))
						(if-else
							(e-block
								(e-empty_record))))))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "sum_")))
				(e-lookup-local
					(p-assign (ident "max_")))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "result")))
			(e-num (value "31")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
