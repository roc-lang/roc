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
TYPE MISMATCH - for_loop_list_u64.md:7:2:7:8
# PROBLEMS
**TYPE MISMATCH**
This expression is used in an unexpected way:
**for_loop_list_u64.md:7:2:7:8:**
```roc
	total_
```
	^^^^^^

It has the type:
    _Num(_size)_

But the type annotation says it should have the type:
    _U64_

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
	(type-module)
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
				(p-assign (ident "total_"))
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
						(p-assign (ident "total_"))
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "total_")))
							(e-lookup-local
								(p-assign (ident "n")))))
					(e-empty_record)))
			(e-lookup-local
				(p-assign (ident "total_"))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "sum")))
			(e-num (value "15")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error")))
	(expressions
		(expr (type "Error"))))
~~~
