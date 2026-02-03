# META
~~~ini
description=For loop with var that persists across iterations with conditional updates
type=snippet
~~~
# SOURCE
~~~roc
result : U64
result = {
	var $lastEven = 0
	var $evenCount = 0
	for n in [1, 2, 3, 4, 5, 6, 7, 8] {
		if n % 2 == 0 {
			$lastEven = n
			$evenCount = $evenCount + 1
		} else {
			{}
		}
	}
	$lastEven * $evenCount
}

expect result == 32
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
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,Comma,Int,CloseSquare,OpenCurly,
KwIf,LowerIdent,OpPercent,Int,OpEquals,Int,OpenCurly,
LowerIdent,OpAssign,LowerIdent,
LowerIdent,OpAssign,LowerIdent,OpPlus,Int,
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
		(s-type-anno (name "result")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "result"))
			(e-block
				(statements
					(s-var (name "$lastEven")
						(e-int (raw "0")))
					(s-var (name "$evenCount")
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
							(e-int (raw "8")))
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
												(p-ident (raw "$lastEven"))
												(e-ident (raw "n")))
											(s-decl
												(p-ident (raw "$evenCount"))
												(e-binop (op "+")
													(e-ident (raw "$evenCount"))
													(e-int (raw "1"))))))
									(e-block
										(statements
											(e-record)))))))
					(e-binop (op "*")
						(e-ident (raw "$lastEven"))
						(e-ident (raw "$evenCount"))))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "result"))
				(e-int (raw "32"))))))
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
				(p-assign (ident "$lastEven"))
				(e-num (value "0")))
			(s-var
				(p-assign (ident "$evenCount"))
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
						(e-num (value "8"))))
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
										(p-assign (ident "$lastEven"))
										(e-lookup-local
											(p-assign (ident "n"))))
									(s-reassign
										(p-assign (ident "$evenCount"))
										(e-binop (op "add")
											(e-lookup-local
												(p-assign (ident "$evenCount")))
											(e-num (value "1"))))
									(e-empty_record))))
						(if-else
							(e-block
								(e-empty_record))))))
			(e-binop (op "mul")
				(e-lookup-local
					(p-assign (ident "$lastEven")))
				(e-lookup-local
					(p-assign (ident "$evenCount")))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "result")))
			(e-num (value "32")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
