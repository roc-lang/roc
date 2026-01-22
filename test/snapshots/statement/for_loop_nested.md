# META
~~~ini
description=Nested for loops
type=snippet
~~~
# SOURCE
~~~roc
product : U64
product = {
	var $result = 0
	for i in [1, 2, 3] {
		for j in [10, 20] {
			$result = $result + (i * j)
		}
	}
	$result
}

expect product == 180
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
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,Comma,Int,CloseSquare,OpenCurly,
KwFor,LowerIdent,KwIn,OpenSquare,Int,Comma,Int,CloseSquare,OpenCurly,
LowerIdent,OpAssign,LowerIdent,OpPlus,OpenRound,LowerIdent,OpStar,LowerIdent,CloseRound,
CloseCurly,
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
		(s-type-anno (name "product")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "product"))
			(e-block
				(statements
					(s-var (name "$result")
						(e-int (raw "0")))
					(s-for
						(p-ident (raw "i"))
						(e-list
							(e-int (raw "1"))
							(e-int (raw "2"))
							(e-int (raw "3")))
						(e-block
							(statements
								(s-for
									(p-ident (raw "j"))
									(e-list
										(e-int (raw "10"))
										(e-int (raw "20")))
									(e-block
										(statements
											(s-decl
												(p-ident (raw "$result"))
												(e-binop (op "+")
													(e-ident (raw "$result"))
													(e-tuple
														(e-binop (op "*")
															(e-ident (raw "i"))
															(e-ident (raw "j"))))))))))))
					(e-ident (raw "$result")))))
		(s-expect
			(e-binop (op "==")
				(e-ident (raw "product"))
				(e-int (raw "180"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "product"))
		(e-block
			(s-var
				(p-assign (ident "$result"))
				(e-num (value "0")))
			(s-for
				(p-assign (ident "i"))
				(e-list
					(elems
						(e-num (value "1"))
						(e-num (value "2"))
						(e-num (value "3"))))
				(e-block
					(s-for
						(p-assign (ident "j"))
						(e-list
							(elems
								(e-num (value "10"))
								(e-num (value "20"))))
						(e-block
							(s-reassign
								(p-assign (ident "$result"))
								(e-binop (op "add")
									(e-lookup-local
										(p-assign (ident "$result")))
									(e-binop (op "mul")
										(e-lookup-local
											(p-assign (ident "i")))
										(e-lookup-local
											(p-assign (ident "j"))))))
							(e-empty_record)))
					(e-empty_record)))
			(e-lookup-local
				(p-assign (ident "$result"))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "product")))
			(e-num (value "180")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
