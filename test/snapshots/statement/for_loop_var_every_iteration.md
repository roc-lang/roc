# META
~~~ini
description=For loop with var reassignment on every iteration
type=snippet
~~~
# SOURCE
~~~roc
result : U64
result = {
	var $prev = 0
	var $count = 0
	for n in [10, 20, 30, 40, 50] {
		$count = $count + 1
		$prev = n
	}
	$prev + $count
}

expect result == 55
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
	(type-module)
	(statements
		(s-type-anno (name "result")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "result"))
			(e-block
				(statements
					(s-var (name "$prev")
						(e-int (raw "0")))
					(s-var (name "$count")
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
									(p-ident (raw "$count"))
									(e-binop (op "+")
										(e-ident (raw "$count"))
										(e-int (raw "1"))))
								(s-decl
									(p-ident (raw "$prev"))
									(e-ident (raw "n"))))))
					(e-binop (op "+")
						(e-ident (raw "$prev"))
						(e-ident (raw "$count"))))))
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
				(p-assign (ident "$prev"))
				(e-num (value "0")))
			(s-var
				(p-assign (ident "$count"))
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
						(p-assign (ident "$count"))
						(e-binop (op "add")
							(e-lookup-local
								(p-assign (ident "$count")))
							(e-num (value "1"))))
					(s-reassign
						(p-assign (ident "$prev"))
						(e-lookup-local
							(p-assign (ident "n"))))
					(e-empty_record)))
			(e-binop (op "add")
				(e-lookup-local
					(p-assign (ident "$prev")))
				(e-lookup-local
					(p-assign (ident "$count")))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-expect
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "result")))
			(e-num (value "55")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "U64")))
	(expressions
		(expr (type "U64"))))
~~~
