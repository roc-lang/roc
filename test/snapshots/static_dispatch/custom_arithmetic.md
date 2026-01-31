# META
~~~ini
description=Custom plus, minus, times methods on nominal type are used by operators
type=file:Vec.roc
~~~
# SOURCE
~~~roc
Vec := { x: I64, y: I64 }.{
    plus : Vec, Vec -> Vec
    plus = |a, b| { x: a.x + b.x, y: a.y + b.y }

    minus : Vec, Vec -> Vec
    minus = |a, b| { x: a.x - b.x, y: a.y - b.y }

    times : Vec, Vec -> Vec
    times = |a, b| { x: a.x * b.x, y: a.y * b.y }
}

v1 : Vec
v1 = { x: 1, y: 2 }

v2 : Vec
v2 = { x: 3, y: 4 }

added : Vec
added = v1 + v2

subtracted : Vec
subtracted = v1 - v2

multiplied : Vec
multiplied = v1 * v2

main : (Vec, Vec, Vec)
main = (added, subtracted, multiplied)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpPlus,LowerIdent,NoSpaceDotLowerIdent,Comma,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpPlus,LowerIdent,NoSpaceDotLowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpBinaryMinus,LowerIdent,NoSpaceDotLowerIdent,Comma,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpBinaryMinus,LowerIdent,NoSpaceDotLowerIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,OpenCurly,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpStar,LowerIdent,NoSpaceDotLowerIdent,Comma,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpStar,LowerIdent,NoSpaceDotLowerIdent,CloseCurly,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,OpBinaryMinus,LowerIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,OpStar,LowerIdent,
LowerIdent,OpColon,OpenRound,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseRound,
LowerIdent,OpAssign,OpenRound,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Vec")
				(args))
			(ty-record
				(anno-record-field (name "x")
					(ty (name "I64")))
				(anno-record-field (name "y")
					(ty (name "I64"))))
			(associated
				(s-type-anno (name "plus")
					(ty-fn
						(ty (name "Vec"))
						(ty (name "Vec"))
						(ty (name "Vec"))))
				(s-decl
					(p-ident (raw "plus"))
					(e-lambda
						(args
							(p-ident (raw "a"))
							(p-ident (raw "b")))
						(e-record
							(field (field "x")
								(e-binop (op "+")
									(e-field-access
										(e-ident (raw "a"))
										(e-ident (raw "x")))
									(e-field-access
										(e-ident (raw "b"))
										(e-ident (raw "x")))))
							(field (field "y")
								(e-binop (op "+")
									(e-field-access
										(e-ident (raw "a"))
										(e-ident (raw "y")))
									(e-field-access
										(e-ident (raw "b"))
										(e-ident (raw "y"))))))))
				(s-type-anno (name "minus")
					(ty-fn
						(ty (name "Vec"))
						(ty (name "Vec"))
						(ty (name "Vec"))))
				(s-decl
					(p-ident (raw "minus"))
					(e-lambda
						(args
							(p-ident (raw "a"))
							(p-ident (raw "b")))
						(e-record
							(field (field "x")
								(e-binop (op "-")
									(e-field-access
										(e-ident (raw "a"))
										(e-ident (raw "x")))
									(e-field-access
										(e-ident (raw "b"))
										(e-ident (raw "x")))))
							(field (field "y")
								(e-binop (op "-")
									(e-field-access
										(e-ident (raw "a"))
										(e-ident (raw "y")))
									(e-field-access
										(e-ident (raw "b"))
										(e-ident (raw "y"))))))))
				(s-type-anno (name "times")
					(ty-fn
						(ty (name "Vec"))
						(ty (name "Vec"))
						(ty (name "Vec"))))
				(s-decl
					(p-ident (raw "times"))
					(e-lambda
						(args
							(p-ident (raw "a"))
							(p-ident (raw "b")))
						(e-record
							(field (field "x")
								(e-binop (op "*")
									(e-field-access
										(e-ident (raw "a"))
										(e-ident (raw "x")))
									(e-field-access
										(e-ident (raw "b"))
										(e-ident (raw "x")))))
							(field (field "y")
								(e-binop (op "*")
									(e-field-access
										(e-ident (raw "a"))
										(e-ident (raw "y")))
									(e-field-access
										(e-ident (raw "b"))
										(e-ident (raw "y"))))))))))
		(s-type-anno (name "v1")
			(ty (name "Vec")))
		(s-decl
			(p-ident (raw "v1"))
			(e-record
				(field (field "x")
					(e-int (raw "1")))
				(field (field "y")
					(e-int (raw "2")))))
		(s-type-anno (name "v2")
			(ty (name "Vec")))
		(s-decl
			(p-ident (raw "v2"))
			(e-record
				(field (field "x")
					(e-int (raw "3")))
				(field (field "y")
					(e-int (raw "4")))))
		(s-type-anno (name "added")
			(ty (name "Vec")))
		(s-decl
			(p-ident (raw "added"))
			(e-binop (op "+")
				(e-ident (raw "v1"))
				(e-ident (raw "v2"))))
		(s-type-anno (name "subtracted")
			(ty (name "Vec")))
		(s-decl
			(p-ident (raw "subtracted"))
			(e-binop (op "-")
				(e-ident (raw "v1"))
				(e-ident (raw "v2"))))
		(s-type-anno (name "multiplied")
			(ty (name "Vec")))
		(s-decl
			(p-ident (raw "multiplied"))
			(e-binop (op "*")
				(e-ident (raw "v1"))
				(e-ident (raw "v2"))))
		(s-type-anno (name "main")
			(ty-tuple
				(ty (name "Vec"))
				(ty (name "Vec"))
				(ty (name "Vec"))))
		(s-decl
			(p-ident (raw "main"))
			(e-tuple
				(e-ident (raw "added"))
				(e-ident (raw "subtracted"))
				(e-ident (raw "multiplied"))))))
~~~
# FORMATTED
~~~roc
Vec := { x : I64, y : I64 }.{
	plus : Vec, Vec -> Vec
	plus = |a, b| { x: a.x + b.x, y: a.y + b.y }

	minus : Vec, Vec -> Vec
	minus = |a, b| { x: a.x - b.x, y: a.y - b.y }

	times : Vec, Vec -> Vec
	times = |a, b| { x: a.x * b.x, y: a.y * b.y }
}

v1 : Vec
v1 = { x: 1, y: 2 }

v2 : Vec
v2 = { x: 3, y: 4 }

added : Vec
added = v1 + v2

subtracted : Vec
subtracted = v1 - v2

multiplied : Vec
multiplied = v1 * v2

main : (Vec, Vec, Vec)
main = (added, subtracted, multiplied)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Vec.plus"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-record
				(fields
					(field (name "x")
						(e-binop (op "add")
							(e-dot-access (field "x")
								(receiver
									(e-lookup-local
										(p-assign (ident "a")))))
							(e-dot-access (field "x")
								(receiver
									(e-lookup-local
										(p-assign (ident "b")))))))
					(field (name "y")
						(e-binop (op "add")
							(e-dot-access (field "y")
								(receiver
									(e-lookup-local
										(p-assign (ident "a")))))
							(e-dot-access (field "y")
								(receiver
									(e-lookup-local
										(p-assign (ident "b"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Vec") (local))
				(ty-lookup (name "Vec") (local))
				(ty-lookup (name "Vec") (local)))))
	(d-let
		(p-assign (ident "Vec.minus"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-record
				(fields
					(field (name "x")
						(e-binop (op "sub")
							(e-dot-access (field "x")
								(receiver
									(e-lookup-local
										(p-assign (ident "a")))))
							(e-dot-access (field "x")
								(receiver
									(e-lookup-local
										(p-assign (ident "b")))))))
					(field (name "y")
						(e-binop (op "sub")
							(e-dot-access (field "y")
								(receiver
									(e-lookup-local
										(p-assign (ident "a")))))
							(e-dot-access (field "y")
								(receiver
									(e-lookup-local
										(p-assign (ident "b"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Vec") (local))
				(ty-lookup (name "Vec") (local))
				(ty-lookup (name "Vec") (local)))))
	(d-let
		(p-assign (ident "Vec.times"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-record
				(fields
					(field (name "x")
						(e-binop (op "mul")
							(e-dot-access (field "x")
								(receiver
									(e-lookup-local
										(p-assign (ident "a")))))
							(e-dot-access (field "x")
								(receiver
									(e-lookup-local
										(p-assign (ident "b")))))))
					(field (name "y")
						(e-binop (op "mul")
							(e-dot-access (field "y")
								(receiver
									(e-lookup-local
										(p-assign (ident "a")))))
							(e-dot-access (field "y")
								(receiver
									(e-lookup-local
										(p-assign (ident "b"))))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Vec") (local))
				(ty-lookup (name "Vec") (local))
				(ty-lookup (name "Vec") (local)))))
	(d-let
		(p-assign (ident "v1"))
		(e-record
			(fields
				(field (name "x")
					(e-num (value "1")))
				(field (name "y")
					(e-num (value "2")))))
		(annotation
			(ty-lookup (name "Vec") (local))))
	(d-let
		(p-assign (ident "v2"))
		(e-record
			(fields
				(field (name "x")
					(e-num (value "3")))
				(field (name "y")
					(e-num (value "4")))))
		(annotation
			(ty-lookup (name "Vec") (local))))
	(d-let
		(p-assign (ident "added"))
		(e-binop (op "add")
			(e-lookup-local
				(p-assign (ident "v1")))
			(e-lookup-local
				(p-assign (ident "v2"))))
		(annotation
			(ty-lookup (name "Vec") (local))))
	(d-let
		(p-assign (ident "subtracted"))
		(e-binop (op "sub")
			(e-lookup-local
				(p-assign (ident "v1")))
			(e-lookup-local
				(p-assign (ident "v2"))))
		(annotation
			(ty-lookup (name "Vec") (local))))
	(d-let
		(p-assign (ident "multiplied"))
		(e-binop (op "mul")
			(e-lookup-local
				(p-assign (ident "v1")))
			(e-lookup-local
				(p-assign (ident "v2"))))
		(annotation
			(ty-lookup (name "Vec") (local))))
	(d-let
		(p-assign (ident "main"))
		(e-tuple
			(elems
				(e-lookup-local
					(p-assign (ident "added")))
				(e-lookup-local
					(p-assign (ident "subtracted")))
				(e-lookup-local
					(p-assign (ident "multiplied")))))
		(annotation
			(ty-tuple
				(ty-lookup (name "Vec") (local))
				(ty-lookup (name "Vec") (local))
				(ty-lookup (name "Vec") (local)))))
	(s-nominal-decl
		(ty-header (name "Vec"))
		(ty-record
			(field (field "x")
				(ty-lookup (name "I64") (builtin)))
			(field (field "y")
				(ty-lookup (name "I64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Vec, Vec -> Vec"))
		(patt (type "Vec, Vec -> Vec"))
		(patt (type "Vec, Vec -> Vec"))
		(patt (type "Vec"))
		(patt (type "Vec"))
		(patt (type "Vec"))
		(patt (type "Vec"))
		(patt (type "Vec"))
		(patt (type "(Vec, Vec, Vec)")))
	(type_decls
		(nominal (type "Vec")
			(ty-header (name "Vec"))))
	(expressions
		(expr (type "Vec, Vec -> Vec"))
		(expr (type "Vec, Vec -> Vec"))
		(expr (type "Vec, Vec -> Vec"))
		(expr (type "Vec"))
		(expr (type "Vec"))
		(expr (type "Vec"))
		(expr (type "Vec"))
		(expr (type "Vec"))
		(expr (type "(Vec, Vec, Vec)"))))
~~~
