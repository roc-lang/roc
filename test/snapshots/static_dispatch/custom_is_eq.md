# META
~~~ini
description=Custom is_eq method on nominal type is used by == operator
type=file:Point.roc
~~~
# SOURCE
~~~roc
Point := { x: I64, y: I64 }.{
    is_eq : Point, Point -> Bool
    is_eq = |a, b| a.x == b.x and a.y == b.y
}

p1 : Point
p1 = { x: 1, y: 2 }

p2 : Point
p2 = { x: 1, y: 2 }

main : Bool
main = p1 == p2

main2 : Bool
main2 = p1 != p2
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,Dot,OpenCurly,
LowerIdent,OpColon,UpperIdent,Comma,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceDotLowerIdent,OpEquals,LowerIdent,NoSpaceDotLowerIdent,OpAnd,LowerIdent,NoSpaceDotLowerIdent,OpEquals,LowerIdent,NoSpaceDotLowerIdent,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,OpEquals,LowerIdent,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,OpNotEquals,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Point")
				(args))
			(ty-record
				(anno-record-field (name "x")
					(ty (name "I64")))
				(anno-record-field (name "y")
					(ty (name "I64"))))
			(associated
				(s-type-anno (name "is_eq")
					(ty-fn
						(ty (name "Point"))
						(ty (name "Point"))
						(ty (name "Bool"))))
				(s-decl
					(p-ident (raw "is_eq"))
					(e-lambda
						(args
							(p-ident (raw "a"))
							(p-ident (raw "b")))
						(e-binop (op "and")
							(e-binop (op "==")
								(e-field-access
									(e-ident (raw "a"))
									(e-ident (raw "x")))
								(e-field-access
									(e-ident (raw "b"))
									(e-ident (raw "x"))))
							(e-binop (op "==")
								(e-field-access
									(e-ident (raw "a"))
									(e-ident (raw "y")))
								(e-field-access
									(e-ident (raw "b"))
									(e-ident (raw "y")))))))))
		(s-type-anno (name "p1")
			(ty (name "Point")))
		(s-decl
			(p-ident (raw "p1"))
			(e-record
				(field (field "x")
					(e-int (raw "1")))
				(field (field "y")
					(e-int (raw "2")))))
		(s-type-anno (name "p2")
			(ty (name "Point")))
		(s-decl
			(p-ident (raw "p2"))
			(e-record
				(field (field "x")
					(e-int (raw "1")))
				(field (field "y")
					(e-int (raw "2")))))
		(s-type-anno (name "main")
			(ty (name "Bool")))
		(s-decl
			(p-ident (raw "main"))
			(e-binop (op "==")
				(e-ident (raw "p1"))
				(e-ident (raw "p2"))))
		(s-type-anno (name "main2")
			(ty (name "Bool")))
		(s-decl
			(p-ident (raw "main2"))
			(e-binop (op "!=")
				(e-ident (raw "p1"))
				(e-ident (raw "p2"))))))
~~~
# FORMATTED
~~~roc
Point := { x : I64, y : I64 }.{
	is_eq : Point, Point -> Bool
	is_eq = |a, b| a.x == b.x and a.y == b.y
}

p1 : Point
p1 = { x: 1, y: 2 }

p2 : Point
p2 = { x: 1, y: 2 }

main : Bool
main = p1 == p2

main2 : Bool
main2 = p1 != p2
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Point.is_eq"))
		(e-lambda
			(args
				(p-assign (ident "a"))
				(p-assign (ident "b")))
			(e-binop (op "and")
				(e-binop (op "eq")
					(e-dot-access (field "x")
						(receiver
							(e-lookup-local
								(p-assign (ident "a")))))
					(e-dot-access (field "x")
						(receiver
							(e-lookup-local
								(p-assign (ident "b"))))))
				(e-binop (op "eq")
					(e-dot-access (field "y")
						(receiver
							(e-lookup-local
								(p-assign (ident "a")))))
					(e-dot-access (field "y")
						(receiver
							(e-lookup-local
								(p-assign (ident "b"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Point") (local))
				(ty-lookup (name "Point") (local))
				(ty-lookup (name "Bool") (builtin)))))
	(d-let
		(p-assign (ident "p1"))
		(e-record
			(fields
				(field (name "x")
					(e-num (value "1")))
				(field (name "y")
					(e-num (value "2")))))
		(annotation
			(ty-lookup (name "Point") (local))))
	(d-let
		(p-assign (ident "p2"))
		(e-record
			(fields
				(field (name "x")
					(e-num (value "1")))
				(field (name "y")
					(e-num (value "2")))))
		(annotation
			(ty-lookup (name "Point") (local))))
	(d-let
		(p-assign (ident "main"))
		(e-binop (op "eq")
			(e-lookup-local
				(p-assign (ident "p1")))
			(e-lookup-local
				(p-assign (ident "p2"))))
		(annotation
			(ty-lookup (name "Bool") (builtin))))
	(d-let
		(p-assign (ident "main2"))
		(e-binop (op "ne")
			(e-lookup-local
				(p-assign (ident "p1")))
			(e-lookup-local
				(p-assign (ident "p2"))))
		(annotation
			(ty-lookup (name "Bool") (builtin))))
	(s-nominal-decl
		(ty-header (name "Point"))
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
		(patt (type "Point, Point -> Bool"))
		(patt (type "Point"))
		(patt (type "Point"))
		(patt (type "Bool"))
		(patt (type "Bool")))
	(type_decls
		(nominal (type "Point")
			(ty-header (name "Point"))))
	(expressions
		(expr (type "Point, Point -> Bool"))
		(expr (type "Point"))
		(expr (type "Point"))
		(expr (type "Bool"))
		(expr (type "Bool"))))
~~~
