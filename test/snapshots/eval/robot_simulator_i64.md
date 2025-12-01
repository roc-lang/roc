# META
~~~ini
description=Robot simulator with I64 coordinates and record updates
type=snippet
~~~
# SOURCE
~~~roc
Robot : { x : I64, y : I64 }

advance_y : Robot -> Robot
advance_y = |robot| { ..robot, y: robot.y + 1 }

retreat_y : Robot -> Robot
retreat_y = |robot| { ..robot, y: robot.y - 1 }

advance_x : Robot -> Robot
advance_x = |robot| { ..robot, x: robot.x + 1 }

retreat_x : Robot -> Robot
retreat_x = |robot| { ..robot, x: robot.x - 1 }

expect advance_y({ x: 0, y: 0 }) == { x: 0, y: 1 }
expect retreat_y({ x: 0, y: 0 }) == { x: 0, y: -1 }
expect advance_x({ x: 0, y: 0 }) == { x: 1, y: 0 }
expect retreat_x({ x: 0, y: 0 }) == { x: -1, y: 0 }
expect advance_y(retreat_y({ x: 5, y: 5 })) == { x: 5, y: 5 }
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpPlus,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpBinaryMinus,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpPlus,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpBinaryMinus,Int,CloseCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,OpEquals,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,OpEquals,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,OpEquals,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,OpEquals,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
KwExpect,LowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceOpenRound,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,CloseRound,CloseRound,OpEquals,OpenCurly,LowerIdent,OpColon,Int,Comma,LowerIdent,OpColon,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Robot")
				(args))
			(ty-record
				(anno-record-field (name "x")
					(ty (name "I64")))
				(anno-record-field (name "y")
					(ty (name "I64")))))
		(s-type-anno (name "advance_y")
			(ty-fn
				(ty (name "Robot"))
				(ty (name "Robot"))))
		(s-decl
			(p-ident (raw "advance_y"))
			(e-lambda
				(args
					(p-ident (raw "robot")))
				(e-record
					(ext
						(e-ident (raw "robot")))
					(field (field "y")
						(e-binop (op "+")
							(e-field-access
								(e-ident (raw "robot"))
								(e-ident (raw "y")))
							(e-int (raw "1")))))))
		(s-type-anno (name "retreat_y")
			(ty-fn
				(ty (name "Robot"))
				(ty (name "Robot"))))
		(s-decl
			(p-ident (raw "retreat_y"))
			(e-lambda
				(args
					(p-ident (raw "robot")))
				(e-record
					(ext
						(e-ident (raw "robot")))
					(field (field "y")
						(e-binop (op "-")
							(e-field-access
								(e-ident (raw "robot"))
								(e-ident (raw "y")))
							(e-int (raw "1")))))))
		(s-type-anno (name "advance_x")
			(ty-fn
				(ty (name "Robot"))
				(ty (name "Robot"))))
		(s-decl
			(p-ident (raw "advance_x"))
			(e-lambda
				(args
					(p-ident (raw "robot")))
				(e-record
					(ext
						(e-ident (raw "robot")))
					(field (field "x")
						(e-binop (op "+")
							(e-field-access
								(e-ident (raw "robot"))
								(e-ident (raw "x")))
							(e-int (raw "1")))))))
		(s-type-anno (name "retreat_x")
			(ty-fn
				(ty (name "Robot"))
				(ty (name "Robot"))))
		(s-decl
			(p-ident (raw "retreat_x"))
			(e-lambda
				(args
					(p-ident (raw "robot")))
				(e-record
					(ext
						(e-ident (raw "robot")))
					(field (field "x")
						(e-binop (op "-")
							(e-field-access
								(e-ident (raw "robot"))
								(e-ident (raw "x")))
							(e-int (raw "1")))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "advance_y"))
					(e-record
						(field (field "x")
							(e-int (raw "0")))
						(field (field "y")
							(e-int (raw "0")))))
				(e-record
					(field (field "x")
						(e-int (raw "0")))
					(field (field "y")
						(e-int (raw "1"))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "retreat_y"))
					(e-record
						(field (field "x")
							(e-int (raw "0")))
						(field (field "y")
							(e-int (raw "0")))))
				(e-record
					(field (field "x")
						(e-int (raw "0")))
					(field (field "y")
						(e-int (raw "-1"))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "advance_x"))
					(e-record
						(field (field "x")
							(e-int (raw "0")))
						(field (field "y")
							(e-int (raw "0")))))
				(e-record
					(field (field "x")
						(e-int (raw "1")))
					(field (field "y")
						(e-int (raw "0"))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "retreat_x"))
					(e-record
						(field (field "x")
							(e-int (raw "0")))
						(field (field "y")
							(e-int (raw "0")))))
				(e-record
					(field (field "x")
						(e-int (raw "-1")))
					(field (field "y")
						(e-int (raw "0"))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "advance_y"))
					(e-apply
						(e-ident (raw "retreat_y"))
						(e-record
							(field (field "x")
								(e-int (raw "5")))
							(field (field "y")
								(e-int (raw "5"))))))
				(e-record
					(field (field "x")
						(e-int (raw "5")))
					(field (field "y")
						(e-int (raw "5"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "advance_y"))
		(e-lambda
			(args
				(p-assign (ident "robot")))
			(e-record
				(ext
					(e-lookup-local
						(p-assign (ident "robot"))))
				(fields
					(field (name "y")
						(e-binop (op "add")
							(e-dot-access (field "y")
								(receiver
									(e-lookup-local
										(p-assign (ident "robot")))))
							(e-num (value "1")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Robot") (local))
				(ty-lookup (name "Robot") (local)))))
	(d-let
		(p-assign (ident "retreat_y"))
		(e-lambda
			(args
				(p-assign (ident "robot")))
			(e-record
				(ext
					(e-lookup-local
						(p-assign (ident "robot"))))
				(fields
					(field (name "y")
						(e-binop (op "sub")
							(e-dot-access (field "y")
								(receiver
									(e-lookup-local
										(p-assign (ident "robot")))))
							(e-num (value "1")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Robot") (local))
				(ty-lookup (name "Robot") (local)))))
	(d-let
		(p-assign (ident "advance_x"))
		(e-lambda
			(args
				(p-assign (ident "robot")))
			(e-record
				(ext
					(e-lookup-local
						(p-assign (ident "robot"))))
				(fields
					(field (name "x")
						(e-binop (op "add")
							(e-dot-access (field "x")
								(receiver
									(e-lookup-local
										(p-assign (ident "robot")))))
							(e-num (value "1")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Robot") (local))
				(ty-lookup (name "Robot") (local)))))
	(d-let
		(p-assign (ident "retreat_x"))
		(e-lambda
			(args
				(p-assign (ident "robot")))
			(e-record
				(ext
					(e-lookup-local
						(p-assign (ident "robot"))))
				(fields
					(field (name "x")
						(e-binop (op "sub")
							(e-dot-access (field "x")
								(receiver
									(e-lookup-local
										(p-assign (ident "robot")))))
							(e-num (value "1")))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Robot") (local))
				(ty-lookup (name "Robot") (local)))))
	(s-alias-decl
		(ty-header (name "Robot"))
		(ty-record
			(field (field "x")
				(ty-lookup (name "I64") (builtin)))
			(field (field "y")
				(ty-lookup (name "I64") (builtin)))))
	(s-expect
		(e-binop (op "eq")
			(e-call
				(e-lookup-local
					(p-assign (ident "advance_y")))
				(e-record
					(fields
						(field (name "x")
							(e-num (value "0")))
						(field (name "y")
							(e-num (value "0"))))))
			(e-record
				(fields
					(field (name "x")
						(e-num (value "0")))
					(field (name "y")
						(e-num (value "1")))))))
	(s-expect
		(e-binop (op "eq")
			(e-call
				(e-lookup-local
					(p-assign (ident "retreat_y")))
				(e-record
					(fields
						(field (name "x")
							(e-num (value "0")))
						(field (name "y")
							(e-num (value "0"))))))
			(e-record
				(fields
					(field (name "x")
						(e-num (value "0")))
					(field (name "y")
						(e-num (value "-1")))))))
	(s-expect
		(e-binop (op "eq")
			(e-call
				(e-lookup-local
					(p-assign (ident "advance_x")))
				(e-record
					(fields
						(field (name "x")
							(e-num (value "0")))
						(field (name "y")
							(e-num (value "0"))))))
			(e-record
				(fields
					(field (name "x")
						(e-num (value "1")))
					(field (name "y")
						(e-num (value "0")))))))
	(s-expect
		(e-binop (op "eq")
			(e-call
				(e-lookup-local
					(p-assign (ident "retreat_x")))
				(e-record
					(fields
						(field (name "x")
							(e-num (value "0")))
						(field (name "y")
							(e-num (value "0"))))))
			(e-record
				(fields
					(field (name "x")
						(e-num (value "-1")))
					(field (name "y")
						(e-num (value "0")))))))
	(s-expect
		(e-binop (op "eq")
			(e-call
				(e-lookup-local
					(p-assign (ident "advance_y")))
				(e-call
					(e-lookup-local
						(p-assign (ident "retreat_y")))
					(e-record
						(fields
							(field (name "x")
								(e-num (value "5")))
							(field (name "y")
								(e-num (value "5")))))))
			(e-record
				(fields
					(field (name "x")
						(e-num (value "5")))
					(field (name "y")
						(e-num (value "5"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Robot -> Robot"))
		(patt (type "Robot -> Robot"))
		(patt (type "Robot -> Robot"))
		(patt (type "Robot -> Robot")))
	(type_decls
		(alias (type "Robot")
			(ty-header (name "Robot"))))
	(expressions
		(expr (type "Robot -> Robot"))
		(expr (type "Robot -> Robot"))
		(expr (type "Robot -> Robot"))
		(expr (type "Robot -> Robot"))))
~~~
