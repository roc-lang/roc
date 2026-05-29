# META
~~~ini
description=Record update with I64 field type and arithmetic
type=snippet
~~~
# SOURCE
~~~roc
Robot : { x : I64, y : I64 }

advance : Robot -> Robot
advance = |robot| { ..robot, y: robot.y + 1 }

retreat : Robot -> Robot
retreat = |robot| { ..robot, y: robot.y - 1 }

expect advance({ x: 7, y: 3 }) == { x: 7, y: 4 }
expect retreat({ x: 7, y: 3 }) == { x: 7, y: 2 }
expect advance(retreat({ x: 0, y: 0 })) == { x: 0, y: 0 }
~~~
# EXPECTED
INFINITE TYPE - record_i64_field_update.md:4:1:4:46
INFINITE TYPE - record_i64_field_update.md:7:1:7:46
# PROBLEMS
**INFINITE TYPE**
I am inferring a weird self-referential type:
**record_i64_field_update.md:4:1:4:46:**
```roc
advance = |robot| { ..robot, y: robot.y + 1 }
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here is my best effort at writing down the type. You will see `<RecursiveType>` for parts of the type that repeat infinitely.

    Robot


**INFINITE TYPE**
I am inferring a weird self-referential type:
**record_i64_field_update.md:7:1:7:46:**
```roc
retreat = |robot| { ..robot, y: robot.y - 1 }
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Here is my best effort at writing down the type. You will see `<RecursiveType>` for parts of the type that repeat infinitely.

    Robot


# TOKENS
~~~zig
UpperIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpPlus,Int,CloseCurly,
LowerIdent,OpColon,UpperIdent,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,DoubleDot,LowerIdent,Comma,LowerIdent,OpColon,LowerIdent,NoSpaceDotLowerIdent,OpBinaryMinus,Int,CloseCurly,
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
		(s-type-anno (name "advance")
			(ty-fn
				(ty (name "Robot"))
				(ty (name "Robot"))))
		(s-decl
			(p-ident (raw "advance"))
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
		(s-type-anno (name "retreat")
			(ty-fn
				(ty (name "Robot"))
				(ty (name "Robot"))))
		(s-decl
			(p-ident (raw "retreat"))
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
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "advance"))
					(e-record
						(field (field "x")
							(e-int (raw "7")))
						(field (field "y")
							(e-int (raw "3")))))
				(e-record
					(field (field "x")
						(e-int (raw "7")))
					(field (field "y")
						(e-int (raw "4"))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "retreat"))
					(e-record
						(field (field "x")
							(e-int (raw "7")))
						(field (field "y")
							(e-int (raw "3")))))
				(e-record
					(field (field "x")
						(e-int (raw "7")))
					(field (field "y")
						(e-int (raw "2"))))))
		(s-expect
			(e-binop (op "==")
				(e-apply
					(e-ident (raw "advance"))
					(e-apply
						(e-ident (raw "retreat"))
						(e-record
							(field (field "x")
								(e-int (raw "0")))
							(field (field "y")
								(e-int (raw "0"))))))
				(e-record
					(field (field "x")
						(e-int (raw "0")))
					(field (field "y")
						(e-int (raw "0"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "advance"))
		(e-lambda
			(args
				(p-assign (ident "robot")))
			(e-record
				(ext
					(e-lookup-local
						(p-assign (ident "robot"))))
				(fields
					(field (name "y")
						(e-dispatch-call (method "plus") (constraint-fn-var 170)
							(receiver
								(e-field-access (field "y")
									(receiver
										(e-lookup-local
											(p-assign (ident "robot"))))))
							(args
								(e-num (value "1"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-lookup (name "Robot") (local))
				(ty-lookup (name "Robot") (local)))))
	(d-let
		(p-assign (ident "retreat"))
		(e-lambda
			(args
				(p-assign (ident "robot")))
			(e-record
				(ext
					(e-lookup-local
						(p-assign (ident "robot"))))
				(fields
					(field (name "y")
						(e-dispatch-call (method "minus") (constraint-fn-var 327)
							(receiver
								(e-field-access (field "y")
									(receiver
										(e-lookup-local
											(p-assign (ident "robot"))))))
							(args
								(e-num (value "1"))))))))
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
		(e-structural-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 473)
					(e-runtime-error (tag "erroneous_value_use"))
					(e-record
						(fields
							(field (name "x")
								(e-num (value "7")))
							(field (name "y")
								(e-num (value "3")))))))
			(rhs
				(e-record
					(fields
						(field (name "x")
							(e-num (value "7")))
						(field (name "y")
							(e-num (value "4"))))))))
	(s-expect
		(e-structural-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 890)
					(e-runtime-error (tag "erroneous_value_use"))
					(e-record
						(fields
							(field (name "x")
								(e-num (value "7")))
							(field (name "y")
								(e-num (value "3")))))))
			(rhs
				(e-record
					(fields
						(field (name "x")
							(e-num (value "7")))
						(field (name "y")
							(e-num (value "2"))))))))
	(s-expect
		(e-method-eq (negated "false")
			(lhs
				(e-call (constraint-fn-var 1308)
					(e-runtime-error (tag "erroneous_value_use"))
					(e-call (constraint-fn-var 1307)
						(e-runtime-error (tag "erroneous_value_use"))
						(e-record
							(fields
								(field (name "x")
									(e-num (value "0")))
								(field (name "y")
									(e-num (value "0"))))))))
			(rhs
				(e-record
					(fields
						(field (name "x")
							(e-num (value "0")))
						(field (name "y")
							(e-num (value "0")))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Error"))
		(patt (type "Error")))
	(type_decls
		(alias (type "Robot")
			(ty-header (name "Robot"))))
	(expressions
		(expr (type "Error"))
		(expr (type "Error"))))
~~~
