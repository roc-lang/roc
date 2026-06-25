# META
~~~ini
description=The ways to satisfy a closed record destructure: list every field, ignore one with `field: _`, or open the pattern with a `..` rest (named or bare)
type=snippet
~~~
# SOURCE
~~~roc
all_fields : { x : U64, y : U64, z : U64 } -> U64
all_fields = |{ x, y, z }| x + y + z

ignore_with_underscore : { x : U64, y : U64, z : U64 } -> U64
ignore_with_underscore = |{ x, y, z: _ }| x + y

bare_rest : { x : U64, y : U64, z : U64 } -> U64
bare_rest = |{ x, y, .. }| x + y

named_rest : { x : U64, y : U64, z : U64 } -> U64
named_rest = |{ x, y, ..rest }| x + y + rest.z
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,CloseCurly,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,Comma,LowerIdent,OpColon,Underscore,CloseCurly,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,Comma,DoubleDot,CloseCurly,OpBar,LowerIdent,OpPlus,LowerIdent,
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,Comma,DoubleDot,LowerIdent,CloseCurly,OpBar,LowerIdent,OpPlus,LowerIdent,OpPlus,LowerIdent,NoSpaceDotLowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "all_fields")
			(ty-fn
				(ty-record
					(anno-record-field (name "x")
						(ty (name "U64")))
					(anno-record-field (name "y")
						(ty (name "U64")))
					(anno-record-field (name "z")
						(ty (name "U64"))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "all_fields"))
			(e-lambda
				(args
					(p-record
						(field (name "x") (rest false))
						(field (name "y") (rest false))
						(field (name "z") (rest false))))
				(e-binop (op "+")
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-ident (raw "y")))
					(e-ident (raw "z")))))
		(s-type-anno (name "ignore_with_underscore")
			(ty-fn
				(ty-record
					(anno-record-field (name "x")
						(ty (name "U64")))
					(anno-record-field (name "y")
						(ty (name "U64")))
					(anno-record-field (name "z")
						(ty (name "U64"))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "ignore_with_underscore"))
			(e-lambda
				(args
					(p-record
						(field (name "x") (rest false))
						(field (name "y") (rest false))
						(field (name "z") (rest false)
							(p-underscore))))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "y")))))
		(s-type-anno (name "bare_rest")
			(ty-fn
				(ty-record
					(anno-record-field (name "x")
						(ty (name "U64")))
					(anno-record-field (name "y")
						(ty (name "U64")))
					(anno-record-field (name "z")
						(ty (name "U64"))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "bare_rest"))
			(e-lambda
				(args
					(p-record
						(field (name "x") (rest false))
						(field (name "y") (rest false))
						(field (rest true))))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "y")))))
		(s-type-anno (name "named_rest")
			(ty-fn
				(ty-record
					(anno-record-field (name "x")
						(ty (name "U64")))
					(anno-record-field (name "y")
						(ty (name "U64")))
					(anno-record-field (name "z")
						(ty (name "U64"))))
				(ty (name "U64"))))
		(s-decl
			(p-ident (raw "named_rest"))
			(e-lambda
				(args
					(p-record
						(field (name "x") (rest false))
						(field (name "y") (rest false))
						(field (name "rest") (rest true))))
				(e-binop (op "+")
					(e-binop (op "+")
						(e-ident (raw "x"))
						(e-ident (raw "y")))
					(e-field-access
						(e-ident (raw "rest"))
						(e-ident (raw "z"))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "all_fields"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs
						(record-destruct (label "x") (ident "x")
							(required
								(p-assign (ident "x"))))
						(record-destruct (label "y") (ident "y")
							(required
								(p-assign (ident "y"))))
						(record-destruct (label "z") (ident "z")
							(required
								(p-assign (ident "z")))))))
			(e-dispatch-call (method "plus") (constraint-fn-var 139)
				(receiver
					(e-dispatch-call (method "plus") (constraint-fn-var 129)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-lookup-local
								(p-assign (ident "y"))))))
				(args
					(e-lookup-local
						(p-assign (ident "z"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "x")
						(ty-lookup (name "U64") (builtin)))
					(field (field "y")
						(ty-lookup (name "U64") (builtin)))
					(field (field "z")
						(ty-lookup (name "U64") (builtin))))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "ignore_with_underscore"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs
						(record-destruct (label "x") (ident "x")
							(required
								(p-assign (ident "x"))))
						(record-destruct (label "y") (ident "y")
							(required
								(p-assign (ident "y"))))
						(record-destruct (label "z") (ident "z")
							(sub-pattern
								(p-underscore))))))
			(e-dispatch-call (method "plus") (constraint-fn-var 175)
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args
					(e-lookup-local
						(p-assign (ident "y"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "x")
						(ty-lookup (name "U64") (builtin)))
					(field (field "y")
						(ty-lookup (name "U64") (builtin)))
					(field (field "z")
						(ty-lookup (name "U64") (builtin))))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "bare_rest"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs
						(record-destruct (label "x") (ident "x")
							(required
								(p-assign (ident "x"))))
						(record-destruct (label "y") (ident "y")
							(required
								(p-assign (ident "y"))))
						(record-destruct (label "#others") (ident "#others")
							(rest-pattern
								(p-underscore))))))
			(e-dispatch-call (method "plus") (constraint-fn-var 211)
				(receiver
					(e-lookup-local
						(p-assign (ident "x"))))
				(args
					(e-lookup-local
						(p-assign (ident "y"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "x")
						(ty-lookup (name "U64") (builtin)))
					(field (field "y")
						(ty-lookup (name "U64") (builtin)))
					(field (field "z")
						(ty-lookup (name "U64") (builtin))))
				(ty-lookup (name "U64") (builtin)))))
	(d-let
		(p-assign (ident "named_rest"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs
						(record-destruct (label "x") (ident "x")
							(required
								(p-assign (ident "x"))))
						(record-destruct (label "y") (ident "y")
							(required
								(p-assign (ident "y"))))
						(record-destruct (label "rest") (ident "rest")
							(rest-pattern
								(p-assign (ident "rest")))))))
			(e-dispatch-call (method "plus") (constraint-fn-var 260)
				(receiver
					(e-dispatch-call (method "plus") (constraint-fn-var 247)
						(receiver
							(e-lookup-local
								(p-assign (ident "x"))))
						(args
							(e-lookup-local
								(p-assign (ident "y"))))))
				(args
					(e-field-access (field "z")
						(receiver
							(e-lookup-local
								(p-assign (ident "rest"))))))))
		(annotation
			(ty-fn (effectful false)
				(ty-record
					(field (field "x")
						(ty-lookup (name "U64") (builtin)))
					(field (field "y")
						(ty-lookup (name "U64") (builtin)))
					(field (field "z")
						(ty-lookup (name "U64") (builtin))))
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ x: U64, y: U64, z: U64 } -> U64"))
		(patt (type "{ x: U64, y: U64, z: U64 } -> U64"))
		(patt (type "{ x: U64, y: U64, z: U64 } -> U64"))
		(patt (type "{ x: U64, y: U64, z: U64 } -> U64")))
	(expressions
		(expr (type "{ x: U64, y: U64, z: U64 } -> U64"))
		(expr (type "{ x: U64, y: U64, z: U64 } -> U64"))
		(expr (type "{ x: U64, y: U64, z: U64 } -> U64"))
		(expr (type "{ x: U64, y: U64, z: U64 } -> U64"))))
~~~
