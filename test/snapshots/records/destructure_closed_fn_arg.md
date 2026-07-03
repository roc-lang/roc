# META
~~~ini
description=Record destructure in a function parameter without `..` is closed: the pattern must cover every annotated field
type=snippet
~~~
# SOURCE
~~~roc
get_sum : { x : U64, y : U64, z : U64 } -> U64
get_sum = |{ x, y }| x + y
~~~
# EXPECTED
TYPE MISMATCH - destructure_closed_fn_arg.md:2:12:2:20
# PROBLEMS

┌───────────────┐
│ TYPE MISMATCH ├─ This expression is used in an unexpected way. ─────────────┐
└┬──────────────┘                                                             │
 │                                                                            │
 │  get_sum = |{ x, y }| x + y                                                │
 │             ‾‾‾‾‾‾‾‾                                                       │
 └───────────────────────────────────────── destructure_closed_fn_arg.md:2:12 ┘

    It has the type:

        { x: _field, y: U64 }

    But the annotation says it should be:

        { x: U64, y: U64, z: U64 }

    Hint: This record is missing the field: `z`

# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,Comma,LowerIdent,OpColon,UpperIdent,CloseCurly,OpArrow,UpperIdent,
LowerIdent,OpAssign,OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpBar,LowerIdent,OpPlus,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "get_sum")
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
			(p-ident (raw "get_sum"))
			(e-lambda
				(args
					(p-record
						(field (name "x") (rest false))
						(field (name "y") (rest false))))
				(e-binop (op "+")
					(e-ident (raw "x"))
					(e-ident (raw "y")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "get_sum"))
		(e-lambda
			(args
				(p-record-destructure
					(destructs
						(record-destruct (label "x") (ident "x")
							(required
								(p-assign (ident "x"))))
						(record-destruct (label "y") (ident "y")
							(required
								(p-assign (ident "y")))))))
			(e-dispatch-call (method "plus") (constraint-fn-var 54)
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
				(ty-lookup (name "U64") (builtin))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{ x: U64, y: U64, z: U64 } -> U64")))
	(expressions
		(expr (type "{ x: U64, y: U64, z: U64 } -> U64"))))
~~~
