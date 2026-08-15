# META
~~~ini
description=Destructuring a record in a lambda parameter infers the same field types as accessing the fields through a named parameter
type=snippet
~~~
# SOURCE
~~~roc
apply : a, (a -> b) -> b
apply = |x, f| f(x)

# repro for https://github.com/roc-lang/roc/issues/10761
# `hi` and `lo` bound by the record pattern must get the same U32 types that
# `pair.hi` and `pair.lo` get below, so both definitions type-check cleanly.
destructured : U64
destructured = apply(
	{ hi: 1.U32, lo: 2.U32 },
	|{ hi, lo }| {
		hi_shifted = hi.to_u64().shl_wrap(32)
		hi_shifted.bitwise_or(lo.to_u64())
	},
)

field_access : U64
field_access = apply(
	{ hi: 1.U32, lo: 2.U32 },
	|pair| {
		hi_shifted = pair.hi.to_u64().shl_wrap(32)
		hi_shifted.bitwise_or(pair.lo.to_u64())
	},
)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,Comma,OpenRound,LowerIdent,OpArrow,LowerIdent,CloseRound,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,LowerIdent,Comma,LowerIdent,OpBar,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,
OpenCurly,LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,CloseCurly,Comma,
OpBar,OpenCurly,LowerIdent,Comma,LowerIdent,CloseCurly,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,
CloseCurly,Comma,
CloseRound,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,LowerIdent,NoSpaceOpenRound,
OpenCurly,LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,Comma,LowerIdent,OpColon,Int,NoSpaceDotUpperIdent,CloseCurly,Comma,
OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,OpAssign,LowerIdent,NoSpaceDotLowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,LowerIdent,NoSpaceDotLowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,CloseRound,
CloseCurly,Comma,
CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "apply")
			(ty-fn
				(ty-var (raw "a"))
				(ty-fn
					(ty-var (raw "a"))
					(ty-var (raw "b")))
				(ty-var (raw "b"))))
		(s-decl
			(p-ident (raw "apply"))
			(e-lambda
				(args
					(p-ident (raw "x"))
					(p-ident (raw "f")))
				(e-apply
					(e-ident (raw "f"))
					(e-ident (raw "x")))))
		(s-type-anno (name "destructured")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "destructured"))
			(e-apply
				(e-ident (raw "apply"))
				(e-record
					(field (field "hi")
						(e-typed-int (raw "1") (type "U32")))
					(field (field "lo")
						(e-typed-int (raw "2") (type "U32"))))
				(e-lambda
					(args
						(p-record
							(field (name "hi") (rest false))
							(field (name "lo") (rest false))))
					(e-block
						(statements
							(s-decl
								(p-ident (raw "hi_shifted"))
								(e-method-call (method ".shl_wrap")
									(receiver
										(e-method-call (method ".to_u64")
											(receiver
												(e-ident (raw "hi")))
											(args)))
									(args
										(e-int (raw "32")))))
							(e-method-call (method ".bitwise_or")
								(receiver
									(e-ident (raw "hi_shifted")))
								(args
									(e-method-call (method ".to_u64")
										(receiver
											(e-ident (raw "lo")))
										(args)))))))))
		(s-type-anno (name "field_access")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "field_access"))
			(e-apply
				(e-ident (raw "apply"))
				(e-record
					(field (field "hi")
						(e-typed-int (raw "1") (type "U32")))
					(field (field "lo")
						(e-typed-int (raw "2") (type "U32"))))
				(e-lambda
					(args
						(p-ident (raw "pair")))
					(e-block
						(statements
							(s-decl
								(p-ident (raw "hi_shifted"))
								(e-method-call (method ".shl_wrap")
									(receiver
										(e-method-call (method ".to_u64")
											(receiver
												(e-field-access
													(receiver
														(e-ident (raw "pair")))
													(segment (mode "required") (field "hi"))))
											(args)))
									(args
										(e-int (raw "32")))))
							(e-method-call (method ".bitwise_or")
								(receiver
									(e-ident (raw "hi_shifted")))
								(args
									(e-method-call (method ".to_u64")
										(receiver
											(e-field-access
												(receiver
													(e-ident (raw "pair")))
												(segment (mode "required") (field "lo"))))
										(args)))))))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "apply"))
		(e-lambda
			(args
				(p-assign (ident "x"))
				(p-assign (ident "f")))
			(e-call (constraint-fn-var 283)
				(e-lookup-local
					(p-assign (ident "f")))
				(e-lookup-local
					(p-assign (ident "x")))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-parens
					(ty-fn (effectful false)
						(ty-rigid-var-lookup (ty-rigid-var (name "a")))
						(ty-rigid-var (name "b"))))
				(ty-rigid-var-lookup (ty-rigid-var (name "b"))))))
	(d-let
		(p-assign (ident "destructured"))
		(e-call (constraint-fn-var 327)
			(e-lookup-local
				(p-assign (ident "apply")))
			(e-record
				(fields
					(field (name "hi")
						(e-typed-int (value "1") (type "U32")))
					(field (name "lo")
						(e-typed-int (value "2") (type "U32")))))
			(e-lambda
				(args
					(p-record-destructure
						(destructs
							(record-destruct (label "hi") (ident "hi")
								(required
									(p-assign (ident "hi"))))
							(record-destruct (label "lo") (ident "lo")
								(required
									(p-assign (ident "lo")))))))
				(e-block
					(s-let
						(p-assign (ident "hi_shifted"))
						(e-dispatch-call (method "shl_wrap") (constraint-fn-var 321)
							(receiver
								(e-dispatch-call (method "to_u64") (constraint-fn-var 312)
									(receiver
										(e-lookup-local
											(p-assign (ident "hi"))))
									(args)))
							(args
								(e-num (value "32")))))
					(e-dispatch-call (method "bitwise_or") (constraint-fn-var 325)
						(receiver
							(e-lookup-local
								(p-assign (ident "hi_shifted"))))
						(args
							(e-dispatch-call (method "to_u64") (constraint-fn-var 323)
								(receiver
									(e-lookup-local
										(p-assign (ident "lo"))))
								(args)))))))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-assign (ident "field_access"))
		(e-call (constraint-fn-var 375)
			(e-lookup-local
				(p-assign (ident "apply")))
			(e-record
				(fields
					(field (name "hi")
						(e-typed-int (value "1") (type "U32")))
					(field (name "lo")
						(e-typed-int (value "2") (type "U32")))))
			(e-lambda
				(args
					(p-assign (ident "pair")))
				(e-block
					(s-let
						(p-assign (ident "hi_shifted"))
						(e-dispatch-call (method "shl_wrap") (constraint-fn-var 362)
							(receiver
								(e-dispatch-call (method "to_u64") (constraint-fn-var 353)
									(receiver
										(e-field-access
											(receiver
												(e-lookup-local
													(p-assign (ident "pair"))))
											(segments
												(segment (name "hi") (mode "required")))))
									(args)))
							(args
								(e-num (value "32")))))
					(e-dispatch-call (method "bitwise_or") (constraint-fn-var 371)
						(receiver
							(e-lookup-local
								(p-assign (ident "hi_shifted"))))
						(args
							(e-dispatch-call (method "to_u64") (constraint-fn-var 369)
								(receiver
									(e-field-access
										(receiver
											(e-lookup-local
												(p-assign (ident "pair"))))
										(segments
											(segment (name "lo") (mode "required")))))
								(args)))))))
		(annotation
			(ty-lookup (name "U64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a, (a -> b) -> b"))
		(patt (type "U64"))
		(patt (type "U64")))
	(expressions
		(expr (type "a, (a -> b) -> b"))
		(expr (type "U64"))
		(expr (type "U64"))))
~~~
