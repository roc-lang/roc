# META
~~~ini
description=to_u32_try should accept U32
type=snippet
~~~
# SOURCE
~~~roc
function : a -> U32 where [ a.to_u32_try : a -> Try(U32, _) ]
function = |convertible| {
    convertible.to_u32_try().ok_or(0)
}

value : U32
value = 123

_ = function(value)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,NoSpaceOpenRound,UpperIdent,Comma,Underscore,CloseRound,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,NoSpaceDotLowerIdent,NoSpaceOpenRound,Int,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "function")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "U32")))
			(where
				(method (module-of "a") (name "to_u32_try")
					(args
						(ty-var (raw "a")))
					(ty-apply
						(ty (name "Try"))
						(ty (name "U32"))
						(_)))))
		(s-decl
			(p-ident (raw "function"))
			(e-lambda
				(args
					(p-ident (raw "convertible")))
				(e-block
					(statements
						(e-method-call (method ".ok_or")
							(receiver
								(e-method-call (method ".to_u32_try")
									(receiver
										(e-ident (raw "convertible")))
									(args)))
							(args
								(e-int (raw "0"))))))))
		(s-type-anno (name "value")
			(ty (name "U32")))
		(s-decl
			(p-ident (raw "value"))
			(e-int (raw "123")))
		(s-decl
			(p-underscore)
			(e-apply
				(e-ident (raw "function"))
				(e-ident (raw "value"))))))
~~~
# FORMATTED
~~~roc
function : a -> U32 where [a.to_u32_try : a -> Try(U32, _)]
function = |convertible| {
	convertible.to_u32_try().ok_or(0)
}

value : U32
value = 123

_ = function(value)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "function"))
		(e-lambda
			(args
				(p-assign (ident "convertible")))
			(e-block
				(e-dispatch-call (method "ok_or") (constraint-fn-var 103)
					(receiver
						(e-dispatch-call (method "to_u32_try") (constraint-fn-var 68)
							(receiver
								(e-lookup-local
									(p-assign (ident "convertible"))))
							(args)))
					(args
						(e-num (value "0"))))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "U32") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_u32_try")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-apply (name "Try") (builtin)
						(ty-lookup (name "U32") (builtin))
						(ty-underscore))))))
	(d-let
		(p-assign (ident "value"))
		(e-num (value "123"))
		(annotation
			(ty-lookup (name "U32") (builtin))))
	(d-let
		(p-underscore)
		(e-call (constraint-fn-var 302)
			(e-lookup-local
				(p-assign (ident "function")))
			(e-lookup-local
				(p-assign (ident "value"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> U32 where [a.to_u32_try : a -> Try(U32, _err)]"))
		(patt (type "U32")))
	(expressions
		(expr (type "a -> U32 where [a.to_u32_try : a -> Try(U32, _err)]"))
		(expr (type "U32"))
		(expr (type "U32"))))
~~~
