# META
~~~ini
description=to_i32_wrap should accept I32
type=snippet
~~~
# SOURCE
~~~roc
function : a -> I32 where [ a.to_i32_wrap : a -> I32 ]
function = |convertible| {
    convertible.to_i32_wrap()
}

value : I32
value = -123

_ = function(value)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,UpperIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,LowerIdent,OpBar,OpenCurly,
LowerIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,CloseRound,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,Int,
Underscore,OpAssign,LowerIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-anno (name "function")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "I32")))
			(where
				(method (mod-of "a") (name "to_i32_wrap")
					(args
						(ty-var (raw "a")))
					(ty (name "I32")))))
		(s-decl
			(p-ident (raw "function"))
			(e-lambda
				(args
					(p-ident (raw "convertible")))
				(e-block
					(statements
						(e-method-call (method ".to_i32_wrap")
							(receiver
								(e-ident (raw "convertible")))
							(args))))))
		(s-type-anno (name "value")
			(ty (name "I32")))
		(s-decl
			(p-ident (raw "value"))
			(e-int (raw "-123")))
		(s-decl
			(p-underscore)
			(e-apply
				(e-ident (raw "function"))
				(e-ident (raw "value"))))))
~~~
# FORMATTED
~~~roc
function : a -> I32 where [a.to_i32_wrap : a -> I32]
function = |convertible| {
	convertible.to_i32_wrap()
}

value : I32
value = -123

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
				(e-dispatch-call (method "to_i32_wrap") (constraint-fn-var 213)
					(receiver
						(e-lookup-local
							(p-assign (ident "convertible"))))
					(args))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "I32") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_i32_wrap")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "I32") (builtin))))))
	(d-let
		(p-assign (ident "value"))
		(e-num (value "-123"))
		(annotation
			(ty-lookup (name "I32") (builtin))))
	(d-let
		(p-underscore)
		(e-call (constraint-fn-var 241)
			(e-lookup-local
				(p-assign (ident "function")))
			(e-lookup-local
				(p-assign (ident "value"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> I32 where [a.to_i32_wrap : a -> I32]"))
		(patt (type "I32")))
	(expressions
		(expr (type "a -> I32 where [a.to_i32_wrap : a -> I32]"))
		(expr (type "I32"))
		(expr (type "I32"))))
~~~
