# META
~~~ini
description=to_u64 should accept U64
type=snippet
~~~
# SOURCE
~~~roc
function : a -> U64 where [ a.to_u64 : a -> U64 ]
function = |convertible| {
    convertible.to_u64()
}

value : U64
value = 123

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
	(type-module)
	(statements
		(s-type-anno (name "function")
			(ty-fn
				(ty-var (raw "a"))
				(ty (name "U64")))
			(where
				(method (module-of "a") (name "to_u64")
					(args
						(ty-var (raw "a")))
					(ty (name "U64")))))
		(s-decl
			(p-ident (raw "function"))
			(e-lambda
				(args
					(p-ident (raw "convertible")))
				(e-block
					(statements
						(e-method-call (method ".to_u64")
							(receiver
								(e-ident (raw "convertible")))
							(args))))))
		(s-type-anno (name "value")
			(ty (name "U64")))
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
function : a -> U64 where [a.to_u64 : a -> U64]
function = |convertible| {
	convertible.to_u64()
}

value : U64
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
				(e-dispatch-call (method "to_u64") (constraint-fn-var 50)
					(receiver
						(e-lookup-local
							(p-assign (ident "convertible"))))
					(args))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "a"))
				(ty-lookup (name "U64") (builtin)))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "a"))) (name "to_u64")
					(args
						(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
					(ty-lookup (name "U64") (builtin))))))
	(d-let
		(p-assign (ident "value"))
		(e-num (value "123"))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(d-let
		(p-underscore)
		(e-call (constraint-fn-var 164)
			(e-lookup-local
				(p-assign (ident "function")))
			(e-lookup-local
				(p-assign (ident "value"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "a -> U64 where [a.to_u64 : a -> U64]"))
		(patt (type "U64")))
	(expressions
		(expr (type "a -> U64 where [a.to_u64 : a -> U64]"))
		(expr (type "U64"))
		(expr (type "U64"))))
~~~
