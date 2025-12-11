# META
~~~ini
description=Type variable alias for static dispatch on type parameters
type=snippet
~~~
# SOURCE
~~~roc
# Type var alias allows static dispatch on type variables
# When a function has a type parameter, you can bind it to an uppercase
# alias and call methods on that type.

# Simple example: calling a method on a type variable
callDefault : {} -> thing where [thing.default : {} -> thing]
callDefault = |_placeholder| {
    Thing : thing
    Thing.default({})
}

# Example with explicit type parameter usage
useTypeVar : {} -> t where [t.default : {} -> t]
useTypeVar = |_| {
    T : t
    result = T.default({})
    result
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,LowerIdent,CloseSquare,
LowerIdent,OpAssign,OpBar,Underscore,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
LowerIdent,
CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-anno (name "callDefault")
			(ty-fn
				(ty-record)
				(ty-var (raw "thing")))
			(where
				(method (module-of "thing") (name "default")
					(args
						(ty-record))
					(ty-var (raw "thing")))))
		(s-decl
			(p-ident (raw "callDefault"))
			(e-lambda
				(args
					(p-ident (raw "_placeholder")))
				(e-block
					(statements
						(s-type-decl
							(header (name "Thing")
								(args))
							(ty-var (raw "thing")))
						(e-apply
							(e-ident (raw "Thing.default"))
							(e-record))))))
		(s-type-anno (name "useTypeVar")
			(ty-fn
				(ty-record)
				(ty-var (raw "t")))
			(where
				(method (module-of "t") (name "default")
					(args
						(ty-record))
					(ty-var (raw "t")))))
		(s-decl
			(p-ident (raw "useTypeVar"))
			(e-lambda
				(args
					(p-underscore))
				(e-block
					(statements
						(s-type-decl
							(header (name "T")
								(args))
							(ty-var (raw "t")))
						(s-decl
							(p-ident (raw "result"))
							(e-apply
								(e-ident (raw "T.default"))
								(e-record)))
						(e-ident (raw "result"))))))))
~~~
# FORMATTED
~~~roc
# Type var alias allows static dispatch on type variables
# When a function has a type parameter, you can bind it to an uppercase
# alias and call methods on that type.

# Simple example: calling a method on a type variable
callDefault : {} -> thing where [thing.default : {} -> thing]
callDefault = |_placeholder| {
	Thing : thing
	Thing.default({})
}

# Example with explicit type parameter usage
useTypeVar : {} -> t where [t.default : {} -> t]
useTypeVar = |_| {
	T : t
	result = T.default({})
	result
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "callDefault"))
		(e-lambda
			(args
				(p-assign (ident "_placeholder")))
			(e-block
				(s-type-var-alias (alias "Thing") (type-var "thing")
					(ty-rigid-var (name "thing")))
				(e-type-var-dispatch (method "default")
					(e-empty_record))))
		(annotation
			(ty-fn (effectful false)
				(ty-record)
				(ty-rigid-var (name "thing")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "thing"))) (name "default")
					(args
						(ty-record))
					(ty-rigid-var-lookup (ty-rigid-var (name "thing")))))))
	(d-let
		(p-assign (ident "useTypeVar"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-type-var-alias (alias "T") (type-var "t")
					(ty-rigid-var (name "t")))
				(s-let
					(p-assign (ident "result"))
					(e-type-var-dispatch (method "default")
						(e-empty_record)))
				(e-lookup-local
					(p-assign (ident "result")))))
		(annotation
			(ty-fn (effectful false)
				(ty-record)
				(ty-rigid-var (name "t")))
			(where
				(method (ty-rigid-var-lookup (ty-rigid-var (name "t"))) (name "default")
					(args
						(ty-record))
					(ty-rigid-var-lookup (ty-rigid-var (name "t"))))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{  } -> thing where [thing.default : {  } -> thing]"))
		(patt (type "{  } -> t where [t.default : {  } -> t]")))
	(expressions
		(expr (type "{  } -> thing where [thing.default : {  } -> thing]"))
		(expr (type "{  } -> t where [t.default : {  } -> t]"))))
~~~
