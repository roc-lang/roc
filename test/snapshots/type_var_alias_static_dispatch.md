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
callDefault : thing where [thing.default : {} -> thing] -> thing
callDefault = |_placeholder| {
    Thing : thing
    Thing.default({})
}

# Example with explicit type parameter usage
useTypeVar : t where [t.default : {} -> t] -> t
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
LowerIdent,OpColon,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,LowerIdent,CloseSquare,OpArrow,LowerIdent,
LowerIdent,OpAssign,OpBar,NamedUnderscore,OpBar,OpenCurly,
UpperIdent,OpColon,LowerIdent,
UpperIdent,NoSpaceDotLowerIdent,NoSpaceOpenRound,OpenCurly,CloseCurly,CloseRound,
CloseCurly,
LowerIdent,OpColon,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,OpenCurly,CloseCurly,OpArrow,LowerIdent,CloseSquare,OpArrow,LowerIdent,
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
				(ty-var (raw "thing")
					(where
						(method (name "default")
							(type
								(ty-fn
									(ty-record)
									(ty-var (raw "thing")))))))
				(ty-var (raw "thing"))))
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
							(e-field-access
								(e-ident (raw "Thing"))
								(e-ident (raw "default")))
							(e-record))))))
		(s-type-anno (name "useTypeVar")
			(ty-fn
				(ty-var (raw "t")
					(where
						(method (name "default")
							(type
								(ty-fn
									(ty-record)
									(ty-var (raw "t")))))))
				(ty-var (raw "t"))))
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
								(e-field-access
									(e-ident (raw "T"))
									(e-ident (raw "default")))
								(e-record)))
						(e-ident (raw "result"))))))))
~~~
# FORMATTED
~~~roc
# Type var alias allows static dispatch on type variables
# When a function has a type parameter, you can bind it to an uppercase
# alias and call methods on that type.

# Simple example: calling a method on a type variable
callDefault : thing where [thing.default : {} -> thing] -> thing
callDefault = |_placeholder| {
	Thing : thing
	Thing.default({})
}

# Example with explicit type parameter usage
useTypeVar : t where [t.default : {} -> t] -> t
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
				(s-type-var-alias
					(alias-name "Thing")
					(type-var-name "thing"))
				(e-type-var-dispatch
					(method "default")
					(args
						(e-record)))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "thing")
					(where
						(method (name "default")
							(ty-fn (effectful false)
								(ty-record)
								(ty-rigid-var-lookup (ty-rigid-var (name "thing")))))))
				(ty-rigid-var-lookup (ty-rigid-var (name "thing"))))))
	(d-let
		(p-assign (ident "useTypeVar"))
		(e-lambda
			(args
				(p-underscore))
			(e-block
				(s-type-var-alias
					(alias-name "T")
					(type-var-name "t"))
				(s-let
					(p-assign (ident "result"))
					(e-type-var-dispatch
						(method "default")
						(args
							(e-record))))
				(e-lookup-local
					(p-assign (ident "result")))))
		(annotation
			(ty-fn (effectful false)
				(ty-rigid-var (name "t")
					(where
						(method (name "default")
							(ty-fn (effectful false)
								(ty-record)
								(ty-rigid-var-lookup (ty-rigid-var (name "t")))))))
				(ty-rigid-var-lookup (ty-rigid-var (name "t")))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "thing where [thing.default : {} -> thing] -> thing"))
		(patt (type "t where [t.default : {} -> t] -> t")))
	(expressions
		(expr (type "thing where [thing.default : {} -> thing] -> thing"))
		(expr (type "t where [t.default : {} -> t] -> t"))))
~~~
