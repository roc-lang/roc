# META
~~~ini
description=Opaque types should allow underscore-prefixed type parameters like _a
type=snippet
~~~
# SOURCE
~~~roc
# Opaque type with underscore-prefixed parameter - should be allowed
OpaqueType(_a) :: Str

# Opaque type with multiple parameters including underscore-prefixed
OpaqueType2(_a, b) :: b

# Opaque type where underscore comes second
OpaqueType3(a, _b) :: a
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,NamedUnderscore,CloseRound,OpDoubleColon,UpperIdent,
UpperIdent,NoSpaceOpenRound,NamedUnderscore,Comma,LowerIdent,CloseRound,OpDoubleColon,LowerIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,NamedUnderscore,CloseRound,OpDoubleColon,LowerIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "OpaqueType")
				(args
					(underscore-ty-var (raw "_a"))))
			(ty (name "Str")))
		(s-type-decl
			(header (name "OpaqueType2")
				(args
					(underscore-ty-var (raw "_a"))
					(ty-var (raw "b"))))
			(ty-var (raw "b")))
		(s-type-decl
			(header (name "OpaqueType3")
				(args
					(ty-var (raw "a"))
					(underscore-ty-var (raw "_b"))))
			(ty-var (raw "a")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-nominal-decl
		(ty-header (name "OpaqueType")
			(ty-args
				(ty-rigid-var (name "_a"))))
		(ty-lookup (name "Str") (builtin)))
	(s-nominal-decl
		(ty-header (name "OpaqueType2")
			(ty-args
				(ty-rigid-var (name "_a"))
				(ty-rigid-var (name "b"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "b"))))
	(s-nominal-decl
		(ty-header (name "OpaqueType3")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "_b"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "a")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(nominal (type "OpaqueType(_a)")
			(ty-header (name "OpaqueType")
				(ty-args
					(ty-rigid-var (name "_a")))))
		(nominal (type "OpaqueType2(_a, b)")
			(ty-header (name "OpaqueType2")
				(ty-args
					(ty-rigid-var (name "_a"))
					(ty-rigid-var (name "b")))))
		(nominal (type "OpaqueType3(a, _b)")
			(ty-header (name "OpaqueType3")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "_b"))))))
	(expressions))
~~~
