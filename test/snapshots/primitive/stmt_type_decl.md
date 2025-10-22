# META
~~~ini
description=A primitive
type=snippet
~~~
# SOURCE
~~~roc
Foo(a,b) : (a,b,Str,U64)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,OpenRound,LowerIdent,Comma,LowerIdent,Comma,UpperIdent,Comma,UpperIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "b"))))
			(ty-tuple
				(ty-var (raw "a"))
				(ty-var (raw "b"))
				(ty (name "Str"))
				(ty (name "U64"))))))
~~~
# FORMATTED
~~~roc
Foo(a, b) : (a, b, Str, U64)
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Foo")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "b"))))
		(ty-tuple
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))
			(ty-rigid-var-lookup (ty-rigid-var (name "b")))
			(ty-lookup (name "Str") (external-module "Str"))
			(ty-lookup (name "U64") (builtin)))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Foo(a, b)")
			(ty-header (name "Foo")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "b"))))))
	(expressions))
~~~
