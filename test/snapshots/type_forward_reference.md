# META
~~~ini
description=Type alias forward references (non-recursive)
type=snippet
~~~
# SOURCE
~~~roc
# Simple forward reference - A references B before B is defined
A : B
B : Str

# Multiple forward references in a chain
First : Second
Second : Third
Third : U64

# Forward reference with type parameters
Wrapper(a) : Inner(a)
Inner(a) : List(a)
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,OpColon,UpperIdent,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "A")
				(args))
			(ty (name "B")))
		(s-type-decl
			(header (name "B")
				(args))
			(ty (name "Str")))
		(s-type-decl
			(header (name "First")
				(args))
			(ty (name "Second")))
		(s-type-decl
			(header (name "Second")
				(args))
			(ty (name "Third")))
		(s-type-decl
			(header (name "Third")
				(args))
			(ty (name "U64")))
		(s-type-decl
			(header (name "Wrapper")
				(args
					(ty-var (raw "a"))))
			(ty-apply
				(ty (name "Inner"))
				(ty-var (raw "a"))))
		(s-type-decl
			(header (name "Inner")
				(args
					(ty-var (raw "a"))))
			(ty-apply
				(ty (name "List"))
				(ty-var (raw "a"))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "B"))
		(ty-lookup (name "Str") (builtin)))
	(s-alias-decl
		(ty-header (name "A"))
		(ty-lookup (name "B") (local)))
	(s-alias-decl
		(ty-header (name "Third"))
		(ty-lookup (name "U64") (builtin)))
	(s-alias-decl
		(ty-header (name "Second"))
		(ty-lookup (name "Third") (local)))
	(s-alias-decl
		(ty-header (name "First"))
		(ty-lookup (name "Second") (local)))
	(s-alias-decl
		(ty-header (name "Inner")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-apply (name "List") (builtin)
			(ty-rigid-var-lookup (ty-rigid-var (name "a")))))
	(s-alias-decl
		(ty-header (name "Wrapper")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-apply (name "Inner") (local)
			(ty-rigid-var-lookup (ty-rigid-var (name "a"))))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "B")
			(ty-header (name "B")))
		(alias (type "A")
			(ty-header (name "A")))
		(alias (type "Third")
			(ty-header (name "Third")))
		(alias (type "Second")
			(ty-header (name "Second")))
		(alias (type "First")
			(ty-header (name "First")))
		(alias (type "Inner(a)")
			(ty-header (name "Inner")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Wrapper(a)")
			(ty-header (name "Wrapper")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions))
~~~
