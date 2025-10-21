# META
~~~ini
description=where_clauses (7)
type=snippet
~~~
# SOURCE
~~~roc
Hash(a, hasher) # After header
	: # After colon
		a # After var
			where [ # After where
				a.hash : hasher # After method
					-> # After arrow
						hasher, # After first clause
				hasher.Hasher]

Decode(a) : a
	where [a.decode : List(U8) -> a]
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_7.md:1:1:8:19
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_7.md:10:1:11:34
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_7.md:1:1:8:19:**
```roc
Hash(a, hasher) # After header
	: # After colon
		a # After var
			where [ # After where
				a.hash : hasher # After method
					-> # After arrow
						hasher, # After first clause
				hasher.Hasher]
```


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_7.md:10:1:11:34:**
```roc
Decode(a) : a
	where [a.decode : List(U8) -> a]
```


# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,
OpColon,
LowerIdent,
KwWhere,OpenSquare,
LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,
OpArrow,
LowerIdent,Comma,
LowerIdent,NoSpaceDotUpperIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,LowerIdent,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,LowerIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Hash")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "hasher"))))
			(ty-var (raw "a")))
		(s-type-decl
			(header (name "Decode")
				(args
					(ty-var (raw "a"))))
			(ty-var (raw "a")))))
~~~
# FORMATTED
~~~roc
Hash(a, hasher) # After header
	: # After colon
		a # After var
			where [ # After where
					a.hash : hasher # After method
					-> # After arrow
						hasher, # After first clause
				hasher.Hasher
			]

Decode(a) : a
	where [a.decode : List(U8) -> a]
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Hash")
			(ty-args
				(ty-rigid-var (name "a"))
				(ty-rigid-var (name "hasher"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
	(s-alias-decl
		(ty-header (name "Decode")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-rigid-var-lookup (ty-rigid-var (name "a")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Hash(a, hasher)")
			(ty-header (name "Hash")
				(ty-args
					(ty-rigid-var (name "a"))
					(ty-rigid-var (name "hasher")))))
		(alias (type "Decode(a)")
			(ty-header (name "Decode")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions))
~~~
