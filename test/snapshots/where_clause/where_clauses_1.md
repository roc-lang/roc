# META
~~~ini
description=where_clauses (1)
type=snippet
~~~
# SOURCE
~~~roc
Hash(a, hasher) : a
	where [a.hash : hasher -> hasher, hasher.Hasher]

Decode(a) : a where [a.decode : List(U8) -> a]
~~~
# EXPECTED
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_1.md:1:1:2:50
WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION - where_clauses_1.md:4:1:4:47
# PROBLEMS
**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_1.md:1:1:2:50:**
```roc
Hash(a, hasher) : a
	where [a.hash : hasher -> hasher, hasher.Hasher]
```


**WHERE CLAUSE NOT ALLOWED IN TYPE DECLARATION**
You cannot define a `where` clause inside a type declaration.

You're attempting do this here:
**where_clauses_1.md:4:1:4:47:**
```roc
Decode(a) : a where [a.decode : List(U8) -> a]
```
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,Comma,LowerIdent,CloseRound,OpColon,LowerIdent,
KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,LowerIdent,OpArrow,LowerIdent,Comma,LowerIdent,NoSpaceDotUpperIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,LowerIdent,KwWhere,OpenSquare,LowerIdent,NoSpaceDotLowerIdent,OpColon,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,OpArrow,LowerIdent,CloseSquare,
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
NO CHANGE
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
