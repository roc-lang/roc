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
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Where Clause Not Allowed In Type Declaration")
		(region (start 1 1) (end 8 19))
		(headline
			(text "You cannot define a ")
			(annotated code "where")
			(reflow " clause inside a type declaration."))
		(document
			(source-region (file "where_clauses_7.md") (start 1 1) (end 8 19) (annotation error) (line-text "Hash(a, hasher) # After header\n\t: # After colon\n\t\ta # After var\n\t\t\twhere [ # After where\n\t\t\t\ta.hash : hasher # After method\n\t\t\t\t\t-> # After arrow\n\t\t\t\t\t\thasher, # After first clause\n\t\t\t\thasher.Hasher]"))))
	(report
		(severity runtime_error)
		(title "Where Clause Not Allowed In Type Declaration")
		(region (start 10 1) (end 11 34))
		(headline
			(text "You cannot define a ")
			(annotated code "where")
			(reflow " clause inside a type declaration."))
		(document
			(source-region (file "where_clauses_7.md") (start 10 1) (end 11 34) (annotation error) (line-text "Decode(a) : a\n\twhere [a.decode : List(U8) -> a]")))))
~~~
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
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Hash")
				(args
					(ty-var (raw "a"))
					(ty-var (raw "hasher"))))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "hash")
					(args
						(ty-var (raw "hasher")))
					(ty-var (raw "hasher")))
				(alias (mod-of "hasher")
					(ty (name "Hasher")))))
		(s-type-decl
			(header (name "Decode")
				(args
					(ty-var (raw "a"))))
			(ty-var (raw "a"))
			(where
				(method (mod-of "a") (name "decode")
					(args
						(ty-apply
							(ty (name "List"))
							(ty (name "U8"))))
					(ty-var (raw "a")))))))
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
				hasher.Hasher,
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
