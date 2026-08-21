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
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Where Clause Not Allowed In Type Declaration")
		(region (start 1 1) (end 2 50))
		(headline
			(text "You cannot define a ")
			(annotated code "where")
			(reflow " clause inside a type declaration."))
		(document
			(source-region (file "where_clauses_1.md") (start 1 1) (end 2 50) (annotation error) (line-text "Hash(a, hasher) : a\n\twhere [a.hash : hasher -> hasher, hasher.Hasher]"))))
	(report
		(severity runtime_error)
		(title "Where Clause Not Allowed In Type Declaration")
		(region (start 4 1) (end 4 47))
		(headline
			(text "You cannot define a ")
			(annotated code "where")
			(reflow " clause inside a type declaration."))
		(document
			(source-region (file "where_clauses_1.md") (start 4 1) (end 4 47) (annotation error) (line-text "Decode(a) : a where [a.decode : List(U8) -> a]")))))
~~~
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
