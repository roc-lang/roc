# META
~~~ini
description=Type redeclaration in same scope should produce error
type=snippet
~~~
# SOURCE
~~~roc
Maybe(a) : [Some(a), None]

Maybe(a) : [Ok(a), Err]
~~~
# EXPECTED
TYPE REDECLARED - type_redeclaration_same_scope.md:3:1:3:24
# PROBLEMS

┌─────────────────┐
│ TYPE REDECLARED ├─ The type `Maybe` is being redeclared. ───────────────────┐
└┬────────────────┘                                                           │
 │                                                                            │
 │  Maybe(a) : [Ok(a), Err]                                                   │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                                   │
 └────────────────────────────────────── type_redeclaration_same_scope.md:3:1 ┘

    The redeclaration is here:

    But Maybe was already declared here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  Maybe(a) : [Some(a), None]                                           │
      │  ‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾‾                                           │
      └───────────────────────────────── type_redeclaration_same_scope.md:1:1 ┘

# TOKENS
~~~zig
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,CloseSquare,
UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,LowerIdent,CloseRound,Comma,UpperIdent,CloseSquare,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "Maybe")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Some"))
						(ty-var (raw "a")))
					(ty (name "None")))))
		(s-type-decl
			(header (name "Maybe")
				(args
					(ty-var (raw "a"))))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Ok"))
						(ty-var (raw "a")))
					(ty (name "Err")))))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "Maybe")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Some")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-tag-name (name "None"))))
	(s-alias-decl
		(ty-header (name "Maybe")
			(ty-args
				(ty-rigid-var (name "a"))))
		(ty-tag-union
			(ty-tag-name (name "Ok")
				(ty-rigid-var-lookup (ty-rigid-var (name "a"))))
			(ty-tag-name (name "Err")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Maybe(a)")
			(ty-header (name "Maybe")
				(ty-args
					(ty-rigid-var (name "a")))))
		(alias (type "Maybe(a)")
			(ty-header (name "Maybe")
				(ty-args
					(ty-rigid-var (name "a"))))))
	(expressions))
~~~
