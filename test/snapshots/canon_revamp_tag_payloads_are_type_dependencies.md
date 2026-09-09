# META
~~~ini
description=Tag payload type names count as type alias dependencies
type=snippet
~~~
# SOURCE
~~~roc
A : [Tag(B)]
B : A
~~~
# EXPECTED
MUTUALLY RECURSIVE TYPE ALIASES - canon_revamp_tag_payloads_are_type_dependencies.md:1:1:1:13
MUTUALLY RECURSIVE TYPE ALIASES - canon_revamp_tag_payloads_are_type_dependencies.md:2:1:2:6
# PROBLEMS
~~~clojure
(reports
	(report
		(severity runtime_error)
		(title "Mutually Recursive Type Aliases")
		(region (start 1 1) (end 1 13))
		(headline
			(reflow "The type alias ")
			(annotated code "A")
			(reflow " and ")
			(annotated code "B")
			(reflow " form a recursive cycle."))
		(document
			(reflow "Type aliases are transparent synonyms and cannot be mutually recursive. ")
			(reflow "If you need recursive types, use nominal types (")
			(annotated code ":=")
			(reflow ") instead.")
			(line-break)
			(line-break)
			(source-region (file "canon_revamp_tag_payloads_are_type_dependencies.md") (start 1 1) (end 1 13) (annotation error) (line-text "A : [Tag(B)]"))
			(line-break)
			(reflow "And it references ")
			(annotated type "B")
			(reflow " declared in ")
			(source-location
				(file "canon_revamp_tag_payloads_are_type_dependencies.md")
				(line 2)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "canon_revamp_tag_payloads_are_type_dependencies.md") (start 2 1) (end 2 6) (annotation dim) (line-text "B : A"))))
	(report
		(severity runtime_error)
		(title "Mutually Recursive Type Aliases")
		(region (start 2 1) (end 2 6))
		(headline
			(reflow "The type alias ")
			(annotated code "B")
			(reflow " and ")
			(annotated code "A")
			(reflow " form a recursive cycle."))
		(document
			(reflow "Type aliases are transparent synonyms and cannot be mutually recursive. ")
			(reflow "If you need recursive types, use nominal types (")
			(annotated code ":=")
			(reflow ") instead.")
			(line-break)
			(line-break)
			(source-region (file "canon_revamp_tag_payloads_are_type_dependencies.md") (start 2 1) (end 2 6) (annotation error) (line-text "B : A"))
			(line-break)
			(reflow "And it references ")
			(annotated type "A")
			(reflow " declared in ")
			(source-location
				(file "canon_revamp_tag_payloads_are_type_dependencies.md")
				(line 1)
				(column 1))
			(reflow ":")
			(line-break)
			(source-region (file "canon_revamp_tag_payloads_are_type_dependencies.md") (start 1 1) (end 1 13) (annotation dim) (line-text "A : [Tag(B)]")))))
~~~
# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,
UpperIdent,OpColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "A")
				(args))
			(ty-tag-union
				(tags
					(ty-apply
						(ty (name "Tag"))
						(ty (name "B"))))))
		(s-type-decl
			(header (name "B")
				(args))
			(ty (name "A")))))
~~~
# FORMATTED
~~~roc
A : [Tag(B)]

B : A
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(s-alias-decl
		(ty-header (name "A"))
		(ty-malformed))
	(s-alias-decl
		(ty-header (name "B"))
		(ty-malformed)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs)
	(type_decls
		(alias (type "Error")
			(ty-header (name "A")))
		(alias (type "Error")
			(ty-header (name "B"))))
	(expressions))
~~~
