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

┌─────────────────────────────────┐
│ MUTUALLY RECURSIVE TYPE ALIASES ├─ The type alias `A` and `B` form a ───────┐
└┬────────────────────────────────┘  recursive cycle.                         │
 │                                                                            │
 │  A : [Tag(B)]                                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                              │
 └──────────────────── canon_revamp_tag_payloads_are_type_dependencies.md:1:1 ┘

    Type aliases are transparent synonyms and cannot be mutually recursive. If
    you need recursive types, use nominal types (`:=`) instead.

    This type is declared here:

    And it references B declared here:
      ┌───────────────────────────────────────────────────────────────────────┐
    2 │  B : A                                                                │
      │  ‾‾‾‾‾                                                                │
      └─────────────── canon_revamp_tag_payloads_are_type_dependencies.md:2:1 ┘


┌─────────────────────────────────┐
│ MUTUALLY RECURSIVE TYPE ALIASES ├─ The type alias `B` and `A` form a ───────┐
└┬────────────────────────────────┘  recursive cycle.                         │
 │                                                                            │
 │  B : A                                                                     │
 │  ‾‾‾‾‾                                                                     │
 └──────────────────── canon_revamp_tag_payloads_are_type_dependencies.md:2:1 ┘

    Type aliases are transparent synonyms and cannot be mutually recursive. If
    you need recursive types, use nominal types (`:=`) instead.

    This type is declared here:

    And it references A declared here:
      ┌───────────────────────────────────────────────────────────────────────┐
    1 │  A : [Tag(B)]                                                         │
      │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                         │
      └─────────────── canon_revamp_tag_payloads_are_type_dependencies.md:1:1 ┘

# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,UpperIdent,NoSpaceOpenRound,UpperIdent,CloseRound,CloseSquare,
UpperIdent,OpColon,UpperIdent,
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
		(alias (type "A")
			(ty-header (name "A")))
		(alias (type "B")
			(ty-header (name "B"))))
	(expressions))
~~~
