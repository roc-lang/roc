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
**MUTUALLY RECURSIVE TYPE ALIASES**
The type alias _A_ and _B_ form a recursive cycle.

Type aliases are transparent synonyms and cannot be mutually recursive. If you need recursive types, use nominal types (`:=`) instead.

This type is declared here:
**canon_revamp_tag_payloads_are_type_dependencies.md:1:1:1:13:**
```roc
A : [Tag(B)]
```
^^^^^^^^^^^^

And it references _B_ declared here:
**canon_revamp_tag_payloads_are_type_dependencies.md:2:1:2:6:**
```roc
B : A
```
^^^^^


**MUTUALLY RECURSIVE TYPE ALIASES**
The type alias _B_ and _A_ form a recursive cycle.

Type aliases are transparent synonyms and cannot be mutually recursive. If you need recursive types, use nominal types (`:=`) instead.

This type is declared here:
**canon_revamp_tag_payloads_are_type_dependencies.md:2:1:2:6:**
```roc
B : A
```
^^^^^

And it references _A_ declared here:
**canon_revamp_tag_payloads_are_type_dependencies.md:1:1:1:13:**
```roc
A : [Tag(B)]
```
^^^^^^^^^^^^


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
