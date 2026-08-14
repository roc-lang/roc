# META
~~~ini
description=Two type aliases that reference each other - tests topological resolution via placeholders
type=snippet
~~~
# SOURCE
~~~roc
A : B
B : A
~~~
# EXPECTED
MUTUALLY RECURSIVE TYPE ALIASES - canon_revamp_mutual_type_aliases.md:1:1:1:6
MUTUALLY RECURSIVE TYPE ALIASES - canon_revamp_mutual_type_aliases.md:2:1:2:6
# PROBLEMS
── ✗ mutually recursive type aliases ─── canon_revamp_mutual_type_aliases.md:1:1

The type alias A and B form a recursive cycle.

A : B
^^^^^

Type aliases are transparent synonyms and cannot be mutually recursive. If you need recursive types, use nominal types (:=) instead.


And it references B declared in canon_revamp_mutual_type_aliases.md:2:1:

B : A
^^^^^

── ✗ mutually recursive type aliases ─── canon_revamp_mutual_type_aliases.md:2:1

The type alias B and A form a recursive cycle.

B : A
^^^^^

Type aliases are transparent synonyms and cannot be mutually recursive. If you need recursive types, use nominal types (:=) instead.


And it references A declared in canon_revamp_mutual_type_aliases.md:1:1:

A : B
^^^^^

# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
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
			(ty (name "B")))
		(s-type-decl
			(header (name "B")
				(args))
			(ty (name "A")))))
~~~
# FORMATTED
~~~roc
A : B

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
