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
MUTUALLY RECURSIVE TYPE ALIASES - canon_revamp_mutual_type_aliases.md:2:1:2:6
UNDECLARED TYPE - canon_revamp_mutual_type_aliases.md:2:5:2:6
# PROBLEMS
**UNDECLARED TYPE**
The type _B_ is not declared in this scope.

This type is referenced here:
**canon_revamp_mutual_type_aliases.md:1:5:1:6:**
```roc
A : B
```
    ^


**MUTUALLY RECURSIVE TYPE ALIASES**
The type alias _B_ and _A_ form a recursive cycle.

Type aliases are transparent synonyms and cannot be mutually recursive. If you need recursive types, use nominal types (`:=`) instead.

This type is declared here:
**canon_revamp_mutual_type_aliases.md:2:1:2:6:**
```roc
B : A
```
^^^^^

And it references _A_ declared here:
**canon_revamp_mutual_type_aliases.md:1:1:1:6:**
```roc
A : B
```
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
	(type-module)
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
		(ty-lookup (name "A") (local))))
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
