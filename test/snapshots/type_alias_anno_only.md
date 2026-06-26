# META
~~~ini
description=Type alias used in annotation-only declaration should resolve correctly
type=snippet
~~~
# SOURCE
~~~roc
MyType : Str

hey : MyType
~~~
# EXPECTED
DECLARATION HAS NO VALUE - type_alias_anno_only.md:3:1:3:13
# PROBLEMS

┌──────────────────────────┐
│ DECLARATION HAS NO VALUE ├─ This declaration has a type annotation but no ──┐
└┬─────────────────────────┘  implementation.                                 │
 │                                                                            │
 │  hey : MyType                                                              │
 │  ‾‾‾‾‾‾‾‾‾‾‾‾                                                              │
 └─────────────────────────────────────────────── type_alias_anno_only.md:3:1 ┘

    Add a value body here, or put hosted functions in a platform type module so
    they are published through the host boundary.

# TOKENS
~~~zig
UpperIdent,OpColon,UpperIdent,
LowerIdent,OpColon,UpperIdent,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
	(statements
		(s-type-decl
			(header (name "MyType")
				(args))
			(ty (name "Str")))
		(s-type-anno (name "hey")
			(ty (name "MyType")))))
~~~
# FORMATTED
~~~roc
NO CHANGE
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "hey"))
		(e-anno-only)
		(annotation
			(ty-lookup (name "MyType") (local))))
	(s-alias-decl
		(ty-header (name "MyType"))
		(ty-lookup (name "Str") (builtin))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "MyType")))
	(type_decls
		(alias (type "MyType")
			(ty-header (name "MyType"))))
	(expressions
		(expr (type "MyType"))))
~~~
