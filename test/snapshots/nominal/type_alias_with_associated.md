# META
~~~ini
description=Type alias with associated items produces error
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo : [A, B, C].{ x = 5 }
~~~
# EXPECTED
TYPE ALIAS WITH ASSOCIATED ITEMS - type_alias_with_associated.md:1:16:1:17
# PROBLEMS

┌──────────────────────────────────┐
│ TYPE ALIAS WITH ASSOCIATED ITEMS ├─ I was parsing a type alias, but only ───┐
└┬─────────────────────────────────┘  nominal types can have associated       │
 │                                    items.                                  │
 │                                                                            │
 │  Foo : [A, B, C].{ x = 5 }                                                 │
 │                 ‾                                                          │
 └──────────────────────────────────────── type_alias_with_associated.md:1:16 ┘

    Use `:=` to define a nominal type with associated items, or remove the
    associated item block from this alias.

    For example:
        Id := U64 implements [
            zero = @Id 0
        ]

    I found `.` here.

# TOKENS
~~~zig
UpperIdent,OpColon,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,LowerIdent,OpAssign,Int,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-mod)
	(statements
		(s-type-decl
			(header (name "Foo")
				(args))
			(ty-tag-union
				(tags
					(ty (name "A"))
					(ty (name "B"))
					(ty (name "C"))))
			(associated
				(s-decl
					(p-ident (raw "x"))
					(e-int (raw "5")))))))
~~~
# FORMATTED
~~~roc
Foo : [A, B, C].{
	x = 5
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.x"))
		(e-num (value "5")))
	(s-alias-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "A"))
			(ty-tag-name (name "B"))
			(ty-tag-name (name "C")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Dec")))
	(type_decls
		(alias (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Dec"))))
~~~
