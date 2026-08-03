# META
~~~ini
description=Nominal type associated items with final expression produces error
type=snippet
~~~
# SOURCE
~~~roc
Foo := [A, B, C].{ x = 5
x }
~~~
# EXPECTED
UNEXPECTED ASSOCIATED EXPRESSION - nominal_associated_with_final_expression.md:2:1:2:2
# PROBLEMS

┌──────────────────────────────────┐
│ UNEXPECTED ASSOCIATED EXPRESSION ├─ I was parsing associated items for a ───┐
└┬─────────────────────────────────┘  nominal type, and I found a plain       │
 │                                    final expression.                       │
 │                                                                            │
 │  x }                                                                       │
 │  ‾                                                                         │
 └─────────────────────────── nominal_associated_with_final_expression.md:2:1 ┘

    Associated item blocks can contain associated types and values. Remove the
    trailing expression or turn it into a named associated value.

    For example:
        Id := U64 implements [
            zero = @Id 0
        ]

    I found `x` here.
    Names that start with lowercase letters are value names or record field
    names, depending on the surrounding syntax.

# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,LowerIdent,OpAssign,Int,
LowerIdent,CloseCurly,
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
					(e-int (raw "5")))
				(e-ident (raw "x"))))))
~~~
# FORMATTED
~~~roc
Foo := [A, B, C].{
	x = 5
	x
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "nominal_associated_with_final_expression.Foo.x"))
		(e-num (value "5")))
	(s-nominal-decl
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
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Dec"))))
~~~
