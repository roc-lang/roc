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
EXPRESSION IN ASSOCIATED ITEMS - nominal_associated_with_final_expression.md:2:1:2:2
# PROBLEMS
**EXPRESSION IN ASSOCIATED ITEMS**
Associated items (such as types or methods) can only have associated types and values, not plain expressions.

To fix this, remove the expression at the very end.

**nominal_associated_with_final_expression.md:2:1:2:2:**
```roc
x }
```
^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,LowerIdent,OpAssign,Int,
LowerIdent,CloseCurly,
EndOfFile,
~~~
# PARSE
~~~clojure
(file
	(type-module)
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
		(patt (type "_a where [_b.from_int_digits : _arg -> _ret]")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "_a where [_b.from_int_digits : _arg -> _ret]"))))
~~~
