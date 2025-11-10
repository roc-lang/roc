# META
~~~ini
description=Nominal type with single statement associated items
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [A, B, C].{ x = 5 }
~~~
# EXPECTED
UNUSED VARIABLE - nominal_type_with_associated_single_statement.md:1:20:1:21
# PROBLEMS
**UNUSED VARIABLE**
Variable `Foo.x` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Foo.x` to suppress this warning.
The unused variable is declared here:
**nominal_type_with_associated_single_statement.md:1:20:1:21:**
```roc
Foo := [A, B, C].{ x = 5 }
```
                   ^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,LowerIdent,OpAssign,Int,CloseCurly,
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
					(e-int (raw "5")))))))
~~~
# FORMATTED
~~~roc
Foo := [A, B, C].{
	x = 5
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.x"))
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
		(patt (type "Num(_size)")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Num(_size)"))))
~~~
