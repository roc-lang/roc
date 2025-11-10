# META
~~~ini
description=Multi-line doc comments on associated items
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [A, B].{
    ## This is a multi-line doc comment
    ## for a nested type declaration
    ## that spans multiple lines
    Bar := [X, Y, Z]

    ## Multi-line documentation
    ## for an associated value
    defaultValue = 42
}
~~~
# EXPECTED
UNUSED VARIABLE - nominal_associated_multiline_doc.md:9:5:9:17
# PROBLEMS
**UNUSED VARIABLE**
Variable `Foo.defaultValue` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Foo.defaultValue` to suppress this warning.
The unused variable is declared here:
**nominal_associated_multiline_doc.md:9:5:9:17:**
```roc
    defaultValue = 42
```
    ^^^^^^^^^^^^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,Comma,UpperIdent,Comma,UpperIdent,CloseSquare,
LowerIdent,OpAssign,Int,
CloseCurly,
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
					(ty (name "B"))))
			(associated
				(s-type-decl
					(header (name "Bar")
						(args))
					(ty-tag-union
						(tags
							(ty (name "X"))
							(ty (name "Y"))
							(ty (name "Z")))))
				(s-decl
					(p-ident (raw "defaultValue"))
					(e-int (raw "42")))))))
~~~
# FORMATTED
~~~roc
Foo := [A, B].{
	Bar := [X, Y, Z]
	defaultValue = 42
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.defaultValue"))
		(e-num (value "42")))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "A"))
			(ty-tag-name (name "B"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "X"))
			(ty-tag-name (name "Y"))
			(ty-tag-name (name "Z")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar"))))
	(expressions
		(expr (type "Num(_size)"))))
~~~
