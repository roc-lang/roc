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
NIL
# PROBLEMS
NIL
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
