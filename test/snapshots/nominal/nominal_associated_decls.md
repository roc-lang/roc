# META
~~~ini
description=Nominal type with declarations in associated blocks
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something].{
        baz = 5
    }
    blah = 6
}
~~~
# EXPECTED
NIL
# PROBLEMS
NIL
# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
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
					(ty (name "Whatever"))))
			(associated
				(s-type-decl
					(header (name "Bar")
						(args))
					(ty-tag-union
						(tags
							(ty (name "Something"))))
					(associated
						(s-decl
							(p-ident (raw "baz"))
							(e-int (raw "5")))))
				(s-decl
					(p-ident (raw "blah"))
					(e-int (raw "6")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something].{
		baz = 5
	}
	blah = 6
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.Bar.baz"))
		(e-num (value "5")))
	(d-let
		(p-assign (ident "Foo.blah"))
		(e-num (value "6")))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "Something")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar"))))
	(expressions
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))))
~~~
