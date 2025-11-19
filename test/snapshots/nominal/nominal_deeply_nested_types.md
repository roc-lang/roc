# META
~~~ini
description=Deeply nested types (3+ levels) in associated blocks
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    Bar := [Something].{
        Baz := [Else].{
            Qux := [Deep].{
                w = 1
            }
            z = 2
        }
        y = 3
    }
    x = 4
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
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
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
						(s-type-decl
							(header (name "Baz")
								(args))
							(ty-tag-union
								(tags
									(ty (name "Else"))))
							(associated
								(s-type-decl
									(header (name "Qux")
										(args))
									(ty-tag-union
										(tags
											(ty (name "Deep"))))
									(associated
										(s-decl
											(p-ident (raw "w"))
											(e-int (raw "1")))))
								(s-decl
									(p-ident (raw "z"))
									(e-int (raw "2")))))
						(s-decl
							(p-ident (raw "y"))
							(e-int (raw "3")))))
				(s-decl
					(p-ident (raw "x"))
					(e-int (raw "4")))))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	Bar := [Something].{
		Baz := [Else].{
			Qux := [Deep].{
				w = 1
			}
			z = 2
		}
		y = 3
	}
	x = 4
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.Bar.Baz.Qux.w"))
		(e-num (value "1")))
	(d-let
		(p-assign (ident "Foo.Bar.Baz.z"))
		(e-num (value "2")))
	(d-let
		(p-assign (ident "Foo.Bar.y"))
		(e-num (value "3")))
	(d-let
		(p-assign (ident "Foo.x"))
		(e-num (value "4")))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-tag-union
			(ty-tag-name (name "Something"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar.Baz"))
		(ty-tag-union
			(ty-tag-name (name "Else"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar.Baz.Qux"))
		(ty-tag-union
			(ty-tag-name (name "Deep")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)"))
		(patt (type "Num(_size)")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar")))
		(nominal (type "Foo.Bar.Baz")
			(ty-header (name "Foo.Bar.Baz")))
		(nominal (type "Foo.Bar.Baz.Qux")
			(ty-header (name "Foo.Bar.Baz.Qux"))))
	(expressions
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))
		(expr (type "Num(_size)"))))
~~~
