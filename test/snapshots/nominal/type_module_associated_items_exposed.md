# META
~~~ini
description=Type module with associated items should not report unused variables
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Blah].{
    Bar := {}.{
        baz = {}
    }

    stuff = {}
}
~~~
# EXPECTED
UNUSED VARIABLE - type_module_associated_items_exposed.md:3:9:3:12
UNUSED VARIABLE - type_module_associated_items_exposed.md:3:9:3:12
UNUSED VARIABLE - type_module_associated_items_exposed.md:6:5:6:10
# PROBLEMS
**UNUSED VARIABLE**
Variable `Bar.baz` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Bar.baz` to suppress this warning.
The unused variable is declared here:
**type_module_associated_items_exposed.md:3:9:3:12:**
```roc
        baz = {}
```
        ^^^


**UNUSED VARIABLE**
Variable `Bar.baz` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Bar.baz` to suppress this warning.
The unused variable is declared here:
**type_module_associated_items_exposed.md:3:9:3:12:**
```roc
        baz = {}
```
        ^^^


**UNUSED VARIABLE**
Variable `Foo.stuff` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Foo.stuff` to suppress this warning.
The unused variable is declared here:
**type_module_associated_items_exposed.md:6:5:6:10:**
```roc
    stuff = {}
```
    ^^^^^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
UpperIdent,OpColonEqual,OpenCurly,CloseCurly,Dot,OpenCurly,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
CloseCurly,
LowerIdent,OpAssign,OpenCurly,CloseCurly,
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
					(ty (name "Blah"))))
			(associated
				(s-type-decl
					(header (name "Bar")
						(args))
					(ty-record)
					(associated
						(s-decl
							(p-ident (raw "baz"))
							(e-record))))
				(s-decl
					(p-ident (raw "stuff"))
					(e-record))))))
~~~
# FORMATTED
~~~roc
Foo := [Blah].{
	Bar := {}.{
		baz = {}
	}
	stuff = {}
}
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.Bar.baz"))
		(e-empty_record))
	(d-let
		(p-assign (ident "Foo.stuff"))
		(e-empty_record))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Blah"))))
	(s-nominal-decl
		(ty-header (name "Foo.Bar"))
		(ty-record)))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "{}"))
		(patt (type "{}")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo")))
		(nominal (type "Foo.Bar")
			(ty-header (name "Foo.Bar"))))
	(expressions
		(expr (type "{}"))
		(expr (type "{}"))))
~~~
