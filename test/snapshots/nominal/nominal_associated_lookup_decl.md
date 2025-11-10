# META
~~~ini
description=Referencing declaration from associated block
type=file:Foo.roc
~~~
# SOURCE
~~~roc
Foo := [Whatever].{
    bar = 42
}

useBar : U64
useBar = Foo.bar
~~~
# EXPECTED
UNUSED VARIABLE - nominal_associated_lookup_decl.md:2:5:2:8
# PROBLEMS
**UNUSED VARIABLE**
Variable `Foo.bar` is not used anywhere in your code.

If you don't need this variable, prefix it with an underscore like `_Foo.bar` to suppress this warning.
The unused variable is declared here:
**nominal_associated_lookup_decl.md:2:5:2:8:**
```roc
    bar = 42
```
    ^^^


# TOKENS
~~~zig
UpperIdent,OpColonEqual,OpenSquare,UpperIdent,CloseSquare,Dot,OpenCurly,
LowerIdent,OpAssign,Int,
CloseCurly,
LowerIdent,OpColon,UpperIdent,
LowerIdent,OpAssign,UpperIdent,NoSpaceDotLowerIdent,
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
				(s-decl
					(p-ident (raw "bar"))
					(e-int (raw "42")))))
		(s-type-anno (name "useBar")
			(ty (name "U64")))
		(s-decl
			(p-ident (raw "useBar"))
			(e-ident (raw "Foo.bar")))))
~~~
# FORMATTED
~~~roc
Foo := [Whatever].{
	bar = 42
}

useBar : U64
useBar = Foo.bar
~~~
# CANONICALIZE
~~~clojure
(can-ir
	(d-let
		(p-assign (ident "Foo.bar"))
		(e-num (value "42")))
	(d-let
		(p-assign (ident "useBar"))
		(e-lookup-local
			(p-assign (ident "Foo.bar")))
		(annotation
			(ty-lookup (name "U64") (builtin))))
	(s-nominal-decl
		(ty-header (name "Foo"))
		(ty-tag-union
			(ty-tag-name (name "Whatever")))))
~~~
# TYPES
~~~clojure
(inferred-types
	(defs
		(patt (type "Num(_size)"))
		(patt (type "Num(Int(Unsigned64))")))
	(type_decls
		(nominal (type "Foo")
			(ty-header (name "Foo"))))
	(expressions
		(expr (type "Num(_size)"))
		(expr (type "Num(Int(Unsigned64))"))))
~~~
